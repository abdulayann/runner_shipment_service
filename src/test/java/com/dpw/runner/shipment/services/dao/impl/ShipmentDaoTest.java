package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@Execution(CONCURRENT)
class ShipmentDaoTest {


    @Container
    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");

    static {
        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
                .withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa");
        postgresContainer.start();
    }

    @Autowired
    private ShipmentDao shipmentDao;

    @MockBean
    private V1ServiceImpl mockV1Service;
    @MockBean
    private IConsolidationDetailsDao consolidationDetailsDao;
    @MockBean
    private JsonHelper jsonHelper;
    @MockBean
    private ValidatorUtility validatorUtility;

    private static ShipmentDetails mockShipment;
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;


    @BeforeAll
    static void beforeAll() throws IOException {
        postgresContainer.start();
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @AfterAll
    static void afterAll() {
        postgresContainer.stop();
    }

    @DynamicPropertySource
    static void dynamicConfiguration(DynamicPropertyRegistry registry){
        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
        registry.add("spring.datasource.username", postgresContainer::getUsername);
        registry.add("spring.datasource.password", postgresContainer::getPassword);
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        mockShipment = jsonTestUtility.getJson("NEW_SHIPMENT", ShipmentDetails.class);

        mockShipment.getCarrierDetails().setId(null);
        mockShipment.getClient().setId(null);
        mockShipment.getConsigner().setId(null);
        mockShipment.getConsignee().setId(null);
        mockShipment.getAdditionalDetails().getNotifyParty().setId(null);


    }


    @Test
    void testSave_create_success() {
        // Create a ShipmentDetails object
        ShipmentDetails shipmentDetails = mockShipment;
        shipmentDetails.setHouseBill("testSave_create_success");
        shipmentDetails.getCarrierDetails().setFlightNumber("678C");

        Object carrierResponses = List.of(CarrierResponse.builder().iATACode("mockIata").build());
        V1DataResponse carrierMasterDataResponse = new V1DataResponse();
        carrierMasterDataResponse.setEntities(carrierResponses);

        when(mockV1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(carrierMasterDataResponse);

        // Call the save method of ShipmentDao
        try {
            ShipmentDetails savedShipment = shipmentDao.save(shipmentDetails, false);
            // Assert that the savedShipment is not null and contains expected values
            assertNotNull(savedShipment);
            assertNotNull(savedShipment.getId());
            assertNotNull(savedShipment.getGuid());
        } catch (RunnerException e) {
            // Handle the exception if necessary
            fail("Exception occurred: " + e.getMessage());
        }
    }

    @Test
    void testSave_fails_on_locked_shipment() {
        // Create a ShipmentDetails object
        ShipmentDetails shipmentDetails = mockShipment;
        shipmentDetails.setIsLocked(true);
        Exception e = assertThrows(ValidationException.class, () -> shipmentDao.update(shipmentDetails, false));
        // Assert that the savedShipment is not null and contains expected values
        assertNotNull(e);
        assertEquals(ShipmentConstants.SHIPMENT_LOCKED, e.getMessage());
    }

    @Test
    void update_fails_when_no_shipment_present_for_input_id() {
        ShipmentDetails shipmentDetails = mockShipment;
        shipmentDetails.setId(1000L);

        try {
            shipmentDao.update(shipmentDetails, false);
            // If no exception is thrown, fail the test
            fail("Expected DataRetrievalFailureException was not thrown");
        } catch (DataRetrievalFailureException e) {
            // Assert that the exception message is correct
            assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
        }
    }

    // Added via codium
    @Test
    @Disabled
    public void test_update_valid_data() throws JsonProcessingException {
        ShipmentDao shipmentDao = new ShipmentDao();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        // Set valid data for shipmentDetails
        // ...

        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(shipmentDetails));

        boolean fromV1Sync = false;
        ShipmentDetails updatedShipment = shipmentDao.update(shipmentDetails, fromV1Sync);
        // Assert that the updatedShipment is not null and contains the expected data
        // ...
    }

    @Test
    void saveAll_success() throws RunnerException, JsonProcessingException {
        ShipmentDetails mockShipment1 = mockShipment;
        mockShipment1.setHouseBill("saveAll_success_mockShipment1");
        ShipmentDetails mockShipment2 = jsonTestUtility.getCopyObject(mockShipment, ShipmentDetails.class);
        mockShipment1.setHouseBill("saveAll_success_mockShipment2");
        ShipmentDetails mockShipment3 = jsonTestUtility.getCopyObject(mockShipment, ShipmentDetails.class);
        mockShipment1.setHouseBill("saveAll_success_mockShipment3");

        shipmentDao.saveAll(List.of(mockShipment1, mockShipment2, mockShipment3));

        // Assert
        assertNotNull(mockShipment1);
        assertNotNull(mockShipment1.getId());
        assertNotNull(mockShipment2);
        assertNotNull(mockShipment2.getId());
        assertNotNull(mockShipment3);
        assertNotNull(mockShipment3.getId());

    }

    @Test
    void applyShipmentValidations_fails_on_multiple_consolidations_linked_to_shipment() {
        ShipmentDetails shipmentDetails = mockShipment;
        ConsolidationDetails linkedConsolidation = new ConsolidationDetails();

        shipmentDetails.setConsolidationList(List.of(linkedConsolidation, linkedConsolidation));

        String errorMessage = "Multiple consolidations are attached to the shipment, please verify.";

        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        assertEquals(1, errors.size());
        assertTrue(errors.contains(errorMessage));
    }

    @Test
    void applyShipmentValidations_fails_on_duplicate_leg_numbers() throws JsonProcessingException {
        ShipmentDetails shipmentDetails = mockShipment;
        Routings routing = new Routings();
        routing.setId(1L);
        routing.setLeg(1L);
        Routings duplicateRouting = jsonTestUtility.getCopyObject(routing, Routings.class);
        duplicateRouting.setId(2L);

        shipmentDetails.setRoutingsList(List.of(routing, duplicateRouting));

        String errorMessage = "Leg No in routings cannot be same for two different legs";

        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        assertEquals(1, errors.size());
        assertTrue(errors.contains(errorMessage));
    }

    @Test
    void applyShipmentValidations_fails_on_duplicate_container_number() throws JsonProcessingException {
        ShipmentDetails shipmentDetails = mockShipment;
        String duplicateContainerNumber = "duplicateContainerNumber";
        Containers containers = new Containers();
        containers.setId(1L);
        containers.setContainerNumber(duplicateContainerNumber);
        Containers duplicateContainer = jsonTestUtility.getCopyObject(containers, Containers.class);
        duplicateContainer.setId(2L);

        shipmentDetails.setContainersList(List.of(containers, duplicateContainer));

        String errorMessage = "Container Number cannot be same for two different containers";

        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        assertEquals(1, errors.size());
        assertTrue(errors.contains(errorMessage));
    }

    @Test
    void applyShipmentValidations_fails_on_duplicate_party_types_shipment_addresses() throws JsonProcessingException {
        ShipmentDetails shipmentDetails = mockShipment;

        String duplicateType = "TYPE-1";
        Parties address = new Parties();
        address.setType(duplicateType);
        Parties duplicateAddress = jsonTestUtility.getCopyObject(address, Parties.class);

        shipmentDetails.setShipmentAddresses(List.of(address, duplicateAddress));

        String types = String.join(", ", Set.of(duplicateType));
        String message = " is a duplicate Party Type.";
        String errorMessage = types + message;

        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        assertEquals(1, errors.size());
        assertTrue(errors.contains(errorMessage));
    }

    @Test
    void applyShipmentValidations_fails_when_not_linked_to_consol_with_same_master_bill() {
        ShipmentDetails shipmentDetails = mockShipment;


        String consolNumber = "CONS0001";
        String message = "%s %s is linked to consolidation %s. Please attach the shipment to that consolidation.";
        String masterBillType = Constants.MBL;
        String masterBill = "MBL#123";
        shipmentDetails.setMasterBill(masterBill);

        String errorMessage = String.format(message, masterBillType, masterBill, consolNumber);
        PageImpl<ConsolidationDetails> consolPage = new PageImpl<>(List.of(
                ConsolidationDetails.builder().consolidationNumber(consolNumber).bol(masterBill).build()
        ));
        // Mock
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(consolPage);

        // Test
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        // Assert
        assertEquals(1, errors.size());
        assertTrue(errors.contains(errorMessage));
    }

    @Test
    void applyShipmentValidations_restricted_unlocation_validation() {}

    @Test
    void applyShipmentValidations_bl_reference_number_validation() {}


    @Test
    void applyShipmentValidations_success() {
        ShipmentDetails shipmentDetails = mockShipment;
        Set<String> errors = shipmentDao.applyShipmentValidations(shipmentDetails, null);

        assertEquals(0, errors.size());
    }

    @Test
    void saveJobStatus_Success() throws RunnerException {

        ShipmentDetails shipmentDetails = mockShipment;
        shipmentDetails.setJobStatus(null);
        PermissionsContext.setPermissions(List.of(
                "Shipments:Retrive:All Shipment:AllShipmentRetrive","all-shipmentRetrieve"
        ));

        String jobStatus = "COM";

        shipmentDao.save(shipmentDetails, false);
        assertNull(shipmentDetails.getJobStatus());

        Long id = shipmentDetails.getId();

        shipmentDao.saveJobStatus(id, jobStatus);
        Optional<ShipmentDetails> optional = shipmentDao.findById(id);
        if(optional.isEmpty()){
            fail("Shipment not saved");
        }
        assertEquals(jobStatus, optional.get().getJobStatus());
    }


}
