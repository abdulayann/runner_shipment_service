//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
//import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
//import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
//import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
//import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import com.fasterxml.jackson.databind.ObjectMapper;
//import org.junit.jupiter.api.*;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.runner.RunWith;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.data.domain.PageRequest;
//import org.springframework.data.jpa.domain.Specification;
//import org.springframework.orm.jpa.JpaObjectRetrievalFailureException;
//import org.springframework.test.context.DynamicPropertyRegistry;
//import org.springframework.test.context.DynamicPropertySource;
//import org.springframework.test.context.TestPropertySource;
//import org.springframework.test.context.junit4.SpringRunner;
//import org.springframework.transaction.annotation.Transactional;
//import org.testcontainers.containers.PostgreSQLContainer;
//import org.testcontainers.junit.jupiter.Container;
//import org.testcontainers.junit.jupiter.Testcontainers;
//
//import java.io.IOException;
//import java.util.List;
//import java.util.Map;
//
//import static org.junit.jupiter.api.Assertions.*;
//
//@RunWith(SpringRunner.class)
//@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//class ConsolidationDaoTest {
//    @Container
//    private static final PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
//
//    static {
//        postgresContainer.withDatabaseName("integration-tests-db")
//                .withUsername("sa")
//                .withPassword("sa");
//        postgresContainer.start();
//    }
//
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        postgresContainer.start();
//        jsonTestUtility = new JsonTestUtility();
//    }
//
//    @AfterAll
//    static void afterAll() {
//        postgresContainer.stop();
//    }
//
//    @DynamicPropertySource
//    static void dynamicConfiguration(DynamicPropertyRegistry registry){
//        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
//        registry.add("spring.datasource.username", postgresContainer::getUsername);
//        registry.add("spring.datasource.password", postgresContainer::getPassword);
//    }
//
//    @Autowired
//    private IConsolidationDetailsDao consolidationsDao;
//
//    private static JsonTestUtility jsonTestUtility;
//    private static ConsolidationDetails testConsol;
//
//    private static ObjectMapper objectMapperTest;
//    private static ConsolidationDetailsRequest testConsolRequest;
//
//    static {
//        try {
//            objectMapperTest = JsonTestUtility.getMapper();
//            jsonTestUtility = new JsonTestUtility();
//        } catch (IOException e) {
//            throw new RuntimeException(e);
//        }
//    }
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//        TenantContext.setCurrentTenant(1);
//        testConsol = jsonTestUtility.getTestConsolidation();
//        testConsolRequest = objectMapperTest.convertValue(testConsol , ConsolidationDetailsRequest.class);
//        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
//        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
//        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
//        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
//    }
//
//    @Test
//    void save() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        ConsolidationDetails result = consolidationsDao.save(consol, false);
//        assertEquals(result, consol);
//    }
//
//    @Test
//    void update() {
//        var consolToBeSaved = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consolToBeSaved , false);
//        result.setBol("SAMPLE_BOL");
//        var newResult = consolidationsDao.update(result, false);
//        assertEquals(newResult.getBol(), result.getBol());
//        assertEquals(result.getGuid(), newResult.getGuid());
//    }
//
//    @Test
//    @Disabled
//    void findAll() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol , false);
//        Specification<ConsolidationDetails> spec =  (root, query, criteriaBuilder) -> {
//            return criteriaBuilder.equal(root.get("transportMode"), "SEA");
//        };
//        var consolList = consolidationsDao.findAll(spec, PageRequest.of(0 , 10));
//        assertFalse(consolList.isEmpty());
//        assertEquals(consolList.stream().toList().get(0).getContainerCategory() , result.getContainerCategory());
//    }
//
//    @Test
//    void findById() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol , false);
//        var consolList = consolidationsDao.findById(result.getId());
//        assertFalse(consolList.isEmpty());
//        assertEquals(consolList.get().getGuid() , result.getGuid());
//    }
//
//    @Test
//    void delete() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol, false);
//        consolidationsDao.delete(result);
//        assertThrows(JpaObjectRetrievalFailureException.class , () -> consolidationsDao.findById(result.getId()));
//    }
//
//    @Test
//    void saveAll() {
//        var consolList = List.of(jsonTestUtility.getTestNewConsolidation());
//        var result = consolidationsDao.saveAll(consolList);
//        assertFalse(result.isEmpty());
//        assertNotNull(result.get(0).getGuid());
//    }
//
//    @Test
//    void findByGuid() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol, false);
//        var newResult = consolidationsDao.findByGuid(result.getGuid());
//        assertTrue(newResult.isPresent());
//        assertEquals(newResult.get().getTransportMode(), result.getTransportMode());
//        assertEquals(newResult.get().getId(), result.getId());
//        assertEquals(newResult.get().getGuid() , result.getGuid());
//    }
//
//    @Test
//    void findByBol() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        consol.setBol("TEST_BOL");
//        var result = consolidationsDao.save(consol, false);
//        var newResult = consolidationsDao.findByBol("TEST_BOL");
//        assertFalse(newResult.isEmpty());
//        assertEquals(newResult.get(0).getGuid() , result.getGuid());
//    }
//
//    @Test
//    void findByReferenceNumber() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        consol.setReferenceNumber("TEST_REFERENCE_NUMBER");
//        var result = consolidationsDao.save(consol, false);
//        var newResult = consolidationsDao.findByReferenceNumber("TEST_REFERENCE_NUMBER");
//        assertFalse(newResult.isEmpty());
//        assertEquals(newResult.get(0).getGuid() , result.getGuid());
//    }
//
//    @Test
//    void findMaxId() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol, false);
//        var newResult = consolidationsDao.findMaxId();
//        assertEquals(newResult , result.getId());
//    }
//
//
//    @Test
//    @Transactional
//    @Disabled
//    void updateConsoleBookingFields() {
//        var consol = jsonTestUtility.getTestNewConsolidation();
//        var result = consolidationsDao.save(consol, false);
//        ConsoleBookingRequest request = ConsoleBookingRequest.builder()
//                .guid(result.getGuid())
//                .bookingId("TEST_ID")
//                .bookingNumber("TEST_NUM")
//                .bookingStatus("TEST_STATUS")
//                .build();
//        var newResult = consolidationsDao.updateConsoleBookingFields(request);
//        assertTrue(newResult == 1);
//        var updatedConsol = consolidationsDao.findByGuid(result.getGuid());
//        assertTrue(updatedConsol.isPresent());
//        assertEquals(updatedConsol.get().getBookingId(), "TEST_ID");
//        assertEquals(updatedConsol.get().getBookingNumber(), "TEST_NUM");
//        assertEquals(updatedConsol.get().getBookingStatus(), "TEST_STATUS");
//    }
//}