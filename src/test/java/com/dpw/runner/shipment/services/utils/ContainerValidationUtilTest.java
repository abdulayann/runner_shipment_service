package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerValidationUtilTest extends CommonMocks {

    private static ObjectMapper objectMapper;

    private static ShipmentDetails testShipment;

    private static Packing testPacking;

    private static Containers testContainer;

    private static JsonTestUtility jsonTestUtility;

    @InjectMocks
    private ContainerValidationUtil containerValidationUtil;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            testContainer = jsonTestUtility.getTestContainer();
            objectMapper = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        testPacking = jsonTestUtility.getTestPacking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).build());
        MockitoAnnotations.initMocks(this);
    }

    @ParameterizedTest
    @ValueSource(strings = {"ABCD123456", "", "123"})
    void testValidateContainerNumberUniqueness_Success(String containerNumber) {
        containerValidationUtil.validateContainerNumberUniqueness(containerNumber, List.of(testContainer));
        assertNotNull(containerNumber);
    }

    @Test
    void testValidateContainerNumberUniqueness_Failure() {
        String containerNumber = "ABCD123456";
        testContainer.setContainerNumber(containerNumber);
        List<Containers> containers = List.of(testContainer, testContainer);
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateContainerNumberUniqueness(containerNumber, containers));
    }

    @Test
    void testValidateUpdateBulkRequest() {
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        containerValidationUtil.validateUpdateBulkRequest(containerV3Requests);
        assertNotNull(containerV3Requests);
    }

    @Test
    void testValidateUpdateBulkRequest1() {
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateUpdateBulkRequest(null));
    }

    @Test
    void testValidateUpdateBulkRequest2() {
        List<ContainerV3Request> containerV3Requests = new ArrayList<>();
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateUpdateBulkRequest(containerV3Requests));
    }

    @Test
    void testValidateUpdateBulkRequest3() {
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateUpdateBulkRequest(containerV3Requests));
    }

    @Test
    void testValidateDeleteBulkRequest() {
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateDeleteBulkRequest(null));
    }

    @Test
    void testValidateDeleteBulkRequest1() {
        List<ContainerV3Request> containerV3Requests = new ArrayList<>();
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateDeleteBulkRequest(containerV3Requests));
    }

    @Test
    void testValidateDeleteBulkRequest3() {
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).openForAttachment(true).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        assertDoesNotThrow(() -> containerValidationUtil.validateDeleteBulkRequest(containerV3Requests));
    }

    @Test
    void testValidateCanAssignPackageToContainer() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        assertThrows(ValidationException.class, () -> containerValidationUtil.validateCanAssignPackageToContainer(testShipment));
    }

    @Test
    void testValidateCanAssignPackageToContainer2() {
        testShipment.setId(2L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        assertDoesNotThrow(() -> containerValidationUtil.validateCanAssignPackageToContainer(testShipment));
    }

    @Test
    void testValidateBeforeAssignContainer() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        Map<Long, ShipmentDetails> shipmentDetailsMap = Map.of(1L, testShipment);
        assertDoesNotThrow(() -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap));
    }

    @Test
    void testValidateBeforeAssignContainer2() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>(Map.of(1L, testShipment));
        shipmentDetailsMap.put(2L, shipmentDetails);
        assertThrows(ValidationException.class, () -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap));
    }

    @Test
    void testValidateBeforeAssignContainer3() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        testShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>(Map.of(1L, testShipment));
        shipmentDetailsMap.put(2L, shipmentDetails);
        assertDoesNotThrow(() -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap));
    }

}
