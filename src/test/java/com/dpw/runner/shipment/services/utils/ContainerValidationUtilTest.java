package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerValidationUtilTest extends CommonMocks {

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

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
    void testValidateContainerNumberUniqueness_ForCreateBulk_Success() {
        ContainerV3Request containerV3Request1 = new ContainerV3Request();
        containerV3Request1.setContainerNumber("CNT123");
        ContainerV3Request containerV3Request2 = new ContainerV3Request();
        containerV3Request2.setContainerNumber("CNT456");
        containerValidationUtil.validateContainerNumberUniquenessForCreateBulk(List.of(containerV3Request1, containerV3Request2));
    }

    @Test
    void testValidateContainerNumberUniqueness_ForCreateBulk_Failure() {
        ContainerV3Request containerV3Request1 = new ContainerV3Request();
        containerV3Request1.setContainerNumber("CNT123");
        ContainerV3Request containerV3Request2 = new ContainerV3Request();
        containerV3Request2.setContainerNumber("CNT123");
        List<ContainerV3Request> requests = List.of(containerV3Request1, containerV3Request2);
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateContainerNumberUniquenessForCreateBulk(requests));
    }

    @Test
    void testValidateCreateBulkRequest() {
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        containerValidationUtil.validateCreateBulkRequest(containerV3Requests);
        assertNotNull(containerV3Requests);
    }

    @Test
    void testValidateCreateBulkRequestFailure() {
        assertThrows(IllegalArgumentException.class, () -> containerValidationUtil.validateCreateBulkRequest(null));
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
        assertThrows(ValidationException.class, () -> containerValidationUtil.validateCanAssignPackageToContainer(testShipment, Constants.CONTAINER));
    }

    @Test
    void testValidateCanAssignPackageToContainer2() {
        testShipment.setId(2L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        assertThrows(ValidationException.class, () -> containerValidationUtil.validateCanAssignPackageToContainer(testShipment, Constants.CONTAINER));
    }

    @Test
    void testValidateBeforeAssignContainer() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        Map<Long, ShipmentDetails> shipmentDetailsMap = Map.of(1L, testShipment);
        AssignContainerRequest request = new AssignContainerRequest();
        assertDoesNotThrow(() -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap, request, Constants.CONTAINER));
    }

    @Test
    void testValidateBeforeAssignContainer2() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>(Map.of(1L, testShipment));
        shipmentDetailsMap.put(2L, shipmentDetails);
        AssignContainerRequest request = new AssignContainerRequest();
        assertThrows(ValidationException.class, () -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap, request, Constants.CONTAINER));
    }

    @Test
    void testValidateBeforeAssignContainer3() {
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(null);
        testShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>(Map.of(1L, testShipment));
        shipmentDetailsMap.put(2L, shipmentDetails);
        AssignContainerRequest request = new AssignContainerRequest();
        assertDoesNotThrow(() -> containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap, request, Constants.CONTAINER));
    }

    @Test
    void testValidateOpenForAttachment_Success() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(1L).build(),
                Containers.builder().consolidationId(2L).build()
        );

        List<ConsolidationDetails> consolidationDetails = List.of(
                ConsolidationDetails.builder().openForAttachment(true).build(),
                ConsolidationDetails.builder().openForAttachment(true).build()
        );

        when(consolidationDetailsDao.findConsolidationsByIds(Set.of(1L, 2L)))
                .thenReturn(consolidationDetails);

        // Act & Assert
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(containersToDelete));
    }

    @Test
    void testValidateOpenForAttachment_ThrowsException_WhenOpenForAttachmentIsFalse() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(1L).build(),
                Containers.builder().consolidationId(2L).build()
        );

        List<ConsolidationDetails> consolidationDetails = List.of(
                ConsolidationDetails.builder().openForAttachment(true).build(),
                ConsolidationDetails.builder().openForAttachment(false).build() // This should trigger exception
        );

        when(consolidationDetailsDao.findConsolidationsByIds(Set.of(1L, 2L)))
                .thenReturn(consolidationDetails);

        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> containerValidationUtil.validateOpenForAttachment(containersToDelete));

        assertEquals("Changes in cargo is not allowed as Shipment Attachment Allowed value is Off",
                exception.getMessage());
    }

    @Test
    void testValidateOpenForAttachment_ThrowsException_WhenOpenForAttachmentIsNull() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(1L).build()
        );

        List<ConsolidationDetails> consolidationDetails = List.of(
                ConsolidationDetails.builder().openForAttachment(null).build() // null should be treated as false
        );

        when(consolidationDetailsDao.findConsolidationsByIds(Set.of(1L)))
                .thenReturn(consolidationDetails);

        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> containerValidationUtil.validateOpenForAttachment(containersToDelete));

        assertEquals("Changes in cargo is not allowed as Shipment Attachment Allowed value is Off",
                exception.getMessage());
    }

    @Test
    void testValidateOpenForAttachment_DoesNotThrow_WhenContainersListIsNull() {
        // Act & Assert
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(null));

        // Verify DAO is not called
        Mockito.verify(consolidationDetailsDao, Mockito.never()).findConsolidationsByIds(Mockito.any());
    }

    @Test
    void testValidateOpenForAttachment_DoesNotThrow_WhenContainersListIsEmpty() {
        // Act & Assert
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(Collections.emptyList()));

        // Verify DAO is not called
        Mockito.verify(consolidationDetailsDao, Mockito.never()).findConsolidationsByIds(Mockito.any());
    }

    @Test
    void testValidateOpenForAttachment_DoesNotThrow_WhenAllConsolidationIdsAreNull() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(null).build(),
                Containers.builder().consolidationId(null).build()
        );

        // Act & Assert
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(containersToDelete));

        // Verify DAO is not called since no valid consolidation IDs exist
        Mockito.verify(consolidationDetailsDao, Mockito.never()).findConsolidationsByIds(Mockito.any());
    }

    @Test
    void testValidateOpenForAttachment_HandlesNullConsolidationDetailsInList() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(1L).build()
        );

        List<ConsolidationDetails> consolidationDetails = new ArrayList<>();
        consolidationDetails.add(null); // Add null object to list

        when(consolidationDetailsDao.findConsolidationsByIds(Set.of(1L)))
                .thenReturn(consolidationDetails);

        // Act & Assert - should not throw since null objects are filtered out
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(containersToDelete));
    }

    @Test
    void testValidateOpenForAttachment_HandlesNullResponseFromDao() {
        // Arrange
        List<Containers> containersToDelete = List.of(
                Containers.builder().consolidationId(1L).build()
        );

        when(consolidationDetailsDao.findConsolidationsByIds(Set.of(1L)))
                .thenReturn(null); // DAO returns null

        // Act & Assert - should not throw when DAO returns null
        assertDoesNotThrow(() -> containerValidationUtil.validateOpenForAttachment(containersToDelete));
    }

    @Test
    void testFclAndLclThrowErrorMessage() {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        shipmentDetailsMap.put(1L, testShipment);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setId(2L);
        shipmentDetailsMap.put(2L, shipmentDetails);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        AssignContainerRequest request = new AssignContainerRequest();
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        shipmentPackIds.put(1L, List.of(1L));
        request.setShipmentPackIds(shipmentPackIds);
        assertThrows(ValidationException.class, () -> containerValidationUtil.fclAndLclThrowErrorMessage(shipmentDetailsMap, request));
    }

    @Test
    void testFclAndLclThrowErrorMessage2() {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        shipmentDetailsMap.put(1L, testShipment);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setId(2L);
        shipmentDetailsMap.put(2L, shipmentDetails);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        AssignContainerRequest request = new AssignContainerRequest();
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        shipmentPackIds.put(1L, List.of(1L));
        request.setShipmentPackIds(shipmentPackIds);
        assertThrows(ValidationException.class, () -> containerValidationUtil.fclAndLclThrowErrorMessage(shipmentDetailsMap, request));
    }

    @Test
    void testFclAndLclThrowErrorMessage3() {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        shipmentDetailsMap.put(1L, testShipment);
        ShipmentDetails shipmentDetails = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setId(2L);
        shipmentDetailsMap.put(2L, shipmentDetails);
        testShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        AssignContainerRequest request = new AssignContainerRequest();
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        shipmentPackIds.put(1L, List.of(1L));
        request.setShipmentPackIds(shipmentPackIds);
        assertThrows(ValidationException.class, () -> containerValidationUtil.fclAndLclThrowErrorMessage(shipmentDetailsMap, request));
    }

    @Test
    void testValidateShipmentForContainer_WithConsolidationList_ShouldPass() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setJobType("EXPORT");
        shipmentDetails.setShipmentId("SHIP001");
        shipmentDetails.setConsolidationList(Set.of(new ConsolidationDetails())); // Non-null consolidation list

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerValidationUtil.validateShipmentForContainer(shipmentDetails));
    }

    @Test
    void testValidateShipmentForContainer_WithDRTJobType_ShouldPass() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT); // DRT job type
        shipmentDetails.setShipmentId("SHIP002");
        shipmentDetails.setConsolidationList(null); // Null consolidation list but DRT type

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerValidationUtil.validateShipmentForContainer(shipmentDetails));
    }

    @Test
    void testValidateShipmentForContainer_WithNullConsolidationAndNonDRTJobType_ShouldThrowException() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setJobType("EXPORT"); // Non-DRT job type
        shipmentDetails.setShipmentId("SHIP003");
        shipmentDetails.setConsolidationList(null); // Null consolidation list

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentForContainer(shipmentDetails));

        assertEquals("Shipment: SHIP003 must be attached with consolidation to create container",
                exception.getMessage());
    }

// Test for validateShipmentCargoType method

    @Test
    void testValidateShipmentCargoType_WithSeaFCL_ShouldPass() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock the utility method to return true for Sea FCL
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_SEA, Constants.CARGO_TYPE_FCL)).thenReturn(true);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_SEA, Constants.CARGO_TYPE_FCL)).thenReturn(false);

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));
    }

    @Test
    void testValidateShipmentCargoType_WithRoadFCLorFTL_ShouldPass() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock the utility method to return true for Road FCL/FTL
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_ROA, Constants.CARGO_TYPE_FCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_ROA, Constants.CARGO_TYPE_FCL)).thenReturn(true);

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));
    }

    @Test
    void testValidateShipmentCargoType_WithInvalidSeaCargoType_ShouldThrowException() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL); // Invalid for SEA

        // Mock the utility methods to return false
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertEquals(String.format("Invalid cargoType: %s for transportMode: %s. Expected: %s.",
                        Constants.SHIPMENT_TYPE_LCL, Constants.TRANSPORT_MODE_SEA, "FCL"),
                exception.getMessage());
    }

    @Test
    void testValidateShipmentCargoType_WithInvalidRoadCargoType_ShouldThrowException() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL); // Invalid for ROAD

        // Mock the utility methods to return false
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_ROA, Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_ROA, Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertEquals(String.format("Invalid cargoType: %s for transportMode: %s. Expected: %s.",
                        Constants.SHIPMENT_TYPE_LCL, Constants.TRANSPORT_MODE_ROA, "FCL or FTL"),
                exception.getMessage());
    }

    @Test
    void testValidateShipmentCargoType_WithInvalidTransportMode_ShouldThrowException() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR"); // Invalid transport mode
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock the utility methods to return false
        when(commonUtils.isSeaFCL("AIR", Constants.CARGO_TYPE_FCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL("AIR", Constants.CARGO_TYPE_FCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertEquals(String.format("Invalid cargoType: %s for transportMode: %s. Expected: %s.",
                        Constants.CARGO_TYPE_FCL, "AIR", "Transport mode must be SEA/ROAD"),
                exception.getMessage());
    }

// Test for getExpectedCargoTypeForTransportMode method coverage (private method tested through public method)

    @Test
    void testGetExpectedCargoTypeForTransportMode_SeaTransportMode_ReturnsFC() {
        // This tests the private method through the public validateShipmentCargoType method
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA.toLowerCase()); // Test case sensitivity
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        // Mock the utility methods to return false to trigger the exception path
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_SEA.toLowerCase(), Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_SEA.toLowerCase(), Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertTrue(exception.getMessage().contains("Expected: FCL"));
    }

    @Test
    void testGetExpectedCargoTypeForTransportMode_RoadTransportMode_ReturnsFCLorFTL() {
        // This tests the private method through the public validateShipmentCargoType method
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA.toLowerCase()); // Test case sensitivity
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        // Mock the utility methods to return false to trigger the exception path
        when(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_ROA.toLowerCase(), Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL(Constants.TRANSPORT_MODE_ROA.toLowerCase(), Constants.SHIPMENT_TYPE_LCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertTrue(exception.getMessage().contains("Expected: FCL or FTL"));
    }

    @Test
    void testGetExpectedCargoTypeForTransportMode_UnknownTransportMode_ReturnsDefaultMessage() {
        // This tests the private method through the public validateShipmentCargoType method
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("UNKNOWN");
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock the utility methods to return false to trigger the exception path
        when(commonUtils.isSeaFCL("UNKNOWN", Constants.CARGO_TYPE_FCL)).thenReturn(false);
        when(commonUtils.isRoadFCLorFTL("UNKNOWN", Constants.CARGO_TYPE_FCL)).thenReturn(false);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> containerValidationUtil.validateShipmentCargoType(shipmentDetails));

        assertTrue(exception.getMessage().contains("Expected: Transport mode must be SEA/ROAD"));
    }

}
