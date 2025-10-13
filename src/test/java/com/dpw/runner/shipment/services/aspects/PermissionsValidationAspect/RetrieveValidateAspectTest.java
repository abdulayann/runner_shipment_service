package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.aspectj.lang.JoinPoint;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RetrieveValidateAspectTest {

    private RetrieveValidateAspect retrieveValidateAspect;

    @Mock
    private JoinPoint joinPoint;

    private static ObjectMapper objectMapper;

    @BeforeAll
    public static void init() {
        objectMapper = JsonTestUtility.getMapper();
    }


    @BeforeEach
    public void setUp() {
        retrieveValidateAspect = new RetrieveValidateAspect();
    }


    @ParameterizedTest
    @ValueSource(strings = {"Shipments:Retrive:All Shipment:AllShipmentRetrive", "Operations:Shipments:AIR:Export:Create",
            "Operations:Shipments:AIR:Export:View", "Operations:Shipments:AIR:Export:Modify", "Operations:Shipments:AIR:Export:Cancel"})
    void testCreateShipmentAspectShouldSuccessfullyReturn(String permission) throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList(permission)));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("AIR");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("EXP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateShipmentRetrieveV2(objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class));
        assert (true);
    }

    @Test
    void testCreateShipmentAspect2() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("SEA");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("EXP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateShipmentRetrieveV2(shipmentDetailsResponse));
    }

    @Test
    void testCreateShipmentAspect3() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("AIR");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("EXP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateShipmentRetrieveV2(shipmentDetailsResponse));
    }

    @Test
    void testCreateShipmentAspectV3() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("AIR");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("EXP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(mockShipment, ShipmentRetrieveLiteResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateShipmentRetrieveV3(shipmentDetailsResponse));
    }

    @Test
    void testCreateShipmentAspect4() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("AIR");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("IMP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateShipmentRetrieveV2(objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class));
        assertNotNull(mockShipment);
    }

    @Test
    void testCreateShipmentAspect5() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateShipmentRetrieveV2(objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class));
        assertNotNull(mockShipment);
    }

    @Test
    void testCreateShipmentAspectException() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>());
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        mockShipment.setTransportMode("SEA");
        mockShipment.setIsDomestic(false);
        mockShipment.setDirection("EXP");
        mockShipment.setShipmentType("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateShipmentRetrieveV2(shipmentDetailsResponse));
    }

    @Test
    void testCreateConsolidationAspect() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:All Consolidation:AllConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("SEA");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("EXP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateConsolidationRetrieveForV2(objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class));
        assert (true);
    }

    @Test
    void testCreateConsolidationAspectV3() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:All Consolidation:AllConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("SEA");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("EXP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateConsolidationRetrieveV3(objectMapper.convertValue(mockConsolidation, ConsolidationDetailsV3Response.class));
        assert (true);
    }

    @Test
    void testCreateConsolidationAspect2() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:Air Consolidation:ImportAirConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("SEA");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("EXP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateConsolidationRetrieveForV2(consolidationDetailsResponse));
    }

    @Test
    void testCreateConsolidationAspect3() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:Air Consolidation:ImportAirConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("AIR");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("EXP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateConsolidationRetrieveForV2(consolidationDetailsResponse));
    }

    @Test
    void testCreateConsolidationAspect4() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:Air Consolidation:ImportAirConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("AIR");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("IMP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateConsolidationRetrieveForV2(objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class));
        assertNotNull(mockConsolidation);
    }

    @Test
    void testCreateConsolidationAspect5() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(List.of("Consolidations:Retrive:Air Consolidation:ImportAirConsolidationRetrive")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateConsolidationRetrieveForV2(objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class));
        assertNotNull(mockConsolidation);
    }

    @Test
    void testCreateConsolidationAspectException() throws ValidationException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>());
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        mockConsolidation.setTransportMode("SEA");
        mockConsolidation.setIsDomestic(false);
        mockConsolidation.setShipmentType("EXP");
        mockConsolidation.setContainerCategory("FCL");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        retrieveValidateAspect = new RetrieveValidateAspect();
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsResponse.class);
        assertThrows(ValidationException.class, () -> retrieveValidateAspect.validateConsolidationRetrieveForV2(consolidationDetailsResponse));
    }
}