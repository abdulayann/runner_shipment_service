package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.aspectj.lang.JoinPoint;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class UpdateValidateAspectTest {

    private UpdateValidateAspect updateValidateAspect;

    @Mock
    private JoinPoint joinPoint;

    @BeforeEach
    public void setUp() {
        updateValidateAspect = new UpdateValidateAspect();
    }


    @Test
    void testUpdateShipmentAspect() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Update:All Shipment:AllShipmentUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel);
        assert (true);
    }

    @Test
    void testUpdateShipmentAspect2() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Update:Air Shipment:ImportAirShipmentUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel));
    }

    @Test
    void testUpdateShipmentAspect3() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Update:Air Shipment:ImportAirShipmentUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel));
    }

    @Test
    void testUpdateShipmentAspect4() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Update:Air Shipment:ImportAirShipmentUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testUpdateShipmentAspect5() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Update:Air Shipment:ImportAirShipmentUpdate")));
        ShipmentDetails mockShipment = new ShipmentDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testUpdateShipmentAspectException() throws RunnerException {
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateShipmentUpdate(joinPoint, commonRequestModel));
    }

    @Test
    void testUpdateConsolidationAspect() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Update:All Consolidation:AllConsolidationUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel);
        assert (true);
    }

    @Test
    void testUpdateConsolidationAspect2() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Update:Air Consolidation:ImportAirConsolidationUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel));
    }

    @Test
    void testUpdateConsolidationAspect3() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Update:Air Consolidation:ImportAirConsolidationUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel));
    }

    @Test
    void testUpdateConsolidationAspect4() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Update:Air Consolidation:ImportAirConsolidationUpdate")));
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testUpdateConsolidationAspect5() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Update:Air Consolidation:ImportAirConsolidationUpdate")));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        mockConsolidation.setConsolidationNumber("AIR-CAN-00001");
        mockConsolidation.setId(1L).setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testUpdateConsolidationAspectException() throws RunnerException {
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
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ConsolidationDetailsRequest mockConsolidationRequest = objectMapper.convertValue(mockConsolidation, ConsolidationDetailsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockConsolidationRequest);
        updateValidateAspect = new UpdateValidateAspect();
        assertThrows(RunnerException.class, () -> updateValidateAspect.validateConsolidationUpdate(joinPoint, commonRequestModel));
    }
}