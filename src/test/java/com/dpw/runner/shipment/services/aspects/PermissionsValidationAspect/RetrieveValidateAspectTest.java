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

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RetrieveValidateAspectTest {

    private RetrieveValidateAspect retrieveValidateAspect;

    @Mock
    private JoinPoint joinPoint;

    @BeforeEach
    public void setUp() {
        retrieveValidateAspect = new RetrieveValidateAspect();
    }


    @Test
    void testCreateShipmentAspect() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Shipments:Retrive:All Shipment:AllShipmentRetrive")));
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
        Optional<ShipmentDetails> shipmentDetails = Optional.of(mockShipment);
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateShipmentRetrieve(shipmentDetails);
        assert (true);
    }

    @Test
    void testCreateShipmentAspectException() throws RunnerException {
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
        Optional<ShipmentDetails> shipmentDetails = Optional.of(mockShipment);
        retrieveValidateAspect = new RetrieveValidateAspect();
        assertThrows(RunnerException.class, () -> retrieveValidateAspect.validateShipmentRetrieve(shipmentDetails));
    }

    @Test
    void testCreateConsolidationAspect() throws RunnerException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        PermissionsContext.setPermissions(new ArrayList<>(Arrays.asList("Consolidations:Retrive:All Consolidation:AllConsolidationRetrive")));
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
        Optional<ConsolidationDetails> consolidationDetails = Optional.of(mockConsolidation);
        retrieveValidateAspect = new RetrieveValidateAspect();
        retrieveValidateAspect.validateConsolidationRetrieve(consolidationDetails);
        assert (true);
    }

    @Test
    void testCreateConsolidationAspectException() throws RunnerException {
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
        Optional<ConsolidationDetails> consolidationDetails = Optional.of(mockConsolidation);
        retrieveValidateAspect = new RetrieveValidateAspect();
        assertThrows(RunnerException.class, () -> retrieveValidateAspect.validateConsolidationRetrieve(consolidationDetails));
    }
}