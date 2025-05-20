package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentValidationV3UtilTest extends CommonMocks {

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @InjectMocks
    private ShipmentValidationV3Util shipmentValidationV3Util;

    private ShipmentDetails shipmentDetails;
    private ShipmentDetails oldEntity;

    @BeforeEach
    void setup() {
        MockitoAnnotations.openMocks(this);

        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);

        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        oldEntity = new ShipmentDetails();

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(true).build());
    }

    @Test
    void testValidateStaleShipmentUpdateError_ThrowsExceptionForStaleUpdate() {
        shipmentDetails.setConsolidationList(Collections.emptySet());

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setIsAttachmentDone(true);
        mapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED);

        when(consoleShipmentMappingDao.findByShipmentId(1L))
                .thenReturn(List.of(mapping));

        ValidationException exception = assertThrows(ValidationException.class,
                () -> shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, false));

        assertEquals(ShipmentConstants.STALE_SHIPMENT_UPDATE_ERROR, exception.getMessage());
    }

    @Test
    void testValidateStaleShipmentUpdateError_NoExceptionIfCreate() {
        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, true));
    }

    @Test
    void testValidateStaleShipmentUpdateError_NoMappings() {
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(Collections.emptyList());

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, false));
    }

    @Test
    void testValidateStaleShipmentUpdateError_MappingsPresentButNoAttachmentDone() {
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setIsAttachmentDone(false);
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, false));
    }

    @Test
    void testValidateStaleShipmentUpdateError_AttachmentDone_ButConsolidationPresent() {
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setIsAttachmentDone(true);
        mapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED);
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));

        shipmentDetails.setConsolidationList(Set.of(new ConsolidationDetails()));

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, false));
    }

    @Test
    void testValidateStaleShipmentUpdateError_AttachmentDone_RequestedTypeNull() {
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setIsAttachmentDone(true);
        mapping.setRequestedType(null); // Important

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));

        shipmentDetails.setConsolidationList(Collections.emptySet());

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, false));
    }

    @Test
    void testValidTransportMode_Invalid_ThrowsException() {
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setTransportModeConfig(true);
        oldEntity.setTransportMode("SEA");
        shipmentDetails.setTransportMode("AIR");

        when(commonUtils.isTransportModeValid("AIR", Constants.SHIPMENT_DETAILS, tenantSettings))
                .thenReturn(false);

        ValidationException ex = assertThrows(ValidationException.class,
                () -> shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(
                        shipmentDetails, oldEntity, false, false, tenantSettings));

        assertEquals(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, "AIR"), ex.getMessage());
    }

    @Test
    void testValidTransportMode_Same_NoException() {
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setTransportModeConfig(true);
        oldEntity.setTransportMode("AIR");
        shipmentDetails.setTransportMode("AIR");

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(
                        shipmentDetails, oldEntity, false, false, tenantSettings));
    }

    @Test
    void testValidTransportModeConfig_ThrowsException_AllConditionsTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setTransportMode("SEA");

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setTransportMode("AIR"); // different from new

        V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
        settings.setTransportModeConfig(true);

        when(commonUtils.isTransportModeValid("SEA", Constants.SHIPMENT_DETAILS, settings)).thenReturn(false);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(newShipment, oldShipment, false, false, settings));

        assertEquals(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, "SEA"), ex.getMessage());
    }

    @ParameterizedTest
    @MethodSource("provideTransportModeValidationCases")
    void testValidTransportModeConfig_NoException(String newMode, String oldMode, boolean configEnabled, boolean importFileFlag) {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setTransportMode(newMode);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setTransportMode(oldMode);

        V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
        settings.setTransportModeConfig(configEnabled);

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(
                        newShipment, oldShipment, false, importFileFlag, settings));
    }

    private static Stream<Arguments> provideTransportModeValidationCases() {
        return Stream.of(
                Arguments.of("SEA", "AIR", false, false),  // config disabled
                Arguments.of("SEA", "AIR", true, true),    // import file true
                Arguments.of("AIR", "AIR", true, false)    // same transport mode
        );
    }

    @Test
    void testValidTransportModeConfig_ValidMode_NoException() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setTransportMode("SEA");

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setTransportMode("AIR");

        V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
        settings.setTransportModeConfig(true);

        when(commonUtils.isTransportModeValid("SEA", Constants.SHIPMENT_DETAILS, settings)).thenReturn(true);

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(newShipment, oldShipment, false, false, settings));
    }

    @Test
    void testValidTransportModeConfig_CreateFlow_InvalidMode_ThrowsException() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setTransportMode("RAIL");

        V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
        settings.setTransportModeConfig(true);

        when(commonUtils.isTransportModeValid("RAIL", Constants.SHIPMENT_DETAILS, settings)).thenReturn(false);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(newShipment, null, true, false, settings));

        assertEquals(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, "RAIL"), ex.getMessage());
    }

    @Test
    void testProcessDGValidations_AirDG_ThrowsRunnerException() {
        shipmentDetails.setTransportMode("AIR");
        shipmentDetails.setContainsHazardous(false);

        ConsolidationDetails c1 = new ConsolidationDetails();
        c1.setHazardous(true);
        c1.setTransportMode("AIR");
        Set<ConsolidationDetails> set = Set.of(c1);

        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put("AirDG", false);
        UserContext.getUser().setPermissions(permissions);

        mockShipmentSettings();

        RunnerException ex = assertThrows(RunnerException.class,
                () -> shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, set));

        assertTrue(ex.getMessage().contains("Air DG permissions"));
    }

    @Test
    void testProcessDGValidations_MultipleDGShipments_ThrowsRunnerException() {
        shipmentDetails.setContainsHazardous(true);
        oldEntity.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setId(1L);
        cd.setHazardous(true);
        cd.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        cd.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        cd.setConsolidationNumber("CON-001");

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(1L);

        when(consoleShipmentMappingDao.findByConsolidationId(1L))
                .thenReturn(List.of(mapping, mapping));

        Set<ConsolidationDetails> set = Set.of(cd);

        RunnerException ex = assertThrows(RunnerException.class,
                () -> shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, set));

        assertTrue(ex.getMessage().contains("LCL Cargo Type"));
    }

    @Test
    void testProcessDGValidationsAIR_MultipleDGShipments_ThrowsRunnerException() {
        shipmentDetails.setContainsHazardous(true);
        oldEntity.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setId(1L);
        cd.setHazardous(true);
        cd.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        cd.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        cd.setConsolidationNumber("CON-001");

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(1L);

        when(consoleShipmentMappingDao.findByConsolidationId(1L))
                .thenReturn(List.of(mapping, mapping));
        mockShipmentSettings();

        Set<ConsolidationDetails> set = Set.of(cd);

        RunnerException ex = assertThrows(RunnerException.class,
                () -> shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, set));

        assertTrue(ex.getMessage().contains("Shipment as DG Shipment"));
    }

    @Test
    void testProcessDGValidationsAIR_MultipleDGShipments() {
        shipmentDetails.setContainsHazardous(true);
        oldEntity.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setId(1L);
        cd.setHazardous(true);
        cd.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        cd.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        cd.setConsolidationNumber("CON-001");

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        mockShipmentSettings();

        Set<ConsolidationDetails> set = Set.of(cd);

        assertDoesNotThrow(() -> shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, set));
    }

    @Test
    void testProcessDGValidations_ExistingHazardous_NoException() {
        shipmentDetails.setContainsHazardous(true);
        oldEntity.setContainsHazardous(true); // Already hazardous

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setId(1L);
        cd.setHazardous(true);
        cd.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        cd.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        cd.setConsolidationNumber("CON-002");

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, Set.of(cd)));
    }

    @Test
    void testProcessDGValidations_EmptyConsolidationSet_NoException() {
        shipmentDetails.setContainsHazardous(true);
        oldEntity.setContainsHazardous(false);

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, Collections.emptySet()));
    }

    @Test
    void testProcessDGValidations_NoHazardous_NoAirDG_NoException() {
        shipmentDetails.setTransportMode("SEA");
        shipmentDetails.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setHazardous(false);
        cd.setTransportMode("SEA");
        cd.setContainerCategory("FCL"); // not LCL

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, Set.of(cd)));
    }

    @Test
    void testProcessDGValidations_AirDG_UserHasPermission_NoException() {
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setHazardous(true);
        cd.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put("AirDG", true);
        UserContext.getUser().setPermissions(permissions);
        mockShipmentSettings();

        assertDoesNotThrow(() ->
                shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, Set.of(cd)));
    }
}
