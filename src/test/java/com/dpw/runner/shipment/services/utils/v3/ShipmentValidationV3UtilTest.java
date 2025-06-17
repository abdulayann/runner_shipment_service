package com.dpw.runner.shipment.services.utils.v3;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
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


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentValidationV3UtilTest extends CommonMocks {

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IDpsEventService dpsEventService;

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


    @Test
    void testValidateShipmentCreateOrUpdate_withNullOrgCodes_shouldNotThrow() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setCoLoadBlNumber("coload");
        ShipmentDetails oldEntity1 = new ShipmentDetails();

        Parties consignor = new Parties();
        consignor.setOrgCode(null);
        shipment.setConsigner(consignor);

        Parties consignee = new Parties();
        consignee.setOrgCode("ORG-CODE");
        shipment.setConsignee(consignee);

        when(dpsEventService.isImplicationPresent(List.of(1L), "CONCR")).thenReturn(Boolean.FALSE);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, oldEntity));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_withDpsImplication() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        ShipmentDetails oldEntity1 = new ShipmentDetails();

        when(dpsEventService.isImplicationPresent(List.of(1L), "CONCR")).thenReturn(Boolean.TRUE);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, oldEntity1));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_validatePartner() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setMasterBill("masterBill");
        ShipmentDetails oldEntity1 = new ShipmentDetails();

        Parties consignor = new Parties();
        consignor.setOrgCode(null);
        shipment.setConsigner(consignor);

        Parties consignee = new Parties();
        consignee.setOrgCode("ORG-CODE");
        shipment.setConsignee(consignee);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, oldEntity1));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_withNullConsignor_shouldNotThrow() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setCoLoadBkgNumber("bkgNumber");

        Parties consignee = new Parties();
        consignee.setOrgCode("ORG-1");
        shipment.setConsignee(consignee);

        shipment.setConsigner(null); // Null consignor

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_AIR() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setControlled(true);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_AIR1() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setControlledReferenceNumber("TEST");

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_EmptyControlled_AIR1() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setControlled(true);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidationForFmcTlcFields_ForFmcTlcField_AIR() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setFmcTlcId("Test");

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForFmcTlcFields(shipment));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_coLoadBLNumber_AIR() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setCoLoadBlNumber("BlNumber");

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_CargoDeliveryDate_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setCargoDeliveryDate(LocalDateTime.now());

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidationForFmcTlcFields_ForFmcTlcField_Origin_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setCarrierDetails(CarrierDetails.builder()
                .originLocCode("USPOR")
                .destinationLocCode("ERTEW")
                .build());

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForFmcTlcFields(shipment));
    }

    @Test
    void testValidationForFmcTlcFields_ForFmcTlcField_Destination_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setCarrierDetails(CarrierDetails.builder()
                .originLocCode("ERTTR")
                .destinationLocCode("USPOR")
                .build());

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForFmcTlcFields(shipment));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForFmcTlcField_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setCarrierDetails(CarrierDetails.builder()
                .originLocCode("ERTTR")
                .destinationLocCode("CAPOR")
                .build());

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }
    @Test
    void testValidateShipmentCreateOrUpdate_ForFmcTlcField_EmptyDestination_SEA() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setCarrierDetails(CarrierDetails.builder()
                .originLocCode("ERTTR")
                .build());

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @ParameterizedTest
    @MethodSource("shipmentCutoffPayloads")
    void testValidationForCutOffFields(ShipmentDetails shipment) {
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    private static Stream<Arguments> shipmentCutoffPayloads() {
        return Stream.of(Arguments.of(ShipmentDetails.builder().terminalCutoff(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().verifiedGrossMassCutoff(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().shippingInstructionCutoff(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().dgCutoff(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().reeferCutoff(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().earliestEmptyEquipmentPickUp(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().latestFullEquipmentDeliveredToCarrier(LocalDateTime.now()).build()),
                Arguments.of(ShipmentDetails.builder().earliestDropOffFullEquipmentToCarrier(LocalDateTime.now()).build()));
    }

    @Test
    void testValidationForCutOffFields() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setLatestArrivalTime(LocalDateTime.now());
        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidationFields_newShipment_allFieldsNull() {
        ShipmentDetails newShipment = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .build();

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, null));
    }

    @Test
    void testValidationFields_newShipment_etaPresent_shouldThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails newShipment = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().eta(now).build())
                .build();

        ValidationException ex = assertThrows(ValidationException.class, () -> {
            shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, null);
        });
        assertEquals("Update not allowed for ETA", ex.getMessage());
    }

    @Test
    void testValidationFields_newShipment_ataPresent_shouldThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails newShipment = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().ata(now).build())
                .build();

        ValidationException ex = assertThrows(ValidationException.class, () -> {
            shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, null);
        });
        assertEquals("Update not allowed for ATA", ex.getMessage());
    }

    @Test
    void testValidationFields_updateShipment_allFieldsSame_shouldPass() {
        LocalDateTime now = LocalDateTime.now();

        CarrierDetails oldCarrier = CarrierDetails.builder()
                .eta(now)
                .ata(now)
                .etd(now)
                .atd(now)
                .build();

        CarrierDetails newCarrier = CarrierDetails.builder()
                .eta(now)
                .ata(now)
                .etd(now)
                .atd(now)
                .build();

        ShipmentDetails oldShipment = ShipmentDetails.builder().carrierDetails(oldCarrier).build();
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(newCarrier).build();

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, oldShipment));
    }

    @Test
    void testValidationFields_updateShipment_etaChanged_shouldThrow() {
        LocalDateTime oldTime = LocalDateTime.now();
        LocalDateTime newTime = oldTime.plusDays(1);

        CarrierDetails oldCarrier = CarrierDetails.builder().eta(oldTime).build();
        CarrierDetails newCarrier = CarrierDetails.builder().eta(newTime).build();

        ShipmentDetails oldShipment = ShipmentDetails.builder().carrierDetails(oldCarrier).build();
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(newCarrier).build();

        ValidationException ex = assertThrows(ValidationException.class, () ->
                shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, oldShipment));
        assertEquals("Update not allowed for ETA", ex.getMessage());
    }

    @Test
    void testValidationFields_updateShipment_ataChanged_shouldThrow() {
        LocalDateTime oldTime = LocalDateTime.now();
        LocalDateTime newTime = oldTime.plusHours(5);

        CarrierDetails oldCarrier = CarrierDetails.builder().ata(oldTime).build();
        CarrierDetails newCarrier = CarrierDetails.builder().ata(newTime).build();

        ShipmentDetails oldShipment = ShipmentDetails.builder().carrierDetails(oldCarrier).build();
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(newCarrier).build();

        ValidationException ex = assertThrows(ValidationException.class, () ->
                shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, oldShipment));
        assertEquals("Update not allowed for ATA", ex.getMessage());
    }

    @Test
    void testValidationFields_updateShipment_newCarrier_null_shouldNotThrow() {
        CarrierDetails oldCarrier = CarrierDetails.builder()
                .eta(null)
                .ata(null)
                .etd(null)
                .atd(null)
                .build();

        ShipmentDetails oldShipment = ShipmentDetails.builder().carrierDetails(oldCarrier).build();
        ShipmentDetails newShipment = ShipmentDetails.builder().build(); // newCarrier = null

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, oldShipment));
    }

    @Test
    void testValidationFields_bothCarrierDetails_null_shouldReturnEarly() {
        ShipmentDetails newShipment = ShipmentDetails.builder().build();
        ShipmentDetails oldShipment = ShipmentDetails.builder().build();

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, oldShipment));
    }

    @Test
    void testValidationFields_oldEntityNull_newCarrierNull_shouldPass() {
        ShipmentDetails newShipment = ShipmentDetails.builder().build(); // carrierDetails null

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, null));
    }

    @Test
    void testValidationForPolPodFields_Exception1() {
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(new CarrierDetails()).build();
        newShipment.getCarrierDetails().setIsSameAsOriginPort(true);
        newShipment.getCarrierDetails().setOrigin("abc");
        newShipment.getCarrierDetails().setOriginPort("xyz");
        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForPolPodFields(newShipment));
    }

    @Test
    void testValidationForPolPodFields_Exception2() {
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(new CarrierDetails()).build();
        newShipment.getCarrierDetails().setIsSameAsDestinationPort(true);
        newShipment.getCarrierDetails().setDestination("abc");
        newShipment.getCarrierDetails().setDestinationPort("xyz");
        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForPolPodFields(newShipment));
    }

    @Test
    void testValidationForPolPodFields_Success() {
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(new CarrierDetails()).build();
        newShipment.getCarrierDetails().setIsSameAsOriginPort(true);
        newShipment.getCarrierDetails().setOrigin("abc");
        newShipment.getCarrierDetails().setOriginPort("abc");
        newShipment.getCarrierDetails().setIsSameAsDestinationPort(true);
        newShipment.getCarrierDetails().setDestination("abc");
        newShipment.getCarrierDetails().setDestinationPort("abc");
        assertDoesNotThrow(() -> shipmentValidationV3Util.validationForPolPodFields(newShipment));
    }

    @Test
    void testValidationForPolPodFields_Success1() {
        ShipmentDetails newShipment = ShipmentDetails.builder().carrierDetails(new CarrierDetails()).build();
        newShipment.getCarrierDetails().setIsSameAsOriginPort(false);
        newShipment.getCarrierDetails().setOrigin("abc");
        newShipment.getCarrierDetails().setOriginPort("abc");
        newShipment.getCarrierDetails().setIsSameAsDestinationPort(false);
        newShipment.getCarrierDetails().setDestination("abc");
        newShipment.getCarrierDetails().setDestinationPort("abc");
        assertDoesNotThrow(() -> shipmentValidationV3Util.validationForPolPodFields(newShipment));
    }
}
