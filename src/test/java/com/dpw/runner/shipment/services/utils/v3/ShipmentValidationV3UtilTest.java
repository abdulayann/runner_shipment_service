package com.dpw.runner.shipment.services.utils.v3;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
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
import org.junit.jupiter.api.function.Executable;
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

        ValidationException ex = assertThrows(ValidationException.class,
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
        cd.setTransportMode(TRANSPORT_MODE_AIR);
        cd.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        cd.setConsolidationNumber("CON-001");

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(1L);

        when(consoleShipmentMappingDao.findByConsolidationId(1L))
                .thenReturn(List.of(mapping, mapping));
        mockShipmentSettings();

        Set<ConsolidationDetails> set = Set.of(cd);

     ValidationException ex = assertThrows(ValidationException.class,
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
        cd.setTransportMode(TRANSPORT_MODE_AIR);
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
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setContainsHazardous(false);

        ConsolidationDetails cd = new ConsolidationDetails();
        cd.setHazardous(true);
        cd.setTransportMode(TRANSPORT_MODE_AIR);

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

        shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, oldEntity);
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipment.getJobType());
    }

    @Test
    void testValidateShipmentCreateOrUpdate_validatePartner() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setMasterBill("masterBill");
        ShipmentDetails oldEntity1 = new ShipmentDetails();

        Parties consignor = new Parties();
        consignor.setOrgCode(null);
        shipment.setConsigner(consignor);

        Parties consignee = new Parties();
        consignee.setOrgCode("ORG-CODE");
        shipment.setConsignee(consignee);

        shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, oldEntity1);
        assertEquals(TRANSPORT_MODE_AIR, shipment.getTransportMode());
    }

    @Test
    void testValidateShipmentCreateOrUpdate_withNullConsignor_shouldNotThrow() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setCoLoadBkgNumber("bkgNumber");

        Parties consignee = new Parties();
        consignee.setOrgCode("ORG-1");
        shipment.setConsignee(consignee);

        shipment.setConsigner(null); // Null consignor

        shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null);
        assertEquals(TRANSPORT_MODE_AIR, shipment.getTransportMode());
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_AIR() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setControlled(true);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_AIR1() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setControlledReferenceNumber("TEST");

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_ForControlledFields_EmptyControlled_AIR1() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);

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
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setFmcTlcId("Test");

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForFmcTlcFields(shipment));
    }

    @Test
    void testValidateShipmentCreateOrUpdate_coLoadBLNumber_AIR() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setCoLoadBlNumber("BlNumber");

        shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipment, null);
        assertEquals(TRANSPORT_MODE_AIR, shipment.getTransportMode());
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
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
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
    void testValidationFields_DOM_shouldPass() {
        ShipmentDetails newShipment = ShipmentDetails.builder().direction("DOM").build();

        assertDoesNotThrow(() -> shipmentValidationV3Util.validationETAETDATAATDFields(newShipment, null));
    }

    @Test
    void testValidationForCutOffFields_DOM_exception() {
        ShipmentDetails newShipment = ShipmentDetails.builder().direction("DOM").latestArrivalTime(LocalDateTime.now()).build();

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validationForCutOffFields(newShipment));
    }

    @Test
    void testValidateShippedOnBoardDate_NullShippedOnBoard() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(new AdditionalDetails());
        shipment.setCarrierDetails(new CarrierDetails());

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
    }

    @Test
    void testValidateShippedOnBoardDate_NullATD() {
        ShipmentDetails shipment = new ShipmentDetails();

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShippedOnboard(LocalDateTime.now().minusDays(1));
        shipment.setAdditionalDetails(additionalDetails);

        shipment.setCarrierDetails(new CarrierDetails());
        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
    }

    @Test
    void testValidateShippedOnBoardDate_ShippedOnBoardAfterATD() {
        ShipmentDetails shipment = new ShipmentDetails();

        LocalDateTime actualTimeOfDeparture = LocalDateTime.now();
        LocalDateTime shippedOnBoard = actualTimeOfDeparture.plusHours(1);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShippedOnboard(shippedOnBoard);
        shipment.setAdditionalDetails(additionalDetails);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(actualTimeOfDeparture);
        shipment.setCarrierDetails(carrierDetails);

        assertThrows(ValidationException.class, () -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
    }

    @Test
    void testValidateShippedOnBoardDate_ShippedOnBoardInFuture() {
        ShipmentDetails shipment = new ShipmentDetails();

        LocalDateTime actualTimeOfDeparture = LocalDateTime.now().plusDays(3);
        LocalDateTime shippedOnBoard = actualTimeOfDeparture.plusDays(1);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShippedOnboard(shippedOnBoard);
        shipment.setAdditionalDetails(additionalDetails);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(actualTimeOfDeparture);
        shipment.setCarrierDetails(carrierDetails);
        Executable executable = () -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment);

        ValidationException exception = assertThrows(ValidationException.class, executable);
        assertEquals("Shipped On Board cannot be a future date.", exception.getMessage());
    }

    @Test
    void testValidateShippedOnBoardDate_NullCarrierDetails() {
        ShipmentDetails shipment = new ShipmentDetails();

        LocalDateTime shippedOnBoard = LocalDateTime.now().minusDays(1);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShippedOnboard(shippedOnBoard);
        shipment.setAdditionalDetails(additionalDetails);
        shipment.setCarrierDetails(null);

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
    }

    @Test
    void testValidateShippedOnBoardDate_NullAdditionalDetails() {
        ShipmentDetails shipment = new ShipmentDetails();

        LocalDateTime actualTimeOfDeparture = LocalDateTime.now();
        shipment.setAdditionalDetails(null);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(actualTimeOfDeparture);
        shipment.setCarrierDetails(carrierDetails);

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
    }

    @Test
    void testValidateShippedOnBoardDate_Success() {
        ShipmentDetails shipment = new ShipmentDetails();

        LocalDateTime shippedOnBoard = LocalDateTime.now().minusDays(1);
        LocalDateTime actualTimeOfDeparture = LocalDateTime.now();

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShippedOnboard(shippedOnBoard);
        shipment.setAdditionalDetails(additionalDetails);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(actualTimeOfDeparture);
        shipment.setCarrierDetails(carrierDetails);

        assertDoesNotThrow(() -> shipmentValidationV3Util.validateShippedOnBoardDate(shipment));
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

    @Test
    void testValidationForCarrierDetailsDates_Success() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(LocalDateTime.now().plusDays(1));
        carrierDetails.setAtd(LocalDateTime.now().plusDays(2));
        carrierDetails.setEta(LocalDateTime.now().plusDays(5));
        carrierDetails.setAta(LocalDateTime.now().plusDays(6));

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setEstimatedPickupDate(LocalDateTime.now());
        additionalDetails.setPickupDate(LocalDateTime.now().plusDays(1));
        additionalDetails.setCargoDeliveredDate(LocalDateTime.now().plusDays(5));

        shipment.setCarrierDetails(carrierDetails);
        shipment.setAdditionalDetails(additionalDetails);
        shipment.setCargoDeliveryDate(LocalDateTime.now().plusDays(5));

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        assertDoesNotThrow(executable);
    }

    @Test
    void testValidationForCarrierDetailsDates_NullAdditionalDetails() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(LocalDateTime.now().plusDays(1));
        carrierDetails.setAtd(LocalDateTime.now().plusDays(2));
        carrierDetails.setEta(LocalDateTime.now().plusDays(5));
        carrierDetails.setAta(LocalDateTime.now().plusDays(6));

        shipment.setCarrierDetails(carrierDetails);
        shipment.setAdditionalDetails(null);

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        assertDoesNotThrow(executable);
    }

    @Test
    void testValidationForCarrierDetailsDates_NullCarrierDetails() {
        ShipmentDetails shipment = new ShipmentDetails();

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setEstimatedPickupDate(LocalDateTime.now());
        additionalDetails.setPickupDate(LocalDateTime.now().plusDays(1));
        additionalDetails.setCargoDeliveredDate(LocalDateTime.now().plusDays(7));

        shipment.setCarrierDetails(null);
        shipment.setAdditionalDetails(additionalDetails);
        shipment.setCargoDeliveryDate(LocalDateTime.now().plusDays(6));

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        assertDoesNotThrow(executable);
    }

    @Test
    void testValidationForCarrierDetailsDates_EstimatedPickupDateAfterETD() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(LocalDateTime.now());
        shipment.setCarrierDetails(carrierDetails);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setEstimatedPickupDate(LocalDateTime.now().plusDays(1));
        shipment.setAdditionalDetails(additionalDetails);

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        ValidationException validationException = assertThrows(ValidationException.class, executable);
        assertEquals("Est. Origin Transport Date should be less than or equal to ETD",
                validationException.getMessage());
    }

    @Test
    void testValidationForCarrierDetailsDate_ActualPickupDateAfterATD() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(LocalDateTime.now());
        shipment.setCarrierDetails(carrierDetails);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setPickupDate(LocalDateTime.now().plusDays(1));
        shipment.setAdditionalDetails(additionalDetails);

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        ValidationException validationException = assertThrows(ValidationException.class, executable);
        assertEquals("Act. Origin Transport Date should be less than or equal to ATD", validationException.getMessage());
    }

    @Test
    void testValidationForCarrierDetailsDate_EstimatedCargoDeliveryDateBeforeETA() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEta(LocalDateTime.now().plusDays(5));
        shipment.setCarrierDetails(carrierDetails);
        shipment.setCargoDeliveryDate(LocalDateTime.now().plusDays(3));

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        ValidationException validationException = assertThrows(ValidationException.class, executable);
        assertEquals("Est. Destination Transport Date should be more than or equal to ETA", validationException.getMessage());
    }

    @Test
    void testValidationForCarrierDetailsDate_ActualCargoDeliveredDateBeforeATA() {
        ShipmentDetails shipment = new ShipmentDetails();

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAta(LocalDateTime.now().plusDays(1));
        shipment.setCarrierDetails(carrierDetails);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setCargoDeliveredDate(LocalDateTime.now().plusDays(5));
        shipment.setAdditionalDetails(additionalDetails);

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        ValidationException validationException = assertThrows(ValidationException.class, executable);
        assertEquals("Act. Destination Transport Date should be less than or equal to ATA",
                validationException.getMessage());
    }

    @Test
    void testValidationForCarrierDetailsDate_AllDateFieldsNull() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(new AdditionalDetails());
        shipment.setCarrierDetails(new CarrierDetails());

        Executable executable = () -> shipmentValidationV3Util.validateCarrierDetailsDates(shipment);
        assertDoesNotThrow(executable);
    }
}
