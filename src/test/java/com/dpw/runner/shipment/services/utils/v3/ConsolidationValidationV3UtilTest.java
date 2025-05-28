package com.dpw.runner.shipment.services.utils.v3;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ConsolidationValidationV3UtilTest {

  @InjectMocks
  private ConsolidationValidationV3Util validationUtil;

  @Mock
  private IDpsEventService dpsEventService;

  @Mock
  private CommonUtils commonUtils;

  @Mock
  private ConsolidationV3Util consolidationV3Util;

  @Mock
  private UserContext userContext;

  private List<ConsoleShipmentMapping> consoleShipmentMappings;
  private ConsolidationDetails consol;
  private List<Long> shipmentIdList;

  @Test
  void testValidateConsolidationIdAndShipmentIds_whenValidInputs_thenNoException() {
    assertDoesNotThrow(() -> validationUtil.validateConsolidationIdAndShipmentIds(1L, List.of(100L, 200L)));
    consoleShipmentMappings = new ArrayList<>();
    consol = new ConsolidationDetails();
    shipmentIdList = new ArrayList<>();
  }

  @Test
  void testValidateConsolidationIdAndShipmentIds_whenInvalidInputs_thenThrowsException() {
    List<Long> list = List.of(100L);
    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
        () -> validationUtil.validateConsolidationIdAndShipmentIds(null, list));
    assertEquals("Consolidation ID and Shipment IDs must not be null or empty", ex.getMessage());

    ex = assertThrows(IllegalArgumentException.class,
        () -> validationUtil.validateConsolidationIdAndShipmentIds(1L, null));
    assertEquals("Consolidation ID and Shipment IDs must not be null or empty", ex.getMessage());

    ex = assertThrows(IllegalArgumentException.class,
        () -> validationUtil.validateConsolidationIdAndShipmentIds(1L, shipmentIdList));
    assertEquals("Consolidation ID and Shipment IDs must not be null or empty", ex.getMessage());
  }

  @Test
  void testCheckCFSValidation_whenShipmentDateAfterCutoff_thenThrowsRunnerException() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setId(1L);
    details.setCfsCutOffDate(LocalDateTime.of(2025, 5, 1, 10, 0));

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setShipmentGateInDate(LocalDateTime.of(2025, 5, 2, 10, 0)); // after cutoff

    when(consolidationV3Util.checkConsolidationEligibleForCFSValidation(any())).thenReturn(true);

    RunnerException ex = assertThrows(RunnerException.class,
        () -> validationUtil.checkCFSValidation(details, false, List.of(shipment)));

    assertEquals("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.",
        ex.getMessage());
  }

  @Test
  void testCheckCFSValidation_whenShipmentListNull_thenFetchFromUtil() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setId(1L);
    details.setCfsCutOffDate(LocalDateTime.of(2025, 5, 10, 10, 0));

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setShipmentGateInDate(LocalDateTime.of(2025, 5, 9, 10, 0)); // before cutoff

    when(consolidationV3Util.checkConsolidationEligibleForCFSValidation(any())).thenReturn(true);
    when(consolidationV3Util.getShipmentsList(1L)).thenReturn(List.of(shipment));

    assertDoesNotThrow(() -> validationUtil.checkCFSValidation(details, false, null));
  }

  @Test
  void testCheckIfShipmentDateGreaterThanConsole() {
    LocalDateTime shipment = LocalDateTime.of(2025, 5, 5, 10, 0);
    LocalDateTime cutoff = LocalDateTime.of(2025, 5, 1, 10, 0);
    assertTrue(validationUtil.checkIfShipmentDateGreaterThanConsole(shipment, cutoff));

    assertFalse(validationUtil.checkIfShipmentDateGreaterThanConsole(null, cutoff));
    assertFalse(validationUtil.checkIfShipmentDateGreaterThanConsole(shipment, null));
  }

  @Test
  void testCheckInterBranchPermission_whenUserHasNoPermission_thenThrowsValidationException() {
    ConsolidationDetails newDetails = new ConsolidationDetails();
    newDetails.setInterBranchConsole(true);

    ConsolidationDetails oldDetails = new ConsolidationDetails();
    oldDetails.setInterBranchConsole(false);

    Map<String, Boolean> permissions = new HashMap<>();
    permissions.put(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, false);
    UserContext.setUser(UsersDto.builder().Permissions(permissions).build());

    ValidationException ex = assertThrows(ValidationException.class,
        () -> validationUtil.checkInterBranchPermission(newDetails, oldDetails));

    assertEquals("User don't have InterBranch Consolidation Permission to change InterBranch Flag", ex.getMessage());
  }

  @Test
  void testCheckInterBranchPermission_whenUserHasPermission_thenNoException() {
    ConsolidationDetails newDetails = new ConsolidationDetails();
    newDetails.setInterBranchConsole(true);

    ConsolidationDetails oldDetails = new ConsolidationDetails();
    oldDetails.setInterBranchConsole(false);

    Map<String, Boolean> permissions = new HashMap<>();
    permissions.put(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, true);
    UserContext.setUser(UsersDto.builder().Permissions(permissions).build());

    assertDoesNotThrow(() -> validationUtil.checkInterBranchPermission(newDetails, oldDetails));
  }

  @Test
  void testValidateConsolidationIdAndShipmentIds_invalidInput_shouldThrowException() {
    shipmentIdList = List.of(1L);
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
        () -> validationUtil.validateConsolidationIdAndShipmentIds(null, shipmentIdList));
    assertEquals("Consolidation ID and Shipment IDs must not be null or empty", exception.getMessage());
  }

  @Test
  void testCheckIfShipmentDateGreaterThanConsole_shouldReturnTrue() {
    LocalDateTime shipmentDate = LocalDateTime.now().plusDays(1);
    LocalDateTime consoleDate = LocalDateTime.now();
    assertTrue(validationUtil.checkIfShipmentDateGreaterThanConsole(shipmentDate, consoleDate));
  }

  @Test
  void testCheckIfShipmentDateGreaterThanConsole_shouldReturnFalseWhenNull() {
    assertFalse(validationUtil.checkIfShipmentDateGreaterThanConsole(null, LocalDateTime.now()));
  }

  @Test
  void testCheckInterBranchPermission_shouldThrowException() {
    ConsolidationDetails newDetails = new ConsolidationDetails();
    newDetails.setInterBranchConsole(true);
    Map<String, Boolean> permissions = new HashMap<>();
    UserContext.setUser(UsersDto.builder().Permissions(permissions).build());

    ValidationException exception = assertThrows(ValidationException.class,
        () -> validationUtil.checkInterBranchPermission(newDetails, null));
    assertEquals("User don't have InterBranch Consolidation Permission to change InterBranch Flag", exception.getMessage());
  }

  @Test
  void testValidationsBeforeAttachShipments_shipmentAlreadyAttached_shouldThrow() {
    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    ShipmentDetails shipment = new ShipmentDetails();
    ConsolidationDetails existingConsole = new ConsolidationDetails();
    existingConsole.setConsolidationNumber("CON001");
    shipment.setConsolidationList(Set.of(existingConsole));
    List<ShipmentDetails> shipmentList = List.of(shipment);

    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails,
            consoleShipmentMappings, shipmentIdList, 1L, shipmentList, true)
    );
    assertTrue(ex.getMessage().contains("Shipment is already attached to Consolidation"));
  }

  @Test
  void testValidationsBeforeAttachShipments_directShipment_shouldThrow() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
    shipment.setConsolidationList(Collections.emptySet());

    List<ShipmentDetails> list = List.of(shipment);
    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consol, consoleShipmentMappings,
            shipmentIdList, 1L, list, true)
    );
    assertTrue(ex.getMessage().contains("Selected shipment is a Direct Shipment"));
  }

  @Test
  void testValidationsBeforeAttachShipments_cancelledShipment_shouldThrow() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setStatus(ShipmentStatus.Cancelled.getValue());
    shipment.setConsolidationList(Collections.emptySet());

    List<ShipmentDetails> list = List.of(shipment);

     assertThrows(IllegalArgumentException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consol,consoleShipmentMappings,
            shipmentIdList, 1L, list, true)
    );
  }

  @Test
  void testValidationsBeforeAttachShipments_lclDgMoreThanOne_shouldThrow() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setHazardous(true);
    details.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
    details.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    details.setShipmentsList(Set.of(new ShipmentDetails()));

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Arrived.getValue());

    List<ShipmentDetails> list = List.of(shipment);
    List<Long> shipmentIds = List.of(123L);

    RunnerException ex = assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(details, new ArrayList<>(),
            shipmentIds, 1L, list, true)
    );
    assertTrue(ex.getMessage().contains("Ocean DG Consolidation LCL Cargo Type"));
  }

  @Test
  void testValidationsBeforeAttachShipments_mappingDifferentConsole_shouldThrow() {
    ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
    mapping.setConsolidationId(2L);
    mapping.setIsAttachmentDone(true);

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());

    RunnerException ex = assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(new ConsolidationDetails(), List.of(mapping),
            List.of(1L), 1L, List.of(shipment), true)
    );
    assertTrue(ex.getMessage().contains("Multiple consolidations are attached"));
  }

  @Test
  void testValidationsBeforeAttachShipments_dpsImplicationPresent_shouldThrow() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime latDate = LocalDateTime.of(2024, 5, 20, 12, 0); // May 20, 2024 at 12:00 PM
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);

    consolidationDetails.setLatDate(latDate);
    shipment.setCargoDeliveryDate(cargoDeliveryDate);

   assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L), 1L, List.of(shipment), true)
    );
  }

  @Test
  void testValidationsBeforeAttachShipments_dpsImplicationPresent_shouldThrow_fromConsolidationTrue() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(true);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setTenantId(1);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);

    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L), 1L, List.of(shipment), true)
    );
  }

  @Test
  void testValidationsBeforeAttachShipments_dpsImplicationPresent_shouldThrow_fromConsolidationFalse() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(true);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setTenantId(1);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(true);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L), 1L, List.of(shipment), false)
    );
  }

  @Test
  void testValidateAirDgHazardousForConsole_dpsImplicationPresent_shouldThrow_fromConsolidationFalse() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(false);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setTenantId(1);
    consolidationDetails.setHazardous(true);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L,2L), 1L, List.of(shipment), false)
    );
  }

  @Test
  void testValidateAirDgHazardousForConsole_dpsImplicationPresent_shouldThrow_fromConsolidationTrue() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(false);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setTenantId(1);
    consolidationDetails.setHazardous(true);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L,2L), 1L, List.of(shipment), true)
    );
  }

  @Test
  void testValidateAirDgHazardousForConsole_dpsImplicationPresent_shouldThrow_fromInterbranch() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(false);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setTenantId(1);
    consolidationDetails.setHazardous(true);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L), 1L, List.of(shipment), true)
    );
  }

  @Test
  void testValidateAirDgHazardousForConsole_dpsImplicationPresent_shouldThrow_fromDPSBranch() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setConsolidationList(Collections.emptySet());
    shipment.setStatus(ShipmentStatus.Created.getValue());
    shipment.setGuid(UUID.randomUUID());
    shipment.setCargoDeliveryDate(LocalDateTime.now());
    shipment.setShipmentId("SHIP001");
    shipment.setTenantId(2);
    shipment.setContainsHazardous(false);

    ConsolidationDetails consolidationDetails = new ConsolidationDetails();
    LocalDateTime cargoDeliveryDate = LocalDateTime.of(2024, 5, 20, 10, 30);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setTenantId(1);
    consolidationDetails.setHazardous(true);

    when(dpsEventService.isImplicationPresent(anySet(), any())).thenReturn(true);

    shipment.setCargoDeliveryDate(cargoDeliveryDate);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAirDGFlag(false);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    assertThrows(RunnerException.class, () ->
        validationUtil.validationsBeforeAttachShipments(consolidationDetails, new ArrayList<>(),
            List.of(1L), 1L, List.of(shipment), true)
    );
  }
}

