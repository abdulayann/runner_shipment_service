package com.dpw.runner.shipment.services.utils.v3;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationV3UtilTest {

  @Mock
  private CommonUtils commonUtils;

  @Mock
  private IConsoleShipmentMappingDao consoleShipmentMappingDao;

  @Mock
  private IShipmentDao shipmentDao;

  @Mock
  private ConsolidationDetails consolidationDetails;

  @InjectMocks
  private ConsolidationV3Util consolidationV3Util;

  @Mock
  private IPartiesDao partiesDao;

  @Mock
  private IEventDao eventDao;

  @Mock
  private IPackingDao packingDao;

  @Mock
  private IContainerDao containerDao;

  @Mock
  private IAwbDao awbDao;

  @Mock
  private IReferenceNumbersDao referenceNumbersDao;

  @Mock
  private ITruckDriverDetailsDao truckDriverDetailsDao;

  @Mock
  private IRoutingsDao routingsDao;

  private static JsonTestUtility jsonTestUtility;
  private static ObjectMapper objectMapperTest;
  private static ConsolidationDetails testConsol;
  private static ConsolidationDetailsResponse testConsolResponse;
  private static ConsolidationEtV3Request testConsolRequest;

  @BeforeAll
  static void init() throws IOException {
    jsonTestUtility = new JsonTestUtility();
    objectMapperTest = JsonTestUtility.getMapper();
  }

  @BeforeEach
  void setUp() {
    consolidationDetails = new ConsolidationDetails();
    testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
    testConsolResponse = objectMapperTest.convertValue(testConsol , ConsolidationDetailsResponse.class);
    testConsolRequest = objectMapperTest.convertValue(testConsol , ConsolidationEtV3Request.class);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_AllCriteriaMet_ReturnsTrue() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setEnableLclConsolidation(true);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    Assertions.assertTrue(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NonSeaTransportMode_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setEnableLclConsolidation(false);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    assertFalse(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NullTransportMode_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setShipmentType(Constants.IMP);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    Assertions.assertFalse(result);
  }

  @Test
  void testCheckConsolidationEligibleForCFSValidation_NonExportShipmentType_ReturnsFalse() {
    // Arrange
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    // Act
    boolean result = consolidationV3Util.checkConsolidationEligibleForCFSValidation(consolidationDetails);

    // Assert
    Assertions.assertFalse(result);
  }

  @Test
  void getShipmentListTest1(){
    ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
    consoleShipmentMapping.setShipmentId(1L);

    when(consoleShipmentMappingDao.findByConsolidationId(anyLong()))
        .thenReturn(List.of(consoleShipmentMapping));
    ShipmentDetails shipmentDetails = new ShipmentDetails();
    PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(List.of(shipmentDetails));

    when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

    List<ShipmentDetails> result = consolidationV3Util.getShipmentsList(1L);
    Assertions.assertNotNull(result);
  }

  @Test
  void testCreate_Success() throws RunnerException {
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
    ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
    commonRequestModel.setData(copy);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    ConsolidationDetails consolidationDetail = testConsol;

    var spyService = Mockito.spy(consolidationV3Util);

    when(commonUtils.convertToEntityList(anyList(), any(), eq(true))).thenReturn(List.of());

    when(commonUtils.convertToEntityList(anyList(), any(), eq(false))).thenReturn(List.of());

    when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
    when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
    when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
    when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
    when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
    when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());

    assertDoesNotThrow(()-> spyService.afterSaveForET(consolidationDetail, null, testConsolRequest, true, shipmentSettingsDetails, false, true));

  }

  @Test
  void testCreate2_Success() throws RunnerException {
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
    ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
    commonRequestModel.setData(copy);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAutoEventCreate(true);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    ConsolidationDetails consolidationDetail = testConsol;

    var spyService = Mockito.spy(consolidationV3Util);

    when(commonUtils.convertToEntityList(anyList(), any(), eq(true))).thenReturn(List.of());

    when(commonUtils.convertToEntityList(anyList(), any(), eq(false))).thenReturn(List.of());

    when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
    when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
    when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
    when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
    when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
    when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());

    assertDoesNotThrow(()-> spyService.afterSaveForET(consolidationDetail, null, testConsolRequest, true, shipmentSettingsDetails, false, true));

  }

  @Test
  void testCreate3_Success() throws RunnerException {
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
    ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
    commonRequestModel.setData(copy);
    ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setAutoEventCreate(true);
    lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

    ConsolidationDetails consolidationDetail = testConsol;

    testConsolRequest.setRoutingsList(null);
    testConsolRequest.setReferenceNumbersList(null);
    testConsolRequest.setTruckDriverDetails(Collections.singletonList(TruckDriverDetailsRequest.builder().build()));

    var spyService = Mockito.spy(consolidationV3Util);

    when(commonUtils.convertToEntityList(anyList(), any(), eq(true))).thenReturn(List.of());

    when(commonUtils.convertToEntityList(anyList(), any(), eq(false))).thenReturn(List.of());

    when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
    when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
    when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
    when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());

    assertDoesNotThrow(()-> spyService.afterSaveForET(consolidationDetail, null, testConsolRequest, true, shipmentSettingsDetails, false, true));

  }
}
