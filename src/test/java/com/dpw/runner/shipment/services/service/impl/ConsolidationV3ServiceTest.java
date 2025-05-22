package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.AutoAttachConsolidationV3Request;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.junit.jupiter.api.AfterEach;
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
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationV3ServiceTest extends CommonMocks {
  @Mock
  private ExecutorService executorService;

  @Mock
  private CacheManager cacheManager;

  @Mock
  private CustomKeyGenerator keyGenerator;

  @Mock
  private IConsolidationDetailsDao consolidationDetailsDao;

  @Mock
  private IConsoleShipmentMappingDao consoleShipmentMappingDao;

  @Mock
  private IPartiesDao partiesDao;

  @Mock
  private IPackingsSync packingsADSync;

  @Mock
  private ConsolidationValidationV3Util consolidationValidationV3Util;

  @Mock
  private ConsolidationV3Util consolidationV3Util;

  @Mock
  private JsonHelper jsonHelper;

  @Mock
  private KafkaProducer producer;

  @Mock
  private ITrackingServiceAdapter trackingServiceAdapter;

  @Mock
  private ILogsHistoryService logsHistoryService;

  @Mock
  private BillingServiceAdapter billingServiceAdapter;

  @Mock
  private IPackingDao packingDao;

  @Mock
  private IPackingV3Service packingV3Service;

  @Mock
  private IEventDao eventDao;

  @Mock
  private IRoutingsV3Service routingsV3Service;

  @Mock
  private IEventsV3Service eventV3Service;

  @Mock
  private IShipmentServiceV3 shipmentV3Service;

  @Mock
  private IContainerDao containerDao;

  @Mock
  private IContainerV3Service containerV3Service;

  @Mock
  private IContainerService containerService;

  @Mock
  private IPackingService packingService;

  @Mock
  private IShipmentDao shipmentDao;

  @Mock
  private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

  @Mock
  private INotificationDao notificationDao;

  @Mock
  private IShipmentSync shipmentSync;

  @Mock
  private IConsolidationSync consolidationSync;

  @Mock
  private MasterDataUtils masterDataUtils;

  @Mock
  private MasterDataKeyUtils masterDataKeyUtils;

  @Mock
  private IAwbDao awbDao;

  @Mock
  private INetworkTransferService networkTransferService;

  @Mock
  private INetworkTransferDao networkTransferDao;

  @Mock
  private IEventService eventService;

  @Mock
  private V1ServiceUtil v1ServiceUtil;

  @Mock
  private IAuditLogService auditLogService;

  @Mock
  private IReferenceNumbersDao referenceNumbersDao;

  @Mock
  private ITruckDriverDetailsDao truckDriverDetailsDao;

  @Mock
  private DependentServiceHelper dependentServiceHelper;

  @Mock
  private BookingIntegrationsUtility bookingIntegrationsUtility;

  @Mock
  private NetworkTransferV3Util networkTransferV3Util;

  @Mock
  private IShipmentSettingsDao shipmentSettingsDao;

  @Mock
  private IV1Service v1Service;

  @Mock
  private ProductIdentifierUtility productEngine;

  @Mock
  private GetNextNumberHelper getNextNumberHelper;

  @Mock
  private ModelMapper modelMapper;

  @Mock
  @Qualifier("executorServiceMasterData")
  private ExecutorService executorServiceMasterData;

  @InjectMocks
  private ConsolidationV3Service consolidationV3Service;

  private static JsonTestUtility jsonTestUtility;
  private static ObjectMapper objectMapperTest;
  private static ConsolidationDetails testConsol;
  private static ShipmentDetails shipmentDetails;
  private static CustomerBookingV3Request customerBookingV3Request;


  private static ConsolidationDetailsResponse testConsolResponse;
  private static ConsolidationDetailsV3Request consolidationDetailsV3Request;

  private static ModelMapper modelMapperTest = new ModelMapper();
  private ConsolidationDetails consolidationDetails;
  private List<ModuleValidationFieldType> missingFields;

  @BeforeAll
  static void init() throws IOException {
    jsonTestUtility = new JsonTestUtility();
    objectMapperTest = JsonTestUtility.getMapper();
    modelMapperTest.getConfiguration().setAmbiguityIgnored(true);
  }

  @BeforeEach
  void setUp() {
    UsersDto mockUser = new UsersDto();
    mockUser.setTenantId(1);
    mockUser.setUsername("user");
    UserContext.setUser(mockUser);
    ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").enableRouteMaster(true).build());
    TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
    testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
    testConsolResponse = objectMapperTest.convertValue(testConsol , ConsolidationDetailsResponse.class);
    consolidationDetailsV3Request = objectMapperTest.convertValue(testConsol , ConsolidationDetailsV3Request.class);
    consolidationV3Service.executorService = Executors.newFixedThreadPool(2);
    consolidationV3Service.executorServiceMasterData = Executors.newFixedThreadPool(2);
    shipmentDetails = jsonTestUtility.getCompleteShipment();
    consolidationDetails = new ConsolidationDetails();
    customerBookingV3Request = new CustomerBookingV3Request();
    missingFields = new ArrayList<>();
  }

  @AfterEach
  void tearDown() {
    consolidationV3Service.executorService.shutdown();
    consolidationV3Service.executorServiceMasterData.shutdown();
  }

  private Runnable mockRunnable() {
    return null;
  }

  @Test
  void createTest_Success(){
    var spyService = Mockito.spy(consolidationV3Service);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationDetailsV3Response consolidationDetailsResponse = new ConsolidationDetailsV3Response();
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsV3Response.class))).thenReturn(consolidationDetailsResponse);
    ConsolidationDetailsV3Response createResponse = spyService.create(consolidationDetailsV3Request);

    assertNotNull(createResponse);
  }

  @Test
  void createTest_Success1(){
    consolidationDetails.setInterBranchConsole(null);
    consolidationDetails.setSourceTenantId(1L);
    consolidationDetails.setDocumentationPartner(0L);
    consolidationDetails.setReceivingBranch(0L);
    List<TriangulationPartner> triangulationPartnerList = new ArrayList<>();
    TriangulationPartner triangulationPartner = new TriangulationPartner();
    triangulationPartner.setTriangulationPartner(0L);
    triangulationPartnerList.add(triangulationPartner);

    var spyService = Mockito.spy(consolidationV3Service);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationDetailsV3Response consolidationDetailsResponse = new ConsolidationDetailsV3Response();
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsV3Response.class))).thenReturn(consolidationDetailsResponse);
    ConsolidationDetailsV3Response createResponse = spyService.create(consolidationDetailsV3Request);

    assertNotNull(createResponse);
  }

  @Test
  void createTest_Success2(){
    consolidationDetails.setInterBranchConsole(null);
    consolidationDetails.setSourceTenantId(1L);
    consolidationDetails.setDocumentationPartner(0L);
    consolidationDetails.setReceivingBranch(0L);
    List<TriangulationPartner> triangulationPartnerList = new ArrayList<>();
    TriangulationPartner triangulationPartner = new TriangulationPartner();
    triangulationPartner.setTriangulationPartner(0L);
    triangulationPartnerList.add(triangulationPartner);
    triangulationPartnerList.add(triangulationPartner);

    var spyService = Mockito.spy(consolidationV3Service);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationDetailsV3Response consolidationDetailsResponse = new ConsolidationDetailsV3Response();
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsV3Response.class))).thenReturn(consolidationDetailsResponse);
    ConsolidationDetailsV3Response createResponse = spyService.create(consolidationDetailsV3Request);

    assertNotNull(createResponse);
  }

  @Test
  void createTest_Failure(){
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(commonUtils.getShipmentSettingFromContext()).thenThrow(new ValidationException("Exception"));

    assertThrows(ValidationException.class, () -> consolidationV3Service.create(consolidationDetailsV3Request));
  }


  @Test
  void testCreateFromBooking_Success() {
    // Setup
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
    ConsolidationDetails consoleDetails = testConsol;
    ConsolidationDetailsResponse expectedResponse = testConsolResponse;

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    mockShipmentSettings();
    var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

    assertNotNull(response);
  }

  @Test
  void testCreateFromBooking_AuditLogException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
    // Setup
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
    ConsolidationDetails consoleDetails = testConsol;
    ConsolidationDetailsResponse expectedResponse = testConsolResponse;

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    lenient().when(jsonHelper.convertValue(consoleDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
    mockShipmentSettings();
    doThrow(new IllegalAccessException("IllegalAccessException")).when(auditLogService).addAuditLog(any());
    assertThrows(ValidationException.class, () -> consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request));
  }

  @Test
  void testCreateFromBooking_RequestIsNull() {
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
    assertThrows(ValidationException.class, () -> consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request));
  }


  @Test
  void testCreateFromBooking_ThrowsValidationException() {
    // Setup
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
    ConsolidationDetails consoleDetails = testConsol;

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(commonUtils.getShipmentSettingFromContext()).thenThrow(new ValidationException("Exception"));

    assertThrows(ValidationException.class, () -> consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request));
  }

  @Test
  void testRetrieveByIdOrGuid_IdSuccess() throws RunnerException {
    ConsolidationDetailsV3Request request = ConsolidationDetailsV3Request.builder().id(1L).build();
    consolidationDetails = testConsol;
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
    var consolidationDetailsResponse = consolidationV3Service.retrieveByIdOrGuid(request);

    assertEquals(request.getId(), consolidationDetailsResponse.get().getId());
  }

  @Test
  void testRetrieveByIdOrGuid_GuidSuccess() throws RunnerException {
    ConsolidationDetailsV3Request request = new ConsolidationDetailsV3Request();
    request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
    consolidationDetails = testConsol;
    when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
    var consolidationDetailsResponse = consolidationV3Service.retrieveByIdOrGuid(request);

    assertEquals(request.getGuid(), consolidationDetailsResponse.get().getGuid());
  }

  @Test
  void testRetrieveByIdOrGuid_NullRequest_Failure() {
    ConsolidationDetailsV3Request request = null;
    assertThrows(NullPointerException.class, ()-> consolidationV3Service.retrieveByIdOrGuid(request));
  }
  @Test
  void testRetrieveByIdOrGuid_EmptyRequest_Failure() {
    ConsolidationDetailsV3Request request = new ConsolidationDetailsV3Request();
    assertThrows(RunnerException.class, ()-> consolidationV3Service.retrieveByIdOrGuid(request));
  }

  @Test
  void testRetrieveByIdOrGuid_DataRetrieveForId_Failure() {
    ConsolidationDetailsV3Request request = ConsolidationDetailsV3Request.builder().id(1L).build();
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
    assertThrows(DataRetrievalFailureException.class, ()-> consolidationV3Service.retrieveByIdOrGuid(request));
  }
  @Test
  void testRetrieveByIdOrGuid_DataRetrieveForGuid_Failure() {
    ConsolidationDetailsV3Request request = new ConsolidationDetailsV3Request();
    request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
    when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
    assertThrows(DataRetrievalFailureException.class, ()-> consolidationV3Service.retrieveByIdOrGuid(request));
  }

  @Test
  void testGenerateConsolidationNumber_Success() throws RunnerException {
    ConsolidationDetails consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setConsolidationLite(false);
    var spyService = Mockito.spy(consolidationV3Service);
    when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(true);
    when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
    when(productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("CONS007262");
    when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
    when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
    when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("BOL23131");
    mockShipmentSettings();
    spyService.generateConsolidationNumber(consolidationDetails);
    assertEquals("CONS007262", consolidationDetails.getConsolidationNumber());
    assertEquals("CONS007262", consolidationDetails.getReferenceNumber());
    assertEquals("BOL23131", consolidationDetails.getBol());
  }

  @Test
  void testGenerateConsolidationNumber_Success1() throws RunnerException {
    ConsolidationDetails consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setConsolidationLite(false);
    var spyService = Mockito.spy(consolidationV3Service);
    when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(true);
    when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
    when(productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("");
    when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
    when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
    when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
    when(v1Service.getMaxConsolidationId()).thenReturn("123311");
    doReturn("BOL2121").when(spyService).generateCustomBolNumber();
    mockShipmentSettings();
    spyService.generateConsolidationNumber(consolidationDetails);
    assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
    assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
    assertEquals("BOL2121", consolidationDetails.getBol());
  }

  @Test
  void testGenerateConsolidationNumber_Success2() throws RunnerException {
    consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setConsolidationLite(false);
    var spyService = Mockito.spy(consolidationV3Service);
    when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(false);
    when(productEngine.populateEnabledTenantProducts()).thenReturn(List.of(tenantProducts));
    when(productEngine.identifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
    when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
    when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
    when(v1Service.getMaxConsolidationId()).thenReturn("123311");
    doReturn("BOL2121").when(spyService).generateCustomBolNumber();
    mockShipmentSettings();
    spyService.generateConsolidationNumber(consolidationDetails);
    assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
    assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
    assertEquals("BOL2121", consolidationDetails.getBol());
  }

  @Test
  void testGenerateConsolidationNumberWithConsolidationLiteTrue() throws RunnerException {
    consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setConsolidationLite(true);
    when(shipmentSettingsDao.getCustomisedSequence()).thenReturn(false);
    var spyService = Mockito.spy(consolidationV3Service);
    when(v1Service.getMaxConsolidationId()).thenReturn("123311");
    mockShipmentSettings();
    spyService.generateConsolidationNumber(consolidationDetails);
    assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
    assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
    assertNull(consolidationDetails.getBol());
  }

  @Test
  void testGenerateCustomBolNumber_Success_Serial() {
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setBolNumberPrefix("CONS");
    shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Serial);
    ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
    when(v1Service.getMaxConsolidationId()).thenReturn("2313");
    mockShipmentSettings();

    String res = consolidationV3Service.generateCustomBolNumber();
    assertEquals("CONS2313", res);
  }

  @Test
  void testGenerateCustomBolNumber_Success_Random() {
    ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
    shipmentSettingsDetails.setBolNumberPrefix("CONS");
    shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Random);
    ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
    mockShipmentSettings();
    String res = consolidationV3Service.generateCustomBolNumber();
    assertEquals(14, res.length());
  }

  @Test
  void testGetAutoAttachConsolidationDetails_Success() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
    mockShipmentSettings();
    ConsolidationListV3Response response = consolidationV3Service.getAutoAttachConsolidationDetails(commonRequestModel);
    assertNotNull(response);
  }

  @Test
  void testGetAutoAttachConsolidationDetails_Success2() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    request.setDirection(Constants.IMP);
    request.setShipId(1L);
    request.setBranchIds(new ArrayList<>());
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
    mockShipmentSettings();
    ConsolidationListV3Response response = consolidationV3Service.getAutoAttachConsolidationDetails(commonRequestModel);
    assertNotNull(response);
  }

  @Test
  void testGetAutoAttachConsolidationDetails_Success3() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    request.setDirection(Constants.IMP);
    request.setShipId(1L);
    request.setBranchIds(new ArrayList<>());
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
    when(consoleShipmentMappingDao.findByShipmentIdAll(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build()));
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
    mockShipmentSettings();
    ConsolidationListV3Response response = consolidationV3Service.getAutoAttachConsolidationDetails(commonRequestModel);
    assertNotNull(response);
  }

  @Test
  void testList() {
    assertThrows(ValidationException.class, () -> consolidationV3Service.list(null, true));
  }

  @Test
  void updateLinkedShipmentData_Exception(){
    V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
    tenantSettingsResponse.setEnableAirMessaging(Boolean.TRUE);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

    consolidationDetails = testConsol;
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setEfreightStatus(Constants.EAW);

    List<ShipmentDetails> shipments = new ArrayList<>();
    AdditionalDetails additionalDetails = new AdditionalDetails();
    additionalDetails.setEfreightStatus(Constants.NON);

    ShipmentDetails shipmentDetails1 = ShipmentDetails
        .builder()
        .additionalDetails(additionalDetails)
        .build();
    shipments.add(shipmentDetails1);

    when(consolidationV3Util.getShipmentsList(consolidationDetails.getId()))
        .thenReturn(shipments);

    assertThrows(RunnerException.class, () -> consolidationV3Service.updateLinkedShipmentData(consolidationDetails,
        null, true));
  }

  @Test
  void updateLinkedShipmentData_Success() throws RunnerException {
    mockTenantSettings();

    consolidationDetails = testConsol;
    CarrierDetails carrierDetails = CarrierDetails.builder().build();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setEfreightStatus(Constants.EAW);
    consolidationDetails.setSendingAgent(new Parties());
    consolidationDetails.setReceivingAgent(new Parties());
    ConsolidationDetails oldConsolidation = consolidationDetails;
    oldConsolidation.setEarliestDropOffFullEquToCarrier(LocalDateTime.now());

    List<ShipmentDetails> shipments = new ArrayList<>();
    AdditionalDetails additionalDetails = new AdditionalDetails();
    additionalDetails.setEfreightStatus(Constants.NON);

    ShipmentDetails shipmentDetails1 = ShipmentDetails
        .builder()
        .additionalDetails(additionalDetails)
        .transportMode(Constants.TRANSPORT_MODE_SEA)
        .carrierDetails(carrierDetails)
        .build();
    shipments.add(shipmentDetails1);

    when(jsonHelper.convertCreateValue(any(), eq(Routings.class))).thenReturn(new Routings());

    when(consolidationV3Util.getShipmentsList(consolidationDetails.getId()))
        .thenReturn(shipments);

    when(routingsV3Service.getRoutingsByShipmentId(any())).thenReturn(new ArrayList<>());
    List<ShipmentDetails> shipmentDetailsList = consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true);
    assertNotNull(shipmentDetailsList);
  }

  @Test
  void updateLinkedShipmentData_Success1() throws RunnerException {
    mockTenantSettings();

    consolidationDetails = testConsol;
    consolidationDetails.setRoutingsList(List.of(Routings.builder().build()));
    CarrierDetails carrierDetails = CarrierDetails.builder().build();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setEfreightStatus(Constants.EAW);
    consolidationDetails.setBookingNumber("BOOKING123");
    consolidationDetails.setInterBranchConsole(false);
    ConsolidationDetails oldConsolidation = consolidationDetails;
    oldConsolidation.setEarliestDropOffFullEquToCarrier(LocalDateTime.now());

    List<ShipmentDetails> shipments = new ArrayList<>();
    AdditionalDetails additionalDetails = new AdditionalDetails();
    additionalDetails.setEfreightStatus(Constants.NON);

    ShipmentDetails shipmentDetails1 = ShipmentDetails
        .builder()
        .transportMode(Constants.TRANSPORT_MODE_SEA)
        .carrierDetails(carrierDetails)
        .bookingNumber("BOOKING1234")
        .direction(DIRECTION_EXP)
        .build();
    shipments.add(shipmentDetails1);

    lenient().when(jsonHelper.convertCreateValue(any(), eq(Routings.class))).thenReturn(new Routings());

    when(consolidationV3Util.getShipmentsList(consolidationDetails.getId()))
        .thenReturn(shipments);

    lenient().when(routingsV3Service.getRoutingsByShipmentId(any())).thenReturn(new ArrayList<>());
    List<ShipmentDetails> shipmentDetailsList = consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true);
    assertNotNull(shipmentDetailsList);
  }

  @Test
  void updateLinkedShipmentData_Exception1() throws RunnerException {
    mockTenantSettings();

    consolidationDetails = testConsol;
    consolidationDetails.setRoutingsList(List.of(Routings.builder().build()));
    CarrierDetails carrierDetails = CarrierDetails.builder().build();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    consolidationDetails.setEfreightStatus(Constants.EAW);
    consolidationDetails.setBookingNumber("BOOKING123");
    consolidationDetails.setInterBranchConsole(true);
    ConsolidationDetails oldConsolidation = consolidationDetails;
    oldConsolidation.setEarliestDropOffFullEquToCarrier(LocalDateTime.now());

    List<ShipmentDetails> shipments = new ArrayList<>();
    AdditionalDetails additionalDetails = new AdditionalDetails();
    additionalDetails.setEfreightStatus(Constants.NON);

    ShipmentDetails shipmentDetails1 = ShipmentDetails
        .builder()
        .transportMode(Constants.TRANSPORT_MODE_SEA)
        .carrierDetails(carrierDetails)
        .bookingNumber("BOOKING1234")
        .isReceivingBranchAdded(true)
        .direction(DIRECTION_EXP)
        .build();
    shipments.add(shipmentDetails1);

    lenient().when(jsonHelper.convertCreateValue(any(), eq(Routings.class))).thenReturn(new Routings());

    when(consolidationV3Util.getShipmentsList(consolidationDetails.getId()))
        .thenReturn(shipments);
    when(consolidationV3Util.checkConsolidationEligibleForCFSValidation(any())).thenReturn(true);
    when(consolidationValidationV3Util.checkIfShipmentDateGreaterThanConsole(any(), any())).thenReturn(true);

    lenient().when(routingsV3Service.getRoutingsByShipmentId(any())).thenReturn(new ArrayList<>());
    assertThrows(RunnerException.class, () -> consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true));

  }

  @Test
  void completeUpdate_NullEntityException() throws RunnerException {
    consolidationDetailsV3Request.setId(1L);
    var spyService = Mockito.spy(consolidationV3Service);
    Mockito.doReturn(Optional.empty()).when(spyService).retrieveByIdOrGuid(any());
    assertThrows(DataRetrievalFailureException.class,
        () -> spyService.completeUpdate(consolidationDetailsV3Request));
  }

  @Test
  void completeUpdate_Success() throws RunnerException {
    consolidationDetailsV3Request.setId(1L);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);

    var spyService = Mockito.spy(consolidationV3Service);
    Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());

    when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(jsonHelper.convertValue(any(), eq(AllocationsResponse.class))).thenReturn(new AllocationsResponse());
    when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(new AchievedQuantitiesResponse());
    when(jsonHelper.convertToJson(any())).thenReturn("ABC");
    when(consolidationDetailsDao.updateV3(any())).thenReturn(consolidationDetails);
    when(jsonHelper.readFromJson(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    mockShipmentSettings();
    mockTenantSettings();
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsV3Response.class))).thenReturn(new ConsolidationDetailsV3Response());

    ConsolidationDetailsV3Response consolidationDetailsV3Response = spyService.completeUpdate(consolidationDetailsV3Request);
    assertNotNull(consolidationDetailsV3Response);
  }
}
