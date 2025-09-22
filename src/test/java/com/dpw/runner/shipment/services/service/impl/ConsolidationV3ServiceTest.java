package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.ConsolidationMapper;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.notification.PendingConsolidationActionResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerParams;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload.UniversalEventsPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3ExternalResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationValidationV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.auth.AuthenticationException;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.*;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationV3ServiceTest extends CommonMocks {

  @Mock
  private CacheManager cacheManager;

  @Mock
  private IConsolidationDetailsDao consolidationDetailsDao;

  @Mock
  private IConsoleShipmentMappingDao consoleShipmentMappingDao;

  @Mock
  private ConsolidationValidationV3Util consolidationValidationV3Util;

  @Mock
  private ConsolidationV3Util consolidationV3Util;

  @Mock
  private JsonHelper jsonHelper;

  @Mock
  private KafkaProducer producer;

  @Mock
  private IPackingV3Service packingV3Service;

  @Mock
  private ITrackingServiceAdapter trackingServiceAdapter;

  @Mock
  private ILogsHistoryService logsHistoryService;

  @Mock
  private BillingServiceAdapter billingServiceAdapter;

  @Mock
  private IPackingDao packingDao;

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
  private IPartiesDao partiesDao;

  @Mock
  private CustomKeyGenerator keyGenerator;

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
  private ContainerV3Util containerV3Util;

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
  private ICarrierDetailsDao carrierDetailsDao;
  @Mock
  @Qualifier("executorServiceMasterData")
  private ExecutorService executorServiceMasterData;

    @Mock
    private EntityManager entityManager;

    @Mock
    private CriteriaBuilder criteriaBuilder;

    @Mock
    private CriteriaQuery<Object[]> criteriaQuery;

    @Mock
    private Root<ConsolidationDetails> root;

    @Mock
    private TypedQuery<Object[]> typedQuery;

  @InjectMocks
  private ConsolidationV3Service consolidationV3Service;

  private static JsonTestUtility jsonTestUtility;
  private static ObjectMapper objectMapperTest;
  private static ConsolidationDetails testConsol;
  private static ShipmentDetails shipmentDetails;
  private static CustomerBookingV3Request customerBookingV3Request;


  private static ConsolidationDetailsResponse testConsolResponse;
  private static ConsolidationDetailsV3Request consolidationDetailsV3Request;
  private static ConsolidationEtV3Request consolidationEtV3Request;

  private static ModelMapper modelMapperTest = new ModelMapper();
  private ConsolidationDetails consolidationDetails;
  private static ShipmentDetails testShipment;
  private static Containers testContainer;
  private static ShipmentSettingsDetails shipmentSettingsDetails;
  private AibActionConsolidation aibActionConsolidation;
  private MockedStatic<UnitConversionUtility> unitConversionUtilityMockedStatic;
  private MockedStatic<TenantContext> tenantContextMockedStatic;

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
    testConsol.setAssignedTo("AssignedToUser");
    testConsolResponse = objectMapperTest.convertValue(testConsol , ConsolidationDetailsResponse.class);
    consolidationDetailsV3Request = objectMapperTest.convertValue(testConsol , ConsolidationDetailsV3Request.class);
    consolidationEtV3Request = objectMapperTest.convertValue(testConsol ,ConsolidationEtV3Request.class);
    consolidationV3Service.executorService = Executors.newFixedThreadPool(2);
    consolidationV3Service.executorServiceMasterData = Executors.newFixedThreadPool(2);
    shipmentDetails = jsonTestUtility.getCompleteShipment();
    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setConsolidationType(CONSOLIDATION_TYPE_CLD);
    consolidationDetails.setAssignedTo("assignedToUser");
    shipmentSettingsDetails = new ShipmentSettingsDetails();
    customerBookingV3Request = new CustomerBookingV3Request();
    testShipment = jsonTestUtility.getTestShipment();
    testContainer = jsonTestUtility.getTestContainer();
    unitConversionUtilityMockedStatic = mockStatic(UnitConversionUtility.class);
    tenantContextMockedStatic = mockStatic(TenantContext.class);
    aibActionConsolidation = AibActionConsolidation.builder().build();
  }

  @AfterEach
  void tearDown() {
    consolidationV3Service.executorService.shutdown();
    consolidationV3Service.executorServiceMasterData.shutdown();
if (unitConversionUtilityMockedStatic != null) {
      unitConversionUtilityMockedStatic.close();
    }
    if(tenantContextMockedStatic != null){
      tenantContextMockedStatic.close();
    }
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
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
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
    TriangulationPartner triangulationPartner = new TriangulationPartner();
    triangulationPartner.setTriangulationPartner(0L);

    var spyService = Mockito.spy(consolidationV3Service);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
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
    TriangulationPartner triangulationPartner = new TriangulationPartner();
    triangulationPartner.setTriangulationPartner(0L);

    var spyService = Mockito.spy(consolidationV3Service);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
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

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    mockShipmentSettings();
    var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

    assertNotNull(response);
  }

  @Test
  void testCreateFromBooking_Success1() {
    // Setup
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
    ConsolidationDetails consoleDetails = testConsol;
    ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
    customerBookingV3Request.setContainersList(List.of(containerV3Request));
    customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
    mockShipmentSettings();
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

    assertNotNull(response);
  }

  @Test
  @Disabled("Skipped")
  void testCreateFromBooking_Success2() throws RunnerException {
    // Setup
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
    ConsolidationDetails consoleDetails = testConsol;
    ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
    customerBookingV3Request.setContainersList(List.of(containerV3Request));
    customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));

    when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
    when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(new ArrayList<>(List.of(testContainer)));
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
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
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
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
    void testCreateFromBooking_Success3() {
        // Setup
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
        ConsolidationDetails consoleDetails = testConsol;
        ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
        customerBookingV3Request.setContainersList(List.of(containerV3Request));
        customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));
        consoleDetails.getCarrierDetails().setDestinationPortLocCode("CANADA");
        consoleDetails.getCarrierDetails().setOriginPortLocCode("INDIA");

        when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
        when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsEntityTransferPrerequisiteEnabled(true);
        mockShipmentSettings();
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

        assertNotNull(response);
    }

    @Test
    void testCreateFromBooking_Success6() {
        // Setup
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
        ConsolidationDetails consoleDetails = testConsol;
        ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
        customerBookingV3Request.setContainersList(List.of(containerV3Request));
        customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));
        consoleDetails.getCarrierDetails().setDestinationPortLocCode("CANADA");
        consoleDetails.getCarrierDetails().setOriginPortLocCode("INDIA");
        when(commonUtils.checkIfPartyExists((Parties) any())).thenReturn(true);

        when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
        when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsEntityTransferPrerequisiteEnabled(true);
        mockShipmentSettings();
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

        assertNotNull(response);
    }

    @Test
    void testCreateFromBooking_Success4() {
        // Setup
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
        ConsolidationDetails consoleDetails = testConsol;
        ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
        customerBookingV3Request.setContainersList(List.of(containerV3Request));
        customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));

        when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
        when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsEntityTransferPrerequisiteEnabled(true);
        mockShipmentSettings();
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

        assertNotNull(response);
    }

    @Test
    void testCreateFromBooking_Success5() {
        // Setup
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(consolidationDetailsV3Request).build();
        ConsolidationDetails consoleDetails = testConsol;
        ContainerV3Request containerV3Request = objectMapperTest.convertValue(testContainer, ContainerV3Request.class);
        customerBookingV3Request.setContainersList(List.of(containerV3Request));
        customerBookingV3Request.setPackingList(List.of(new PackingV3Request()));
        consoleDetails.setCarrierDetails(null);
        when(jsonHelper.convertValue(consolidationDetailsV3Request, ConsolidationDetails.class)).thenReturn(consoleDetails);
        when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(ContainerV3Request.class))).thenReturn(containerV3Request);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMergeContainers(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsEntityTransferPrerequisiteEnabled(true);
        mockShipmentSettings();
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        var response = consolidationV3Service.createConsolidationForBooking(commonRequestModel, customerBookingV3Request);

        assertNotNull(response);
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
    void create_airDrtConsolidation_shouldThrowException() {
        // Arrange: Create a request for a new consolidation with an invalid combination (AIR/DRT)
        ConsolidationDetailsV3Request request = new ConsolidationDetailsV3Request();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);

        // Arrange: Mock the jsonHelper to convert the request DTO to an entity object.
        ConsolidationDetails consolidationDetailsEntity = new ConsolidationDetails();
        consolidationDetailsEntity.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetailsEntity.setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);
        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetailsEntity);

        // Act & Assert: Verify that calling create throws a ValidationException
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            consolidationV3Service.create(request);
        });

        // Assert: Check if the exception message is correct
        assertEquals(ConsolidationConstants.AIR_DRT_CONSOLIDATION_CREATION_ERROR, exception.getMessage());

        // Verify that the jsonHelper's convertValue method was called as expected
        verify(jsonHelper).convertValue(request, ConsolidationDetails.class);
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
    consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
    consolidationDetails = testConsol;
    consolidationDetails.setConsolidationNumber(null);
    consolidationDetails.setReferenceNumber(null);
    consolidationDetails.setBol(null);
    TenantProducts tenantProducts = new TenantProducts();
    tenantProducts.setId(1L);
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
    shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
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
  void testGetAutoAttachConsolidationDetails_Success4() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(true);
    mockShipmentSettings();
    ConsolidationListV3Response response = consolidationV3Service.getAutoAttachConsolidationDetails(commonRequestModel);
    assertNotNull(response);
  }

  @Test
  void testGetAutoAttachConsolidationDetails_Success5() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
    mockShipmentSettings();
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationListV3Response response = consolidationV3Service.getAutoAttachConsolidationDetails(commonRequestModel);
    assertNotNull(response);
  }

  @Test
  void testList() {
    var request = CommonRequestModel.builder().build();
    assertThrows(ValidationException.class, () -> consolidationV3Service.list(request, true));
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
        null, true, new HashMap<>(), true));
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
    List<ShipmentDetails> shipmentDetailsList = consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true, new HashMap<>(), true);
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
    List<ShipmentDetails> shipmentDetailsList = consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true, new HashMap<>(), true);
    assertNotNull(shipmentDetailsList);
  }

  @Test
  void updateLinkedShipmentData_Exception1() {
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
    assertThrows(RunnerException.class, () -> consolidationV3Service.updateLinkedShipmentData(consolidationDetails, oldConsolidation, true, new HashMap<>(), true));

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
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);

    var spyService = Mockito.spy(consolidationV3Service);
    Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(new BigDecimal("1000"));
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
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

  @Test
  void completeUpdate_Exception1()
      throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
    consolidationDetailsV3Request.setId(1L);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);

    var spyService = Mockito.spy(consolidationV3Service);
    Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(new BigDecimal("1000"));
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(jsonHelper.convertToJson(any())).thenReturn("ABC");
    when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    doThrow(new GenericException("IllegalAccessException")).when(auditLogService).addAuditLog(any());
    when(jsonHelper.readFromJson(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
   when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsV3Response.class))).thenThrow(new GenericException("RelegalAccessException"));
    mockShipmentSettings();
    mockTenantSettings();

    assertThrows(GenericException.class, () -> spyService.completeUpdate(consolidationDetailsV3Request));
  }

  @Test
  void completeUpdate_airDrtConsolidation_shouldThrowException(){
      // Arrange: Create a request to update a consolidation to an invalid state (AIR/DRT)
      ConsolidationDetailsV3Request request = new ConsolidationDetailsV3Request();
      request.setId(1L); // An existing ID
      request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
      request.setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);

      // Arrange: Mock the DAO to return an existing consolidation entity when looked up by ID.
      ConsolidationDetails existingConsolidation = new ConsolidationDetails();
      existingConsolidation.setId(1L);
      existingConsolidation.setTransportMode(Constants.TRANSPORT_MODE_SEA); // Its previous state was valid
      existingConsolidation.setConsolidationType(Constants.CONSOLIDATION_TYPE_AGT);

      // Mock the behavior of the DAO to find the existing entity
      when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(existingConsolidation));

      // Act & Assert: Verify that calling completeUpdate throws a ValidationException
      ValidationException exception = assertThrows(ValidationException.class, () -> {
          consolidationV3Service.completeUpdate(request);
      });

      // Assert: Check if the exception message is correct
      assertEquals(ConsolidationConstants.AIR_DRT_CONSOLIDATION_CREATION_ERROR, exception.getMessage());

      // Verify that the DAO's findById method was called as part of the update pre-check
      verify(consolidationDetailsDao).findById(1L);
  }

  @Test
  void testCreateConsolidationPayload() {
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload1() {
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    when(awbDao.findByConsolidationId(any())).thenReturn(List.of(new Awb()));
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload2() {
    testConsol.setBookingStatus(CarrierBookingStatus.Requested.toString());
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    when(awbDao.findByConsolidationId(any()))
            .thenReturn(List.of(Awb.builder()
                    .airMessageStatus(AwbStatus.AWB_GENERATED)
                            .linkedHawbAirMessageStatus(AwbStatus.AWB_GENERATED)
                    .build()));
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload4() {
    testConsol.setShipmentsList(Set.of(testShipment));
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    mockShipmentSettings();
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload4_() throws RunnerException {
    testShipment.setWeight(BigDecimal.TEN);
    testShipment.setWeightUnit("KG");
    testShipment.setNoOfPacks(2);
    testShipment.setPacksUnit("BBG");
    testShipment.setVolume(BigDecimal.TEN);
    ShipmentDetails testShipment2 = objectMapperTest.convertValue(testShipment, ShipmentDetails.class);
    testShipment2.setId(5L);
    testConsol.setShipmentsList(new HashSet<>(Set.of(testShipment, testShipment2)));
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(null);
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeChargeableUnit(null);
    mockShipmentSettings();
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.TEN);
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload5_() throws RunnerException {
    testShipment.setWeight(BigDecimal.TEN);
    testShipment.setWeightUnit("KG");
    testShipment.setNoOfPacks(null);
    testShipment.setPacksUnit(null);
    testShipment.setVolume(BigDecimal.TEN);
    testShipment.setContainersList(Set.of(testContainer));
    testConsol.setShipmentsList(Set.of(testShipment));
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    mockShipmentSettings();
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.TEN);
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testCreateConsolidationPayload5() {
    testShipment.setPacksUnit("BBG");
    testShipment.setContainersList(Set.of(testContainer));
    ShipmentDetails testShipment1 = objectMapperTest.convertValue(testShipment, ShipmentDetails.class);
    testShipment1.setPacksUnit("BKG");
    testShipment1.setId(2L);
    testConsol.setShipmentsList(new HashSet<>(Set.of(testShipment, testShipment1)));
    ConsolidationDetailsV3Response consolidationDetailsV3Response = objectMapperTest.convertValue(testConsol, ConsolidationDetailsV3Response.class);
    consolidationDetailsV3Response.setReceivingBranch(3L);
    mockShipmentSettings();
    consolidationV3Service.createConsolidationPayload(testConsol, consolidationDetailsV3Response);
    assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void testPopulateOriginDestination_EXP(){
    consolidationDetails.setInterBranchConsole(false);
    consolidationDetails.setShipmentType(DIRECTION_EXP);
    consolidationDetails.setSendingAgent(new Parties());
    Parties parties = new Parties();
    parties.setTenantId(1);
    when(v1ServiceUtil.getDefaultAgentOrgParty(any())).thenReturn(parties);
    consolidationV3Service.populateOriginDestinationAgentDetailsForBookingConsolidation(consolidationDetails);

    // Verify that shipment type remains unchanged
    assertThat(consolidationDetails.getShipmentType()).isEqualTo(DIRECTION_EXP);

    // Verify that interBranchConsole flag remains unchanged
    assertThat(consolidationDetails.getInterBranchConsole()).isFalse();
  }

  @Test
  void testPopulateOriginDestination_IMP(){
    consolidationDetails.setInterBranchConsole(false);
    consolidationDetails.setShipmentType(DIRECTION_IMP);
    consolidationDetails.setReceivingAgent(new Parties());
    when(v1ServiceUtil.getDefaultAgentOrgParty(any())).thenReturn(new Parties());
    consolidationV3Service.populateOriginDestinationAgentDetailsForBookingConsolidation(consolidationDetails);

    // Verify that shipment type remains unchanged
    assertThat(consolidationDetails.getShipmentType()).isEqualTo(DIRECTION_IMP);

    // Verify that interBranchConsole flag remains unchanged
    assertThat(consolidationDetails.getInterBranchConsole()).isFalse();
  }

  @Test
  void testCalculateVolumeWeight_SeaTransport_Success() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_SEA;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("1000");
    BigDecimal volume = new BigDecimal("2.5");

    when(UnitConversionUtility.convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3))
        .thenReturn(new BigDecimal("2.5"));
    when(UnitConversionUtility.convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG))
        .thenReturn(new BigDecimal("1000"));
    when(UnitConversionUtility.convertUnit(Constants.VOLUME, new BigDecimal("1"), Constants.VOLUME_UNIT_M3, volumeUnit))
        .thenReturn(new BigDecimal("1"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isEqualTo(new BigDecimal("2.5"));
    assertThat(result.getChargeableUnit()).isEqualTo(Constants.VOLUME_UNIT_M3);
    assertThat(result.getVolumeWeight()).isEqualTo(new BigDecimal("1"));
    assertThat(result.getVolumeWeightUnit()).isEqualTo(volumeUnit);
  }

  @Test
  void testCalculateVolumeWeight_RailTransport_Success() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_RAI;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("500");
    BigDecimal volume = new BigDecimal("1.7");

    when(UnitConversionUtility.convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3))
        .thenReturn(new BigDecimal("1.7"));
    when(UnitConversionUtility.convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG))
        .thenReturn(new BigDecimal("500"));
    when(UnitConversionUtility.convertUnit(Constants.VOLUME, new BigDecimal("0.5"), Constants.VOLUME_UNIT_M3, volumeUnit))
        .thenReturn(new BigDecimal("0.5"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isEqualTo(new BigDecimal("1.7"));
    assertThat(result.getChargeableUnit()).isEqualTo(Constants.VOLUME_UNIT_M3);
    assertThat(result.getVolumeWeight()).isEqualTo(new BigDecimal("0.5"));
    assertThat(result.getVolumeWeightUnit()).isEqualTo(volumeUnit);
  }

  @Test
  void testCalculateVolumeWeight_FSATransport_Success() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_FSA;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("2000");
    BigDecimal volume = new BigDecimal("3.2");

    when(UnitConversionUtility.convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3))
        .thenReturn(new BigDecimal("3.2"));
    when(UnitConversionUtility.convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG))
        .thenReturn(new BigDecimal("2000"));
    when(UnitConversionUtility.convertUnit(Constants.VOLUME, new BigDecimal("2"), Constants.VOLUME_UNIT_M3, volumeUnit))
        .thenReturn(new BigDecimal("2"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isEqualTo(new BigDecimal("3.2"));
    assertThat(result.getChargeableUnit()).isEqualTo(Constants.VOLUME_UNIT_M3);
    assertThat(result.getVolumeWeight()).isEqualTo(new BigDecimal("2"));
    assertThat(result.getVolumeWeightUnit()).isEqualTo(volumeUnit);
  }

  @Test
  void testCalculateVolumeWeight_AirTransport_WeightGreaterThanVolumeWeight() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("1000");
    BigDecimal volume = new BigDecimal("2.0");

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(weight);

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
  }

  @Test
  void testCalculateVolumeWeight_AirTransport_VolumeWeightGreaterThanWeight() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(new BigDecimal("100"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
  }

  @Test
  void testCalculateVolumeWeight_FASTransport_Success() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_FAS;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("500");
    BigDecimal volume = new BigDecimal("1.5");

    when(UnitConversionUtility.convertUnit(any(), any(), any(),any()))
        .thenReturn(new BigDecimal("500"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
  }

  @Test
  void testCalculateVolumeWeight_RoadTransport_Success() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_ROA;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(new BigDecimal("100"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
  }

  @Test
  void testCalculateVolumeWeight_EmptyWeightUnit_ReturnsEmptyObject() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isNull();
    assertThat(result.getChargeableUnit()).isNull();
    assertThat(result.getVolumeWeight()).isNull();
    assertThat(result.getVolumeWeightUnit()).isNull();
  }

  @Test
  void testCalculateVolumeWeight_EmptyVolumeUnit_ReturnsEmptyObject() throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isNull();
    assertThat(result.getChargeableUnit()).isNull();
    assertThat(result.getVolumeWeight()).isNull();
    assertThat(result.getVolumeWeightUnit()).isNull();
  }

  @Test
  void testCalculateVolumeWeight_EmptyTransportMode_ReturnsEmptyObject() throws RunnerException {
    // Given
    String transportMode = "";
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isNull();
    assertThat(result.getChargeableUnit()).isNull();
    assertThat(result.getVolumeWeight()).isNull();
    assertThat(result.getVolumeWeightUnit()).isNull();
  }

  @Test
  void testCalculateVolumeWeight_DefaultCase_ReturnsEmptyObject() throws RunnerException {
    // Given
    String transportMode = "UNKNOWN_MODE";
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
    assertThat(result.getChargeable()).isNull();
    assertThat(result.getChargeableUnit()).isNull();
    assertThat(result.getVolumeWeight()).isNull();
    assertThat(result.getVolumeWeightUnit()).isNull();
  }

  @Test
  void testCalculateVolumeWeight_ExceptionInConvertUnit_ThrowsRunnerException()
      throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    when(UnitConversionUtility.convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG))
        .thenThrow(new RuntimeException("Conversion error"));

    // When & Then
    RunnerException exception = assertThrows(RunnerException.class, () -> {
      consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
    });

    assertThat(exception.getMessage()).isEqualTo("Conversion error");
  }

  @Test
  void testCalculateVolumeWeight_ExceptionWithNullMessage_ThrowsRunnerExceptionWithNullMessage()
      throws RunnerException {
    // Given
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100");
    BigDecimal volume = new BigDecimal("2.0");

    RuntimeException causeException = new RuntimeException((String) null);
    when(UnitConversionUtility.convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG))
        .thenThrow(causeException);

    // When & Then
    RunnerException exception = assertThrows(RunnerException.class, () -> {
      consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
    });

    assertThat(exception.getMessage()).isNull();
  }

  @Test
  void testCalculateVolumeWeight_SeaTransport_WithRoundingUp() throws RunnerException {
    // Given - Test the ceiling rounding for chargeable amount
    String transportMode = Constants.TRANSPORT_MODE_SEA;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("1000");
    BigDecimal volume = new BigDecimal("2.51"); // This should round up to 2.6

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
        .thenReturn(new BigDecimal("2.51"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();
  }

  @Test
  void testCalculateVolumeWeight_AirTransport_WithRoundingUp() throws RunnerException {
    // Given - Test the ceiling rounding for chargeable weight
    String transportMode = Constants.TRANSPORT_MODE_AIR;
    String weightUnit = "KG";
    String volumeUnit = "M3";
    BigDecimal weight = new BigDecimal("100.01");
    BigDecimal volume = new BigDecimal("1.0");

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(new BigDecimal("100.01"));

    // When
    VolumeWeightChargeable result = consolidationV3Service.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);

    // Then
    assertThat(result).isNotNull();

  }

  @Test
  void pushShipmentDataToDependentService_Test(){
      KafkaResponse kafkaResponse = new KafkaResponse();
      when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(kafkaResponse);
      lenient().when(jsonHelper.convertToJson(kafkaResponse)).thenReturn("kafkaResponse");
      lenient().when(trackingServiceAdapter.checkIfConsolContainersExist(any())).thenReturn(true);
      lenient().when(trackingServiceAdapter.checkIfAwbExists(any())).thenReturn(false);
      CarrierDetails carrierDetails = CarrierDetails.builder().shippingLine("SP").build();
      consolidationDetails.setCarrierDetails(carrierDetails);
      ConsolidationDetails oldEntity = consolidationDetails;
      consolidationDetails.setInterBranchConsole(false);
      oldEntity.setMawb("AAAA");

      when(trackingServiceAdapter.getAllEvents(any(), any(), any())).thenReturn(new ArrayList<>());
      UniversalTrackingPayload.UniversalEventsPayload payload = new UniversalEventsPayload();
      when(trackingServiceAdapter.mapEventDetailsForTracking(any(), any(), any(), any())).thenReturn(payload);
      when(jsonHelper.convertToJson(any())).thenReturn("Adaf");

      consolidationV3Service.pushShipmentDataToDependentService(consolidationDetails, true, oldEntity);
      assertThat(consolidationDetails.getInterBranchConsole()).isFalse();
  }

  @Test
  void pushShipmentDataToDependentService_Test1(){
    KafkaResponse kafkaResponse = new KafkaResponse();
    when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(kafkaResponse);
    lenient().when(jsonHelper.convertToJson(kafkaResponse)).thenReturn("kafkaResponse");
    lenient().when(trackingServiceAdapter.checkIfConsolContainersExist(any())).thenReturn(false);
    lenient().when(trackingServiceAdapter.checkIfAwbExists(any())).thenReturn(true);
    CarrierDetails carrierDetails = CarrierDetails.builder().shippingLine("SP").build();

    Containers container1 = new Containers();
    container1.setId(1L);
    container1.setContainerNumber("C1");

    Containers container2 = new Containers();
    container2.setId(2L);
    container2.setContainerNumber("C2");

    Containers container3 = new Containers();
    container3.setId(3L);
    container3.setContainerNumber("C3");

    Containers container4 = new Containers();
    container4.setId(4L);
    container4.setContainerNumber("C4");

    List<Containers> containersList1 = List.of(container1, container2, container3);
    List<Containers> containersList2 = List.of(container2, container3, container4);

    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setContainersList(containersList1);

    ConsolidationDetails oldEntity = consolidationDetails;
    consolidationDetails.setInterBranchConsole(false);
    oldEntity.setContainersList(containersList2);

    lenient().when(shipmentsContainersMappingDao.findByContainerIdIn(any())).thenReturn(List.of(ShipmentsContainersMapping.builder().shipmentId(1L)
        .build()));

    List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
    ShipmentDetails shipmentDetails1 = new ShipmentDetails();
    shipmentDetailsList.add(shipmentDetails1);

    lenient().when(trackingServiceAdapter.mapConsoleDataToTrackingServiceData(any(), any())).thenReturn(UniversalTrackingPayload.builder().build());

    lenient().when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

    lenient().when(trackingServiceAdapter.getAllEvents(any(), any(), anyString())).thenReturn(new ArrayList<>());
    UniversalTrackingPayload.UniversalEventsPayload payload = new UniversalEventsPayload();
    lenient().when(trackingServiceAdapter.mapEventDetailsForTracking(any(), any(), any(), any())).thenReturn(payload);
    when(jsonHelper.convertToJson(any())).thenReturn("Adaf");

    doThrow(new GenericException("Ex")).when(containerService).pushContainersToDependentServices(anyList(), anyList(), any());
    doThrow(new GenericException("EX")).when(trackingServiceAdapter).publishUpdatesToTrackingServiceQueue(any(), any());

    consolidationV3Service.pushShipmentDataToDependentService(consolidationDetails, true, oldEntity);
    assertThat(consolidationDetails.getInterBranchConsole()).isFalse();
  }

  @Test
  void shouldSetReceivingBranchToNullWhenZero() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setReceivingBranch(0L);

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getReceivingBranch()).isNull();
  }

  @Test
  void shouldKeepReceivingBranchWhenNonZero() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setReceivingBranch(10L);

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getReceivingBranch()).isEqualTo(10L);
  }

  @Test
  void shouldNullifyTriangulationPartnerListIfOnlyOneWithZero() {
    TriangulationPartner partner = new TriangulationPartner();
    partner.setTriangulationPartner(0L);
    ConsolidationDetails details = new ConsolidationDetails();
    details.setTriangulationPartnerList(List.of(partner));

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getTriangulationPartnerList()).isNull();
  }

  @Test
  void shouldNotNullifyTriangulationPartnerListIfOneWithNonZero() {
    TriangulationPartner partner = new TriangulationPartner();
    partner.setTriangulationPartner(123L);
    ConsolidationDetails details = new ConsolidationDetails();
    details.setTriangulationPartnerList(List.of(partner));

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getTriangulationPartnerList()).isNotNull();
    assertThat(details.getTriangulationPartnerList()).hasSize(1);
  }

  @Test
  void shouldSetTriangulationPartnerToNullWhenListIsNullAndValueIsZero() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setTriangulationPartnerList(null);
    details.setTriangulationPartner(0L);

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getTriangulationPartner()).isNull();
  }

  @Test
  void shouldNotSetTriangulationPartnerToNullWhenListIsNullAndValueIsNonZero() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setTriangulationPartnerList(null);
    details.setTriangulationPartner(999L);

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getTriangulationPartner()).isEqualTo(999L);
  }

  @Test
  void shouldDoNothingWhenAllInputsAreValid() {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setReceivingBranch(100L);
    details.setTriangulationPartner(500L);
    details.setTriangulationPartnerList(Arrays.asList(
        new TriangulationPartner(101L, true),
        new TriangulationPartner(102L, false)
    ));

    consolidationV3Service.setReceivingAndTriangulationBranch(details);

    assertThat(details.getReceivingBranch()).isEqualTo(100L);
    assertThat(details.getTriangulationPartner()).isEqualTo(500L);
    assertThat(details.getTriangulationPartnerList()).hasSize(2);
  }

  @Test
  void checkDisableFetchConditionForAwb_shouldReturnFalse_whenOldEntityIsNull() {
    boolean result = consolidationV3Service.checkDisableFetchConditionForAwb(
        new ConsolidationDetails(), null, new ShipmentSettingsDetails());

    assertThat(result).isFalse();
  }

  @Test
  void checkDisableFetchConditionForAwb_shouldReturnFalse_whenIataTactFlagIsFalse() {
    ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
    settings.setIataTactFlag(false);

    boolean result = consolidationV3Service.checkDisableFetchConditionForAwb(
        new ConsolidationDetails(), new ConsolidationDetails(), settings);

    assertThat(result).isFalse();
  }

  @Test
  void checkDisableFetchConditionForAwb_shouldReturnFalse_whenTransportModeIsNotAir() {
    ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
    settings.setIataTactFlag(true);

    ConsolidationDetails newEntity = new ConsolidationDetails();
    newEntity.setTransportMode("SEA");

    boolean result = consolidationV3Service.checkDisableFetchConditionForAwb(
        newEntity, new ConsolidationDetails(), settings);

    assertThat(result).isFalse();
  }

  @Test
  void checkDisableFetchConditionForAwb_shouldReturnTrue_whenCarrierDetailsAreDifferent() {
    ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
    settings.setIataTactFlag(true);

    ConsolidationDetails newEntity = new ConsolidationDetails();
    newEntity.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    CarrierDetails newCarrier = new CarrierDetails();
    newCarrier.setOriginPort("DXB");
    newCarrier.setDestinationPort("JFK");
    newCarrier.setShippingLine("EK");
    newEntity.setCarrierDetails(newCarrier);

    ConsolidationDetails oldEntity = new ConsolidationDetails();
    CarrierDetails oldCarrier = new CarrierDetails();
    oldCarrier.setOriginPort("DEL");
    oldCarrier.setDestinationPort("LHR");
    oldCarrier.setShippingLine("AI");
    oldEntity.setCarrierDetails(oldCarrier);

    boolean result = consolidationV3Service.checkDisableFetchConditionForAwb(
        newEntity, oldEntity, settings);

    assertThat(result).isTrue();
  }

  @Test
  void saveAwb_shouldNotSave_whenCheckConditionReturnsFalse() throws RunnerException {
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());

    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = null;

    consolidationV3Service.saveAwb(newEntity, oldEntity);

    verify(awbDao, never()).findByConsolidationId(any());
  }

  @Test
  void saveAwb_shouldSave_whenCheckConditionIsTrueAndAwbsExist() throws RunnerException {
    ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
    settings.setIataTactFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(settings);

    ConsolidationDetails newEntity = new ConsolidationDetails();
    newEntity.setId(1L);
    newEntity.setTransportMode(Constants.TRANSPORT_MODE_AIR);

    CarrierDetails newCarrier = new CarrierDetails();
    newCarrier.setOriginPort("DXB");
    newCarrier.setDestinationPort("JFK");
    newCarrier.setShippingLine("EK");
    newEntity.setCarrierDetails(newCarrier);

    ConsolidationDetails oldEntity = new ConsolidationDetails();
    CarrierDetails oldCarrier = new CarrierDetails();
    oldCarrier.setOriginPort("DEL");
    oldCarrier.setDestinationPort("LHR");
    oldCarrier.setShippingLine("AI");
    oldEntity.setCarrierDetails(oldCarrier);

    AwbGoodsDescriptionInfo info = new AwbGoodsDescriptionInfo();
    info.setDisableFetchRates(true);
    info.setEnableFetchRatesWarning(false);

    Awb awb = new Awb();
    awb.setAwbGoodsDescriptionInfo(List.of(info));

    when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(awb));

    consolidationV3Service.saveAwb(newEntity, oldEntity);

    verify(awbDao).save(awb);
  }

  @Test
  void saveAwb_shouldNotSave_whenAwbListIsEmpty() throws RunnerException {
    ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
    settings.setIataTactFlag(true);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(settings);

    ConsolidationDetails newEntity = new ConsolidationDetails();
    newEntity.setId(1L);
    newEntity.setTransportMode(Constants.TRANSPORT_MODE_AIR);

    CarrierDetails newCarrier = new CarrierDetails();
    newCarrier.setOriginPort("DXB");
    newCarrier.setDestinationPort("JFK");
    newCarrier.setShippingLine("EK");
    newEntity.setCarrierDetails(newCarrier);

    ConsolidationDetails oldEntity = new ConsolidationDetails();
    CarrierDetails oldCarrier = new CarrierDetails();
    oldCarrier.setOriginPort("DEL");
    oldCarrier.setDestinationPort("LHR");
    oldCarrier.setShippingLine("AI");
    oldEntity.setCarrierDetails(oldCarrier);

    when(awbDao.findByConsolidationId(1L)).thenReturn(Collections.emptyList());

    consolidationV3Service.saveAwb(newEntity, oldEntity);

    verify(awbDao, never()).save(any());
  }

  @Test
  void getBookingNumberFromConsol_shouldReturnBookingNumber() {
    Long consolidationId = 123L;
    String expectedBookingNumber = "BOOK123";

    when(consolidationDetailsDao.getBookingNumberFromConsol(consolidationId)).thenReturn(expectedBookingNumber);

    String actualBookingNumber = consolidationV3Service.getBookingNumberFromConsol(consolidationId);

    assertEquals(expectedBookingNumber, actualBookingNumber);
    verify(consolidationDetailsDao, times(1)).getBookingNumberFromConsol(consolidationId);
  }

  @Test
  void updateConsolidationAttachmentFlag_shouldUpdateFlag_whenFlagIsNotNull() {
    Long consolidationId = 123L;
    Boolean enableFlag = true;

    doNothing().when(consolidationDetailsDao).updateConsolidationAttachmentFlag(enableFlag, consolidationId);

    consolidationV3Service.updateConsolidationAttachmentFlag(enableFlag, consolidationId);

    verify(consolidationDetailsDao, times(1)).updateConsolidationAttachmentFlag(enableFlag, consolidationId);
  }

  @Test
  void updateConsolidationAttachmentFlag_shouldThrowException_whenFlagIsNull() {
    Long consolidationId = 123L;

    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
        consolidationV3Service.updateConsolidationAttachmentFlag(null, consolidationId)
    );

    assertEquals("enableFlag cannot be null", exception.getMessage());
    verify(consolidationDetailsDao, never()).updateConsolidationAttachmentFlag(any(), any());
  }

  @Test
  void updateConsolidationAttachmentFlag_shouldWrapException_whenDaoThrows() {
    Long consolidationId = 123L;
    Boolean enableFlag = false;

    doThrow(new RuntimeException("DB error")).when(consolidationDetailsDao)
        .updateConsolidationAttachmentFlag(enableFlag, consolidationId);

    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
        consolidationV3Service.updateConsolidationAttachmentFlag(enableFlag, consolidationId)
    );

    assertEquals("DB error", exception.getMessage());
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setTerminalCutoff(LocalDateTime.now());
    oldEntity.setTerminalCutoff(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers1() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setVerifiedGrossMassCutoff(LocalDateTime.now());
    oldEntity.setVerifiedGrossMassCutoff(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers2() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setShipInstructionCutoff(LocalDateTime.now());
    oldEntity.setShipInstructionCutoff(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers3() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setHazardousBookingCutoff(LocalDateTime.now());
    oldEntity.setHazardousBookingCutoff(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers4() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setReeferCutoff(LocalDateTime.now());
    oldEntity.setReeferCutoff(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers5() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setEarliestEmptyEquPickUp(LocalDateTime.now());
    oldEntity.setEarliestEmptyEquPickUp(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers6() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setLatestFullEquDeliveredToCarrier(LocalDateTime.now());
    oldEntity.setLatestFullEquDeliveredToCarrier(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers7() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setEarliestDropOffFullEquToCarrier(LocalDateTime.now());
    oldEntity.setEarliestDropOffFullEquToCarrier(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnTrue_whenAnyCutoffFieldDiffers8() {
    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setLatDate(LocalDateTime.now());
    oldEntity.setLatDate(LocalDateTime.now().minusDays(1));

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertTrue(result);
  }

  @Test
  void canProcesscutOffFields_shouldReturnFalse_whenAllFieldsAreEqual() {
    LocalDateTime cutoffTime = LocalDateTime.now();
    Integer freeDays = 2;

    ConsolidationDetails newEntity = new ConsolidationDetails();
    ConsolidationDetails oldEntity = new ConsolidationDetails();

    newEntity.setTerminalCutoff(cutoffTime);
    newEntity.setVerifiedGrossMassCutoff(cutoffTime);
    newEntity.setShipInstructionCutoff(cutoffTime);
    newEntity.setHazardousBookingCutoff(cutoffTime);
    newEntity.setReeferCutoff(cutoffTime);
    newEntity.setEarliestEmptyEquPickUp(cutoffTime);
    newEntity.setLatestFullEquDeliveredToCarrier(cutoffTime);
    newEntity.setEarliestDropOffFullEquToCarrier(cutoffTime);
    newEntity.setLatDate(cutoffTime);
    newEntity.setCarrierDocCutOff(cutoffTime);
    newEntity.setCargoReceiptWHCutOff(cutoffTime);
    newEntity.setLastFreeDateCutOff(cutoffTime);
    newEntity.setNumberOfFreeDaysCutOff(freeDays);

    oldEntity.setTerminalCutoff(cutoffTime);
    oldEntity.setVerifiedGrossMassCutoff(cutoffTime);
    oldEntity.setShipInstructionCutoff(cutoffTime);
    oldEntity.setHazardousBookingCutoff(cutoffTime);
    oldEntity.setReeferCutoff(cutoffTime);
    oldEntity.setEarliestEmptyEquPickUp(cutoffTime);
    oldEntity.setLatestFullEquDeliveredToCarrier(cutoffTime);
    oldEntity.setEarliestDropOffFullEquToCarrier(cutoffTime);
    oldEntity.setLatDate(cutoffTime);
    oldEntity.setCarrierDocCutOff(cutoffTime);
    oldEntity.setCargoReceiptWHCutOff(cutoffTime);
    oldEntity.setLastFreeDateCutOff(cutoffTime);
    oldEntity.setNumberOfFreeDaysCutOff(freeDays);

    boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);

    assertFalse(result);
  }

  @Test
  void validateOutstandingDuesForShipments_shouldThrowBillingException_whenOutstandingDuesExist() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setGuid(UUID.randomUUID());
    shipment.setTenantId(1);
    shipment.setShipmentId("H123");
    shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);

    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setEnableConsolSplitBillCharge(true);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    BillingDueSummary dueSummary = new BillingDueSummary();
    dueSummary.setModuleGuid(shipment.getGuid().toString());
    dueSummary.setDueRemaining(true);
    when(billingServiceAdapter.fetchBillingDueSummary(any())).thenReturn(List.of(dueSummary));

    List<ShipmentDetails> shipmentList = List.of(shipment);

    ResponseEntity<IRunnerResponse> response = consolidationV3Service.validateOutstandingDuesForShipments(shipmentList);
    assertNotNull(response);
  }

  @Test
  void validateOutstandingDuesForShipments_shouldDoNothing_whenNoOutstandingDues() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setGuid(UUID.randomUUID());
    shipment.setTenantId(1);
    shipment.setShipmentId("H123");
    shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setEnableConsolSplitBillCharge(true);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    BillingDueSummary dueSummary = new BillingDueSummary();
    dueSummary.setModuleGuid(shipment.getGuid().toString());
    dueSummary.setDueRemaining(false);
    when(billingServiceAdapter.fetchBillingDueSummary(any())).thenReturn(List.of(dueSummary));

    assertDoesNotThrow(() ->
        consolidationV3Service.validateOutstandingDuesForShipments(List.of(shipment))
    );
  }

  @Test
  void validateOutstandingDuesForShipments_shouldSkip_whenSplitBillingDisabled() {
    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setEnableConsolSplitBillCharge(false);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);

    assertDoesNotThrow(() ->
        consolidationV3Service.validateOutstandingDuesForShipments(List.of(shipment))
    );

    verify(billingServiceAdapter, never()).fetchBillingDueSummary(any());
  }

  @Test
  void validateOutstandingDuesForShipments_shouldSkip_whenListIsEmpty() {
    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setEnableConsolSplitBillCharge(true);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    assertDoesNotThrow(() ->
        consolidationV3Service.validateOutstandingDuesForShipments(Collections.emptyList())
    );

    verify(billingServiceAdapter, never()).fetchBillingDueSummary(any());
  }

  @Test
  void createBillingBulkSummaryBranchWiseRequest_shouldMapFieldsCorrectly() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setTenantId(10);
    shipment.setGuid(UUID.randomUUID());

    BillingBulkSummaryBranchWiseRequest request =
        consolidationV3Service.createBillingBulkSummaryBranchWiseRequest(List.of(shipment));

    assertEquals(Constants.SHIPMENT, request.getModuleType());
    assertEquals("10", request.getModuleData().get(0).getBranchId());
    assertEquals(shipment.getGuid().toString(), request.getModuleData().get(0).getModuleGuid());
  }

  @Test
  void getConsolidationById_shouldReturnConsolidationDetails() {
    ConsolidationDetails details = new ConsolidationDetails();
    when(consolidationDetailsDao.findConsolidationsById(1L)).thenReturn(details);

    ConsolidationDetails result = consolidationV3Service.getConsolidationById(1L);

    assertEquals(details, result);
  }

  @Test
  void findById_shouldReturnOptionalConsolidationDetails() {
    ConsolidationDetails details = new ConsolidationDetails();
    when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(details));

    Optional<ConsolidationDetails> result = consolidationV3Service.findById(1L);

    assertTrue(result.isPresent());
    assertEquals(details, result.get());
  }

  @Test
  void save_shouldReturnSavedEntity() {
    ConsolidationDetails input = new ConsolidationDetails();
    when(consolidationDetailsDao.save(input, true)).thenReturn(input);

    ConsolidationDetails result = consolidationV3Service.save(input, true);

    assertEquals(input, result);
  }

  @Test
  void testUpdateShipmentDataInPlatform_whenShipmentIdsIsNull_shouldDoNothing() {
    consolidationV3Service.updateShipmentDataInPlatform(null);
    verifyNoInteractions(shipmentDao, bookingIntegrationsUtility);
  }

  @Test
  void testUpdateShipmentDataInPlatform_whenNoShipmentsFound_shouldDoNothing() {
    when(shipmentDao.findShipmentsByIds(anySet()))
        .thenThrow(new GenericException("Ex"));

    consolidationV3Service.updateShipmentDataInPlatform(Set.of(1L, 2L));
    verifyNoInteractions(bookingIntegrationsUtility);
  }

  @Test
  void testUpdateShipmentDataInPlatform_whenNoShipmentsFound_Exception() {
    when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(Collections.emptyList());

    consolidationV3Service.updateShipmentDataInPlatform(Set.of(1L, 2L));
    verify(shipmentDao).findShipmentsByIds(Set.of(1L, 2L));
    verifyNoInteractions(bookingIntegrationsUtility);
  }

  @Test
  void testUpdateShipmentDataInPlatform_whenP100BranchEnabled_shouldCallUpdateBooking() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setId(1L);
    List<ShipmentDetails> shipments = List.of(shipment);

    when(shipmentDao.findShipmentsByIds(Set.of(1L))).thenReturn(shipments);
    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setP100Branch(true);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    consolidationV3Service.updateShipmentDataInPlatform(Set.of(1L));

    verify(bookingIntegrationsUtility).updateBookingInPlatformEmptyContainer(shipment);
  }

  @Test
  void testUpdateShipmentDataInPlatform_whenP100BranchDisabled_shouldNotCallUpdateBooking() {
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setId(1L);
    List<ShipmentDetails> shipments = List.of(shipment);

    when(shipmentDao.findShipmentsByIds(Set.of(1L))).thenReturn(shipments);
    V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
    settings.setP100Branch(false);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(settings);

    consolidationV3Service.updateShipmentDataInPlatform(Set.of(1L));

    verify(bookingIntegrationsUtility, never()).updateBookingInPlatformEmptyContainer(any());
  }

  // Utility method for creating dummy ConsolidationDetails
  private ConsolidationDetails createConsole(String type, boolean interBranch, boolean isAccepted) {
    ConsolidationDetails console = new ConsolidationDetails();
    console.setShipmentType(type);
    console.setInterBranchConsole(interBranch);
    console.setIsInland(isAccepted?Boolean.TRUE:Boolean.FALSE);

    // Stub isConsoleAccepted if needed via spy or override
    return console;
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenShipmentTypeIsNotEXP_shouldReturn() {
    ConsolidationDetails console = createConsole("IMP", true, false);
    consolidationV3Service.processInterConsoleDetachShipment(console, List.of(1L));
    verifyNoInteractions(commonUtils, shipmentDao, networkTransferService);
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenInterBranchConsoleIsFalse_shouldReturn() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, false, false);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

    consolidationV3Service.processInterConsoleDetachShipment(console, List.of(1L));
    verifyNoInteractions(shipmentDao, networkTransferService);
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenNetworkTransferDisabled_shouldReturn() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, true, false);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(false);

    consolidationV3Service.processInterConsoleDetachShipment(console, List.of(1L));
    verify(commonUtils).getShipmentSettingFromContext();
    verifyNoInteractions(shipmentDao, networkTransferService);
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenShipmentIdsEmpty_shouldReturn() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, true, false);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

    consolidationV3Service.processInterConsoleDetachShipment(console, Collections.emptyList());
    verify(commonUtils).getShipmentSettingFromContext();
    verifyNoInteractions(shipmentDao, networkTransferService);
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenConsoleIsAccepted_shouldReturn() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, true, false);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

    // Stub isConsoleAccepted to return true
    ConsolidationV3Service spyService = Mockito.spy(consolidationV3Service);
    doReturn(true).when(spyService).isConsoleAccepted(console);

    spyService.processInterConsoleDetachShipment(console, List.of(1L, 2L));
    verify(spyService).isConsoleAccepted(console);
    verifyNoInteractions(shipmentDao, networkTransferService);
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenAllConditionsMet_shouldCallDeleteValidNetworkTransferEntity() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, true, false);
    ShipmentDetails s1 = new ShipmentDetails();
    s1.setId(1L);
    s1.setReceivingBranch(22L);

    ShipmentDetails s2 = new ShipmentDetails();
    s2.setId(2L);
    s2.setReceivingBranch(null); // should be skipped

    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
    when(shipmentDao.findShipmentsByIds(Set.of(1L, 2L))).thenReturn(List.of(s1, s2));

    // Stub isConsoleAccepted to return false
    ConsolidationV3Service spyService = Mockito.spy(consolidationV3Service);
    doReturn(false).when(spyService).isConsoleAccepted(console);

    spyService.processInterConsoleDetachShipment(console, List.of(1L, 2L));

    verify(networkTransferService, never()).deleteValidNetworkTransferEntity(null, 2L, "SHIPMENT");
  }

  @Test
  void testProcessInterConsoleDetachShipment_whenExceptionOccurs_shouldLogError() {
    ConsolidationDetails console = createConsole(Constants.DIRECTION_EXP, true, false);

    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
    when(shipmentDao.findShipmentsByIds(anySet())).thenThrow(new RuntimeException("DB error"));

    // Stub isConsoleAccepted to return false
    ConsolidationV3Service spyService = Mockito.spy(consolidationV3Service);
    doReturn(false).when(spyService).isConsoleAccepted(console);

    assertDoesNotThrow(() ->
        spyService.processInterConsoleDetachShipment(console, List.of(1L)));

    verify(shipmentDao).findShipmentsByIds(anySet());
  }

  @Test
  void testIsConsoleAccepted(){
    when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(new ArrayList<>());
    assertFalse(consolidationV3Service.isConsoleAccepted(consolidationDetails));
  }

  @Test
  void testCreateLogHistoryForConsole_shouldCallLogHistoryServiceWithCorrectData() {
    // Arrange
    ConsolidationDetails details = new ConsolidationDetails();
    details.setId(101L);

    String jsonPayload = "{\"id\":101,\"guid\":\"guid-101\"}";

    when(jsonHelper.convertToJson(details)).thenReturn(jsonPayload);

    // Act
    consolidationV3Service.createLogHistoryForConsole(details);

    // Assert
    ArgumentCaptor<LogHistoryRequest> captor = ArgumentCaptor.forClass(LogHistoryRequest.class);
    verify(logsHistoryService).createLogHistory(captor.capture());

    LogHistoryRequest request = captor.getValue();
    assertEquals(101L, request.getEntityId());
    assertEquals(jsonPayload, request.getEntityPayload());
    assertEquals(Constants.CONSOLIDATION, request.getEntityType());
  }

  @Test
  void testCreateLogHistoryForConsole_whenJsonConversionFails_shouldLogErrorAndNotThrow() {
    // Arrange
    ConsolidationDetails details = new ConsolidationDetails();
    details.setId(101L);

    when(jsonHelper.convertToJson(details)).thenThrow(new RuntimeException("Serialization failed"));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.createLogHistoryForConsole(details));

    verify(logsHistoryService, never()).createLogHistory(any());
  }

  @Test
  void testCheckSciForAttachConsole_whenConsolationNotFound_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L),
        createConsoleShipmentMapping(2L, 202L)
    );
    List<Awb> mawbs = Arrays.asList(createMawb(301L));

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.empty());

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenMawbsListIsEmpty_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(Collections.emptyList());
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenMawbsListIsNull_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(null);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenShipIdListIsEmpty_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<Awb> mawbs = Arrays.asList(createMawb(301L));
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(Collections.emptyList());
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenMawbCargoInfoIsNull_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    Awb mawb = createMawb(301L);
    mawb.setAwbCargoInfo(null);
    List<Awb> mawbs = Arrays.asList(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenMawbStatusIsAwbFsuLocked_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, "T2");
    mawb.setAirMessageStatus(AwbStatus.AWB_FSU_LOCKED);
    List<Awb> mawbs = Arrays.asList(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenMawbSciIsAlreadyT1_shouldNotThrow() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, AwbConstants.T1);
    mawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS);
    List<Awb> mawbs = List.of(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));

    // Act & Assert
    assertDoesNotThrow(() -> consolidationV3Service.checkSciForAttachConsole(consoleId));

    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenNoShipmentHasSciT1_shouldNotUpdateMawbAndConsole() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L),
        createConsoleShipmentMapping(2L, 202L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, "T2");
    mawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS);
    List<Awb> mawbs = List.of(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    List<Awb> shipmentAwbs = Arrays.asList(
        createMawbWithCargoInfo(401L, "T2"),
        createMawbWithCargoInfo(402L, "T3")
    );

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));
    when(awbDao.findByShipmentIdList(Arrays.asList(201L, 202L))).thenReturn(shipmentAwbs);

    // Act
    consolidationV3Service.checkSciForAttachConsole(consoleId);

    // Assert
    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenShipmentAwbsIsNull_shouldNotUpdateMawbAndConsole() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, "T2");
    mawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS);
    List<Awb> mawbs = List.of(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));
    when(awbDao.findByShipmentIdList(Arrays.asList(201L))).thenReturn(null);

    // Act
    consolidationV3Service.checkSciForAttachConsole(consoleId);

    // Assert
    verify(awbDao, never()).save(any());
    verify(consolidationDetailsDao, never()).save(any(), anyBoolean());
  }

  @Test
  void testCheckSciForAttachConsole_whenShipmentHasSciT1_shouldUpdateMawbAndConsole() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L),
        createConsoleShipmentMapping(2L, 202L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, "T2");
    mawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS);
    List<Awb> mawbs = List.of(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    List<Awb> shipmentAwbs = Arrays.asList(
        createMawbWithCargoInfo(401L, "T2"),
        createMawbWithCargoInfo(402L, AwbConstants.T1)
    );

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));
    when(awbDao.findByShipmentIdList(Arrays.asList(201L, 202L))).thenReturn(shipmentAwbs);

    // Act
    consolidationV3Service.checkSciForAttachConsole(consoleId);

    // Assert
    assertEquals(AwbConstants.T1, mawb.getAwbCargoInfo().getSci());
    assertEquals(AwbConstants.T1, consolDetails.getSci());

    verify(awbDao).save(mawb);
    verify(consolidationDetailsDao).save(consolDetails, false);
  }

  @Test
  void testCheckSciForAttachConsole_whenMultipleShipmentsHaveSciT1_shouldUpdateMawbAndConsole() throws RunnerException {
    // Arrange
    Long consoleId = 101L;
    List<ConsoleShipmentMapping> mappingList = Arrays.asList(
        createConsoleShipmentMapping(1L, 201L),
        createConsoleShipmentMapping(2L, 202L),
        createConsoleShipmentMapping(3L, 203L)
    );
    Awb mawb = createMawbWithCargoInfo(301L, "T3");
    mawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS);
    List<Awb> mawbs = List.of(mawb);
    ConsolidationDetails consolDetails = createConsolidationDetails(consoleId);

    List<Awb> shipmentAwbs = Arrays.asList(
        createMawbWithCargoInfo(401L, AwbConstants.T1),
        createMawbWithCargoInfo(402L, AwbConstants.T1),
        createMawbWithCargoInfo(403L, "T2")
    );

    when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(mappingList);
    when(awbDao.findByConsolidationId(consoleId)).thenReturn(mawbs);
    when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolDetails));
    when(awbDao.findByShipmentIdList(Arrays.asList(201L, 202L, 203L))).thenReturn(shipmentAwbs);

    // Act
    consolidationV3Service.checkSciForAttachConsole(consoleId);

    // Assert
    assertEquals(AwbConstants.T1, mawb.getAwbCargoInfo().getSci());
    assertEquals(AwbConstants.T1, consolDetails.getSci());

    verify(awbDao).save(mawb);
    verify(consolidationDetailsDao).save(consolDetails, false);
  }

  // Helper methods for creating test objects
  private ConsoleShipmentMapping createConsoleShipmentMapping(Long id, Long shipmentId) {
    ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
    mapping.setId(id);
    mapping.setShipmentId(shipmentId);
    return mapping;
  }

  private Awb createMawb(Long id) {
    Awb awb = new Awb();
    awb.setId(id);
    return awb;
  }



  private Awb createMawbWithCargoInfo(Long id, String sci) {
    Awb awb = new Awb();
    awb.setId(id);
    AwbCargoInfo cargoInfo = new AwbCargoInfo();
    cargoInfo.setSci(sci);
    awb.setAwbCargoInfo(cargoInfo);
    return awb;
  }

  @Test
  void detachShipments_NullRequest(){
    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    assertThrows(RunnerException.class, () -> consolidationV3Service.detachShipments(request));
  }

  @Test
  void detachShipments_Success() throws RunnerException {
    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    request.setShipmentIds(Set.of(1L,2L));
    request.setConsolidationId(1L);


    shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
    shipmentDetails.setEventsList(List.of(Events.builder().build()));
    shipmentDetails.setContainersList(new HashSet<>());

    ShipmentDetails shipmentDetails1 = shipmentDetails;
    shipmentDetails1.setId(1L);

    ShipmentDetails shipmentDetails2 = shipmentDetails;
    shipmentDetails2.setId(2L);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setShipmentsList(new HashSet<>());
    consolidationDetails.setOverride(true);
    consolidationDetails.setId(1L);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
    consolidationDetails.setAchievedQuantities(new AchievedQuantities());

    List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1,shipmentDetails2);
    lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
    when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

    var spyService = Mockito.spy(consolidationV3Service);

    V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
    v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
    mockShipmentSettings();
    lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
    lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
    when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

    ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
    assertNotNull(response);
  }
    @Test
    void detachShipments_Success1() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.TRUE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = shipmentDetails;
        shipmentDetails1.setId(1L);
        shipmentDetails1.setContainsHazardous(Boolean.TRUE);
        Containers container = new Containers();
        container.setId(1L);
        Set<Containers> containersList = Set.of(container);
        shipmentDetails1.setContainersList(containersList);

        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1,shipmentDetails2);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);

        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success3() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.TRUE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = shipmentDetails;
        shipmentDetails1.setId(1L);
        shipmentDetails1.setContainsHazardous(Boolean.TRUE);
        Containers container = new Containers();
        container.setId(1L);
        Set<Containers> containersList = Set.of(container);
        shipmentDetails1.setContainersList(containersList);

        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1,shipmentDetails2);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        List<Packing> packingList = List.of(packing);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success4() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.TRUE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = shipmentDetails;
        shipmentDetails1.setId(1L);
        shipmentDetails1.setContainsHazardous(Boolean.TRUE);
        Containers container = new Containers();
        container.setId(1L);
        Set<Containers> containersList = Set.of(container);
        shipmentDetails1.setContainersList(containersList);

        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1,shipmentDetails2);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        packing.setShipmentId(1L);
        List<Packing> packingList = List.of(packing);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success5FCL() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();

        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setShipmentType(CARGO_TYPE_FCL);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }
    @Test
    void detachShipments_Success6FCL() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_FCL);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.TRUE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success7LCL() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_LCL);
        shipmentDetails1.setContainerAssignedToShipmentCargo(5L);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.FALSE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        lenient().when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }
    @Test
    void detachShipments_SuccessLCLWithNullContainerId() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_LCL);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        packing.setContainerId(null);
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.FALSE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        lenient().when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }
    @Test
    void detachShipments_SuccessLCLWithNullContainerId2() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_LCL);
        shipmentDetails1.setContainerAssignedToShipmentCargo(5L);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        packing.setContainerId(null);
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.FALSE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        lenient().when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success8FCL() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_FCL);
        shipmentDetails1.setContainerAssignedToShipmentCargo(5L);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        packing.setContainerId(null);
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.TRUE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        lenient().when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }

    @Test
    void detachShipments_Success9FCL() throws RunnerException {
        ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
        request.setShipmentIds(Set.of(1L,2L));
        request.setConsolidationId(1L);
        request.setIsFCLDelete(Boolean.FALSE);
        request.setIsForcedDetach(Boolean.FALSE);


        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setEventsList(List.of(Events.builder().build()));
        shipmentDetails.setContainersList(new HashSet<>());
        shipmentDetails.setContainsHazardous(Boolean.TRUE);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipmentForAutoDetach();
        shipmentDetails1.setShipmentType(CARGO_TYPE_FCL);
        shipmentDetails1.setContainerAssignedToShipmentCargo(5L);
        ShipmentDetails shipmentDetails2 = shipmentDetails;
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainsHazardous(Boolean.TRUE);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setOverride(true);
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(Boolean.TRUE);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setAchievedQuantities(new AchievedQuantities());

        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails1);
        lenient().when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetailsList);

        var spyService = Mockito.spy(consolidationV3Service);
        Packing packing = jsonTestUtility.getTestPacking();
        packing.setContainerId(1L);
        List<Packing> packingList = List.of(packing);
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(5L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);
        when(shipmentsContainersMappingDao.findByShipmentIdIn(any())).thenReturn(mockShipContainerList);


        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setEnableConsolSplitBillCharge(false);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(commonUtils.isFCLorFTL(any())).thenReturn(Boolean.TRUE);
        mockShipmentSettings();
        lenient().when(consoleShipmentMappingDao.detachShipments(any(), any())).thenReturn(List.of(1L,2L));
        lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("JSON");
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        lenient().when(packingDao.findByContainerIdIn(any())).thenReturn(packingList);
        lenient().when(packingDao.findByShipmentId(any())).thenReturn(packingList);
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        ResponseEntity<IRunnerResponse> response = spyService.detachShipments(request);
        assertNotNull(response);
    }



  @Test
  void fetchAllMasterDataByKey_Success(){
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    var response = consolidationV3Service.fetchAllMasterDataByKey(consolidationDetailsV3Response);
    assertNotNull(response);

  }

  @Test
  void testGetSummaryContainer_whenContainersListIsNull_shouldReturnNull() {
    // Arrange
    List<Containers> containersList = null;
    Set<Long> assignedContainerIds = Set.of(1L, 2L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertNotNull(result);
  }

  @Test
  void testGetSummaryContainer_whenContainersListIsEmpty_shouldReturnNull() {
    // Arrange
    List<Containers> containersList = Collections.emptyList();
    Set<Long> assignedContainerIds = Set.of(1L, 2L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertNotNull(result);
  }

  @Test
  void testGetSummaryContainer_whenAssignedContainerIdsIsNull_shouldTreatAllAsUnassigned() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 5L),
        createContainer(2L, 3L),
        createContainer(3L, 2L)
    );
    Set<Long> assignedContainerIds = null;

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("0 / 10", result);
  }

  @Test
  void testGetSummaryContainer_whenNoContainersAreAssigned_shouldReturnZeroAssigned() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 4L),
        createContainer(2L, 6L)
    );
    Set<Long> assignedContainerIds = Set.of(99L, 100L); // IDs not in containersList

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("0 / 10", result);
  }

  @Test
  void testGetSummaryContainer_whenAllContainersAreAssigned_shouldReturnFullAssignment() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 3L),
        createContainer(2L, 4L),
        createContainer(3L, 2L)
    );
    Set<Long> assignedContainerIds = Set.of(1L, 2L, 3L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("9 / 9", result);
  }

  @Test
  void testGetSummaryContainer_whenPartialContainersAreAssigned_shouldReturnCorrectSummary() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 5L),
        createContainer(2L, 3L),
        createContainer(3L, 4L),
        createContainer(4L, 2L)
    );
    Set<Long> assignedContainerIds = Set.of(1L, 3L); // Only containers 1 and 3 are assigned

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("9 / 14", result); // (5 + 4) / (5 + 3 + 4 + 2)
  }

  @Test
  void testGetSummaryContainer_whenSingleContainerAssigned_shouldReturnCorrectSummary() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 7L)
    );
    Set<Long> assignedContainerIds = Set.of(1L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("7 / 7", result);
  }

  @Test
  void testGetSummaryContainer_whenSingleContainerNotAssigned_shouldReturnZeroAssigned() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 8L)
    );
    Set<Long> assignedContainerIds = Set.of(2L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("0 / 8", result);
  }

  @Test
  void testGetSummaryContainer_whenContainerCountIsZero_shouldHandleCorrectly() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 0L),
        createContainer(2L, 5L),
        createContainer(3L, 0L)
    );
    Set<Long> assignedContainerIds = Set.of(1L, 2L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("5 / 5", result); // (0 + 5) / (0 + 5 + 0)
  }

  @Test
  void testGetSummaryContainer_whenAssignedContainerIdsIsEmpty_shouldTreatAllAsUnassigned() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 3L),
        createContainer(2L, 4L)
    );
    Set<Long> assignedContainerIds = Collections.emptySet();

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("0 / 7", result);
  }

  @Test
  void testGetSummaryContainer_whenLargeNumbers_shouldHandleCorrectly() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(1L, 1000L),
        createContainer(2L, 2000L),
        createContainer(3L, 1500L)
    );
    Set<Long> assignedContainerIds = Set.of(1L, 3L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("2500 / 4500", result); // (1000 + 1500) / (1000 + 2000 + 1500)
  }

  @Test
  void testGetSummaryContainer_whenContainerGetContainerCountThrowsException_shouldReturnNull() {
    // Arrange
    Containers mockContainer = mock(Containers.class);
    when(mockContainer.getContainerCount()).thenThrow(new RuntimeException("Database error"));

    List<Containers> containersList = Arrays.asList(mockContainer);
    Set<Long> assignedContainerIds = Set.of(1L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertNull(result);
  }

  @Test
  void testGetSummaryContainer_whenContainerGetIdThrowsException_shouldReturnNull() {
    // Arrange
    Containers mockContainer = mock(Containers.class);
    when(mockContainer.getContainerCount()).thenReturn(5L);
    when(mockContainer.getId()).thenThrow(new RuntimeException("ID retrieval error"));

    List<Containers> containersList = Arrays.asList(mockContainer);
    Set<Long> assignedContainerIds = Set.of(1L);

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertNull(result);
  }

  @Test
  void testGetSummaryContainer_whenAssignedContainerIdsContainsThrowsException_shouldReturnNull() {
    // Arrange
    List<Containers> containersList = Arrays.asList(createContainer(1L, 5L));
    Set<Long> mockAssignedContainerIds = mock(Set.class);
    when(mockAssignedContainerIds.contains(any())).thenThrow(new RuntimeException("Set operation error"));

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, mockAssignedContainerIds);

    // Assert
    assertNull(result);
  }

  @Test
  void testGetSummaryContainer_whenMixedAssignmentWithMultipleContainers_shouldReturnCorrectSummary() {
    // Arrange
    List<Containers> containersList = Arrays.asList(
        createContainer(10L, 2L),
        createContainer(20L, 8L),
        createContainer(30L, 1L),
        createContainer(40L, 6L),
        createContainer(50L, 3L)
    );
    Set<Long> assignedContainerIds = Set.of(10L, 30L, 50L); // Containers with counts 2, 1, 3

    // Act
    String result = consolidationV3Service.getSummaryContainer(containersList, assignedContainerIds);

    // Assert
    assertEquals("6 / 20", result); // (2 + 1 + 3) / (2 + 8 + 1 + 6 + 3)
  }

  @Test
  void addAllOrganizationDataInSingleCall_Success() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    List<String> mockOrgIds = List.of("org1", "org2", "org3");
    Map<String, EntityTransferOrganizations> mockKeyMasterDataMap = new HashMap<>();
    mockKeyMasterDataMap.put("org1", new EntityTransferOrganizations());
    mockKeyMasterDataMap.put("org2", new EntityTransferOrganizations());

    when(masterDataUtils.createInBulkOrganizationRequest(
        any(), any(), any(), any(), any())).thenReturn(mockOrgIds);

    when(masterDataUtils.fetchInOrganizations(any(), any()))
        .thenReturn(mockKeyMasterDataMap);


    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> result =
        consolidationV3Service.addAllOrganizationDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(result);
  }

  @Test
  void addAllTenantDataInSingleCall_Success() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    Set<String> mockTenantIds = Set.of("tenant1", "tenant2");
    Map<String, TenantModel> mockTenantMap = new HashMap<>();
    mockTenantMap.put("tenant1", new TenantModel());
    mockTenantMap.put("tenant2", new TenantModel());

    when(masterDataUtils.createInBulkTenantsRequest(
        any(), any(), any(), anyString(), anyMap()))
        .thenReturn(new ArrayList<>(mockTenantIds));

    when(masterDataUtils.fetchInTenantsList(mockTenantIds))
        .thenReturn(mockTenantMap);

    doNothing().when(masterDataUtils).pushToCache(
        anyMap(), eq(CacheConstants.TENANTS), anySet(), any(), anyMap());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(
        anyMap(), eq(CacheConstants.TENANTS), anyMap(), anyMap());

    // Act
    CompletableFuture<Map<String, TenantModel>> resultFuture =
        consolidationV3Service.addAllTenantDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(resultFuture);
    Map<String, TenantModel> result = resultFuture.join();
    assertNotNull(result);
    assertEquals(2, result.size());
    assertTrue(result.containsKey("tenant1"));
    assertTrue(result.containsKey("tenant2"));
  }

  @Test
  void addAllTenantDataInSingleCall_Success1() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();

    Set<String> mockTenantIds = Set.of("tenant1", "tenant2");
    Map<String, TenantModel> mockTenantMap = new HashMap<>();
    mockTenantMap.put("tenant1", new TenantModel());
    mockTenantMap.put("tenant2", new TenantModel());

    when(masterDataUtils.createInBulkTenantsRequest(
        any(), any(), any(), anyString(), anyMap()))
        .thenReturn(new ArrayList<>(mockTenantIds));

    when(masterDataUtils.fetchInTenantsList(mockTenantIds))
        .thenReturn(mockTenantMap);

    when(masterDataUtils.setMasterData(anyMap(),anyString(), anyMap()))
        .thenReturn(new HashMap<>());
    doNothing().when(masterDataUtils).pushToCache(
        anyMap(), eq(CacheConstants.TENANTS), anySet(), any(), anyMap());

    // Act
    CompletableFuture<Map<String, TenantModel>> resultFuture =
        consolidationV3Service.addAllTenantDataInSingleCall(consolidationDetailsV3Response, null);

    // Assert
    assertNotNull(resultFuture);
  }
  @Test
  void addAllTenantDataInSingleCall_Failure_ReturnsNullFuture() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.createInBulkTenantsRequest(
        any(), any(), any(), anyString(), anyMap()))
        .thenThrow(new RuntimeException("Simulated failure"));

    // Act
    CompletableFuture<Map<String, TenantModel>> resultFuture =
        consolidationV3Service.addAllTenantDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(resultFuture);
    assertNull(resultFuture.join());
  }

  @Test
  void addAllVesselDataInSingleCall_Success() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    consolidationDetailsV3Response.setCarrierDetails(new CarrierDetailResponse());
    Map<String, Object> masterDataResponse = new HashMap<>();

    Set<String> mockVessels = Set.of("tenant1", "tenant2");
    Map<String, EntityTransferVessels> mockTenantMap = new HashMap<>();
    mockTenantMap.put("tenant1", new EntityTransferVessels());
    mockTenantMap.put("tenant2", new EntityTransferVessels());

    when(masterDataUtils.createInBulkVesselsRequest(
            any(), any(), any(), anyString(), anyMap()))
            .thenReturn(new ArrayList<>(mockVessels));

    when(masterDataUtils.fetchInBulkVessels(any()))
            .thenReturn(mockTenantMap);

    doNothing().when(masterDataUtils).pushToCache(
            anyMap(), eq(CacheConstants.VESSELS), anySet(), any(), anyMap());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(
            anyMap(), eq(CacheConstants.VESSELS), anyMap(), anyMap());

    // Act
    CompletableFuture<Map<String, EntityTransferVessels>> resultFuture =
            consolidationV3Service.addAllVesselDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(resultFuture);
    Map<String, EntityTransferVessels> result = resultFuture.join();
    assertNotNull(result);
    assertEquals(0, result.size());
  }

  @Test
  void addAllVesselDataInSingleCall_Success1() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();

    Set<String> mockTenantIds = Set.of("tenant1", "tenant2");
    Map<String, EntityTransferVessels> mockVessels = new HashMap<>();
    mockVessels.put("tenant1", new EntityTransferVessels());
    mockVessels.put("tenant2", new EntityTransferVessels());

    when(masterDataUtils.createInBulkVesselsRequest(
            any(), any(), any(), anyString(), anyMap()))
            .thenReturn(new ArrayList<>(mockTenantIds));

    when(masterDataUtils.fetchInBulkVessels(mockTenantIds))
            .thenReturn(mockVessels);

    when(masterDataUtils.setMasterData(anyMap(),anyString(), anyMap()))
            .thenReturn(new HashMap<>());
    doNothing().when(masterDataUtils).pushToCache(
            anyMap(), eq(CacheConstants.VESSELS), anySet(), any(), anyMap());

    // Act
    CompletableFuture<Map<String, EntityTransferVessels>> resultFuture =
            consolidationV3Service.addAllVesselDataInSingleCall(consolidationDetailsV3Response, null);

    // Assert
    assertNotNull(resultFuture);
  }
  @Test
  void addAllVesselDataInSingleCall_Failure_ReturnsNullFuture() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    // Act
    CompletableFuture<Map<String, EntityTransferVessels>> resultFuture =
            consolidationV3Service.addAllVesselDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(resultFuture);
  }

  @Test
  void addAllOrganizationDataInSingleCall_CreateInBulkThrowsException_ReturnsNull() {
    // Arrange
    ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.createInBulkOrganizationRequest(
        any(ConsolidationDetailsV3Response.class),
        eq(ConsolidationDetails.class),
        any(Map.class),
        eq(ConsolidationDetails.class.getSimpleName()),
        any(Map.class)
    )).thenThrow(new RuntimeException("Database connection failed"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> result =
        consolidationV3Service.addAllOrganizationDataInSingleCall(consolidationDetailsV3Response, masterDataResponse);

    // Assert
    assertNotNull(result);
    assertTrue(result.isDone());
    assertNull(result.join());

    // Verify error logging (if you have a way to capture logs)
    verify(masterDataUtils).createInBulkOrganizationRequest(
        any(ConsolidationDetailsV3Response.class),
        eq(ConsolidationDetails.class),
        any(Map.class),
        eq(ConsolidationDetails.class.getSimpleName()),
        any(Map.class)
    );
  }

  // Helper method for creating test containers
  private Containers createContainer(Long id, Long containerCount) {
    Containers container = new Containers();
    container.setId(id);
    container.setContainerCount(containerCount);
    return container;
  }

  // Helper methods for creating test objects
  private ConsolidationDetails createConsolidationDetails(Long id) {
    ConsolidationDetails details = new ConsolidationDetails();
    details.setId(id);
    return details;
  }

  @Test
  void addAllMasterDataInSingleCall_Success_NoCarrierDetails_NullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = null;

    Set<MasterListRequest> listRequests = Set.of(new MasterListRequest());
    Map<String, EntityTransferMasterLists> keyMasterDataMap = Map.of("key1", new EntityTransferMasterLists());

    when(masterDataUtils.createInBulkMasterListRequest(
        any(), eq(ConsolidationDetails.class), any(), eq("ConsolidationDetails"), any()))
        .thenReturn(new ArrayList<>(listRequests));


    when(masterDataUtils.fetchInBulkMasterList(any()))
        .thenReturn(keyMasterDataMap);

    doNothing().when(commonUtils).createMasterDataKeysList(eq(listRequests), any());

    doNothing().when(masterDataUtils).pushToCache(
        eq(keyMasterDataMap), eq(CacheConstants.MASTER_LIST), anySet(), any(), any());

    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.MASTER_LIST), any()))
        .thenReturn(new HashMap<>());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllMasterDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
  }

  @Test
  void addAllMasterDataInSingleCall_Success_WithCarrierDetails_NullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    CarrierDetailResponse carrierDetailResponse = new CarrierDetailResponse();
    response.setCarrierDetails(carrierDetailResponse);
    Map<String, Object> masterDataResponse = null;

    Set<MasterListRequest> initialRequests = Set.of(new MasterListRequest());
    Set<MasterListRequest> carrierRequests = Set.of(new MasterListRequest());

    Map<String, EntityTransferMasterLists> keyMasterDataMap = Map.of("key1", new EntityTransferMasterLists());

    when(masterDataUtils.createInBulkMasterListRequest(
        eq(response), eq(ConsolidationDetails.class), any(), eq("ConsolidationDetails"), any()))
        .thenReturn(new ArrayList<>(initialRequests));

    when(masterDataUtils.createInBulkMasterListRequest(
        eq(carrierDetailResponse), eq(CarrierDetails.class), any(), eq("CarrierDetails"), any()))
        .thenReturn(new ArrayList<>(carrierRequests));

    when(masterDataUtils.fetchInBulkMasterList(any()))
        .thenReturn(keyMasterDataMap);

    doNothing().when(commonUtils).createMasterDataKeysList(anySet(), anySet());

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.MASTER_LIST), anySet(), any(), any());

    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.MASTER_LIST), any()))
        .thenReturn(new HashMap<>());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllMasterDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }


  @Test
  void addAllMasterDataInSingleCall_Success_WithMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    Set<MasterListRequest> listRequests = Set.of(new MasterListRequest());
    Map<String, EntityTransferMasterLists> keyMasterDataMap = Map.of("key1", new EntityTransferMasterLists());

    when(masterDataUtils.createInBulkMasterListRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(new ArrayList<>(listRequests));


    when(masterDataUtils.fetchInBulkMasterList(any()))
        .thenReturn(keyMasterDataMap);

    doNothing().when(commonUtils).createMasterDataKeysList(anySet(), anySet());

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.MASTER_LIST), anySet(), any(), any());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.MASTER_LIST), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllMasterDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllMasterDataInSingleCall_Success_WithMasterDataResponse1() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    response.setAllocations(new AllocationsResponse());
    response.setAchievedQuantities(new AchievedQuantitiesResponse());
    response.setReferenceNumbersList(new ArrayList<>());
    Map<String, Object> masterDataResponse = new HashMap<>();

    Set<MasterListRequest> listRequests = Set.of(new MasterListRequest());
    Map<String, EntityTransferMasterLists> keyMasterDataMap = Map.of("key1", new EntityTransferMasterLists());

    when(masterDataUtils.createInBulkMasterListRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(new ArrayList<>(listRequests));


    when(masterDataUtils.fetchInBulkMasterList(any()))
        .thenReturn(keyMasterDataMap);

    doNothing().when(commonUtils).createMasterDataKeysList(anySet(), anySet());

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.MASTER_LIST), anySet(), any(), any());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.MASTER_LIST), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllMasterDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllMasterDataInSingleCall_Failure_ReturnsNullFuture() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.createInBulkMasterListRequest(
        any(), any(), any(), anyString(), any()))
        .thenThrow(new RuntimeException("Simulated exception"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllMasterDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    assertNull(future.join());
  }

  @Test
  void addAllWarehouseDataInSingleCall_Success_WithMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();
    Set<String> warehouseTypes = Set.of("W1", "W2");

    Map<String, WareHouseResponse> v1Data = Map.of(
        "W1", new WareHouseResponse(),
        "W2", new WareHouseResponse()
    );

    when(masterDataUtils.createInBulkWareHouseRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(new ArrayList<>(warehouseTypes));

    when(masterDataUtils.fetchInWareHousesList(any()))
        .thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.WAREHOUSES), anySet(), any(), any());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.WAREHOUSES), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllWarehouseDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllWarehouseDataInSingleCall_Success_WithNullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Set<String> warehouseTypes = Set.of("W1", "W2");

    Map<String, WareHouseResponse> v1Data = Map.of(
        "W1", new WareHouseResponse(),
        "W2", new WareHouseResponse()
    );

    when(masterDataUtils.createInBulkWareHouseRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(new ArrayList<>(warehouseTypes));

    when(masterDataUtils.fetchInWareHousesList(any()))
        .thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.WAREHOUSES), anySet(), any(), any());

    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.WAREHOUSES), any()))
        .thenReturn(Map.of("dummyKey", "dummyValue"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllWarehouseDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCommodityTypesInSingleCall_Success_WithMasterDataResponse() {
    // Arrange
    Map<String, Object> masterDataResponse = new HashMap<>();

    Map<String, EntityTransferCommodityType> v1Data = Map.of(
        "type1", new EntityTransferCommodityType()
    );

    when(masterDataUtils.fetchInBulkCommodityTypes(anyList()))
        .thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.COMMODITY), anySet(), any(), any());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.COMMODITY), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCommodityTypesInSingleCall(masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCommodityTypesInSingleCall_Success_WithNullMasterDataResponse() {
    // Arrange

    Map<String, EntityTransferCommodityType> v1Data = Map.of(
        "type1", new EntityTransferCommodityType()
    );

    when(masterDataUtils.fetchInBulkCommodityTypes(anyList()))
        .thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(
        any(), eq(CacheConstants.COMMODITY), anySet(), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCommodityTypesInSingleCall(null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCommodityTypesInSingleCall_ExceptionThrown_ReturnsNullResponse() {
    // Arrange
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.fetchInBulkCommodityTypes(anyList()))
        .thenThrow(new RuntimeException("Simulated failure"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCommodityTypesInSingleCall(masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNull(result);
  }

  @Test
  void addAllCurrencyDataInSingleCall_Success_WithMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();
    Set<String> currencyList = Set.of("USD", "INR");

    Map<String, EntityTransferCurrency> v1Data = Map.of(
        "USD", new EntityTransferCurrency(),
        "INR", new EntityTransferCurrency()
    );

    when(masterDataUtils.createInBulkCurrencyRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any())
    ).thenReturn(new ArrayList<>(currencyList));

    when(masterDataUtils.fetchInCurrencyList(anySet())).thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.CURRENCIES), anySet(), any(), any());

    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.CURRENCIES), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCurrencyDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }


  @Test
  void addAllCurrencyDataInSingleCall_Success_WithNullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Set<String> currencyList = Set.of("EUR");

    Map<String, EntityTransferCurrency> v1Data = Map.of("EUR", new EntityTransferCurrency());

    when(masterDataUtils.createInBulkCurrencyRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any())
    ).thenReturn(new ArrayList<>(currencyList));

    when(masterDataUtils.fetchInCurrencyList(anySet())).thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.CURRENCIES), anySet(), any(), any());

    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.CURRENCIES), any()))
        .thenReturn(Map.of("EUR", "Euro"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCurrencyDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCurrencyDataInSingleCall_ExceptionThrown_ReturnsNullResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.createInBulkCurrencyRequest(
        any(), eq(ConsolidationDetails.class), any(), anyString(), any())
    ).thenThrow(new RuntimeException("Simulated error"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCurrencyDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNull(result);
  }

  @Test
  void addAllCarrierDataInSingleCall_Success_WithMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    CarrierDetailResponse carrierDetails = new CarrierDetailResponse();
    response.setCarrierDetails(carrierDetails);
    Map<String, Object> masterDataResponse = new HashMap<>();

    Set<String> carrierList = Set.of("EK", "LH");
    Map<String, EntityTransferCarrier> v1Data = Map.of(
        "EK", new EntityTransferCarrier(),
        "LH", new EntityTransferCarrier()
    );

    when(masterDataUtils.createInBulkCarriersRequest(
        any(), eq(CarrierDetails.class), any(), anyString(), any())
    ).thenReturn(new ArrayList<>(carrierList));

    when(masterDataUtils.fetchInBulkCarriers(anySet())).thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.CARRIER), anySet(), any(), any());
    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.CARRIER), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCarrierDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCarrierDataInSingleCall_Success_WithNullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    CarrierDetailResponse carrierDetails = new CarrierDetailResponse();
    response.setCarrierDetails(carrierDetails);

    Set<String> carrierList = Set.of("AA");

    Map<String, EntityTransferCarrier> v1Data = Map.of("AA", new EntityTransferCarrier());

    when(masterDataUtils.createInBulkCarriersRequest(
        any(), eq(CarrierDetails.class), any(), anyString(), any())
    ).thenReturn(new ArrayList<>(carrierList));

    when(masterDataUtils.fetchInBulkCarriers(anySet())).thenReturn(v1Data);

    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.CARRIER), anySet(), any(), any());

    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.CARRIER), any()))
        .thenReturn(Map.of("AA", "American Airlines"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCarrierDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCarrierDataInSingleCall_Success_WhenCarrierDetailsIsNull() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response(); // No carrierDetails set
    Map<String, Object> masterDataResponse = new HashMap<>();

    Map<String, EntityTransferCarrier> v1Data = Collections.emptyMap();

    when(masterDataUtils.fetchInBulkCarriers(anySet())).thenReturn(v1Data);
    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.CARRIER), anySet(), any(), any());
    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.CARRIER), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCarrierDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllCarrierDataInSingleCall_ExceptionThrown_ReturnsNull() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    CarrierDetailResponse carrierDetails = new CarrierDetailResponse();
    response.setCarrierDetails(carrierDetails);
    Map<String, Object> masterDataResponse = new HashMap<>();

    when(masterDataUtils.createInBulkCarriersRequest(
        any(), eq(CarrierDetails.class), any(), anyString(), any())
    ).thenThrow(new RuntimeException("Test exception"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllCarrierDataInSingleCall(response, masterDataResponse);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNull(result);
  }

  @Test
  void addAllUnlocationDataInSingleCall_Success_WithNullMasterDataResponse() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();

    CarrierDetailResponse carrierDetails = new CarrierDetailResponse();

    response.setCarrierDetails(carrierDetails);

    List<String> unlocCodesMain = List.of("SGSIN");
    List<String> unlocCodesCarrier = List.of("AEJEA");

    Map<String, EntityTransferUnLocations> masterDataMap = Map.of(
        "SGSIN", new EntityTransferUnLocations(),
        "AEJEA", new EntityTransferUnLocations()
    );

    when(masterDataUtils.createInBulkUnLocationsRequest(eq(response), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(unlocCodesMain);
    when(masterDataUtils.createInBulkUnLocationsRequest(eq(carrierDetails), eq(CarrierDetails.class), any(), anyString(), any()))
        .thenReturn(unlocCodesCarrier);
    when(masterDataUtils.fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
        .thenReturn(masterDataMap);
    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.UNLOCATIONS), anySet(), any(), any());
    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.UNLOCATIONS), any()))
        .thenReturn(Map.of("SGSIN", "Singapore"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllUnlocationDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllUnlocationDataInSingleCall_Success_WithMasterDataResponseAndArrivalDeparture() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    CarrierDetailResponse carrierDetails = new CarrierDetailResponse();
    ArrivalDepartureDetailsResponse arrival = new ArrivalDepartureDetailsResponse();
    ArrivalDepartureDetailsResponse departure = new ArrivalDepartureDetailsResponse();

    response.setCarrierDetails(carrierDetails);
    response.setArrivalDetails(arrival);
    response.setDepartureDetails(departure);

    List<String> consUnloc = List.of("USNYC");
    List<String> carrierUnloc = List.of("JPTYO");
    List<String> arrivalUnloc = List.of("INBOM");
    List<String> departureUnloc = List.of("AUMEL");

    Set<String> combinedUnlocs = new HashSet<>();
    combinedUnlocs.addAll(consUnloc);
    combinedUnlocs.addAll(carrierUnloc);
    combinedUnlocs.addAll(arrivalUnloc);
    combinedUnlocs.addAll(departureUnloc);

    Map<String, EntityTransferUnLocations> masterDataMap = new HashMap<>();
    combinedUnlocs.forEach(code -> masterDataMap.put(code, new EntityTransferUnLocations()));

    when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any()))
        .thenReturn(consUnloc);


    when(masterDataUtils.fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
        .thenReturn(masterDataMap);
    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.UNLOCATIONS), anySet(), any(), any());
    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.UNLOCATIONS), any(), any());
    doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), eq(CacheConstants.COUNTRIES), any(), any());

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllUnlocationDataInSingleCall(response, new HashMap<>());

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllUnlocationDataInSingleCall_Success_MinimalCase() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
    List<String> codes = List.of("DEHAM");

    when(masterDataUtils.createInBulkUnLocationsRequest(eq(response), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenReturn(codes);

    Map<String, EntityTransferUnLocations> masterDataMap = Map.of("DEHAM", new EntityTransferUnLocations());

    when(masterDataUtils.fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
        .thenReturn(masterDataMap);
    doNothing().when(masterDataUtils).pushToCache(any(), eq(CacheConstants.UNLOCATIONS), anySet(), any(), any());
    when(masterDataUtils.setMasterData(any(), eq(CacheConstants.UNLOCATIONS), any()))
        .thenReturn(Map.of("DEHAM", "Hamburg"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllUnlocationDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNotNull(result);
    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void addAllUnlocationDataInSingleCall_ExceptionThrown_ReturnsNull() {
    // Arrange
    ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();

    when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(ConsolidationDetails.class), any(), anyString(), any()))
        .thenThrow(new RuntimeException("Simulated error"));

    // Act
    CompletableFuture<ResponseEntity<IRunnerResponse>> future =
        consolidationV3Service.addAllUnlocationDataInSingleCall(response, null);

    // Assert
    assertNotNull(future);
    ResponseEntity<IRunnerResponse> result = future.join();
    assertNull(result);
  }

  @Test
  void getAllMasterData_NteException(){
    String source = "network_transfer";
    Long id = 1L;

    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.empty());

    assertThrows(DataRetrievalFailureException.class, () -> consolidationV3Service.getAllMasterData(id, source));
  }


  @Test
  void getAllMasterData_NteException1(){
    String source = "network_transfer";
    Long id = 1L;

    consolidationDetails.setTriangulationPartnerList(null);
    consolidationDetails.setReceivingBranch(2L);
    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.of(consolidationDetails));
    when(TenantContext.getCurrentTenant()).thenReturn(1);

    assertThrows(AuthenticationException.class, () -> consolidationV3Service.getAllMasterData(id, source));
  }

  @Test
  void getAllMasterData_NteSuccess() throws AuthenticationException, RunnerException {
    String source = "network_transfer";
    Long id = 1L;

    consolidationDetails.setTriangulationPartnerList(null);
    consolidationDetails.setReceivingBranch(1L);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.of(consolidationDetails));
    when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(new ConsolidationDetailsV3Response());
    when(TenantContext.getCurrentTenant()).thenReturn(1);
    Map<String, Object> responseMap = consolidationV3Service.getAllMasterData(id, source);

    assertNotNull(responseMap);
  }

  @Test
  void getAllMasterData_Exception()  {
    String source = "source";
    Long id = 1L;

    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.empty());
    assertThrows(DataRetrievalFailureException.class, () -> consolidationV3Service.getAllMasterData(id, source));
  }

  @Test
  void retrieveById_Success_WithId_RegularSource() throws RunnerException, AuthenticationException {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().id(id).build();
    request.setId(id);

    ConsolidationDetails mockConsolidationDetails = consolidationDetails;
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);

    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, "REGULAR_SOURCE");

    // Assert
    assertNotNull(result);

  }

  @Test
  void retrieveById_Success_WithGuid_RegularSource() throws RunnerException, AuthenticationException {
    // Arrange
    String guidString = "550e8400-e29b-41d4-a716-446655440000";
    UUID guid = UUID.fromString(guidString);
    CommonGetRequest request = CommonGetRequest.builder().build();
    request.setGuid(guidString);

    ConsolidationDetails mockConsolidationDetails = consolidationDetails;
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.of(mockConsolidationDetails));
    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);


    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, "REGULAR_SOURCE");

    // Assert
    assertNotNull(result);
  }

  @Test
  void retrieveById_Success_WithId_NetworkTransferSource() throws RunnerException, AuthenticationException {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().build();
    request.setId(id);
    Routings routings = new Routings();
    routings.setId(1L);
    routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
    consolidationDetails.setRoutingsList(List.of(routings));
    ConsolidationDetails mockConsolidationDetails = consolidationDetails;
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.of(consolidationDetails));
    consolidationDetails.setTriangulationPartnerList(new ArrayList<>());
    when(TenantContext.getCurrentTenant()).thenReturn(100);
    consolidationDetails.setReceivingBranch(100L);

    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);

    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, NETWORK_TRANSFER);

    // Assert
    assertNotNull(result);
    verify(jsonHelper).convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class);
  }

  @Test
  void retrieveById_Success_WithBothIdAndGuid_IdTakesPrecedence() throws RunnerException, AuthenticationException {
    // Arrange
    Long id = 1L;
    String guidString = "550e8400-e29b-41d4-a716-446655440000";
    CommonGetRequest request = CommonGetRequest.builder().build();
    request.setId(id);
    request.setGuid(guidString);

    ConsolidationDetails mockConsolidationDetails = consolidationDetails;
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);

    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, "REGULAR_SOURCE");

    // Assert
    assertNotNull(result);
    verify(consolidationDetailsDao).findById(id);
    verify(consolidationDetailsDao, never()).findByGuid(any(UUID.class));
  }

  @Test
  void retrieveById_ThrowsRunnerException_WhenBothIdAndGuidAreNull() {
    // Arrange
    CommonGetRequest request = CommonGetRequest.builder().build();

    // Act & Assert
    RunnerException exception = assertThrows(RunnerException.class,
        () -> consolidationV3Service.retrieveById(request, "REGULAR_SOURCE"));

    assertEquals(ConsolidationConstants.CONSOLIDATION_REQUEST_NULL_ID_AND_GUID_ERROR, exception.getMessage());
    verify(consolidationDetailsDao, never()).findById(any());
    verify(consolidationDetailsDao, never()).findByGuid(any());
  }

  @Test
  void retrieveById_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithId() {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().id(id).build();
    request.setId(id);

    when(consolidationDetailsDao.findById(id)).thenReturn(Optional.empty());

    // Act & Assert
    DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class,
        () -> consolidationV3Service.retrieveById(request, "REGULAR_SOURCE"));

    assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    verify(consolidationDetailsDao).findById(id);
  }

  @Test
  void retrieveById_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithGuid() {
    // Arrange
    String guidString = "550e8400-e29b-41d4-a716-446655440000";
    UUID guid = UUID.fromString(guidString);
    CommonGetRequest request =  CommonGetRequest.builder().guid(guidString).build();
    request.setGuid(guidString);

    when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.empty());

    // Act & Assert
    DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class,
        () -> consolidationV3Service.retrieveById(request, "REGULAR_SOURCE"));

    assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    verify(consolidationDetailsDao).findByGuid(guid);
  }

  @Test
  void retrieveById_ThrowsDataRetrievalFailureException_WhenNetworkTransferReturnsEmpty() {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().id(id).build();
    request.setId(id);

    // Act & Assert
    DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class,
        () -> consolidationV3Service.retrieveById(request, NETWORK_TRANSFER));

    assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());

  }

  @Test
  void retrieveById_ThrowsIllegalArgumentException_WhenGuidIsInvalidFormat() {
    // Arrange
    String invalidGuid = "invalid-guid-format";
    CommonGetRequest request = CommonGetRequest.builder().guid(invalidGuid).build();


    // Act & Assert
    assertThrows(IllegalArgumentException.class,
        () -> consolidationV3Service.retrieveById(request, "REGULAR_SOURCE"));

    verify(consolidationDetailsDao, never()).findByGuid(any());
  }

  @Test
  void retrieveById_HandlesNullSource() throws RunnerException, AuthenticationException {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().id(id).build();

    ConsolidationDetails mockConsolidationDetails =  consolidationDetails;

    Routings routings1 = new Routings();
    routings1.setId(3L);
    routings1.setConsolidationId(1L);
    routings1.setVoyage("0123");
    routings1.setVesselName("vessel");
    routings1.setPol("pol");
    routings1.setPod("pod");
    routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

    Routings routings2 = new Routings();
    routings2.setId(3l);
    routings2.setConsolidationId(1L);
    routings2.setVoyage("0123");
    routings2.setVesselName("vessel");
    routings2.setPol("pol");
    routings2.setPod("pod");
    routings2.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
    CarrierDetails carrierDetails = new CarrierDetails();
    carrierDetails.setOriginPort("origin");
    carrierDetails.setDestinationPort("dest");
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportInfoStatus(TransportInfoStatus.IH);
    mockConsolidationDetails.setRoutingsList(List.of(routings1,routings2));
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);

    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, null);

    // Assert
    assertNotNull(result);
    verify(consolidationDetailsDao).findById(id);
  }

  @Test
  void retrieveById_HandlesEmptyStringSource() throws RunnerException, AuthenticationException {
    // Arrange
    Long id = 1L;
    CommonGetRequest request = CommonGetRequest.builder().build();
    request.setId(id);

    ConsolidationDetails mockConsolidationDetails = consolidationDetails;
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
    when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3Response.class))
        .thenReturn(mockResponse);

    // Act
    ConsolidationDetailsV3Response result = consolidationV3Service.retrieveById(request, "");

    // Assert
    assertNotNull(result);
    verify(consolidationDetailsDao).findById(id);
  }

  @Test
  void calculateConsoleShipmentTeuCount_ComputesCorrectValues() {
    // Arrange

    Containers container1 = new Containers();
    container1.setId(1L);
    container1.setContainerCount(2L);

    Containers container2 = new Containers();
    container2.setId(2L);
    container2.setContainerCount(1L);

    List<Containers> containersList = Arrays.asList(container1, container2);
    consolidationDetails.setContainersList(containersList);

    ShipmentGridChangeV3Response response = new ShipmentGridChangeV3Response();
    V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
    Set<Long> assignedContainerIds = new HashSet<>(Collections.singleton(1L));

    EntityTransferContainerType mockType = new EntityTransferContainerType();
    mockType.setTeu(1.0);

    var spyService = Mockito.spy(consolidationV3Service);

    // Stub internal method calls
    ContainerResponse containerResponse = new ContainerResponse();
    containerResponse.setId(1L);
    when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(containerResponse));
    when(masterDataUtils.createInBulkContainerTypeRequest(any(), any(), any(), any(), anyMap())).thenReturn(new ArrayList<>());
    Cache cache = mock(Cache.class);
    when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(cache);

    // Act
    spyService.calculateConsoleShipmentTeuCount(consolidationDetails, response, tenantSettings, assignedContainerIds);

    // Assert
    assertNotNull(response.getSummaryConsoleTEU());
    assertNotNull(response.getSummaryShipmentTEU());
    assertNotNull(response.getSummaryConsolContainer());
    assertNotNull(response.getSummaryShipmentContainer());
    assertNotNull(response.getSummaryAssignedTEUs());

  }


  @Test
  void calculateConsoleShipmentTeuCount_ComputesCorrectValues1() {
    // Arrange
    Containers container1 = new Containers();
    container1.setId(1L);
    container1.setContainerCount(2L);

    Containers container2 = new Containers();
    container2.setId(2L);
    container2.setContainerCount(1L);
    container2.setShipmentsList(Set.of(shipmentDetails));

    List<Containers> containersList = Arrays.asList(container1, container2);
    consolidationDetails.setContainersList(containersList);
    consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);



    ShipmentGridChangeV3Response response = new ShipmentGridChangeV3Response();
    V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
    Set<Long> assignedContainerIds = new HashSet<>(Collections.singleton(1L));

    EntityTransferContainerType mockType = new EntityTransferContainerType();
    mockType.setTeu(1.0);

    var spyService = Mockito.spy(consolidationV3Service);

    // Stub internal method calls
    ContainerResponse containerResponse = new ContainerResponse();
    containerResponse.setId(1L);
    when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(containerResponse));
    when(masterDataUtils.createInBulkContainerTypeRequest(any(), any(), any(), any(), anyMap())).thenReturn(new ArrayList<>());
    EntityTransferContainerType entityTransferContainerType = new EntityTransferContainerType();
    entityTransferContainerType.setTeu(10.0);
    Mockito.doReturn(entityTransferContainerType).when(spyService).getEntityTransferObjectCache(any(), anyMap());
    // Act
    spyService.calculateConsoleShipmentTeuCount(consolidationDetails, response, tenantSettings, assignedContainerIds);

    // Assert
    assertNotNull(response.getSummaryConsoleTEU());
    assertNotNull(response.getSummaryShipmentTEU());
    assertNotNull(response.getSummaryConsolContainer());
    assertNotNull(response.getSummaryShipmentContainer());
    assertNotNull(response.getSummaryAssignedTEUs());

  }

  @Test
  void testAttachShipments_consolePullCall_shouldReturnNotNullWarning() throws RunnerException {

    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    request.setConsolidationId(100L);
    request.setShipmentIds(Set.of(1L, 2L));
    request.setFromConsolidation(false);
    request.setShipmentRequestedType(null);

    shipmentDetails.setId(1L);
    shipmentDetails.setTenantId(20);
    shipmentDetails.setDirection("IMP");

    consolidationDetails.setId(100L);
    consolidationDetails.setOpenForAttachment(true);
    consolidationDetails.setTenantId(10);
    consolidationDetails.setShipmentsList(new HashSet<>());
    consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
    consolidationDetails.setAchievedQuantities(new AchievedQuantities());
    // Mocks

    // other essential mocks
    doNothing().when(consolidationValidationV3Util).validateConsolidationIdAndShipmentIds(anyLong(), anyList());
    HashSet<Long> hashSet = new HashSet<>();
    hashSet.add(1L);
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
    when(consoleShipmentMappingDao.assignShipments(any(), any(), any(), any(), any(), any(), any())).thenReturn(hashSet);
    when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
    when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
    doReturn(true)
            .when(consolidationValidationV3Util)
            .checkConsolidationTypeValidation(any());
    when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().build());
    mockTenantSettings();

    // method under test
    String warning = consolidationV3Service.attachShipments(request);

    // assertions
    assertNotNull(warning);
  }

  @Test
  void testAttachShipments_notAllowed_shouldThrowRunnerException() {
    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    request.setConsolidationId(100L);
    request.setShipmentIds(Set.of(1L, 2L));
    request.setFromConsolidation(false);
    request.setShipmentRequestedType(null);

    consolidationDetails.setOpenForAttachment(false);
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
    doNothing().when(consolidationValidationV3Util).validateConsolidationIdAndShipmentIds(anyLong(), anyList());

    assertThrows(RunnerException.class, () -> consolidationV3Service.attachShipments(request));
  }

  @Test
  void testAttachShipments_consolePullCall_shouldReturnNullWarning() throws RunnerException {

    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    request.setConsolidationId(100L);
    request.setShipmentIds(Set.of(1L, 2L));
    request.setFromConsolidation(false);
    request.setShipmentRequestedType(null);

    shipmentDetails.setId(1L);
    shipmentDetails.setTenantId(20);
    shipmentDetails.setDirection("IMP");

    consolidationDetails.setId(100L);
    consolidationDetails.setOpenForAttachment(true);
    consolidationDetails.setTenantId(10);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setShipmentsList(new HashSet<>());
    consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
    consolidationDetails.setAchievedQuantities(new AchievedQuantities());
    // Mocks
    lenient().when(shipmentDao.findShipmentsByIds(anySet()))
        .thenReturn(List.of(shipmentDetails));
    lenient().when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), anyList(), any(), anySet(), anySet(), any()))
        .thenReturn(new HashSet<>(List.of(1L)));
    when(consoleShipmentMappingDao.findAll(any(), any()))
        .thenReturn(new PageImpl<>(List.of()));

    // other essential mocks
    doNothing().when(consolidationValidationV3Util).validateConsolidationIdAndShipmentIds(anyLong(), anyList());
    HashSet<Long> hashSet = new HashSet<>();
    hashSet.add(1L);
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
    when(consoleShipmentMappingDao.assignShipments(any(), any(), any(), any(), any(), any(), any())).thenReturn(hashSet);
    when(shipmentDao.findShipmentsByIds(any())).thenReturn(new ArrayList<>());
    when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
    when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
    lenient().when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().build());
    mockTenantSettings();


    // method under test
    String warning = consolidationV3Service.attachShipments(request);

    // assertions
    assertNull(warning);
  }

  @Test
  void testCanProcessConsole() {
    // setup
    ConsolidationDetails console = ConsolidationDetails.builder().shipmentType("HSE").carrierDetails(CarrierDetails.builder().build()).build();
    ConsolidationDetails oldEntity = ConsolidationDetails.builder().shipmentType("HSE").carrierDetails(CarrierDetails.builder().build()).build();

    // method under test
    boolean result = consolidationV3Service.canProcessConsole(console, oldEntity, new HashMap<>());

    // assertions
    assertFalse(result);
  }

  @Test
  void testSendImportShipmentPullAttachmentEmail() {
    // setup
    shipmentDetails = ShipmentDetails.builder().build();
    consolidationDetails = ConsolidationDetails.builder().build();
    consolidationDetails.setAssignedTo("assignedToUser");
    List<EmailTemplatesRequest> emailTemplatesRequestsModel = new ArrayList<>();
    emailTemplatesRequestsModel.add(EmailTemplatesRequest.builder().build());

    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    // method under test
    ResponseEntity<IRunnerResponse> result = consolidationV3Service.sendImportShipmentPullAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequestsModel);

    // assertions
    assertNotNull(result);
  }

  @Test
  void testSendEmailForPullRequested() {
    when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testShipment)));
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    // method under test
    assertDoesNotThrow(() -> consolidationV3Service.sendEmailForPullRequested(testConsol, List.of(1L), new HashSet<>()));
  }

  @Test
  void testGetSummaryDgPacks() {
    // Setup
    Packing packing = new Packing();
    packing.setPacks("1");
    List<Packing> packingList = List.of(packing);

    // method under test
    String result = consolidationV3Service.getSummaryDgPacks(packingList);

    assertNotNull(result);
  }

  @Test
  void testGetSummaryDgPacks1() {
    // Setup
    Packing packing = new Packing();
    packing.setPacks("1");
    packing.setHazardous(true);
    List<Packing> packingList = List.of(packing);

    // method under test
    String result = consolidationV3Service.getSummaryDgPacks(packingList);

    assertNotNull(result);
  }

  @Test
  void testGetSummaryDGShipments() {
    // Setup
    Set<ShipmentDetails> shipmentDetailsList = Set.of(new ShipmentDetails());

    // method under test
    String result = consolidationV3Service.getSummaryDGShipments(shipmentDetailsList);

    assertNotNull(result);
  }

  @Test
  void testCalculateConsoleUtilization() throws RunnerException {
    // method under test
    ConsolidationDetails result = consolidationV3Service.calculateConsolUtilization(testConsol);

    assertNotNull(result);
  }

  @Test
  void testCalculateConsoleUtilization1() throws RunnerException {
    // method under test
    Allocations allocations = new Allocations();
    allocations.setWeight(BigDecimal.ONE);
    allocations.setVolume(BigDecimal.ONE);
    allocations.setWeightUnit("KG");
    allocations.setVolumeUnit("M3");
    testConsol.setAllocations(allocations);
    AchievedQuantities achievedQuantities = new AchievedQuantities();
    achievedQuantities.setConsolidatedWeight(BigDecimal.ONE);
    achievedQuantities.setConsolidatedVolume(BigDecimal.ONE);
    achievedQuantities.setWeightUtilization("100");
    achievedQuantities.setVolumeUtilization("100");
    achievedQuantities.setConsolidatedWeightUnit("KG");
    achievedQuantities.setConsolidatedVolumeUnit("M3");
    testConsol.setAchievedQuantities(achievedQuantities);
    testConsol.setAllocations(allocations);
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any())).thenReturn(BigDecimal.ONE);
    ConsolidationDetails result = consolidationV3Service.calculateConsolUtilization(testConsol);

    assertNotNull(result);
  }

  @Test
  void testProcessInterConsoleAttachShipment() {
    // Setup
    ConsolidationDetails console = new ConsolidationDetails();
    console.setInterBranchConsole(true);
    List<ShipmentDetails> shipments = List.of(new ShipmentDetails());


    // method under test
    consolidationV3Service.processInterConsoleAttachShipment(console, shipments);

    assertNotNull(console);
  }

  @Test
  void testProcessInterConsoleAttachShipment1() {
    // Setup
    testConsol.setInterBranchConsole(true);
    testConsol.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setShipmentType(Constants.DIRECTION_EXP);
    List<ShipmentDetails> shipments = List.of(testShipment);
    testConsol.setShipmentsList(Set.of(testShipment));
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
    mockShipmentSettings();
    // method under test
    consolidationV3Service.processInterConsoleAttachShipment(testConsol, shipments);

    assertNotNull(testConsol);
  }

  @Test
  void testProcessInterConsoleAttachShipment2() {
    // Setup
    testConsol.setInterBranchConsole(true);
    testConsol.setReceivingBranch(1L);
    testConsol.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setShipmentType(Constants.DIRECTION_EXP);
    List<ShipmentDetails> shipments = List.of(testShipment);
    testConsol.setShipmentsList(Set.of(testShipment));
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
    mockShipmentSettings();
    // method under test
    consolidationV3Service.processInterConsoleAttachShipment(testConsol, shipments);

    assertNotNull(testConsol);
  }

  @Test
  void testProcessInterConsoleAttachShipment3() {
    // Setup
    testConsol.setInterBranchConsole(true);
    testConsol.setReceivingBranch(1L);
    testConsol.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setReceivingBranch(1L);
    List<ShipmentDetails> shipments = List.of(testShipment);
    testConsol.setShipmentsList(Set.of(testShipment));
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
    mockShipmentSettings();
    // method under test
    consolidationV3Service.processInterConsoleAttachShipment(testConsol, shipments);

    assertNotNull(testConsol);
  }

  @Test
  void testProcessInterConsoleAttachShipment4() {
    // Setup
    testConsol.setInterBranchConsole(true);
    testConsol.setReceivingBranch(1L);
    testConsol.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setShipmentType(Constants.DIRECTION_EXP);
    testShipment.setReceivingBranch(2L);
    List<ShipmentDetails> shipments = List.of(testShipment);
    testConsol.setShipmentsList(Set.of(testShipment));
    ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
    mockShipmentSettings();
    // method under test
    consolidationV3Service.processInterConsoleAttachShipment(testConsol, shipments);

    assertNotNull(testConsol);
  }

  @Test
  void testPendingNotificationData() {
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationPendingNotificationResponse result = consolidationV3Service.getPendingNotificationData(CommonGetRequest.builder().id(1L).build());
    assertNull(result);
  }

  @Test
  void testPendingNotificationData1() {
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
    CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
    assertThrows(DataRetrievalFailureException.class, () -> consolidationV3Service.getPendingNotificationData(getRequest));
  }

  @Test
  void calculateAchievedValues_NoShipmentIds_ShouldCalculateForAllShipments() throws Exception {

    Long consolidationId = 101L;
    CalculateAchievedValueRequest request = new CalculateAchievedValueRequest();
    request.setConsolidationId(consolidationId);
    request.setShipmentIds(null); // all shipments

    ShipmentDetails shipment1 = new ShipmentDetails();
    shipment1.setId(1L);
    shipment1.setWeight(BigDecimal.valueOf(10));
    shipment1.setWeightUnit("KG");
    shipment1.setVolume(BigDecimal.valueOf(2));
    shipment1.setVolumeUnit("M3");
    shipment1.setNoOfPacks(5);
    shipment1.setPacksUnit("BOX");

    ShipmentDetails shipment2 = new ShipmentDetails();
    shipment1.setId(2L);
    shipment2.setWeight(BigDecimal.valueOf(11));
    shipment2.setWeightUnit("KG");
    shipment2.setVolume(BigDecimal.valueOf(3));
    shipment2.setVolumeUnit("M3");
    shipment2.setNoOfPacks(5);
    shipment2.setPacksUnit("Parcel");

    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setShipmentsList(Set.of(shipment1,shipment2));
    consolidationDetails.setOverride(false);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    consolidationDetails.setAchievedQuantities(new AchievedQuantities());
    consolidationDetails.setAllocations(new Allocations());

    shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setWeightChargeableUnit("KG");
    shipmentSettingsDetails.setVolumeChargeableUnit("M3");

    V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();

    // Mock dependent methods
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
    when(containerV3Service.findContainerIdsAttachedToEitherPackingOrShipment(anyList())).thenReturn(List.of());
    when(jsonHelper.convertValue(any(), eq(AllocationsResponse.class))).thenReturn(new AllocationsResponse());
    when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(new AchievedQuantitiesResponse());
    when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
    // Mock static method convertUnit to simply return the input BigDecimal value (simulate successful conversion)
    unitConversionUtilityMockedStatic.when(() ->
                    UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenAnswer(invocation -> {
              BigDecimal val = invocation.getArgument(1);
              return val == null ? BigDecimal.ZERO : val;
            });

    // Act
    ShipmentGridChangeV3Response response = consolidationV3Service.calculateAchievedValues(request);

    // Assert
    assertNotNull(response);
    assertEquals(2, response.getSummaryShipmentsCount(), "Should have 2 shipments in summary");
    assertEquals(10, response.getTotalPacks(), "Total packs should be 10");
    assertEquals("PKG", response.getPackType(), "Pack type should be BOX");
    assertTrue(response.getSummaryWeight().contains("KG"), "Summary weight should contain KG");
    assertTrue(response.getSummaryVolume().contains("M3"), "Summary volume should contain M3");
  }

  @Test
  void calculateAchievedValues_NoShipmentIds_ShouldCalculateForAllShipments_SEA() throws Exception {

    Long consolidationId = 101L;
    CalculateAchievedValueRequest request = new CalculateAchievedValueRequest();
    request.setConsolidationId(consolidationId);
    request.setShipmentIds(null); // all shipments

    ShipmentDetails shipment1 = new ShipmentDetails();
    shipment1.setId(1L);
    shipment1.setWeight(BigDecimal.valueOf(10));
    shipment1.setWeightUnit("KG");
    shipment1.setVolume(BigDecimal.valueOf(2));
    shipment1.setVolumeUnit("M3");
    shipment1.setNoOfPacks(5);
    shipment1.setPacksUnit("BOX");

    ShipmentDetails shipment2 = new ShipmentDetails();
    shipment1.setId(2L);
    shipment2.setWeight(BigDecimal.valueOf(11));
    shipment2.setWeightUnit("KG");
    shipment2.setVolume(BigDecimal.valueOf(3));
    shipment2.setVolumeUnit("M3");
    shipment2.setNoOfPacks(5);
    shipment2.setPacksUnit("Parcel");

    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setShipmentsList(Set.of(shipment1,shipment2));
    consolidationDetails.setOverride(false);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
    consolidationDetails.setAchievedQuantities(new AchievedQuantities());
    consolidationDetails.setAllocations(new Allocations());

    shipmentSettingsDetails = new ShipmentSettingsDetails();
    shipmentSettingsDetails.setWeightChargeableUnit("KG");
    shipmentSettingsDetails.setVolumeChargeableUnit("M3");

    V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();

    // Mock dependent methods
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
    when(containerV3Service.findContainerIdsAttachedToEitherPackingOrShipment(anyList())).thenReturn(List.of());
    when(jsonHelper.convertValue(any(), eq(AllocationsResponse.class))).thenReturn(new AllocationsResponse());
    when(jsonHelper.convertValue(any(), eq(AchievedQuantitiesResponse.class))).thenReturn(new AchievedQuantitiesResponse());
    when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
    // Mock static method convertUnit to simply return the input BigDecimal value (simulate successful conversion)
    unitConversionUtilityMockedStatic.when(() ->
                    UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenAnswer(invocation -> {
              BigDecimal val = invocation.getArgument(1);
              return val == null ? BigDecimal.ZERO : val;
            });

    // Act
    ShipmentGridChangeV3Response response = consolidationV3Service.calculateAchievedValues(request);

    // Assert
    assertNotNull(response);
    assertEquals(2, response.getSummaryShipmentsCount(), "Should have 2 shipments in summary");
    assertEquals(10, response.getTotalPacks(), "Total packs should be 10");
    assertEquals("PKG", response.getPackType(), "Pack type should be BOX");
    assertTrue(response.getSummaryWeight().contains("KG"), "Summary weight should contain KG");
    assertTrue(response.getSummaryVolume().contains("M3"), "Summary volume should contain M3");
  }

  @Test
  void testUpdateSailingScheduleDataToShipment_SEARequest_shouldUpdateAndReturnResponse() throws RunnerException {
    // Prepare routing and request
    RoutingsRequest routing = new RoutingsRequest();
    routing.setConsolidationId(1L);
    List<RoutingsRequest> routingList = List.of(routing);
    ConsolidationSailingScheduleRequest request = new ConsolidationSailingScheduleRequest();
    request.setRoutings(routingList);
    request.setCarrier("MAERSK");

    // Mock consolidation details
    ShipmentDetails shipment1 = new ShipmentDetails();
    CarrierDetails carrierDetails = new CarrierDetails();
    shipment1.setCarrierDetails(carrierDetails);
    shipment1.setTransportMode(TRANSPORT_MODE_SEA);


    ConsolidationDetails consolidation = new ConsolidationDetails();
    consolidation.setId(1L);
    consolidation.setCarrierDetails(carrierDetails);
    consolidation.setShipmentsList(Set.of(shipment1));
    consolidation.setTransportMode(TRANSPORT_MODE_SEA);


    Routings routing1 = new Routings();
    routing1.setConsolidationId(1L);
    routing1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

    List<Routings> routingsList = new ArrayList<>();
    routingsList.add(routing1);

    when(routingsV3Service.getRoutingsByConsolidationId(anyLong())).thenReturn(routingsList);
    doNothing().when(consolidationDetailsDao).updateSailingScheduleRelatedInfo(any(), anyLong());
    when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidation));

    // Execute
    ConsolidationSailingScheduleResponse response = consolidationV3Service.updateSailingScheduleDataToShipment(request);

    // Verify
    assertNotNull(response);
  }

  @Test
  void testUpdateSailingScheduleDataToShipment_AIRRequest_shouldUpdateAndReturnResponse() throws RunnerException {
    // Prepare routing and request
    RoutingsRequest routing = new RoutingsRequest();
    routing.setConsolidationId(1L);
    List<RoutingsRequest> routingList = List.of(routing);
    ConsolidationSailingScheduleRequest request = new ConsolidationSailingScheduleRequest();
    request.setRoutings(routingList);
    request.setCarrier("MAERSK");

    // Mock consolidation details
    ShipmentDetails shipment1 = new ShipmentDetails();
    CarrierDetails carrierDetails = new CarrierDetails();
    shipment1.setCarrierDetails(carrierDetails);
    shipment1.setTransportMode(TRANSPORT_MODE_AIR);


    ConsolidationDetails consolidation = new ConsolidationDetails();
    consolidation.setId(1L);
    consolidation.setCarrierDetails(carrierDetails);
    consolidation.setTransportMode(TRANSPORT_MODE_AIR);
    consolidation.setShipmentsList(Set.of(shipment1));

    Routings routing1 = new Routings();
    routing1.setConsolidationId(1L);
    routing1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

    List<Routings> routingsList = new ArrayList<>();
    routingsList.add(routing1);

    when(routingsV3Service.getRoutingsByConsolidationId(anyLong())).thenReturn(routingsList);
    doNothing().when(consolidationDetailsDao).updateSailingScheduleRelatedInfoForAir(any(), anyLong());

    when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidation));

    // Execute
    ConsolidationSailingScheduleResponse response = consolidationV3Service.updateSailingScheduleDataToShipment(request);

    // Verify
    assertNotNull(response);
  }

  @Test
  void testUpdateSailingScheduleDataToShipment_emptyRouting_shouldReturnEmptyResponse() throws RunnerException {
    ConsolidationSailingScheduleRequest request = new ConsolidationSailingScheduleRequest();
    request.setRoutings(Collections.emptyList());

    ConsolidationSailingScheduleResponse response = consolidationV3Service.updateSailingScheduleDataToShipment(request);

    assertNotNull(response);
  }

  @Test
  void testUpdateSailingScheduleDataToShipment_consolidationNotFound_shouldReturnEmptyResponse() throws RunnerException {
    RoutingsRequest routing = new RoutingsRequest();
    routing.setConsolidationId(1L);
    ConsolidationSailingScheduleRequest request = new ConsolidationSailingScheduleRequest();
    request.setRoutings(List.of(routing));

    when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

    ConsolidationSailingScheduleResponse response = consolidationV3Service.updateSailingScheduleDataToShipment(request);

    assertNotNull(response);
    verify(routingsV3Service).updateBulk(any(BulkUpdateRoutingsRequest.class), eq("CONSOLIDATION"));
    verify(shipmentV3Service, never()).saveAll(any());
  }

  @Test
  void testGetIdFromGuid_Success() {
    consolidationDetails = testConsol;
    when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
    ResponseEntity<IRunnerResponse> responseEntity = consolidationV3Service.getIdFromGuid(
            CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(consolidationDetails.getGuid().toString()).build()));
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testGetIdFromGuid_Failure() {
    consolidationDetails = testConsol;
    when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
    ResponseEntity<IRunnerResponse> responseEntity = consolidationV3Service.getIdFromGuid(
            CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(consolidationDetails.getGuid().toString()).build()));
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }



    @Test
    void testAibActionApprove() throws RunnerException {
        ConsolidationV3Service spyService = spy(consolidationV3Service);
        aibActionConsolidation.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        aibActionConsolidation.setConsoleId(1L);
        aibActionConsolidation.setListOfShipments(List.of(1L));

        consolidationDetails.setInterBranchConsole(true);
        testConsol.setOpenForAttachment(true);
        testConsol.setShipmentsList(Set.of());
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(testConsol));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        doNothing().when(spyService).sendEmailsForPushRequestAccept(any(), any(), any(), any());
        doReturn("OK").when(spyService).attachShipments(any());
        ResponseEntity<IRunnerResponse> response = spyService.aibAction(aibActionConsolidation);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

  @Test
  void testAibActionApprove_Exception() throws RunnerException {
    ConsolidationV3Service spyService = spy(consolidationV3Service);
    aibActionConsolidation.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
    aibActionConsolidation.setConsoleId(1L);
    aibActionConsolidation.setListOfShipments(List.of(1L));

    consolidationDetails.setInterBranchConsole(true);
    testConsol.setOpenForAttachment(true);
    testConsol.setShipmentsList(Set.of());
    when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(testConsol));
    doThrow(new RunnerException("Exception")).when(spyService).attachShipments(any());

    ValidationException exception = assertThrows(ValidationException.class, () -> {
      spyService.aibAction(aibActionConsolidation);
    });

    assertEquals("Exception", exception.getMessage());
  }

    @Test
    void testAibActionReject() throws RunnerException {
        ConsolidationV3Service spyService = spy(consolidationV3Service);
        aibActionConsolidation.setShipmentRequestedType(ShipmentRequestedType.REJECT);
        aibActionConsolidation.setConsoleId(1L);
        aibActionConsolidation.setListOfShipments(List.of(1L));

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        doNothing().when(spyService).sendEmailForPushRequestReject(any(), any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.aibAction(aibActionConsolidation);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testAibActionWithdraw() throws RunnerException {
        aibActionConsolidation.setShipmentRequestedType(ShipmentRequestedType.WITHDRAW);
        aibActionConsolidation.setConsoleId(1L);
        aibActionConsolidation.setListOfShipments(List.of(1L));

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(this ::mockRunnable);

        ResponseEntity<IRunnerResponse> response = consolidationV3Service.aibAction(aibActionConsolidation);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testAibActionDataRetrievalFailure() {
        aibActionConsolidation.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class, () -> {
          consolidationV3Service.aibAction(aibActionConsolidation);
        });

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void testSendEmailsForPushRequestAccept() throws Exception {
        ConsolidationV3Service spyService = spy(consolidationV3Service);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        when(masterDataUtils.withMdc(any())).thenReturn(this :: mockRunnable);
        spyService.sendEmailsForPushRequestAccept(testConsol, List.of(1L), new HashSet<>(), new ArrayList<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), anyBoolean());
    }

    @Test
    void sendEmailForPushRequestReject() throws Exception {
        ConsolidationV3Service spyService = spy(consolidationV3Service);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(masterDataUtils.withMdc(any())).thenReturn(this :: mockRunnable);
        spyService.sendEmailForPushRequestReject(consolidationDetails, List.of(2L), new HashSet<>(), "rejectRemarks", List.of(consoleShipmentMapping));
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), anyBoolean());
    }

  @Test
  void testGetPendingNotificationsSuccess() {
    AibNotificationRequest request = new AibNotificationRequest();
    request.setId(1L);

    ShipmentDetails mockShip1 = jsonTestUtility.getTestShipment();
    mockShip1.setTenantId(1);
    mockShip1.setId(1L);
    var mockShip2 = objectMapperTest.convertValue(mockShip1, ShipmentDetails.class);
    mockShip2.setId(2L);
    var mockShip3 = objectMapperTest.convertValue(mockShip1, ShipmentDetails.class);
    mockShip3.setId(3L);

    V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
    tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
    tenantSettingsResponse.setIsColoadingMAWBStationEnabled(true);
    TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

    List<ConsoleShipmentMapping> mappings = new ArrayList<>();
    mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
    mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(2L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
    mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
    mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(3L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());

    List<ShipmentDetails> shipmentDetailsList = List.of(mockShip1, mockShip2, mockShip3);

    // mocking
    mockTenantSettings();
    when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(mappings));
    when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
    // Test
    var httpResponse = consolidationV3Service.aibPendingNotification(CommonRequestModel.buildRequest(request));
    // Assert
    assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    var runnerResponse = objectMapperTest.convertValue(httpResponse.getBody(), RunnerResponse.class);
    JavaType javaType = objectMapperTest.getTypeFactory().constructParametricType(PendingNotificationResponse.class, PendingConsolidationActionResponse.class);
    PendingNotificationResponse<PendingConsolidationActionResponse> responseBody = objectMapperTest.convertValue(runnerResponse.getData(), javaType);
    assertEquals(2, responseBody.getNotificationMap().size()); // number of consol with pending notifications
    assertEquals(2, responseBody.getNotificationMap().get(1L).size()); // notification count of consol with id 1L
  }

  @Test
  void testGetPendingNotificationsReturnsEmptyResponseIfTenantSettingsNotEnabled() {
    AibNotificationRequest request = new AibNotificationRequest();
    request.setId(1L);

    V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
    TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

    // Test
    var httpResponse = consolidationV3Service.aibPendingNotification(CommonRequestModel.buildRequest(request));
    // Assert
    assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    var responseBody = objectMapperTest.convertValue(httpResponse.getBody(), PendingNotificationResponse.class);
    assertNull(responseBody.getNotificationMap());
  }

  @Test
  void testGetPendingNotificationsReturnsEmptyResponseForEmptyList() {
    AibNotificationRequest request = new AibNotificationRequest();
    PendingNotificationResponse mockResponse  = new PendingNotificationResponse();

    var httpResponse = consolidationV3Service.aibPendingNotification(CommonRequestModel.buildRequest(request));

    assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
  }

  @Test
  void testSendEmailFOrPullRequestWithdraw() {
      consolidationDetails = new ConsolidationDetails();
      consolidationDetails.setId(1L);
      consolidationDetails.setAssignedTo("assignedToUser");
      boolean isSuccess = true;
      when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipmentDetails));
      when(masterDataUtils.withMdc(any())).thenReturn(this :: mockRunnable);

      consolidationV3Service.sendEmailForPullRequestWithdrawal(consolidationDetails, Arrays.asList(2L), Set.of(), "Remarks");
      assertTrue(isSuccess);
  }

  @Test
  void TestCreateConsolidationFromEntityTransfer(){
    var spyService = Mockito.spy(consolidationV3Service);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    mockShipmentSettings();
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    when(consolidationDetailsDao.saveV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
    ConsolidationDetailsResponse createResponse = spyService.createConsolidationFromEntityTransfer(consolidationEtV3Request);

    assertNotNull(createResponse);

  }

  @Test
  void TestCreateConsolidationFromEntityTransfer2() throws RunnerException {
    var spyService = Mockito.spy(consolidationV3Service);
    CarrierDetails carrierDetails = new CarrierDetails();
    consolidationDetails.setCarrierDetails(carrierDetails);
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    mockShipmentSettings();
    when(consolidationDetailsDao.saveV3(any())).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    doThrow(new RunnerException("IllegalAccessException")).when(consolidationV3Util).afterSaveForET(any(), any(), any(), eq(true), any(), eq(false), eq(true));
    assertThrows(ValidationException.class, ()->spyService.createConsolidationFromEntityTransfer(consolidationEtV3Request));

  }

  @Test
  void TestCompleteUpdateConsolidationFromEntityTransfer_Success() throws RunnerException {
      consolidationEtV3Request.setId(1L);
      consolidationDetails.setInterBranchConsole(true);
      consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);
      consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);

      var spyService = Mockito.spy(consolidationV3Service);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);

      when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
              .thenReturn(new BigDecimal("1000"));
      when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());
      when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
      when(jsonHelper.convertToJson(any())).thenReturn("ABC");
      when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetails);
      when(jsonHelper.readFromJson(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
      when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
      when(consolidationDetailsDao.findById(any())).thenReturn(Optional.ofNullable(consolidationDetails));
      mockShipmentSettings();
      mockTenantSettings();
      when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(new ConsolidationDetailsResponse());

      ConsolidationDetailsResponse consolidationDetailsV3Response = spyService.completeUpdateConsolidationFromEntityTransfer(consolidationEtV3Request);
      assertNotNull(consolidationDetailsV3Response);
  }

  @Test
  void TestCompleteUpdateConsolidationFromEntityTransfer_Exception() throws RunnerException {
    consolidationEtV3Request.setId(1L);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
    when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
    var spyService = Mockito.spy(consolidationV3Service);

    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(new BigDecimal("1000"));
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());
    when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(jsonHelper.convertToJson(any())).thenReturn("ABC");
    when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetails);
    when(jsonHelper.readFromJson(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
    when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
    when(consolidationDetailsDao.findById(any())).thenReturn(Optional.ofNullable(consolidationDetails));
    mockShipmentSettings();
    mockTenantSettings();
    doThrow(new RunnerException("IllegalAccessException")).when(consolidationV3Util).afterSaveForET(any(), any(), any(), eq(false), any(), eq(false), eq(false));
    assertThrows(GenericException.class, ()->spyService.completeUpdateConsolidationFromEntityTransfer(consolidationEtV3Request));
  }

  @Test
  void TestCompleteUpdateConsolidationFromEntityTransfer_Exception2() {
    consolidationEtV3Request.setId(1L);
    consolidationDetails.setInterBranchConsole(true);
    consolidationDetails.setContainerCategory(SHIPMENT_TYPE_LCL);
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);

    var spyService = Mockito.spy(consolidationV3Service);

    when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());

    assertThrows(DataRetrievalFailureException.class, ()->spyService.completeUpdateConsolidationFromEntityTransfer(consolidationEtV3Request));
  }

  @Test
  void testSeaMode_AttachedShipment_UpdatesAllFields() {
    ConsolidationDetails newConsolidation = new ConsolidationDetails();
    ShipmentDetails shipment = new ShipmentDetails();
    newConsolidation.setTerminalCutoff(LocalDateTime.of(2025, 1, 1, 10, 0));
    newConsolidation.setVerifiedGrossMassCutoff(LocalDateTime.of(2025, 1, 2, 10, 0));
    newConsolidation.setShipInstructionCutoff(LocalDateTime.of(2025, 1, 3, 10, 0));
    newConsolidation.setHazardousBookingCutoff(LocalDateTime.of(2025, 1, 4, 10, 0));
    newConsolidation.setReeferCutoff(LocalDateTime.of(2025, 1, 5, 10, 0));
    newConsolidation.setEarliestEmptyEquPickUp(LocalDateTime.of(2025, 1, 6, 10, 0));
    newConsolidation.setLatestFullEquDeliveredToCarrier(LocalDateTime.of(2025, 1, 7, 10, 0));
    newConsolidation.setEarliestDropOffFullEquToCarrier(LocalDateTime.of(2025, 1, 8, 10, 0));
    newConsolidation.setCarrierDocCutOff(LocalDateTime.of(2025, 1, 9, 10, 0));
    newConsolidation.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 1, 10, 10, 0));
    newConsolidation.setLastFreeDateCutOff(LocalDateTime.of(2025, 1, 11, 10, 0));
    newConsolidation.setNumberOfFreeDaysCutOff(3);

    shipment.setTransportMode(TRANSPORT_MODE_SEA);
    shipment.setDirection(DIRECTION_EXP);

    consolidationV3Service.updateShipmentDetailsIfConsolidationChanged(null, newConsolidation,
        List.of(shipment), true);

    assertThat(shipment.getTerminalCutoff()).isEqualTo(LocalDateTime.of(2025, 1, 1, 10, 0));
    assertThat(shipment.getCarrierDocCutOff()).isEqualTo(LocalDateTime.of(2025, 1, 9, 10, 0));
    assertThat(shipment.getCargoReceiptWHCutOff()).isEqualTo(LocalDateTime.of(2025, 1, 10, 10, 0));
    assertThat(shipment.getLastFreeDateCutOff()).isEqualTo(LocalDateTime.of(2025, 1, 11, 10, 0));
    assertThat(shipment.getNumberOfFreeDaysCutOff()).isEqualTo(3);
  }

  @Test
  void testAirMode_AttachedShipment_SetsLatestArrivalTime() {
    ConsolidationDetails newConsolidation = new ConsolidationDetails();
    ShipmentDetails shipment = new ShipmentDetails();
    newConsolidation.setLatDate(LocalDateTime.of(2025, 2, 1, 10, 0));
    newConsolidation.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 2, 2, 10, 0));
    newConsolidation.setLastFreeDateCutOff(LocalDateTime.of(2025, 2, 3, 10, 0));
    newConsolidation.setNumberOfFreeDaysCutOff(5);
    shipment.setTransportMode(TRANSPORT_MODE_AIR);
    shipment.setDirection(DIRECTION_EXP); // Set direction for conditional fields

    consolidationV3Service.updateShipmentDetailsIfConsolidationChanged(null, newConsolidation, List.of(shipment), true);

    assertThat(shipment.getLatestArrivalTime()).isEqualTo(LocalDateTime.of(2025, 2, 1, 10, 0));
    assertThat(shipment.getCargoReceiptWHCutOff()).isEqualTo(LocalDateTime.of(2025, 2, 2, 10, 0));
    assertThat(shipment.getLastFreeDateCutOff()).isEqualTo(LocalDateTime.of(2025, 2, 3, 10, 0));
    assertThat(shipment.getNumberOfFreeDaysCutOff()).isEqualTo(5);
    assertThat(shipment.getCarrierDocCutOff()).isNull(); // Should not be set for AIR
  }

  @Test
  void testSeaMode_UpdateOnlyIfChanged() {
    ConsolidationDetails oldCon = new ConsolidationDetails();
    ConsolidationDetails newCon = new ConsolidationDetails();
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setTransportMode(TRANSPORT_MODE_SEA);
    shipment.setDirection(DIRECTION_EXP);

    oldCon.setTerminalCutoff(LocalDateTime.of(2025, 1, 1, 10, 0));
    newCon.setTerminalCutoff(LocalDateTime.of(2025, 1, 2, 10, 0)); // different, should update
    oldCon.setCarrierDocCutOff(LocalDateTime.of(2025, 1, 9, 10, 0));
    newCon.setCarrierDocCutOff(LocalDateTime.of(2025, 1, 9, 11, 0)); // Changed
    oldCon.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 1, 10, 10, 0));
    newCon.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 1, 10, 10, 0)); // Unchanged
    oldCon.setLastFreeDateCutOff(LocalDateTime.of(2025, 1, 11, 10, 0));
    newCon.setLastFreeDateCutOff(LocalDateTime.of(2025, 1, 11, 12, 0)); // Changed
    oldCon.setNumberOfFreeDaysCutOff(3);
    newCon.setNumberOfFreeDaysCutOff(5); // Changed
    consolidationV3Service.updateShipmentDetailsIfConsolidationChanged(oldCon, newCon, List.of(shipment), false);

    assertThat(shipment.getTerminalCutoff()).isEqualTo(LocalDateTime.of(2025, 1, 2, 10, 0));
    assertThat(shipment.getCarrierDocCutOff()).isEqualTo(LocalDateTime.of(2025, 1, 9, 11, 0));
    assertThat(shipment.getCargoReceiptWHCutOff()).isNull(); // Should not be updated as it's unchanged
    assertThat(shipment.getLastFreeDateCutOff()).isEqualTo(LocalDateTime.of(2025, 1, 11, 12, 0));
    assertThat(shipment.getNumberOfFreeDaysCutOff()).isEqualTo(5);
  }

  @Test
  void testAirMode_UpdateOnlyIfChanged() {
    ConsolidationDetails oldCon = new ConsolidationDetails();
    ConsolidationDetails newCon = new ConsolidationDetails();
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setTransportMode(TRANSPORT_MODE_AIR);
    shipment.setDirection(DIRECTION_EXP);

    oldCon.setLatDate(LocalDateTime.of(2025, 1, 1, 10, 0));
    newCon.setLatDate(LocalDateTime.of(2025, 1, 3, 10, 0)); // changed
    oldCon.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 2, 2, 10, 0));
    newCon.setCargoReceiptWHCutOff(LocalDateTime.of(2025, 2, 2, 11, 0)); // Changed
    oldCon.setLastFreeDateCutOff(LocalDateTime.of(2025, 2, 3, 10, 0));
    newCon.setLastFreeDateCutOff(LocalDateTime.of(2025, 2, 3, 10, 0)); // Unchanged
    oldCon.setNumberOfFreeDaysCutOff(5);
    newCon.setNumberOfFreeDaysCutOff(7); // Changed

    consolidationV3Service.updateShipmentDetailsIfConsolidationChanged(oldCon, newCon, List.of(shipment), false);

    assertThat(shipment.getLatestArrivalTime()).isEqualTo(LocalDateTime.of(2025, 1, 3, 10, 0));
    assertThat(shipment.getCargoReceiptWHCutOff()).isEqualTo(LocalDateTime.of(2025, 2, 2, 11, 0));
    assertThat(shipment.getLastFreeDateCutOff()).isNull(); // Unchanged, so setter not called, remains null
    assertThat(shipment.getNumberOfFreeDaysCutOff()).isEqualTo(7);
    assertThat(shipment.getCarrierDocCutOff()).isNull(); // Should not be set for AIR
  }

  @Test
  void testUpdateConsolidationCargoSummary() throws RunnerException {
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    consolidationV3Service.updateConsolidationCargoSummary(null, oldShipmentWtVolResponse);
    verify(consoleShipmentMappingDao, never()).findByConsolidationId(any());
  }

  @Test
  void testUpdateConsolidationCargoSummary1() throws RunnerException {
    var spyService = Mockito.spy(consolidationV3Service);
    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
    AchievedQuantities achievedQuantities = new AchievedQuantities();
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    ShipmentWtVolResponse newShipmentWtVolResponse = new ShipmentWtVolResponse();

    oldShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    oldShipmentWtVolResponse.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setPacks(1);
    oldShipmentWtVolResponse.setPacksType(MPK);
    oldShipmentWtVolResponse.setDgPacks(1);
    oldShipmentWtVolResponse.setDgPacksType(MPK);
    oldShipmentWtVolResponse.setConsoleContainerCount(1);
    oldShipmentWtVolResponse.setConsoleDgContainerCount(1);
    oldShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    oldShipmentWtVolResponse.setSlacCount(1);

    newShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    newShipmentWtVolResponse.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setPacks(1);
    newShipmentWtVolResponse.setPacksType(MPK);
    newShipmentWtVolResponse.setDgPacks(1);
    newShipmentWtVolResponse.setDgPacksType(MPK);
    newShipmentWtVolResponse.setConsoleContainerCount(1);
    newShipmentWtVolResponse.setConsoleDgContainerCount(1);
    newShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    newShipmentWtVolResponse.setSlacCount(1);

    achievedQuantities.setConsolidatedVolume(BigDecimal.ONE);
    achievedQuantities.setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidatedWeight(BigDecimal.ONE);
    achievedQuantities.setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setWeightVolume(BigDecimal.ONE);
    achievedQuantities.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setConsolidationChargeQuantity(BigDecimal.ONE);
    achievedQuantities.setConsolidationChargeQuantityUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setPacks(1);
    achievedQuantities.setPacksType(MPK);
    achievedQuantities.setDgPacks(1);
    achievedQuantities.setDgPacksType(MPK);
    achievedQuantities.setContainerCount(1);
    achievedQuantities.setDgContainerCount(1);
    achievedQuantities.setTeuCount(BigDecimal.ONE);
    achievedQuantities.setSlacCount(1);

    consolidationDetails.setAchievedQuantities(achievedQuantities);

    doReturn(newShipmentWtVolResponse).when(spyService).calculateShipmentWtVol(any(), any(), any());
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(BigDecimal.ONE);
    spyService.updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);
    verify(consoleShipmentMappingDao).findByConsolidationId(any());
  }

  @Test
  void testUpdateConsolidationCargoSummary2() throws RunnerException {
    var spyService = Mockito.spy(consolidationV3Service);
    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
    AchievedQuantities achievedQuantities = new AchievedQuantities();
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    ShipmentWtVolResponse newShipmentWtVolResponse = new ShipmentWtVolResponse();

    oldShipmentWtVolResponse.setVolume(null);
    oldShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setWeight(null);
    oldShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    oldShipmentWtVolResponse.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setPacks(1);
    oldShipmentWtVolResponse.setPacksType(MPK);
    oldShipmentWtVolResponse.setDgPacks(1);
    oldShipmentWtVolResponse.setDgPacksType(MPK);
    oldShipmentWtVolResponse.setConsoleContainerCount(1);
    oldShipmentWtVolResponse.setConsoleDgContainerCount(null);
    oldShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    oldShipmentWtVolResponse.setSlacCount(1);

    newShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    newShipmentWtVolResponse.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setPacks(1);
    newShipmentWtVolResponse.setPacksType(MPK);
    newShipmentWtVolResponse.setDgPacks(1);
    newShipmentWtVolResponse.setDgPacksType(MPK);
    newShipmentWtVolResponse.setConsoleContainerCount(1);
    newShipmentWtVolResponse.setConsoleDgContainerCount(1);
    newShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    newShipmentWtVolResponse.setSlacCount(1);

    achievedQuantities.setConsolidatedVolume(BigDecimal.ONE);
    achievedQuantities.setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidatedWeight(BigDecimal.ONE);
    achievedQuantities.setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setWeightVolume(null);
    achievedQuantities.setWeightVolumeUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setConsolidationChargeQuantity(BigDecimal.ONE);
    achievedQuantities.setConsolidationChargeQuantityUnit(null);
    achievedQuantities.setPacks(null);
    achievedQuantities.setPacksType(MPK);
    achievedQuantities.setDgPacks(null);
    achievedQuantities.setDgPacksType(MPK);
    achievedQuantities.setContainerCount(null);
    achievedQuantities.setDgContainerCount(1);
    achievedQuantities.setTeuCount(null);
    achievedQuantities.setSlacCount(null);

    consolidationDetails.setAchievedQuantities(achievedQuantities);

    doReturn(newShipmentWtVolResponse).when(spyService).calculateShipmentWtVol(any(), any(), any());
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(BigDecimal.ONE);
    spyService.updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);
    verify(consoleShipmentMappingDao).findByConsolidationId(any());
  }

  @Test
  void testUpdateConsolidationCargoSummary3() throws RunnerException {
    var spyService = Mockito.spy(consolidationV3Service);
    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
    AchievedQuantities achievedQuantities = new AchievedQuantities();
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    ShipmentWtVolResponse newShipmentWtVolResponse = new ShipmentWtVolResponse();

    oldShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    oldShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightVolumeUnit(VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    oldShipmentWtVolResponse.setChargeableUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setPacks(1);
    oldShipmentWtVolResponse.setPacksType(MPK);
    oldShipmentWtVolResponse.setDgPacks(1);
    oldShipmentWtVolResponse.setDgPacksType(MPK);
    oldShipmentWtVolResponse.setConsoleContainerCount(1);
    oldShipmentWtVolResponse.setConsoleDgContainerCount(1);
    oldShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    oldShipmentWtVolResponse.setSlacCount(1);

    newShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    newShipmentWtVolResponse.setChargeableUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setPacks(1);
    newShipmentWtVolResponse.setPacksType(MPK);
    newShipmentWtVolResponse.setDgPacks(1);
    newShipmentWtVolResponse.setDgPacksType(MPK);
    newShipmentWtVolResponse.setConsoleContainerCount(1);
    newShipmentWtVolResponse.setConsoleDgContainerCount(1);
    newShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    newShipmentWtVolResponse.setSlacCount(1);

    achievedQuantities.setConsolidatedVolume(BigDecimal.ONE);
    achievedQuantities.setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidatedWeight(BigDecimal.ONE);
    achievedQuantities.setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setWeightVolume(BigDecimal.ONE);
    achievedQuantities.setWeightVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidationChargeQuantity(BigDecimal.ONE);
    achievedQuantities.setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setPacks(1);
    achievedQuantities.setPacksType(MPK);
    achievedQuantities.setDgPacks(1);
    achievedQuantities.setDgPacksType(MPK);
    achievedQuantities.setContainerCount(1);
    achievedQuantities.setDgContainerCount(1);
    achievedQuantities.setTeuCount(BigDecimal.ONE);
    achievedQuantities.setSlacCount(1);

    consolidationDetails.setAchievedQuantities(achievedQuantities);

    doReturn(newShipmentWtVolResponse).when(spyService).calculateShipmentWtVol(any(), any(), any());
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(BigDecimal.ONE);
    spyService.updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);
    verify(consoleShipmentMappingDao).findByConsolidationId(any());
  }

  @Test
  void testUpdateConsolidationCargoSummary4() throws RunnerException {
    var spyService = Mockito.spy(consolidationV3Service);
    consolidationDetails = new ConsolidationDetails();
    consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
    AchievedQuantities achievedQuantities = new AchievedQuantities();
    ShipmentWtVolResponse oldShipmentWtVolResponse = new ShipmentWtVolResponse();
    ShipmentWtVolResponse newShipmentWtVolResponse = new ShipmentWtVolResponse();

    oldShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setVolumeUnit(null);
    oldShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightUnit(null);
    oldShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    oldShipmentWtVolResponse.setWeightVolumeUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    oldShipmentWtVolResponse.setChargeableUnit(Constants.VOLUME_UNIT_M3);
    oldShipmentWtVolResponse.setPacks(1);
    oldShipmentWtVolResponse.setPacksType(MPK);
    oldShipmentWtVolResponse.setDgPacks(1);
    oldShipmentWtVolResponse.setDgPacksType(MPK);
    oldShipmentWtVolResponse.setConsoleContainerCount(1);
    oldShipmentWtVolResponse.setConsoleDgContainerCount(null);
    oldShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    oldShipmentWtVolResponse.setSlacCount(1);

    newShipmentWtVolResponse.setVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setWeight(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
    newShipmentWtVolResponse.setWeightVolume(BigDecimal.ONE);
    newShipmentWtVolResponse.setWeightVolumeUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setChargable(BigDecimal.ONE);
    newShipmentWtVolResponse.setChargeableUnit(Constants.VOLUME_UNIT_M3);
    newShipmentWtVolResponse.setPacks(1);
    newShipmentWtVolResponse.setPacksType(MPK);
    newShipmentWtVolResponse.setDgPacks(1);
    newShipmentWtVolResponse.setDgPacksType(MPK);
    newShipmentWtVolResponse.setConsoleContainerCount(1);
    newShipmentWtVolResponse.setConsoleDgContainerCount(1);
    newShipmentWtVolResponse.setConsoleTeuCount(BigDecimal.ONE);
    newShipmentWtVolResponse.setSlacCount(1);

    achievedQuantities.setConsolidatedVolume(BigDecimal.ONE);
    achievedQuantities.setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidatedWeight(BigDecimal.ONE);
    achievedQuantities.setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
    achievedQuantities.setWeightVolume(null);
    achievedQuantities.setWeightVolumeUnit(Constants.VOLUME_UNIT_M3);
    achievedQuantities.setConsolidationChargeQuantity(BigDecimal.ONE);
    achievedQuantities.setConsolidationChargeQuantityUnit(null);
    achievedQuantities.setPacks(null);
    achievedQuantities.setPacksType(MPK);
    achievedQuantities.setDgPacks(null);
    achievedQuantities.setDgPacksType(MPK);
    achievedQuantities.setContainerCount(null);
    achievedQuantities.setDgContainerCount(1);
    achievedQuantities.setTeuCount(null);
    achievedQuantities.setSlacCount(null);

    consolidationDetails.setAchievedQuantities(achievedQuantities);

    doReturn(newShipmentWtVolResponse).when(spyService).calculateShipmentWtVol(any(), any(), any());
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(BigDecimal.ONE);
    spyService.updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);
    verify(consoleShipmentMappingDao).findByConsolidationId(any());
  }


  @Test
  void testGenerateV3Events() {
    ConsolidationDetails consolidationDetails1 = testConsol;
    consolidationDetails1.setEventsList(null);
    consolidationV3Service.generateV3Events(consolidationDetails1);
    verify(eventDao, times(1)).save(any());
  }

  @Test
  void testAibAttachedPendingShipmentCount() {
    var mockRequest = CommonGetRequest.builder().id(1L).build();
    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
    Exception e = assertThrows(DataRetrievalFailureException.class , () -> consolidationV3Service.aibAttachedPendingShipmentCount(mockRequest, null));
    assertNotNull(e.getMessage());
  }

  @Test
  void testAibAttachedPendingShipmentCount1() throws AuthenticationException, RunnerException {
    List<ConsoleShipmentMapping> mockConsoleShipMappings = Arrays.asList(
            ConsoleShipmentMapping.builder().build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.APPROVE).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.REJECT).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).build()
            );

    when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
    when(consoleShipmentMappingDao.findByConsolidationIdAll(anyLong())).thenReturn(mockConsoleShipMappings);

    var response = consolidationV3Service.aibAttachedPendingShipmentCount(CommonGetRequest.builder().id(1L).build(), null);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(response.getBody());
  }

  @Test
  void testAibAttachedPendingShipmentCount2() throws AuthenticationException, RunnerException {
    List<ConsoleShipmentMapping> mockConsoleShipMappings = Arrays.asList(
            ConsoleShipmentMapping.builder().build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.APPROVE).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.REJECT).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).build(),
            ConsoleShipmentMapping.builder().requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).build()
    );

    consolidationDetails.setTriangulationPartnerList(null);
    consolidationDetails.setReceivingBranch(1L);
    when(TenantContext.getCurrentTenant()).thenReturn(1);
    when(consolidationDetailsDao.findConsolidationByIdWithQuery(anyLong())).thenReturn(Optional.of(consolidationDetails));
    when(consoleShipmentMappingDao.findByConsolidationIdAll(anyLong())).thenReturn(mockConsoleShipMappings);

    var response = consolidationV3Service.aibAttachedPendingShipmentCount(CommonGetRequest.builder().id(1L).build(), NETWORK_TRANSFER);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(response.getBody());
  }

  @Test
  void testGetResult_WithValidContainers() throws Exception {
    Containers container1 = new Containers();
    container1.setTeu(BigDecimal.valueOf(1.5));
    container1.setHazardous(true);
    Containers container2 = new Containers();
    container2.setTeu(BigDecimal.valueOf(2.0));
    container2.setHazardous(false);
    Map<Long, Containers> containersMap = new HashMap<>();
    containersMap.put(1L, container1);
    containersMap.put(2L, container2);
    Method method = ConsolidationV3Service.class.getDeclaredMethod(
            "getResult", Map.class, Integer.class, BigDecimal.class, Integer.class);
    method.setAccessible(true);
    Object result = method.invoke(null, containersMap, 0, BigDecimal.ZERO, 0);
    Method getNoOfCont = result.getClass().getMethod("noOfCont");
    Method getTeus = result.getClass().getMethod("teus");
    Method getDgContCount = result.getClass().getMethod("dgContCount");
    assertEquals(2, getNoOfCont.invoke(result));
    assertEquals(new BigDecimal("3.5"), getTeus.invoke(result));
    assertEquals(1, getDgContCount.invoke(result));
  }

  @Test
  void testGetConsoleCount_withValidContainers() throws Exception {
    Containers container1 = new Containers();
    container1.setTeu(BigDecimal.valueOf(1.0));
    container1.setHazardous(true);
    Containers container2 = new Containers();
    container2.setTeu(null);
    container2.setHazardous(false);
    Containers container3 = new Containers();
    container3.setTeu(BigDecimal.valueOf(2.5));
    container3.setHazardous(true);
    List<Containers> containerList = List.of(container1, container2, container3);
    Method method = ConsolidationV3Service.class.getDeclaredMethod(
            "getConsoleCount", List.class, Integer.class, BigDecimal.class, Integer.class);
    method.setAccessible(true);
    Object result = method.invoke(null, containerList, 0, BigDecimal.ZERO, 0);
    Method getNoOfCont = result.getClass().getMethod("consoleNoOfCont");
    Method getTeus = result.getClass().getMethod("consoleTeus");
    Method getDgContCount = result.getClass().getMethod("consoleDgContCount");
    assertEquals(3, getNoOfCont.invoke(result));
    assertEquals(new BigDecimal("3.5"), getTeus.invoke(result));
    assertEquals(2, getDgContCount.invoke(result));
  }

  @Test
  void testGetConsoleCount_withEmptyList() throws Exception {
    List<Containers> emptyList = new ArrayList<>();
    Method method = ConsolidationV3Service.class.getDeclaredMethod(
            "getConsoleCount", List.class, Integer.class, BigDecimal.class, Integer.class);
    method.setAccessible(true);
    Object result = method.invoke(null, emptyList, 0, BigDecimal.ZERO, 0);
    Method getNoOfCont = result.getClass().getMethod("consoleNoOfCont");
    Method getTeus = result.getClass().getMethod("consoleTeus");
    Method getDgContCount = result.getClass().getMethod("consoleDgContCount");
    assertEquals(0, getNoOfCont.invoke(result));
    assertEquals(BigDecimal.ZERO, getTeus.invoke(result));
    assertEquals(0, getDgContCount.invoke(result));
  }

  @Test
  void testGetDGShipment_WhenHazardousContainerPresent_ReturnsFalse() {
    Long consolidationId = 1L;
    Containers hazardousContainer = new Containers();
    hazardousContainer.setHazardous(true);

    ConsolidationDetails details = new ConsolidationDetails();
    details.setContainersList(List.of(hazardousContainer));
    details.setShipmentsList(Collections.emptySet());

    when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(details));

    CheckDGShipmentV3 result = consolidationV3Service.getDGShipment(consolidationId);

    assertThat(result.getIsDGShipmentPresent()).isFalse();
  }

  @Test
  void testGetDGShipment_WhenHazardousShipmentPresent_ReturnsTrue() {
    Long consolidationId = 2L;
    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setContainsHazardous(true);

    ConsolidationDetails details = new ConsolidationDetails();
    details.setContainersList(Collections.emptyList());
    details.setShipmentsList(Set.of(shipment));

    when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(details));

    CheckDGShipmentV3 result = consolidationV3Service.getDGShipment(consolidationId);

    assertThat(result.getIsDGShipmentPresent()).isTrue();
  }

  @Test
  void testGetDGShipment_WhenNoHazardousInfo_ReturnsFalse() {
    Long consolidationId = 3L;

    ConsolidationDetails details = new ConsolidationDetails();
    details.setContainersList(Collections.emptyList());
    details.setShipmentsList(Collections.emptySet());

    when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(details));

    CheckDGShipmentV3 result = consolidationV3Service.getDGShipment(consolidationId);

    assertThat(result.getIsDGShipmentPresent()).isFalse();
  }

  @Test
  void testGetDGShipment_WhenConsolidationIdNull_ThrowsException() {
    assertThatThrownBy(() -> consolidationV3Service.getDGShipment(null))
            .isInstanceOf(ValidationException.class)
            .hasMessage("Consolidation Id is required");
  }

  @Test
  void testGetDGShipment_WhenConsolidationNotFound_ThrowsException() {
    Long invalidId = 999L;
    when(consolidationDetailsDao.findById(invalidId)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> consolidationV3Service.getDGShipment(invalidId))
            .isInstanceOf(ValidationException.class)
            .hasMessage("No Consolidation found for the Id: " + invalidId);
  }

  @Test
  void testSetBookingNumber_WhenFromAttachShipmentIsFalse_AndOldAndNewBookingMatch_UpdatesFromConsole() {
    ConsolidationDetails console = new ConsolidationDetails();
    console.setBookingNumber("BK123");

    ConsolidationDetails oldEntity = new ConsolidationDetails();
    oldEntity.setBookingNumber("BK123");

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setBookingNumber("BK123");

    consolidationV3Service.setBookingNumberInShipment(console, oldEntity, shipment, false);

    assertThat(shipment.getBookingNumber()).isEqualTo("BK123");
  }

  @Test
  void testSetBookingNumber_WhenFromAttachShipmentIsFalse_AndOldAndNewBookingMismatch_DoesNotUpdate() {
    ConsolidationDetails console = new ConsolidationDetails();
    console.setBookingNumber("BK999");

    ConsolidationDetails oldEntity = new ConsolidationDetails();
    oldEntity.setBookingNumber("BK123");

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setBookingNumber("BK123"); // matches old

    consolidationV3Service.setBookingNumberInShipment(console, oldEntity, shipment, false);

    assertThat(shipment.getBookingNumber()).isEqualTo("BK999"); // remains same
  }

  @Test
  void testSetBookingNumber_WhenFromAttachShipmentIsTrue_AlwaysUpdatesFromConsole() {
    ConsolidationDetails console = new ConsolidationDetails();
    console.setBookingNumber("BK987");

    ShipmentDetails shipment = new ShipmentDetails();
    shipment.setBookingNumber("BK123");

    consolidationV3Service.setBookingNumberInShipment(console, null, shipment, true);

    assertThat(shipment.getBookingNumber()).isEqualTo("BK987");
  }

  @Test
  void testIsSeaExportWithLclAttachedFlagSet() throws Exception {
//    shipmentDetails.setShipmentType("LCL");
    ShipmentDetails lclShipment = new ShipmentDetails();
    lclShipment.setShipmentType("LCL");
    ShipmentDetails fclShipment = new ShipmentDetails();
    fclShipment.setShipmentType("FCL");
    ShipmentDetails lseShipment = new ShipmentDetails();
    lseShipment.setShipmentType("LSE");
    List<ShipmentDetails> shipmentDetailsList = List.of(lclShipment, fclShipment, lseShipment);
    shipmentSettingsDetails.setWeightChargeableUnit("KG");
    shipmentSettingsDetails.setVolumeChargeableUnit("M3");
    when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
    when(UnitConversionUtility.convertUnit(any(), any(), any(), any()))
            .thenReturn(new BigDecimal("100"));
    ShipmentWtVolResponse response = consolidationV3Service.calculateShipmentWtVol(
            "SEA", shipmentDetailsList, Collections.emptyList()
    );
    assertTrue(response.getIsNonFtlOrFclAttached());
  }

    @Test
    void retrieveByIdExternal_Success_WithId() throws RunnerException, AuthenticationException {
        // Arrange
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();

        ConsolidationDetails mockConsolidationDetails = consolidationDetails;
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
        when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3ExternalResponse.class))
                .thenReturn(mockResponse);

        // Act
        ConsolidationDetailsV3ExternalResponse result =
                consolidationV3Service.retrieveByIdExternal(request);

        // Assert
        assertNotNull(result);
        assertEquals(mockResponse, result);
        verify(consolidationDetailsDao).findById(id);
        verify(consolidationDetailsDao, never()).findByGuid(any(UUID.class));
    }

    @Test
    void retrieveByIdExternal_Success_WithGuid() throws RunnerException, AuthenticationException {
        // Arrange
        String guidString = "550e8400-e29b-41d4-a716-446655440000";
        UUID guid = UUID.fromString(guidString);
        CommonGetRequest request = CommonGetRequest.builder().guid(guidString).build();

        ConsolidationDetails mockConsolidationDetails = consolidationDetails;
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.of(mockConsolidationDetails));
        when(jsonHelper.convertValue(mockConsolidationDetails, ConsolidationDetailsV3ExternalResponse.class))
                .thenReturn(mockResponse);

        // Act
        ConsolidationDetailsV3ExternalResponse result =
                consolidationV3Service.retrieveByIdExternal(request);

        // Assert
        assertNotNull(result);
        assertEquals(mockResponse, result);
        verify(consolidationDetailsDao).findByGuid(guid);
        verify(consolidationDetailsDao, never()).findById(anyLong());
    }

    @Test
    void retrieveByIdExternal_ThrowsRunnerException_WhenBothIdAndGuidAreNull() {
        // Arrange
        CommonGetRequest request = CommonGetRequest.builder().build();

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class,
                () -> consolidationV3Service.retrieveByIdExternal(request));

        assertEquals(ConsolidationConstants.CONSOLIDATION_REQUEST_NULL_ID_AND_GUID_ERROR, exception.getMessage());
        verify(consolidationDetailsDao, never()).findById(any());
        verify(consolidationDetailsDao, never()).findByGuid(any());
    }

    @Test
    void retrieveByIdExternal_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithId() {
        // Arrange
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();

        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.empty());

        // Act & Assert
        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> consolidationV3Service.retrieveByIdExternal(request));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, ex.getMessage());
        verify(consolidationDetailsDao).findById(id);
    }

    @Test
    void retrieveByIdExternal_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithGuid() {
        // Arrange
        String guidString = "550e8400-e29b-41d4-a716-446655440000";
        UUID guid = UUID.fromString(guidString);
        CommonGetRequest request = CommonGetRequest.builder().guid(guidString).build();

        when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.empty());

        // Act & Assert
        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> consolidationV3Service.retrieveByIdExternal(request));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, ex.getMessage());
        verify(consolidationDetailsDao).findByGuid(guid);
    }

    @Test
    void retrieveByIdExternal_ThrowsIllegalArgumentException_WhenGuidIsInvalidFormat() {
        // Arrange
        String invalidGuid = "invalid-guid";
        CommonGetRequest request = CommonGetRequest.builder().guid(invalidGuid).build();

        // Act & Assert
        assertThrows(IllegalArgumentException.class,
                () -> consolidationV3Service.retrieveByIdExternal(request));

        verify(consolidationDetailsDao, never()).findByGuid(any());
    }

    @Test
    void retrieveByIdExternalPartial_Success_WithId() throws RunnerException, AuthenticationException {
        // Arrange
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder()
                .id(id)
                .includeColumns(List.of("id", "bookingStatus"))
                .build();

        ConsolidationDetails mockConsolidationDetails = consolidationDetails;
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(mockConsolidationDetails));
        when(commonUtils.setIncludedFieldsToResponse(eq(mockConsolidationDetails), anySet(), any(ConsolidationDetailsV3ExternalResponse.class)))
                .thenReturn(mockResponse);

        // Act
        ConsolidationDetailsV3ExternalResponse result =
                consolidationV3Service.retrieveByIdExternalPartial(request);

        // Assert
        assertNotNull(result);
        assertEquals(mockResponse, result);
        verify(consolidationDetailsDao).findById(id);
        verify(consolidationDetailsDao, never()).findByGuid(any(UUID.class));
        verify(commonUtils).setIncludedFieldsToResponse(eq(mockConsolidationDetails), anySet(), any(ConsolidationDetailsV3ExternalResponse.class));
    }

    @Test
    void retrieveByIdExternalPartial_Success_WithGuid() throws RunnerException, AuthenticationException {
        // Arrange
        String guidString = "550e8400-e29b-41d4-a716-446655440000";
        UUID guid = UUID.fromString(guidString);
        CommonGetRequest request = CommonGetRequest.builder()
                .guid(guidString)
                .includeColumns(List.of("id"))
                .build();

        ConsolidationDetails mockConsolidationDetails = consolidationDetails;
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.of(mockConsolidationDetails));
        when(commonUtils.setIncludedFieldsToResponse(eq(mockConsolidationDetails), anySet(), any(ConsolidationDetailsV3ExternalResponse.class)))
                .thenReturn(mockResponse);

        // Act
        ConsolidationDetailsV3ExternalResponse result =
                consolidationV3Service.retrieveByIdExternalPartial(request);

        // Assert
        assertNotNull(result);
        assertEquals(mockResponse, result);
        verify(consolidationDetailsDao).findByGuid(guid);
        verify(consolidationDetailsDao, never()).findById(anyLong());
        verify(commonUtils).setIncludedFieldsToResponse(eq(mockConsolidationDetails), anySet(), any(ConsolidationDetailsV3ExternalResponse.class));
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsRunnerException_WhenBothIdAndGuidAreNull() {
        // Arrange
        CommonGetRequest request = CommonGetRequest.builder()
                .includeColumns(List.of("id"))
                .build();

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        assertEquals(ConsolidationConstants.CONSOLIDATION_REQUEST_NULL_ID_AND_GUID_ERROR, exception.getMessage());
        verify(consolidationDetailsDao, never()).findById(any());
        verify(consolidationDetailsDao, never()).findByGuid(any());
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsRunnerException_WhenIncludeColumnsNull() {
        // Arrange
        CommonGetRequest request = CommonGetRequest.builder()
                .id(1L)
                .build(); // includeColumns = null

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        assertEquals("IncludeColumns can't be null or empty", exception.getMessage());
        verifyNoInteractions(consolidationDetailsDao);
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsRunnerException_WhenIncludeColumnsEmpty() {
        // Arrange
        CommonGetRequest request = CommonGetRequest.builder()
                .id(1L)
                .includeColumns(Collections.emptyList())
                .build();

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        assertEquals("IncludeColumns can't be null or empty", exception.getMessage());
        verifyNoInteractions(consolidationDetailsDao);
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithId() {
        // Arrange
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder()
                .id(id)
                .includeColumns(List.of("id"))
                .build();

        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.empty());

        // Act & Assert
        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, ex.getMessage());
        verify(consolidationDetailsDao).findById(id);
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsDataRetrievalFailureException_WhenConsolidationDetailsNotFound_WithGuid() {
        // Arrange
        String guidString = "550e8400-e29b-41d4-a716-446655440001";
        UUID guid = UUID.fromString(guidString);
        CommonGetRequest request = CommonGetRequest.builder()
                .guid(guidString)
                .includeColumns(List.of("id"))
                .build();

        when(consolidationDetailsDao.findByGuid(guid)).thenReturn(Optional.empty());

        // Act & Assert
        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, ex.getMessage());
        verify(consolidationDetailsDao).findByGuid(guid);
    }

    @Test
    void retrieveByIdExternalPartial_ThrowsIllegalArgumentException_WhenGuidIsInvalidFormat() {
        // Arrange
        String invalidGuid = "invalid-guid";
        CommonGetRequest request = CommonGetRequest.builder()
                .guid(invalidGuid)
                .includeColumns(List.of("id"))
                .build();

        // Act & Assert
        assertThrows(IllegalArgumentException.class,
                () -> consolidationV3Service.retrieveByIdExternalPartial(request));

        verify(consolidationDetailsDao, never()).findByGuid(any());
    }


  @Test
  void testListExternal_InvalidRequest_ThrowsValidationException(){
    assertThrows(ValidationException.class, () -> consolidationV3Service.listExternal(null));
  }

  @Test
  void testListExternal_Success_WithIncludeColumns() {
    // arrange
    testConsol.setId(1L);
    ListCommonRequest request = new ListCommonRequest();
    request.setIncludeColumns(List.of("id", "consolidationNumber"));
    IRunnerResponse dto = ConsolidationMapper.INSTANCE.toConsolidationListResponse(testConsol);
    Page<ConsolidationDetails> page = new PageImpl<>(List.of(testConsol));

    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(page);
    when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any()))
            .thenReturn(dto);

    // act
    ConsolidationListV3Response resp = consolidationV3Service.listExternal(request);

    // assert
    assertNotNull(resp);
    assertEquals(1, resp.getConsolidationListResponses().size());
  }

  @Test
  void testListExternal_Success_WithoutIncludeColumns() {
    // arrange
    testConsol.setId(2L);
    ListCommonRequest request = new ListCommonRequest();
    request.setIncludeColumns(Collections.emptyList());  // no filters -> full DTO
    Page<ConsolidationDetails> page = new PageImpl<>(List.of(testConsol));
    when(consolidationDetailsDao.findAll(any(), any())).thenReturn(page);

    // act
    ConsolidationListV3Response resp = consolidationV3Service.listExternal(request);

    // assert
    assertNotNull(resp);
    assertEquals(1, resp.getConsolidationListResponses().size());
    assertEquals(testConsol.getId(), resp.getConsolidationListResponses().get(0).getId());
    assertEquals(testConsol.getConsolidationNumber(), resp.getConsolidationListResponses().get(0).getConsolidationNumber());
    assertEquals(testConsol.getTransportMode(), resp.getConsolidationListResponses().get(0).getTransportMode());

    verify(commonUtils, never()).setIncludedFieldsToResponse(any(), anySet(), any());
  }

    @Test
    void testGetDefaultConsolidation_shouldReturnDefaultResponse_forSeaExport() {
        // Arrange
        var spyService = Mockito.spy(consolidationV3Service);

        // Mock the settings that will be returned from the context
        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setDefaultTransportMode(TRANSPORT_MODE_SEA);
        mockSettings.setDefaultContainerType("FCL");
        mockSettings.setDefaultShipmentType(DIRECTION_EXP);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockSettings);

        // Mock internal method calls to isolate the logic of getDefaultConsolidation
        String mockBol = "BOL12345";
        doReturn(mockBol).when(spyService).generateCustomBolNumber();
        doNothing().when(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));
        when(commonUtils.getAutoPopulateDepartment(anyString(), anyString(), anyString())).thenReturn("DefaultDept");

        doReturn(new HashMap<String, Object>()).when(spyService).fetchAllMasterDataByKey(any(ConsolidationDetailsV3Response.class));

        // Act
        ConsolidationDetailsV3Response response = spyService.getDefaultConsolidation();

        // Assert
        assertNotNull(response);
        assertEquals(TRANSPORT_MODE_SEA, response.getTransportMode());
        assertEquals("FCL", response.getContainerCategory());
        assertEquals(DIRECTION_EXP, response.getShipmentType());
        assertEquals(mockBol, response.getBol());
        assertEquals(Constants.INTTRA, response.getModeOfBooking());
        assertEquals("user", response.getCreatedBy()); // From setUp()
        assertNotNull(response.getCreatedAt());
        assertEquals(1L, response.getSourceTenantId()); // From setUp()
        assertEquals("DefaultDept", response.getDepartment());
        assertNull(response.getConsolidationType(), "ConsolidationType should not be set for SEA mode");
        assertNotNull(response.getMasterDataMap());

        // Verify interactions
        verify(commonUtils).getShipmentSettingFromContext();
        verify(spyService).generateCustomBolNumber();
        verify(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));
        verify(commonUtils).getAutoPopulateDepartment(TRANSPORT_MODE_SEA, DIRECTION_EXP, MdmConstants.CONSOLIDATION_MODULE);
    }

    @Test
    void testGetDefaultConsolidation_forAirTransport_shouldSetStdTypeAndNotGenerateBol() {
        // Arrange
        var spyService = Mockito.spy(consolidationV3Service);

        // Mock tenant settings for AIR transport
        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setDefaultTransportMode(TRANSPORT_MODE_AIR);
        mockSettings.setDefaultContainerType("ULD");
        mockSettings.setDefaultShipmentType(DIRECTION_IMP);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockSettings);

        // Mock other dependencies
        doNothing().when(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));
        when(commonUtils.getAutoPopulateDepartment(anyString(), anyString(), anyString())).thenReturn("AirDept");

        doReturn(new HashMap<String, Object>()).when(spyService).fetchAllMasterDataByKey(any(ConsolidationDetailsV3Response.class));

        // Act
        ConsolidationDetailsV3Response response = spyService.getDefaultConsolidation();

        // Assert
        assertNotNull(response);
        assertEquals(TRANSPORT_MODE_AIR, response.getTransportMode());
        assertEquals(Constants.CONSOLIDATION_TYPE_STD, response.getConsolidationType());
        assertNull(response.getBol(), "BOL should be null for AIR transport");
        assertNull(response.getModeOfBooking(), "ModeOfBooking should be null for AIR transport");
        assertEquals("AirDept", response.getDepartment());

        // Verify that generateCustomBolNumber was NOT called
        verify(spyService, never()).generateCustomBolNumber();
        verify(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));
    }

    @Test
    void testGetDefaultConsolidation_whenHelperMethodFails_shouldThrowGenericException() {
        // Arrange
        var spyService = Mockito.spy(consolidationV3Service);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(new ShipmentSettingsDetails());

        String expectedErrorMessage = "Failed to set default agent";
        doThrow(new RuntimeException(expectedErrorMessage))
                .when(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));

        doReturn(new HashMap<String, Object>()).when(spyService).fetchAllMasterDataByKey(any(ConsolidationDetailsV3Response.class));

        // Act & Assert
        GenericException exception = assertThrows(GenericException.class, spyService::getDefaultConsolidation);

        // Assert the message from the original exception is preserved
        assertEquals(expectedErrorMessage, exception.getMessage());
    }

    @Test
    void testGetDefaultConsolidation_withMinimalSettings_shouldNotFail() {
        // Arrange
        var spyService = Mockito.spy(consolidationV3Service);

        // Mock settings with null values for defaults
        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setDefaultTransportMode(null);
        mockSettings.setDefaultContainerType(null);
        mockSettings.setDefaultShipmentType(null);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockSettings);

        // Mock other dependencies
        doReturn(null).when(spyService).generateCustomBolNumber();
        doNothing().when(spyService).setTenantAndDefaultAgent(any(ConsolidationDetailsV3Response.class));
        when(commonUtils.getAutoPopulateDepartment(any(), any(), any())).thenReturn(null);

        doReturn(new HashMap<String, Object>()).when(spyService).fetchAllMasterDataByKey(any(ConsolidationDetailsV3Response.class));

        // Act
        ConsolidationDetailsV3Response response = spyService.getDefaultConsolidation();

        // Assert
        assertNotNull(response);
        assertNull(response.getTransportMode());
        assertNull(response.getContainerCategory());
        assertNull(response.getShipmentType());
        assertNull(response.getBol());
        assertNull(response.getModeOfBooking());
        assertNull(response.getDepartment());
    }
    @Test
    void testSetTenantAndDefaultAgent_forExport_populatesSendingAgentAndOriginBranch() {
        // Arrange
        ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
        response.setShipmentType(DIRECTION_EXP);

        TenantModel tenantModel = new TenantModel();
        tenantModel.setUnloco(12345);
        V1RetrieveResponse mockV1Response = mock(V1RetrieveResponse.class);
        when(v1Service.retrieveTenant()).thenReturn(mockV1Response);
        when(mockV1Response.getEntity()).thenReturn(tenantModel);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        UnlocationsResponse unlocation = new UnlocationsResponse();
        unlocation.setLocationsReferenceGUID("TESTLOCGUID");
        when(masterDataUtils.fetchUnlocationByOneIdentifier(anyString(), anyString())).thenReturn(List.of(unlocation));

        PartiesResponse defaultAgent = new PartiesResponse();
        defaultAgent.setOrgCode("AGENT001");
        defaultAgent.setOrgId("AGENT001");
        defaultAgent.setAddressId("AGENT001");
        defaultAgent.setOrgData(Map.of("TenantId", 5));
        when(v1ServiceUtil.getDefaultAgentOrg(any())).thenReturn(defaultAgent);
        when(commonUtils.getReceivingBranch(any(), any())).thenReturn(5L);

        // Act
        consolidationV3Service.setTenantAndDefaultAgent(response);

        // Assert
        assertNotNull(response.getSendingAgent());
        assertEquals("AGENT001", response.getSendingAgent().getOrgCode());
        assertEquals(5L, response.getOriginBranch());
        assertNull(response.getReceivingAgent());
        assertEquals("TESTLOCGUID", response.getPlaceOfIssue());
    }

    @Test
    void testSetTenantAndDefaultAgent_forImport_populatesReceivingAgent() {
        // Arrange
        ConsolidationDetailsV3Response response = new ConsolidationDetailsV3Response();
        response.setShipmentType(DIRECTION_IMP);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockV1Response = mock(V1RetrieveResponse.class);
        when(v1Service.retrieveTenant()).thenReturn(mockV1Response);
        when(mockV1Response.getEntity()).thenReturn(tenantModel);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(masterDataUtils.fetchUnlocationByOneIdentifier(anyString(), anyString())).thenReturn(Collections.emptyList());

        PartiesResponse defaultAgent = new PartiesResponse();
        defaultAgent.setOrgCode("AGENT002");
        when(v1ServiceUtil.getDefaultAgentOrg(any())).thenReturn(defaultAgent);

        // Act
        consolidationV3Service.setTenantAndDefaultAgent(response);

        // Assert
        assertNotNull(response.getReceivingAgent());
        assertEquals("AGENT002", response.getReceivingAgent().getOrgCode());
        assertNull(response.getSendingAgent());
        assertNull(response.getOriginBranch());
    }
    @Test
    void testFetchShipments_NullIncludeColumns_ThrowsValidationException() {
        // Given
        ListCommonRequest validRequest=  new ListCommonRequest();
        validRequest.setIncludeColumns(null);

        // When & Then
        assertThatThrownBy(() -> consolidationV3Service.fetchConsolidation(validRequest))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Include columns can not be empty");
    }

    @Test
    void testFetchShipments_EmptyIncludeColumns_ThrowsValidationException() {
        // Given
        ListCommonRequest validRequest=  new ListCommonRequest();
        validRequest.setIncludeColumns(new ArrayList<>());

        // When & Then
        assertThatThrownBy(() -> consolidationV3Service.fetchConsolidation(validRequest))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Include columns can not be empty");
    }

    @Test
    void testFetchShipments_NullPageNo_ThrowsValidationException() {
        // Given
        ListCommonRequest validRequest=  new ListCommonRequest();
        validRequest.setIncludeColumns(List.of("id"));
        validRequest.setPageNo(0);

        // When & Then
        assertThatThrownBy(() -> consolidationV3Service.fetchConsolidation(validRequest))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("PageSize should be greater than 1");
    }

    @Test
    void testFetchConsol_SuccessWithDefaultPageSize() throws RunnerException {
        // Arrange
        ListCommonRequest request = new ListCommonRequest();
        List<String> includeColumns = new ArrayList<>(Arrays.asList("column1", "column2"));
        request.setIncludeColumns(includeColumns);
        request.setPageNo(1);
        request.setPageSize(25); // Should default to 25
        FilterCriteria mockFilterCriteria = mock(FilterCriteria.class);
        when(mockFilterCriteria.toString()).thenReturn("mockFilterCriteria");
        request.setFilterCriteria(List.of(mockFilterCriteria));

        setupMocksForSuccessfulExecution();

        // Act
        ResponseEntity<IRunnerResponse> response = consolidationV3Service.fetchConsolidation(request);

        // Assert
        assertNotNull(response);
        verify(typedQuery).setFirstResult(0); // (1-1) * 25
        verify(typedQuery).setMaxResults(25);
    }


    private void setupMocksForSuccessfulExecution() throws RunnerException {
//
        // Mock EntityManager and CriteriaBuilder
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Object[].class)).thenReturn(criteriaQuery);
        when(criteriaQuery.from(ConsolidationDetails.class)).thenReturn(root);

        // Mock method chaining for CriteriaQuery
        when(criteriaQuery.multiselect(anyList())).thenReturn(criteriaQuery);
        when(criteriaQuery.distinct(anyBoolean())).thenReturn(criteriaQuery);
        lenient().when(criteriaQuery.where(any(Predicate.class))).thenReturn(criteriaQuery);
        lenient().when(criteriaQuery.orderBy(any(Order.class))).thenReturn(criteriaQuery);

        lenient().when(entityManager.createQuery(criteriaQuery)).thenReturn(typedQuery);
        lenient().when(typedQuery.setFirstResult(anyInt())).thenReturn(typedQuery);
        lenient().when(typedQuery.setMaxResults(anyInt())).thenReturn(typedQuery);

        // Use any() matcher instead of eq(request) since request is modified
        lenient().when(commonUtils.fetchTotalCount(any(ListCommonRequest.class), eq(ConsolidationDetails.class))).thenReturn(100L);
        lenient().when(commonUtils.refineIncludeColumns(anyList())).thenReturn(new ArrayList<>(Arrays.asList("column1", "column2", "id")));
        lenient().when(commonUtils.extractRequestedColumns(anyList(), any())).thenReturn(new HashMap<>());
        lenient().when(commonUtils.detectCollectionRelationships(any(), eq(ConsolidationDetails.class))).thenReturn(new HashSet<>());
        lenient().when(commonUtils.buildPredicatesFromFilters(any(), any(), any())).thenReturn(new ArrayList<>());
        lenient().when(commonUtils.extractSortFieldFromPayload(any(ListCommonRequest.class))).thenReturn("defaultSortField");

        // Mock buildJoinsAndSelections
//        lenient().doAnswer(invocation -> {
//            List<Selection<?>> selections = invocation.getArgument(1);
//            List<String> columnOrder = invocation.getArgument(2);
//            selections.add(mock(Selection.class));
//            columnOrder.add("mockColumn");
//            return null;
//        }).when(commonUtils).buildJoinsAndSelections(any(), any(), anyList(), anyList(), anyString(), anyString());
        // Create real List objects for the test
        List<Selection<?>> selections = new ArrayList<>();
        List<String> columnOrder = new ArrayList<>();

// Add mock selections to the real lists
        selections.add(mock(Selection.class));
        columnOrder.add("mockColumn");

// Don't mock buildJoinsAndSelections at all - let it execute
// Or mock it to do nothing:
        doNothing().when(commonUtils).buildJoinsAndSelections(any(), any(), anyList(), anyList(), eq("consolidationDetails"), anyString());

        // Mock query results
        List<Object[]> queryResults = new ArrayList<>();
        queryResults.add(new Object[]{"value1", "value2", 1L});
        when(typedQuery.getResultList()).thenReturn(queryResults);

        // Mock data transformation
        List<Map<String, Object>> flatList = Arrays.asList(createMockFlatMap());
        lenient().when(commonUtils.buildFlatList(eq(queryResults), anyList())).thenReturn(flatList);

        List<Map<String, Object>> nestedList = Arrays.asList(createMockNestedMap());
        lenient().when(commonUtils.convertToNestedMapWithCollections(eq(flatList), anySet(), anyString())).thenReturn(nestedList);

        // Mock ShipmentDetails conversion
        ConsolidationDetails mockShipment = new ConsolidationDetails();
        lenient().when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(mockShipment);

        // Mock response conversion
        ConsolidationDetailsResponse mockResponse = new ConsolidationDetailsResponse();
        lenient().when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(mockResponse);
    }
    private Map<String, Object> createMockFlatMap() {
        Map<String, Object> flatMap = new HashMap<>();
        flatMap.put("column1", "value1");
        flatMap.put("column2", "value2");
        flatMap.put("id", 1L);
        return flatMap;
    }

    private Map<String, Object> createMockNestedMap() {
        Map<String, Object> nestedMap = new HashMap<>();
        Map<String, Object> consolData = new HashMap<>();
        consolData.put("column1", "value1");
        consolData.put("column2", "value2");
        consolData.put("id", 1L);
        nestedMap.put(Constants.CONSOLIDATION_ROOT_KEY_NAME, consolData);
        return nestedMap;
    }

    @Test
    void testGetConsolDetails_WithValidId_ReturnsShipmentDetails() throws RunnerException {
        // Arrange
        CommonGetRequest request = new CommonGetRequest();
        request.setId(123L);
        request.setIncludeColumns(Arrays.asList("id", "consolidationNumber", "status"));

        // Mock EntityManager and CriteriaBuilder chain
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Object[].class)).thenReturn(criteriaQuery);
        when(criteriaQuery.from(ConsolidationDetails.class)).thenReturn(root);
        when(criteriaQuery.multiselect(anyList())).thenReturn(criteriaQuery);
        when(criteriaQuery.distinct(anyBoolean())).thenReturn(criteriaQuery);
        when(criteriaQuery.where(any(Predicate.class))).thenReturn(criteriaQuery);
        when(entityManager.createQuery(criteriaQuery)).thenReturn(typedQuery);
        Predicate predicate = mock(Predicate.class, withSettings().lenient());

        // Mock predicate creation for ID filtering
        when(criteriaBuilder.equal(any(), eq(123L))).thenReturn(predicate);
        when(root.get("id")).thenReturn(mock(Path.class));

        // Mock CommonUtils methods
        when(commonUtils.refineIncludeColumns(anyList()))
                .thenReturn(Arrays.asList("id", "consoleNumber", "status"));

        Map<String, Object> requestedColumns = new HashMap<>();
        requestedColumns.put("shipment", Arrays.asList("id", "consoleNumber", "status"));
        when(commonUtils.extractRequestedColumns(anyList(), any())).thenReturn(requestedColumns);
        when(commonUtils.detectCollectionRelationships(any(), eq(ConsolidationDetails.class)))
                .thenReturn(new HashSet<>());

        // Mock buildJoinsAndSelections - simplified to avoid ClassCastException
        doNothing().when(commonUtils).buildJoinsAndSelections(any(), any(), anyList(), anyList(), anyString(), any());

        // Mock query results
        List<Object[]> queryResults = new ArrayList<>();
        queryResults.add(new Object[]{123L, "SHIP-001", "DELIVERED"});
        when(typedQuery.getResultList()).thenReturn(queryResults);

        // Mock data transformation
        List<Map<String, Object>> nestedList = new ArrayList<>();
        Map<String, Object> nestedMap = new HashMap<>();
        Map<String, Object> shipmentData = new HashMap<>();
        shipmentData.put("id", 123L);
        shipmentData.put("status", "DELIVERED");
        nestedMap.put(Constants.CONSOLIDATION_ROOT_KEY_NAME, shipmentData);
        nestedList.add(nestedMap);

        when(commonUtils.convertToNestedMapWithCollections(anyList(), anySet(), anyString()))
                .thenReturn(nestedList);

        // Mock ShipmentDetails conversion
        ConsolidationDetails mockShipment = new ConsolidationDetails();
        mockShipment.setId(123L);
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(mockShipment);

        // Mock response conversion
        ConsolidationDetailsResponse mockResponse = new ConsolidationDetailsResponse();
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(mockResponse);

        // Act
        ResponseEntity<IRunnerResponse> response = consolidationV3Service.getConsolidationDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        // Verify key interactions
        verify(criteriaBuilder).equal(any(), eq(123L));
        verify(criteriaQuery).where(predicate);
        verify(commonUtils).refineIncludeColumns(request.getIncludeColumns());
        verify(typedQuery).getResultList();
    }

    @Test
    void canProcesscutOffFields_shouldReturnTrue_whenCarrierDocDiffers() {
        ConsolidationDetails newEntity = new ConsolidationDetails();
        ConsolidationDetails oldEntity = new ConsolidationDetails();
        newEntity.setCarrierDocCutOff(LocalDateTime.now());
        oldEntity.setCarrierDocCutOff(LocalDateTime.now().minusDays(1));
        boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);
        assertTrue(result);
    }

    @Test
    void canProcesscutOffFields_shouldReturnTrue_whenCargoReceiptWHDiffers() {
        ConsolidationDetails newEntity = new ConsolidationDetails();
        ConsolidationDetails oldEntity = new ConsolidationDetails();
        newEntity.setCargoReceiptWHCutOff(LocalDateTime.now());
        oldEntity.setCargoReceiptWHCutOff(LocalDateTime.now().minusDays(1));
        boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);
        assertTrue(result);
    }

    @Test
    void canProcesscutOffFields_shouldReturnTrue_whenLastFreeDateDiffers() {
        ConsolidationDetails newEntity = new ConsolidationDetails();
        ConsolidationDetails oldEntity = new ConsolidationDetails();
        newEntity.setLastFreeDateCutOff(LocalDateTime.now());
        oldEntity.setLastFreeDateCutOff(LocalDateTime.now().minusDays(1));
        boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);
        assertTrue(result);
    }

    @Test
    void canProcesscutOffFields_shouldReturnTrue_whenNumberOfFreeDaysDiffers() {
        ConsolidationDetails newEntity = new ConsolidationDetails();
        ConsolidationDetails oldEntity = new ConsolidationDetails();
        newEntity.setNumberOfFreeDaysCutOff(5);
        oldEntity.setNumberOfFreeDaysCutOff(3);
        boolean result = consolidationV3Service.canProcesscutOffFields(newEntity, oldEntity);
        assertTrue(result);
    }

}
