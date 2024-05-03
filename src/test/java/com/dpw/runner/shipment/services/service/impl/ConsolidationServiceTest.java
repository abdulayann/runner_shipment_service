package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.patchRequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ConsolidationDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ConsolidationServiceTest {

    @Mock
    private PartialFetchUtils partialFetchUtils;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IPartiesDao partiesDao;

    @Mock
    private CSVParsingUtil<ConsolidationDetails> parser;

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private IPackingDao packingDao;

    @Mock
    private IEventDao eventDao;

    @Mock
    private INotesDao notesDao;

    @Mock
    private IReferenceNumbersDao referenceNumbersDao;

    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Mock
    private IRoutingsDao routingsDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IContainerService containerService;

    @Mock
    private IPackingService packingService;
    @Mock
    private CommonUtils commonUtils;

    @Mock
    private UserContext userContext;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IShipmentSync shipmentSync;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private ISBUtils sbUtils;

    @Mock
    private ISBProperties isbProperties;

    @Mock
    private AzureServiceBusTopic azureServiceBusTopic;

    @Mock
    private IConsolidationSync consolidationSync;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    CacheManager cacheManager;

    @Mock
    CustomKeyGenerator keyGenerator;

    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;

    @Mock
    private ConsolidationDetailsMapper consolidationDetailsMapper;

    @Mock
    private CarrierDetailsMapper carrierDetailsMapper;

    @Mock
    private ProductIdentifierUtility productEngine;

    @Mock
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private KafkaProducer producer;

    @Mock
    private GetNextNumberHelper getNextNumberHelper;

    @Mock
    private IPackingsSync packingsADSync;
    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @InjectMocks
    private ConsolidationService consolidationService;
    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetails testConsol;
    private static ConsolidationDetailsResponse testConsolResponse;
    private static ConsolidationDetailsRequest testConsolRequest;

    private static ModelMapper modelMapperTest = new ModelMapper();

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        testConsolResponse = modelMapperTest.map(testConsol , ConsolidationDetailsResponse.class);
        testConsolRequest = modelMapperTest.map(testConsol , ConsolidationDetailsRequest.class);
    }
//    @AfterEach
//    void tearDown() {
//        reset(CommonUtils.class);
//    }

    @Test
    public void testRetrieveByIdAsync_ValidId_Success() {

        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));


        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().getBody() instanceof IRunnerResponse);
        assertEquals(200, future.join().getStatusCodeValue());
    }

    @Test
    public void testRetrieveByIdAsync_NullId_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        requestModel.setData(getRequest);

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().hasBody());
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    public void testRetrieveByIdAsync_DetailsNotFound_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());


        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
//        assertTrue(future.join().getBody() instanceof String);
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    public void testRetrieveByIdAsync_ExceptionThrown_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenThrow(new RuntimeException("Test Exception"));


        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().hasBody());
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    void testFetchConsolidation_Success_with_data(){
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);

        when(consolidationDetailsDao.findAll(any() , any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(modelMapper.map(any() , any())).thenReturn(response);

        consolidationService.fetchConsolidations(CommonRequestModel.builder().data(sampleRequest).build());

        verify(consolidationDetailsDao, times(1)).findAll(any(), any());
    }

    @Test
    public void testCompleteRetrieveById_Success_NoColumnsIncluded() throws ExecutionException, InterruptedException {
        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(testConsolResponse));
//        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);

        assertNotNull(responseEntity);
        assertEquals(200, responseEntity.getStatusCodeValue());
    }

    @Test
    @Disabled
    public void testCompleteRetrieveById_Success_WithColumnsIncluded() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        getRequest.setIncludeColumns(List.of("guid", "id"));
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(testConsolResponse));
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);
        when(partialFetchUtils.fetchPartialListData(any(), any())).thenReturn(testConsolResponse);
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);

        assertNotNull(responseEntity);
        assertEquals(200, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_RequestIsNull() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_RequestIdIsNull() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        requestModel.setData(getRequest);


        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_ExceptionThrown() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.failedFuture(new RuntimeException());
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);


        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    void testCompleteUpdate_Failure() throws Exception {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = testConsolRequest;
        commonRequestModel.setData(copy);

        var spyService = Mockito.spy(consolidationService);
        Mockito.doReturn(Optional.empty()).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.completeUpdate(commonRequestModel));
        verify(auditLogService, times(0)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    public void testGetGuidFromId_Success() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        commonRequestModel.setData(getRequest);

        ConsolidationDetails consolidationDetails = testConsol;
        var guid = UUID.randomUUID();
        consolidationDetails.setGuid(guid);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(commonRequestModel);

        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    public void testGetGuidFromId_RequestIsNull() {
        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(null);
        assertEquals(400, response.getStatusCodeValue());
    }

    @Test
    public void testGetGuidFromId_ConsolidationDetailsNotFound() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        commonRequestModel.setData(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(commonRequestModel);

        assertEquals(400, response.getStatusCodeValue());
    }

    @Test
    public void testShowCreateBooking_CreatePermissionDenied() {
        PermissionsContext.setPermissions(Collections.emptyList());
        assertThrows(RunnerException.class, () -> consolidationService.showCreateBooking("CREATE"));
    }

    @Test
    public void testShowCreateBooking_ViewPermissionDenied() {
        PermissionsContext.setPermissions(Collections.emptyList());
        assertThrows(RunnerException.class, () -> consolidationService.showCreateBooking("VIEW"));
    }

    @Test
    public void testUpdateConsoleBookingFields_Success() {
        ConsoleBookingRequest request = new ConsoleBookingRequest();
        request.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.updateConsoleBookingFields(request)).thenReturn(1);
        ResponseEntity<IRunnerResponse> response = consolidationService.updateConsoleBookingFields(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    public void testUpdateConsoleBookingFields_Failure() {
        ConsoleBookingRequest request = new ConsoleBookingRequest();
        request.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.updateConsoleBookingFields(request)).thenReturn(0);
        assertThrows(com.dpw.runner.shipment.services.exception.exceptions.ValidationException.class, () -> consolidationService.updateConsoleBookingFields(commonRequestModel));
    }

    @Test
    public void testCreateFromBooking_Success() {
        // Setup
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);

        ResponseEntity<IRunnerResponse> response = consolidationService.createFromBooking(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCreateFromBooking_AuditLogException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Setup
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        doThrow(new IllegalAccessException("IllegalAccessException")).when(auditLogService).addAuditLog(any());

        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }

    @Test
    void testCreateFromBooking_RequestIsNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }


    @Test
    public void testCreateFromBooking_ThrowsValidationException() {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenThrow(new ValidationException("TEST"));

        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }

    @Test
    public void testValidateMawbNumber_Success() {
        String mawb = "ABC123";
        String type = "type";
        ValidateMawbNumberRequest request = ValidateMawbNumberRequest.builder().mawb(mawb).type(type).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ValidateMawbNumberResponse expectedResponse = ValidateMawbNumberResponse.builder().success(true).build();
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);


        CarrierResponse carrierResponse = CarrierResponse.builder().build();
        when(consolidationDetailsDao.isMAWBNumberValid(mawb)).thenReturn(true);
        when(v1Service.fetchCarrierMasterData(any(CarrierListObject.class), anyBoolean())).thenReturn(V1DataResponse.builder().entities(carrierResponse).build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierResponse.class))).thenReturn(List.of(carrierResponse));

        ResponseEntity<IRunnerResponse> response = consolidationService.validateMawbNumber(commonRequestModel);

        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    public void testValidateMawbNumber_InvalidMawb() {
        String mawb = "INVALID_MAWB";
        ValidateMawbNumberRequest request = ValidateMawbNumberRequest.builder().mawb(mawb).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.isMAWBNumberValid(any())).thenReturn(false);
        assertThrows(ValidationException.class, () -> consolidationService.validateMawbNumber(commonRequestModel));
    }

    @Test
    public void testGenerateCustomHouseBLNumber_Success() throws RunnerException {
        String generatedNumber = "ABC123";
        GenerateCustomHblResponse expectedResponse = GenerateCustomHblResponse.builder().hblNumber(generatedNumber).build();
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        ConsolidationService spyService = spy(consolidationService);
        doReturn(generatedNumber).when(spyService).generateCustomBolNumber();

        ResponseEntity<IRunnerResponse> response = spyService.generateCustomHouseBLNumber();

        assertEquals(expectedEntity, response);
    }

    @Test
    public void testGenerateCustomHouseBLNumber_Exception(){
        ConsolidationService spyService = spy(consolidationService);
        doThrow(new RuntimeException("Test exception")).when(spyService).generateCustomBolNumber();
        assertThrows(RunnerException.class, ()->spyService.generateCustomHouseBLNumber());
    }

    @Test
    @Disabled
    public void testGetDefaultConsolidation_Success() {
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setDefaultTransportMode("Sea");
        tenantSettings.setDefaultContainerType("ContainerType");
        tenantSettings.setDefaultShipmentType("ShipmentType");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        TenantModel tenantModel = new TenantModel();

        when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        UserContext.setUser(UsersDto.builder().Username("Username").TenantId(1).build());
        LocalDateTime currentTime = LocalDateTime.now();

        ResponseEntity<IRunnerResponse> response = consolidationService.getDefaultConsolidation();

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        ConsolidationDetailsResponse responseBody = (ConsolidationDetailsResponse)((RunnerResponse) response.getBody()).getData();
        assertNotNull(responseBody);
        assertEquals("ContainerType", responseBody.getContainerCategory());
        assertEquals("ShipmentType", responseBody.getShipmentType());
        assertEquals("BOL Number", responseBody.getBol());
        assertEquals("Username", responseBody.getCreatedBy());
        assertEquals(currentTime.getDayOfYear(), responseBody.getCreatedAt().getDayOfYear());
        assertEquals("1", responseBody.getSourceTenantId());
        assertEquals("PlaceOfIssue", responseBody.getPlaceOfIssue());
    }

    @Test
    void testCreate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), eq(true))).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        ResponseEntity<IRunnerResponse> response = spyService.create(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCreate_SyncFailure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenThrow(new RunnerException("Test"));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        ResponseEntity<IRunnerResponse> response = spyService.create(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCreate_AuditLogFailure() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        doThrow(new IllegalAccessException("IllegalAccessException")).when(auditLogService).addAuditLog(any());

        assertThrows(ValidationException.class, ()->spyService.create(commonRequestModel));
    }

    @Test
    void testRetrieveByIdOrGuid_IdSuccess() throws RunnerException {
        ConsolidationDetailsRequest request = ConsolidationDetailsRequest.builder().id(1L).build();
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        var consolidationDetailsResponse = consolidationService.retrieveByIdOrGuid(request);

        assertEquals(request.getId(), consolidationDetailsResponse.get().getId());
    }

    @Test
    void testRetrieveByIdOrGuid_GuidSuccess() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        var consolidationDetailsResponse = consolidationService.retrieveByIdOrGuid(request);

        assertEquals(request.getGuid(), consolidationDetailsResponse.get().getGuid());
    }

    @Test
    void testRetrieveByIdOrGuid_NullRequest_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = null;
        assertThrows(NullPointerException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }
    @Test
    void testRetrieveByIdOrGuid_EmptyRequest_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        assertThrows(RunnerException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }

    @Test
    void testRetrieveByIdOrGuid_DataRetrieveForId_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = ConsolidationDetailsRequest.builder().id(1L).build();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }
    @Test
    void testRetrieveByIdOrGuid_DataRetrieveForGuid_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }

    @Test
    void testUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        ResponseEntity<IRunnerResponse> response = spyService.update(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testUpdate_DataRetrieval_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        var spyService = Mockito.spy(consolidationService);
        Mockito.doReturn(Optional.empty()).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(ValidationException.class, () -> spyService.update(commonRequestModel));
    }

    @Test
    void testUpdate_GuidNotMatched_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetails entity = new ConsolidationDetails();
        entity.setId(1L);
        entity.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        ConsolidationDetailsRequest copy = modelMapperTest.map(entity , ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails oldEntity = new ConsolidationDetails();
        oldEntity.setId(1L);
        oldEntity.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb7ba23f0"));

        var spyService = Mockito.spy(consolidationService);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(entity);
        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(ValidationException.class, () -> spyService.update(commonRequestModel));
    }

    @Test
    void testAttachShipments_Success_Sea() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setContainersList(List.of(containers));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(anyLong(), any(), any())).thenReturn(List.of(2L));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        doNothing().when(containerService).afterSaveList(anyList(),anyBoolean());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachShipments_Success_Air() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(anyLong(), any(), any())).thenReturn(List.of(2L));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachShipments_ConsoleMapping_Failure() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(2L);
        consoleShipmentMapping.setShipmentId(1L);

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(1L, shipmentIds);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Sea() {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setContainersList(List.of(containers));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Air() {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Sync_Failure() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(packingsADSync.sync(anyList(), any())).thenThrow(new RuntimeException("Test"));
        when(consolidationSync.sync(any(), any(), anyBoolean())).thenThrow(new RunnerException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipmentsReverseSync_Success() {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setContainersList(List.of(containers));

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        ResponseEntity<HttpStatus> responseEntity = consolidationService.detachShipmentsReverseSync(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    // TODO: static method CommonUtils fix
//    @Test
    void testPartialUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest consoleRequest = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        ConsolidationPatchRequest copy = modelMapperTest.map(consoleRequest, ConsolidationPatchRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        mockStatic(CommonUtils.class);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(CommonUtils.convertToEntityList(anyList(), any())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        ResponseEntity<IRunnerResponse> response = spyService.partialUpdate(commonRequestModel, false);

        assertEquals(expectedEntity, response);

    }

    @Test
    void testPartialUpdate_DaoUpdate_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest consoleRequest = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        ConsolidationPatchRequest copy = modelMapperTest.map(consoleRequest, ConsolidationPatchRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenThrow(new RuntimeException("Test"));
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        ResponseEntity<IRunnerResponse> response = spyService.partialUpdate(commonRequestModel, false);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCompleteUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(consolidationDetails)).thenReturn("");
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCalculateUtilization_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_M3);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateUtilization_Success_WithWeightAndVolume() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateUtilization_ConsoleNotFound_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchieved_AllocatedForSameUnit_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchieved_AllocatedForSameUnit_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Success_Sea() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Success_Air() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchievedValues_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);

        ResponseEntity<IRunnerResponse> responseEntity = spyService.calculateAchievedValues(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchievedValues_Failure() {
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchievedValues(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateContainerSummary_Success() throws RunnerException {
        CalculateContainerSummaryRequest request = new CalculateContainerSummaryRequest();
        request.setContainersList(List.of());
        Containers containers = new Containers();
        containers.setId(1L);
        ContainerSummaryResponse response = new ContainerSummaryResponse();

        when(jsonHelper.convertValueToList(request.getContainersList(), Containers.class)).thenReturn(List.of(containers));
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateContainerSummary_Failure() throws RunnerException {
        CalculateContainerSummaryRequest request = new CalculateContainerSummaryRequest();
        request.setContainersList(List.of());
        Containers containers = new Containers();
        containers.setId(1L);

        when(jsonHelper.convertValueToList(request.getContainersList(), Containers.class)).thenReturn(List.of(containers));
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackSummary_Success() throws RunnerException {
        CalculatePackSummaryRequest request = new CalculatePackSummaryRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);
        PackSummaryResponse response = new PackSummaryResponse();

        when(jsonHelper.convertValueToList(request.getPackingList(), Packing.class)).thenReturn(List.of(packing));
        when(packingService.calculatePackSummary(anyList(), any(), any(), any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackSummary_Failure() throws RunnerException {
        CalculatePackSummaryRequest request = new CalculatePackSummaryRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);

        when(jsonHelper.convertValueToList(request.getPackingList(), Packing.class)).thenReturn(List.of(packing));
        when(packingService.calculatePackSummary(anyList(), any(), any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Success_IsAssignTrue() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(true);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(1L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ConsolePacksListResponse.PacksList packsList = modelMapperTest.map(packing, ConsolePacksListResponse.PacksList.class);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        ShipmentDetails shipmentDetails3 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        shipmentDetails3.setId(3L);
        shipmentDetails3.setShipmentType(Constants.CARGO_TYPE_FCL);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails1.getClient(), PartiesResponse.class);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class)).thenReturn(packsList);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(jsonHelper.convertValue(shipmentDetails1.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        when(shipmentDao.findById(3L)).thenReturn(Optional.of(shipmentDetails3));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Success_IsAssignFalse() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(false);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(1L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ConsolePacksListResponse.PacksList packsList = modelMapperTest.map(packing, ConsolePacksListResponse.PacksList.class);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails1.getClient(), PartiesResponse.class);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class)).thenReturn(packsList);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(jsonHelper.convertValue(shipmentDetails1.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Success_IsAssignFalse_NoPack() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(false);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ShipmentDetails shipmentDetails2 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        shipmentDetails2.setId(2L);
        shipmentDetails2.setShipmentType(Constants.CARGO_TYPE_FCL);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails2.getClient(), PartiesResponse.class);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(shipmentDao.findById(2L)).thenReturn(Optional.of(shipmentDetails2));
        when(jsonHelper.convertValue(shipmentDetails2.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Failure() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(true);
        request.setConsolidationId(1L);
        request.setContainerId(1L);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


}