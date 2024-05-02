package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.ValidateMawbNumberRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.GenerateCustomHblResponse;
import com.dpw.runner.shipment.services.dto.response.ValidateMawbNumberResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
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
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
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
    private IFileRepoDao fileRepoDao;

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


}