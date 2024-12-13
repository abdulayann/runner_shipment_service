package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDumpDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.EventsDump;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto.InvoiceDto;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto.InvoiceDto.AccountReceivableDto;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto.InvoiceDto.AccountReceivableDto.BillDto;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import org.apache.commons.lang3.StringUtils;
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
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource("classpath:application-test.properties")
class EventServiceTest extends CommonMocks {

    @Mock
    private IEventDao eventDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IShipmentSync shipmentSync;

    @Mock
    private IConsolidationDetailsDao consolidationDao;

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private SyncConfig syncConfig;

    @Mock
    private DateTimeChangeLogService dateTimeChangeLogService;

    @Mock
    private TrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private IEventDumpDao eventDumpDao;

    @Mock
    private IV1Service v1Service;

    private static JsonTestUtility jsonTestUtility;
    private static Events testData;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetails testConsol;
    private static ConsolidationDetails testConsolidation;
    private static ShipmentDetails testShipment;
    private static EventsRequestV2 testEventsRequestV2;
    private static ConsolidationDetailsResponse testConsolResponse;
    private static ConsolidationDetailsRequest testConsolRequest;
    private static ModelMapper modelMapperTest = new ModelMapper();


    @InjectMocks
    private EventService eventService;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        testData = jsonTestUtility.getTestEventData();
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        testConsolResponse = modelMapperTest.map(testConsol , ConsolidationDetailsResponse.class);
        testConsolRequest = modelMapperTest.map(testConsol , ConsolidationDetailsRequest.class);
        testShipment = jsonTestUtility.getTestShipment();
        testConsolidation = jsonTestUtility.getTestConsolidation();
        testEventsRequestV2 = jsonTestUtility.getTestEventsRequestV2();
    }


    @Test
    void create() {
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EventsResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);

        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(testData);
        when(eventDao.save(any())).thenReturn(testData);
        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.create(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void createWithEmptyRequest() {
        EventsRequest request = objectMapperTest.convertValue(null, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.create(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        RunnerResponse runnerResponse = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals("Empty request received", runnerResponse.getError().getMessage());
    }

    @Test
    void createThrowsException() {
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EventsResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);

        String errorMessage = "Custom error message";

        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(testData);
        when(eventDao.save(any())).thenThrow(new RuntimeException(errorMessage));

        ResponseEntity<IRunnerResponse> responseEntity = eventService.create(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void createThrowsExceptionWithEmptyMessage() {
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EventsResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);

        String errorMessage = DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;

        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(testData);
        when(eventDao.save(any())).thenThrow(new RuntimeException());

        ResponseEntity<IRunnerResponse> responseEntity = eventService.create(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void update() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        testData.setId(1L);
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EventsResponse eventsResponse = objectMapperTest.convertValue(testData, EventsResponse.class);

        when(eventDao.findById(anyLong())).thenReturn(Optional.of(testData));
        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(testData);

        when(jsonHelper.convertToJson(any(Events.class))).thenReturn(StringUtils.EMPTY);
        when(eventDao.save(any(Events.class))).thenReturn(testData);
        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn(eventsResponse);


        ResponseEntity<IRunnerResponse> responseEntity = eventService.update(commonRequestModel);

        Mockito.verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        Assertions.assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildSuccessResponse(eventsResponse) , responseEntity);
    }

    @Test
    void updateWithEmptyRequest(){
        EventsRequest request = objectMapperTest.convertValue(null, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Exception e = assertThrows(RunnerException.class, () -> eventService.update(commonRequestModel));

        assertEquals(EventConstants.EMPTY_REQUEST_ERROR, e.getMessage());
    }

    @Test
    void updateWithEmptyRequestId() {
        EventsRequest request = objectMapperTest.convertValue(new Events(), EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Exception e = assertThrows(RunnerException.class, () -> eventService.update(commonRequestModel));

        assertEquals(EventConstants.EMPTY_REQUEST_ID_ERROR, e.getMessage());
    }

    @Test
    void updateThrowsExceptionWhenOldEntityIsNotPresent() {
        testData.setId(100L);
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(eventDao.findById(anyLong())).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> eventService.update(commonRequestModel));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

//    @Test
    void updateThrowsExceptionWhenOldEntityGuidDoesNotMatches() throws RunnerException {
        testData.setId(1L);
        Events event = testData;
        event.setGuid(UUID.fromString("6bdfc1c7-06ad-4422-9d2e-a33a18d793d8"));
        EventsRequest request = objectMapperTest.convertValue(event, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        testData.setGuid(UUID.fromString("3c56d2da-a751-4c1a-b838-d7ea32b5cc3f"));

        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(event);
        when(eventDao.findById(anyLong())).thenReturn(Optional.of(testData));


        //        Exception e = assertThrows(DataRetrievalFailureException.class, () -> eventService.update(commonRequestModel));
//        assertEquals("Provided GUID doesn't match with the existing one !", e.getMessage());
    }

    @Test
    void updateThrowsException() throws RunnerException {
        testData.setId(1L);
        EventsRequest request = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        String errorMessage = DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;

        when(eventDao.findById(anyLong())).thenReturn(Optional.of(testData));
        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(testData);

        when(jsonHelper.convertToJson(any(Events.class))).thenReturn(StringUtils.EMPTY);
        when(eventDao.save(any())).thenThrow(new RuntimeException());

        ResponseEntity<IRunnerResponse> responseEntity = eventService.update(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void list() {
        testData.setId(1L);
        ListCommonRequest getRequest = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        when(eventDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn((EventsResponse) response);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.list(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(response), page.getTotalPages(), page.getTotalElements()), responseEntity);

    }

    @Test
    void listWithEmptyRequest() {
        testData.setId(1L);
        ListCommonRequest getRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        String errorMessage = EventConstants.EMPTY_REQUEST_ERROR;

        ResponseEntity<IRunnerResponse> responseEntity = eventService.list(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void listThrowsException() {
        testData.setId(1L);
        ListCommonRequest getRequest = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        String errorMessage = DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;

        when(eventDao.findAll(any(), any())).thenThrow(new RuntimeException());

        ResponseEntity<IRunnerResponse> responseEntity = eventService.list(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void listAsync() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel request = CommonRequestModel.buildRequest(listCommonRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        when(eventDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn((EventsResponse) response);

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = eventService.listAsync(request);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(response), page.getTotalPages(), page.getTotalElements()),
                responseEntity.get());
    }

    @Test
    void listAsyncWithEmptyRequest() throws ExecutionException, InterruptedException {
        testData.setId(1L);
        ListCommonRequest getRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        String errorMessage = EventConstants.EMPTY_REQUEST_ERROR;

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = eventService.listAsync(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.get().getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void listAsyncThrowsException() throws ExecutionException, InterruptedException {
        testData.setId(1L);
        ListCommonRequest getRequest = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);
        Page<Events> page = new PageImpl<>(List.of(testData) , PageRequest.of(0 , 10) , 1);

        String errorMessage = DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;

        when(eventDao.findAll(any(), any())).thenThrow(new RuntimeException());

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = eventService.listAsync(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        RunnerResponse runnerResponse  = objectMapperTest.convertValue(responseEntity.get().getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }


    @Test
    void delete() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        when(eventDao.findById(id)).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any(Events.class))).thenReturn(StringUtils.EMPTY);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.delete(commonRequestModel);

        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        verify(eventDao, times(1)).delete(any(Events.class));
        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.OK.value());
    }

    @Test
    void deleteEntityNotPresent() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        when(eventDao.findById(id)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = eventService.delete(commonRequestModel);

        assertNotNull(responseEntity);
        RunnerResponse runnerResponse = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void retrieveById() {
        testData.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        EventsResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);

        when(eventDao.findById(anyLong())).thenReturn(Optional.of(testData));
        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(response));
    }

    @Test
    void retrieveByIdEntityNotPresent() {
        testData.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        EventsResponse response = objectMapperTest.convertValue(testData, EventsResponse.class);

        when(eventDao.findById(anyLong())).thenReturn(Optional.empty());
//        when(jsonHelper.convertValue(any(Events.class), eq(EventsResponse.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        RunnerResponse runnerResponse = objectMapperTest.convertValue(responseEntity.getBody(), RunnerResponse.class);
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void trackEventsForInputShipmentThrowsErrorWhenShipmentNotPresent() throws RunnerException {
        Long shipmentId = 1L;
        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);


        var exception = assertThrows(DataRetrievalFailureException.class,
                () -> eventService.trackEvents(trackingEventsRequest));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void trackEventsForInputConsolidationThrowsErrorWhenConsolidationNotPresent() throws RunnerException {
        Long consolidationId = 1L;
        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidationId);

        var exception = assertThrows(DataRetrievalFailureException.class,
                () -> eventService.trackEvents(trackingEventsRequest));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void trackEventsForEmptyRequest() throws RunnerException {
        var exception = assertThrows(RunnerException.class,
                () -> eventService.trackEvents(new TrackingEventsRequest()));

        assertEquals("Both shipmentId and consolidationId are empty !", exception.getMessage());
    }

    @Test
    void trackEventsForInputShipment() throws RunnerException {
        Long shipmentId = 1L;
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP01";
        shipment.setShipmentId(referenceNumber);

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEventsList(List.of(new Events()));
        EventsResponse eventsResponse = new EventsResponse();

        TrackingRequest trackingRequest = TrackingRequest.builder().referenceNumber(referenceNumber).build();
//        ResponseEntity<TrackingEventsResponse> mockResponseEntity = ResponseEntity.ok(trackingEventsResponse);

        Events mockEvent = Events.builder().build();
        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenReturn(trackingEventsResponse);
        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(List.of(mockEvent));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));

        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void trackEventsForInputConsolidation() throws RunnerException {
        Long consolidationId = 1L;
        var mockConsolidation = jsonTestUtility.getTestConsolidation();
        mockConsolidation.setId(consolidationId);
        String referenceNumber = mockConsolidation.getReferenceNumber() != null ? mockConsolidation.getReferenceNumber() : "CON01";
        mockConsolidation.setReferenceNumber(referenceNumber);

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidationId);

        EventsResponse eventsResponse = new EventsResponse();

        Events mockEvent = Events.builder().build();

        when(consolidationDao.findById(consolidationId)).thenReturn(Optional.of(mockConsolidation));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));

        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void trackEventsForInputShipmentSavesUpstreamEvents() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        Long shipmentId = 1L;
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP01";
        shipment.setShipmentId(referenceNumber);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipment.setBookingNumber("1234-5678");
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setEmptyContainerReturned(false);
        shipment.setAdditionalDetails(additionalDetails);

        Events mockEvent = getMockEvent(1L, EventConstants.GATE_IN_WITH_CONTAINER_EMPTY, EventConstants.GATE_IN_WITH_CONTAINER_EMPTY, "originPort");

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEventsList(List.of(mockEvent));
        EventsResponse eventsResponse = new EventsResponse();

        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenReturn(trackingEventsResponse);
        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(List.of(mockEvent));
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);

        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        verify(eventDumpDao, times(1)).saveAll(any());
        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void trackEventsForInputShipmentThrowsException() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        Long shipmentId = 1L;
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP01";
        shipment.setShipmentId(referenceNumber);

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEvents(List.of(new EventsRequestV2()));
        EventsResponse eventsResponse = new EventsResponse();

        TrackingRequest trackingRequest = TrackingRequest.builder().referenceNumber(referenceNumber).build();
        ResponseEntity<TrackingEventsResponse> mockResponseEntity = ResponseEntity.ok(trackingEventsResponse);

        V1ErrorResponse v1ErrorResponse = new V1ErrorResponse();
        v1ErrorResponse.setError(new V1ErrorResponse.V1Error());


        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenThrow(new HttpServerErrorException(HttpStatus.UNAUTHORIZED));

        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);

        var e = assertThrows(RunnerException.class, () -> eventService.trackEvents(trackingEventsRequest));

        assertNotNull(e);
    }

    @Test
    void updateAtaAtdInShipmentWithAtaEventCode() {
        List<Events> eventsList = new ArrayList<>();
        Events upstreamEvent = new Events();

        upstreamEvent.setActual(LocalDateTime.now());
        upstreamEvent.setEventCode(EventConstants.ATA_EVENT_CODES.get(0));
        eventsList.add(upstreamEvent);
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        ShipmentSettingsDetails currentTenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        currentTenantSettings.setIsAtdAtaAutoPopulateEnabled(true);

        eventService.updateAtaAtdInShipment(eventsList, shipmentDetails, currentTenantSettings);

        assertNotNull(shipmentDetails.getCarrierDetails());
        assertEquals(shipmentDetails.getCarrierDetails().getAta(), upstreamEvent.getActual());
    }

    @Test
    void updateAtaAtdInShipmentWithAtdEventCode() {
        List<Events> eventsList = new ArrayList<>();
        Events upstreamEvent = new Events();

        upstreamEvent.setActual(LocalDateTime.now());
        upstreamEvent.setEventCode(EventConstants.ATD_EVENT_CODES.get(0));
        eventsList.add(upstreamEvent);
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        ShipmentSettingsDetails currentTenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        currentTenantSettings.setIsAtdAtaAutoPopulateEnabled(true);

        eventService.updateAtaAtdInShipment(eventsList, shipmentDetails, currentTenantSettings);

        assertNotNull(shipmentDetails.getCarrierDetails());
        assertEquals(shipmentDetails.getCarrierDetails().getAtd(), upstreamEvent.getActual());
    }

    @Test
    void testV1EventsCreateAndUpdate_Success() throws RunnerException {
        Events mockEvent = new Events();
        when(eventDao.findByGuid(any())).thenReturn(Optional.of(mockEvent));
        when(modelMapper.map(any(), any())).thenReturn(mockEvent);

        ResponseEntity<IRunnerResponse> responseEntity = eventService.V1EventsCreateAndUpdate(CommonRequestModel.buildRequest(testEventsRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1EventsCreateAndUpdate_Success_SyncQueue() throws RunnerException, NoSuchFieldException, IllegalAccessException {
        Field field = SyncConfig.class.getField("IS_REVERSE_SYNC_ACTIVE");
        field.setAccessible(true);
        field.set(syncConfig, false);
        ResponseEntity<IRunnerResponse> responseEntity = new ResponseEntity<>(HttpStatus.OK);
        ResponseEntity<IRunnerResponse> response = eventService.V1EventsCreateAndUpdate(CommonRequestModel.buildRequest(testEventsRequestV2), true);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testV1EventsCreateAndUpdate_NewPack_Success() throws RunnerException {
        Events mockEvent = new Events();
        EventsResponse mockEventsResponse = new EventsResponse();
        when(eventDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(mockEvent);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(eventDao.save(any())).thenReturn(mockEvent);
        when(objectMapper.convertValue(any(), eq(EventsResponse.class))).thenReturn(mockEventsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = eventService.V1EventsCreateAndUpdate(CommonRequestModel.buildRequest(testEventsRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1EventsCreateAndUpdate_Failure() throws RunnerException {
        Events mockEvent = new Events();
        when(eventDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(mockEvent);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(eventDao.save(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(testEventsRequestV2);
        var e  = assertThrows(RuntimeException.class, () -> eventService.V1EventsCreateAndUpdate(commonRequestModel, false));
        assertNotNull(e);
    }

    private Events getMockEvent(Long eventId, String eventCode, String entityType, String locationRole){
        Events mockEvent = new Events();
        mockEvent.setId(eventId);
        mockEvent.setGuid(UUID.randomUUID());
        mockEvent.setEventCode(eventCode);
        mockEvent.setEntityType(entityType);
        mockEvent.setLocationRole(locationRole);

        return mockEvent;
    }


    @Test
    void TestGateInWithContainerFullTrackEvent() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        Long shipmentId = 1L;
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP02";
        shipment.setShipmentId(referenceNumber);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipment.setBookingNumber("5678-1234");

        Events mockEvent = getMockEvent(1L, EventConstants.GATE_IN_WITH_CONTAINER_FULL,
                EventConstants.GATE_IN_WITH_CONTAINER_FULL, "originPort");

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEventsList(List.of(mockEvent));
        EventsResponse eventsResponse = new EventsResponse();
        V1DataResponse v1DataResponse = new V1DataResponse();

        EntityTransferMasterLists entityTransferMasterLists = EntityTransferMasterLists.builder()
                .ItemValue("ItemValue")
                .ItemType(99)
                .ItemDescription("ItemDescription").build();
        v1DataResponse.setEntities(List.of(entityTransferMasterLists));

        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenReturn(trackingEventsResponse);
        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(List.of(mockEvent));
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);

        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferMasterLists.class))).thenReturn(List.of(entityTransferMasterLists));

        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        verify(eventDumpDao, times(1)).saveAll(any());
        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void TestVesselDepartureWithContainerTrackEvent() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        Long shipmentId = 1L;
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP03";
        shipment.setShipmentId(referenceNumber);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipment.setBookingNumber("938-1234");

        Events mockEvent = getMockEvent(2L, EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER,
                EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER, "originPort");

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEventsList(List.of(mockEvent));
        EventsResponse eventsResponse = new EventsResponse();

        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenReturn(trackingEventsResponse);
        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(List.of(mockEvent));
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);


        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        verify(eventDumpDao, times(1)).saveAll(any());
        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }


    @Test
    void TestGateOutWithContainerFullTrackEvent() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        Long shipmentId = 1L;
        shipment.setId(shipmentId);
        String referenceNumber = shipment.getShipmentId() != null ? shipment.getShipmentId() : "SHP04";
        shipment.setShipmentId(referenceNumber);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipment.setBookingNumber("938-5284");

        Events mockEvent = getMockEvent(6L, EventConstants.GATE_OUT_WITH_CONTAINER_FULL,
                EventConstants.GATE_OUT_WITH_CONTAINER_FULL, "destinationPort");

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);

        TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
        trackingEventsResponse.setShipmentAta(LocalDateTime.now());
        trackingEventsResponse.setShipmentAtd(LocalDateTime.now());
        trackingEventsResponse.setEventsList(List.of(mockEvent));
        EventsResponse eventsResponse = new EventsResponse();

        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        when(trackingServiceAdapter.getTrackingEventsResponse(any())).thenReturn(trackingEventsResponse);
        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(List.of(mockEvent));
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);


        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(eventsResponse);
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);

        var httpResponse = eventService.trackEvents(trackingEventsRequest);

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        verify(eventDumpDao, times(1)).saveAll(any());
        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void processUpstreamTrackingMessageReturnsTrueIfInputIsNull() {
        var response = eventService.processUpstreamTrackingMessage(null, "messageId");
        assertTrue(response);
    }

    @Test
    void processUpstreamTrackingMessageReturnsTrueIfReferenceNumberIsNull() {
        var container = jsonTestUtility.getJson("TRACKING_CONTAINER", TrackingServiceApiResponse.Container.class);

        container.getContainerBase().setShipmentReference(null);

        when(trackingServiceAdapter.generateEventsFromTrackingResponse(any())).thenReturn(List.of(new Events()));

        var response = eventService.processUpstreamTrackingMessage(container, "messageId");
        assertTrue(response);
    }

    @Test
    void processUpstreamTrackingMessageReturnsTrueIfTrackingEventsIsNullorEmpty() {
        var container = new TrackingServiceApiResponse.Container();
        when(trackingServiceAdapter.generateEventsFromTrackingResponse(any())).thenReturn(Collections.emptyList());
        var response = eventService.processUpstreamTrackingMessage(container, "messageId");
        assertTrue(response);
    }

    @Test
    void processUpstreamTrackingMessageReturnsTrueInSuccess() {
        var container = jsonTestUtility.getJson("TRACKING_CONTAINER", TrackingServiceApiResponse.Container.class);

        String refNum = container.getContainerBase().getShipmentReference();

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();

        Events mockEvent = Events.builder().build();
        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(trackingServiceAdapter.generateEventsFromTrackingResponse(any())).thenReturn(List.of(new Events()));
        when(shipmentDao.findByShipmentId(refNum)).thenReturn(List.of(shipmentDetails1, shipmentDetails2));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(mockEvent));

        var response = eventService.processUpstreamTrackingMessage(container, "messageId");
        assertTrue(response);
    }

    @Test
    void processUpstreamTrackingMessageForAirShipment() {
        var container = jsonTestUtility.getJson("TRACKING_CONTAINER", TrackingServiceApiResponse.Container.class);

        String refNum = container.getContainerBase().getShipmentReference();

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTransportMode("AIR");

        Events mockEvent = Events.builder().eventCode(EventConstants.TRCF).build();
        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(trackingServiceAdapter.generateEventsFromTrackingResponse(any())).thenReturn(List.of(mockEvent));
        when(shipmentDao.findByShipmentId(refNum)).thenReturn(List.of(shipmentDetails1));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(eventDao.saveAll(anyList())).thenReturn(List.of(mockEvent));
        when(modelMapper.map(any(), eq(Events.class))).thenReturn(mockEvent);
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(mockEvent));

        var response = eventService.processUpstreamTrackingMessage(container, "messageId");
        assertTrue(response);
    }

    @Test
    void processUpstreamTrackingMessageReturnsFalseIfShipmentSaveFails() throws RunnerException {
        var container = jsonTestUtility.getJson("TRACKING_CONTAINER", TrackingServiceApiResponse.Container.class);

        String refNum = container.getContainerBase().getShipmentReference();

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();

        Events mockEvent = Events.builder().build();
        EventsDump mockEventDump = objectMapperTest.convertValue(mockEvent, EventsDump.class);

        when(trackingServiceAdapter.generateEventsFromTrackingResponse(any())).thenReturn(List.of(new Events()));
        when(shipmentDao.findByShipmentId(refNum)).thenReturn(List.of(shipmentDetails1, shipmentDetails2));
        when(modelMapper.map(any(), eq(EventsDump.class))).thenReturn(mockEventDump);
        when(eventDumpDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEventDump)));
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockEvent)));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(mockEvent));
        when(shipmentDao.saveWithoutValidation(any())).thenThrow(new RuntimeException());

        var response = eventService.processUpstreamTrackingMessage(container, "");
        assertFalse(response);
    }

    @Test
    void testListEventsV2ForInputShipmentId() throws RunnerException {
        Long shipmentId = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);
        List<EventsResponse> eventsResponseList = new ArrayList<>();

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);
        mockShipmentSettings();

        var httpResponse = eventService.listV2(CommonRequestModel.buildRequest(trackingEventsRequest));

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void testListEventsV2ForInputConsolidationId() throws RunnerException {
        Long consolidation = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidation);
        List<EventsResponse> eventsResponseList = new ArrayList<>();

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);
        mockShipmentSettings();

        var httpResponse = eventService.listV2(CommonRequestModel.buildRequest(trackingEventsRequest));

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void processUpstreamBillingCommonEventMessage_SuccessfulExecution() {
        // Arrange
        BillingInvoiceDto billingInvoiceDto = mock(BillingInvoiceDto.class);
        InvoiceDto invoiceDto = mock(InvoiceDto.class);
        AccountReceivableDto accountReceivableDto = mock(AccountReceivableDto.class);
        BillDto billDto1 = mock(BillDto.class);
        BillDto billDto2 = mock(BillDto.class);

        UUID guid1 = UUID.randomUUID();
        UUID guid2 = UUID.randomUUID();

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setGuid(guid1);
        shipment1.setId(1L);
        shipment1.setShipmentId("S123");
        shipment1.setTenantId(1);

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setGuid(guid2);
        shipment2.setId(2L);
        shipment2.setShipmentId("S456");
        shipment2.setTenantId(2);

        EventsRequest mockRequest1 = new EventsRequest(); // Populate as needed
        EventsRequest mockRequest2 = new EventsRequest();
        Events mockEvent = new Events();

        when(billingInvoiceDto.getPayload()).thenReturn(invoiceDto);
        when(invoiceDto.getAccountReceivable()).thenReturn(accountReceivableDto);
        when(accountReceivableDto.getBills()).thenReturn(List.of(billDto1, billDto2));

        when(billDto1.getModuleId()).thenReturn(guid1.toString());
        when(billDto1.getModuleTypeCode()).thenReturn(Constants.SHIPMENT);
        when(billDto2.getModuleId()).thenReturn(guid2.toString());
        when(billDto2.getModuleTypeCode()).thenReturn(Constants.SHIPMENT);

        when(shipmentDao.findByGuids(anyList())).thenReturn(List.of(shipment1, shipment2));
        when(jsonHelper.convertValue(any(EventsRequest.class), eq(Events.class))).thenReturn(mockEvent);

        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        doNothing().when(eventDao).updateEventDetails(any());

        when(eventDao.findAll(any(), any())).thenReturn(Page.empty());
//        doNothing().when(eventDao).delete(any());
        when(eventDao.save(any())).thenReturn(mockEvent);

        // Act
        eventService.processUpstreamBillingCommonEventMessage(billingInvoiceDto);

        // Assert
        verify(shipmentDao).findByGuids(List.of(guid1, guid2));
        verify(eventDao, times(2)).save(any());
        verify(commonUtils, times(2)).updateEventWithMasterData(anyList());
        verify(eventDao, never()).delete(any()); // No duplicate events, so delete should not be called
    }


}