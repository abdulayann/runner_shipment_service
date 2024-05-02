package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventServiceTest {

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
    private ISyncQueueService syncQueueService;

    @Mock
    private SyncConfig syncConfig;

    private static JsonTestUtility jsonTestUtility;
    private static Events testData;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetails testConsol;
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

//    @Test
//    void trackEvents() throws RunnerException {
//        var shipment = jsonTestUtility.getTestShipment();
//        var consolidation = jsonTestUtility.getTestConsolidation();
//        shipment.setId(12L);
//        consolidation.setId(12L);
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
//
//        when(consolidationDao.findById(consolidation.getId())).thenReturn(Optional.of(consolidation));
//
//        when(restTemplate.postForEntity(anyObject(), any(), eq(TrackingEventsResponse.class))).thenReturn(ResponseEntity.of(Optional.of(TrackingEventsResponse.builder().build())));
//        when(modelMapper.map(any(), eq(EventsResponse.class))).thenReturn(EventsResponse.builder().build());
//
//        var response = eventService.trackEvents(Optional.of(12L) , Optional.of(12L));
//
//        assertNotNull(response);
//    }

}