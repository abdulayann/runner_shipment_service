package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource("classpath:application-test.properties")
class EventV3ServiceTest extends CommonMocks {
    @Mock
    private IEventDao eventDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private EventService eventV2Service;

    @InjectMocks
    private EventV3Service eventV3Service;

    private static JsonTestUtility jsonTestUtility;

    @Mock
    private IV1Service v1Service;
    private static Events testData;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetails testConsol;
    private static ConsolidationDetails testConsolidation;
    private static ShipmentDetails testShipment;
    private static EventsRequestV2 testEventsRequestV2;
    private static ConsolidationDetailsResponse testConsolResponse;
    private static ConsolidationDetailsRequest testConsolRequest;

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
        testData = jsonTestUtility.getTestEventData();
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        testConsolResponse = objectMapperTest.convertValue(testConsol, ConsolidationDetailsResponse.class);
        testConsolRequest = objectMapperTest.convertValue(testConsol, ConsolidationDetailsRequest.class);
        testShipment = jsonTestUtility.getTestShipment();
        testConsolidation = jsonTestUtility.getTestConsolidation();
        testEventsRequestV2 = jsonTestUtility.getTestEventsRequestV2();
    }

    @Test
    void testListEventsV2ForInputShipmentId() {
        Long shipmentId = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);
        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(EventsResponse.builder().build());

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Collections.singletonList(EventsResponse.builder().build()));
        mockShipmentSettings();

        var httpResponse = eventV3Service.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), null);

        assertNotNull(httpResponse);
        assertEquals(eventsResponseList, httpResponse);
    }

    @Test
    void testListEventsV2ForInputShipmentId2() {
        Long shipmentId = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);
        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(EventsResponse.builder().build());

        when(eventDao.findAllWithoutTenantFilter(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Collections.singletonList(EventsResponse.builder().build()));
        mockShipmentSettings();

        var httpResponse = eventV3Service.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), Constants.NETWORK_TRANSFER);

        assertNotNull(httpResponse);
        assertEquals(eventsResponseList, httpResponse);
    }

    @Test
    void testListEventsV2ForInputConsolidationId() {
        Long consolidation = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidation);
        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(EventsResponse.builder().build());

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        mockShipmentSettings();
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Collections.singletonList(EventsResponse.builder().build()));
        var httpResponse = eventV3Service.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), null);

        assertNotNull(httpResponse);
        assertEquals(eventsResponseList, httpResponse);
    }

    @Test
    void testListEventsV2ForInputConsolidationId2() {
        Long consolidation = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidation);
        List<EventsResponse> eventsResponseList = new ArrayList<>();
        eventsResponseList.add(EventsResponse.builder().build());

        when(eventDao.findAllWithoutTenantFilter(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Collections.singletonList(EventsResponse.builder().build()));
        mockShipmentSettings();

        var httpResponse = eventV3Service.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), Constants.NETWORK_TRANSFER);

        assertNotNull(httpResponse);
        assertEquals(eventsResponseList, httpResponse);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenEventResponsesIsNull() {
        // Act
        eventV3Service.populateBranchNames(null);

        // Assert
        // No interactions with v1Service or jsonHelper
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenEventResponsesIsEmpty() {
        // Act
        eventV3Service.populateBranchNames(Collections.emptyList());

        // Assert
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenV1ServiceReturnsNull() {
        // Arrange
        List<EventsResponse> responses = List.of(new EventsResponse());
        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(null);

        // Act
        eventV3Service.populateBranchNames(responses);

        // Assert
        verify(v1Service).listCousinBranches(Collections.emptyMap());
        verifyNoInteractions(jsonHelper);
    }


    @Test
    void populateBranchNames_shouldReturnEarly_whenEntitiesInResponseIsNull() {
        // Arrange
        List<EventsResponse> responses = List.of(new EventsResponse());
        V1DataResponse response = new V1DataResponse();
        response.setEntities(null);

        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(response);

        // Act
        eventV3Service.populateBranchNames(responses);

        // Assert
        verify(v1Service).listCousinBranches(Collections.emptyMap());
        verifyNoInteractions(jsonHelper);
    }

    @Test
    void populateBranchNames_shouldHandleV1ServiceExceptionGracefully() {
        // Arrange
        List<EventsResponse> responses = List.of(new EventsResponse());
        when(v1Service.listCousinBranches(Collections.emptyMap())).thenThrow(new V1ServiceException("failed"));

        // Act
        eventV3Service.populateBranchNames(responses);

        // Assert
        verify(v1Service).listCousinBranches(Collections.emptyMap());
    }

    @Test
    void populateBranchNames_shouldHandleGenericExceptionGracefully() {
        // Arrange
        List<EventsResponse> responses = List.of(new EventsResponse());
        V1DataResponse dataResponse = new V1DataResponse();
        dataResponse.setEntities(List.of(new Object())); // raw data

        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(dataResponse);
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class)))
                .thenThrow(new RuntimeException("conversion failed"));

        // Act
        eventV3Service.populateBranchNames(responses);

        // Assert
        verify(jsonHelper).convertValueToList(any(), eq(V1TenantResponse.class));
    }

    @Test
    void populateBranchNames_shouldPopulateBranchNames_whenCodeMatchExists() {
        // Arrange
        EventsResponse response1 = new EventsResponse();
        response1.setBranch("DXB");

        EventsResponse response2 = new EventsResponse();
        response2.setBranch("DEL");

        List<EventsResponse> eventResponses = List.of(response1, response2);

        V1TenantResponse tenant1 = new V1TenantResponse();
        tenant1.setCode("DXB");
        tenant1.setDisplayName("Dubai");

        V1TenantResponse tenant2 = new V1TenantResponse();
        tenant2.setCode("DEL");
        tenant2.setDisplayName("Delhi");

        V1DataResponse dataResponse = new V1DataResponse();
        dataResponse.setEntities(List.of(tenant1, tenant2));

        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(dataResponse);
        when(jsonHelper.convertValueToList(dataResponse.getEntities(), V1TenantResponse.class))
                .thenReturn(List.of(tenant1, tenant2));

        // Act
        eventV3Service.populateBranchNames(eventResponses);

        // Assert
        assertEquals("Dubai", response1.getBranchName());
        assertEquals("Delhi", response2.getBranchName());
    }

    @Test
    void populateBranchNames_shouldNotSetBranchName_whenCodeNotFoundInMap() {
        // Arrange
        EventsResponse response = new EventsResponse();
        response.setBranch("ABC");

        V1TenantResponse tenant = new V1TenantResponse();
        tenant.setCode("XYZ");
        tenant.setDisplayName("Unknown");

        V1DataResponse dataResponse = new V1DataResponse();
        dataResponse.setEntities(List.of(tenant));

        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(dataResponse);
        when(jsonHelper.convertValueToList(dataResponse.getEntities(), V1TenantResponse.class))
                .thenReturn(List.of(tenant));

        // Act
        eventV3Service.populateBranchNames(List.of(response));

        // Assert
        assertNull(response.getBranchName());
    }

    @Test
    void testCreate_success() {
        // Arrange
        EventsRequest request1 = objectMapperTest.convertValue(testData, EventsRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request1);

        IRunnerResponse mockRunnerResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expectedResponse = ResponseEntity.ok(mockRunnerResponse);

        when(eventV2Service.create(commonRequestModel)).thenReturn(expectedResponse);

        // Act
        ResponseEntity<IRunnerResponse> actualResponse = eventV3Service.create(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, actualResponse);
        verify(eventV2Service, times(1)).create(commonRequestModel);
    }

    @Test
    void testUpdate_success() throws RunnerException {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.update(commonRequestModel)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.update(commonRequestModel);

        assertEquals(expected, actual);
        verify(eventV2Service).update(commonRequestModel);
    }

    @Test
    void testList_success() {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.list(commonRequestModel)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.list(commonRequestModel);

        assertEquals(expected, actual);
        verify(eventV2Service).list(commonRequestModel);
    }

    @Test
    void testListAsync_success() throws Exception {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> response = ResponseEntity.ok(mockResponse);

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.completedFuture(response);

        when(eventV2Service.listAsync(commonRequestModel)).thenReturn(future);

        CompletableFuture<ResponseEntity<IRunnerResponse>> actual = eventV3Service.listAsync(commonRequestModel);

        assertEquals(response, actual.get());
        verify(eventV2Service).listAsync(commonRequestModel);
    }

    @Test
    void testDelete_success() {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.delete(commonRequestModel)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.delete(commonRequestModel);

        assertEquals(expected, actual);
        verify(eventV2Service).delete(commonRequestModel);
    }

    @Test
    void testRetrieveById_success() {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.retrieveById(commonRequestModel)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.retrieveById(commonRequestModel);

        assertEquals(expected, actual);
        verify(eventV2Service).retrieveById(commonRequestModel);
    }

    @Test
    void testConvertRequestToEntity_success() {
        EventsRequest eventsRequest = mock(EventsRequest.class);
        Events expectedEntity = mock(Events.class);

        when(jsonHelper.convertValue(eventsRequest, Events.class)).thenReturn(expectedEntity);

        Events actual = eventV3Service.convertRequestToEntity(eventsRequest);

        assertEquals(expectedEntity, actual);
        verify(jsonHelper).convertValue(eventsRequest, Events.class);
    }

    @Test
    void testV1EventsCreateAndUpdate_success() throws RunnerException {
        CommonRequestModel request = mock(CommonRequestModel.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.v1EventsCreateAndUpdate(request, true)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.v1EventsCreateAndUpdate(request, true);

        assertEquals(expected, actual);
        verify(eventV2Service).v1EventsCreateAndUpdate(request, true);
    }

    @Test
    void testTrackEvents_success() throws RunnerException {
        TrackingEventsRequest trackingRequest = mock(TrackingEventsRequest.class);
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        ResponseEntity<IRunnerResponse> expected = ResponseEntity.ok(mockResponse);

        when(eventV2Service.trackEvents(trackingRequest)).thenReturn(expected);

        ResponseEntity<IRunnerResponse> actual = eventV3Service.trackEvents(trackingRequest);

        assertEquals(expected, actual);
        verify(eventV2Service).trackEvents(trackingRequest);
    }

}
