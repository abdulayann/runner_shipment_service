package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource("classpath:application-test.properties")
class EventV3ServiceTest extends CommonMocks {
    @Mock
    private IEventDao eventDao;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private EventV3Service eventService;

    @Mock
    private IV1Service v1Service;

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
    }


    @Test
    void testListEventsV2ForInputShipmentId() {
        Long shipmentId = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setShipmentId(shipmentId);
        List<EventsResponse> eventsResponseList = new ArrayList<>();

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);
        mockShipmentSettings();

        var httpResponse = eventService.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), any());

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void testListEventsV2ForInputConsolidationId() {
        Long consolidation = 1L;

        TrackingEventsRequest trackingEventsRequest = new TrackingEventsRequest();
        trackingEventsRequest.setConsolidationId(consolidation);
        List<EventsResponse> eventsResponseList = new ArrayList<>();

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Events())));
        when(jsonHelper.convertValueToList(any(), eq(EventsResponse.class))).thenReturn(eventsResponseList);
        mockShipmentSettings();

        var httpResponse = eventService.listV2(CommonRequestModel.buildRequest(trackingEventsRequest), any());

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildSuccessResponse(eventsResponseList);

        assertNotNull(httpResponse);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenEventResponsesIsNull() {
        // Act
        eventService.populateBranchNames(null);

        // Assert
        // No interactions with v1Service or jsonHelper
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenEventResponsesIsEmpty() {
        // Act
        eventService.populateBranchNames(Collections.emptyList());

        // Assert
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void populateBranchNames_shouldReturnEarly_whenV1ServiceReturnsNull() {
        // Arrange
        List<EventsResponse> responses = List.of(new EventsResponse());
        when(v1Service.listCousinBranches(Collections.emptyMap())).thenReturn(null);

        // Act
        eventService.populateBranchNames(responses);

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
        eventService.populateBranchNames(responses);

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
        eventService.populateBranchNames(responses);

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
        eventService.populateBranchNames(responses);

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
        eventService.populateBranchNames(eventResponses);

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
        eventService.populateBranchNames(List.of(response));

        // Assert
        assertNull(response.getBranchName());
    }
}
