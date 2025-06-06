package com.dpw.runner.shipment.services.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {EventsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventsV3ControllerTest {

    @Mock
    private IEventsV3Service eventService;
    @Mock
    private IEventsSync eventsSync;
    @InjectMocks
    private EventsV3Controller eventsController;

    @Test
    void listEventsV2() {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.listV2(any(), any())).thenReturn(Collections.singletonList(EventsResponse.builder().build()));
        // Test
        var responseEntity = eventsController.listEventsV2(request, null);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listEventsV2ReturnBadRequest() {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.listV2(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.listEventsV2(request, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listEventsV2ReturnBadRequestWithErrorMessage() {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.listV2(any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.listEventsV2(request, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createBookingCarriageData() {
        // Mock
        when(eventService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.createBookingCarriageData(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createBookingCarriageData2() {
        // Mock
        when(eventService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.createBookingCarriageData(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createBookingCarriageData3() {
        // Mock
        when(eventService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.createBookingCarriageData(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(eventService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.update(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(eventService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.update(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(eventService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.update(EventsRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncEventsToService() {
        // Test
        var responseEntity = eventsController.syncEventsToService(new EventsRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails2() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails3() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetailsV2() throws RunnerException {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.trackEvents(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.trackEventDetailsV2(request);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetailsV2ReturnBadRequest() throws RunnerException {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.trackEvents(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.trackEventDetailsV2(request);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetailsV2ReturnBadRequestWithErrorMessage() throws RunnerException {
        TrackingEventsRequest request = new TrackingEventsRequest();
        request.setShipmentId(123L);
        // Mock
        when(eventService.trackEvents(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.trackEventDetailsV2(request);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void getEvents2() {
        // Mock
        when(eventsSync.sync(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.getEvents(List.of());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getEvents3() {
        // Mock
        when(eventsSync.sync(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.getEvents(List.of(new Events()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(eventService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.delete(1111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() throws RunnerException {
        // Mock
        when(eventService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(eventService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.retrieveById(1111L, List.of("containersList"));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
