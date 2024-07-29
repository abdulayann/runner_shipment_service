package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {EventsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventsControllerTest {

    @Mock
    private IEventService eventService;
    @Mock
    private IEventsSync eventsSync;
    @InjectMocks
    private EventsController eventsController;

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
    void syncEventsToService() throws RunnerException {
        // Mock
        when(eventService.V1EventsCreateAndUpdate(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.syncEventsToService(new EventsRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncEventsToService2() throws RunnerException {
        // Mock
        when(eventService.V1EventsCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.syncEventsToService(new EventsRequestV2(), true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncEventsToService3() throws RunnerException {
        // Mock
        when(eventService.V1EventsCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.syncEventsToService(new EventsRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails2() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void trackEventDetails3() throws RunnerException {
        // Mock
        when(eventService.trackEvents(any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = eventsController.trackEventDetails(Optional.of(123L), Optional.empty());
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
