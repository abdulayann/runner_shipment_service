package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
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

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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
}
