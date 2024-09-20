package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dto.request.RoutingsUpdateRequest;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import java.util.ArrayList;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {ReportController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingControllerTest {

    @Mock
    private IRoutingsService routingsService;
    @InjectMocks
    private RoutingController routingController;

    @Test
    void updateRoutingsTest() {
        RoutingsUpdateRequest routingsUpdateRequest = RoutingsUpdateRequest.builder()
                .routingsRequests(new ArrayList<>()).build();

        // Mock
        when(routingsService.updateRoutings(any())).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        // Test
        var responseEntity = routingController.updateRoutings(routingsUpdateRequest);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
