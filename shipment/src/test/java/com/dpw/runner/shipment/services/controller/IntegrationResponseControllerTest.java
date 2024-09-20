package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IIntegrationResponseService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {IntegrationResponseController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class IntegrationResponseControllerTest {

    @Mock
    private IIntegrationResponseService integrationResponseService;
    @InjectMocks
    private IntegrationResponseController integrationResponseController;

    @Test
    void fetchIntegrationResponses() {
        // Mock
        when(integrationResponseService.fetchIntegrationResponses(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = integrationResponseController.fetchIntegrationResponses(IntegrationResponseRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchIntegrationResponses2() {
        // Mock
        when(integrationResponseService.fetchIntegrationResponses(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = integrationResponseController.fetchIntegrationResponses(IntegrationResponseRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchIntegrationResponses3() {
        // Mock
        when(integrationResponseService.fetchIntegrationResponses(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = integrationResponseController.fetchIntegrationResponses(IntegrationResponseRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }



}
