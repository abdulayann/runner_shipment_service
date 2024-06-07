package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {EntityTransferController.class})
@ExtendWith(MockitoExtension.class)
class EntityTransferControllerTest {

    @Mock
    private IEntityTransferService entityTransferService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private EntityTransferController entityTransferController;

    @Test
    void sendShipment() {
        // Mock
        when(entityTransferService.sendShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment2() {
        // Mock
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment3() {
        // Mock
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation2() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation3() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidationValidation() {
        // Mock
        when(entityTransferService.sendConsolidationValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendConsolidationValidation(ValidateSendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidationValidation2() {
        // Mock
        when(entityTransferService.sendConsolidationValidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendConsolidationValidation(ValidateSendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidationValidation3() {
        // Mock
        when(entityTransferService.sendConsolidationValidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendConsolidationValidation(ValidateSendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendShipmentValidation() {
        // Mock
        when(entityTransferService.sendShipmentValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendShipmentValidation(ValidateSendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendShipmentValidation2() {
        // Mock
        when(entityTransferService.sendShipmentValidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendShipmentValidation(ValidateSendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendShipmentValidation3() {
        // Mock
        when(entityTransferService.sendShipmentValidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendShipmentValidation(ValidateSendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkTaskExist() {
        // Mock
        when(entityTransferService.checkTaskExist(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.checkTaskExist(CheckTaskExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkTaskExist2() {
        // Mock
        when(entityTransferService.checkTaskExist(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.checkTaskExist(CheckTaskExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkTaskExist3() {
        // Mock
        when(entityTransferService.checkTaskExist(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.checkTaskExist(CheckTaskExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void postArValidation() throws RunnerException {
        // Mock
        when(entityTransferService.postArValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.postArValidation(PostArValidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void postArValidation1() throws RunnerException {
        // Mock
        when(entityTransferService.postArValidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.postArValidation(PostArValidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
    @Test
    void postArValidation2() throws RunnerException {
        // Mock
        when(entityTransferService.postArValidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.postArValidation(PostArValidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
