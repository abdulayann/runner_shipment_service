package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferV3Service;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.fasterxml.jackson.databind.JsonMappingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {EntityTransferController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EntityTransferV3ControllerTest {

    @Mock
    private IEntityTransferV3Service entityTransferService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private EntityTransferV3Controller entityTransferController;

    @Test
    void sendShipment() {
        // Mock
        when(entityTransferService.sendShipment(any())).thenReturn(new ArrayList<>());
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment2() {
        // Mock
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException());
        SendShipmentRequest sendShipmentRequest = SendShipmentRequest.builder().build();
        // Assert
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendShipment(sendShipmentRequest));
    }

    @Test
    void sendConsolidation() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenReturn(new ArrayList<>());
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation2() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException());

        SendConsolidationRequest  sendConsolidationRequest = SendConsolidationRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendConsolidation(sendConsolidationRequest));
    }

    @Test
    void sendConsolidation3() {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        SendConsolidationRequest  sendConsolidationRequest = SendConsolidationRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendConsolidation(sendConsolidationRequest));
    }

    @Test
    void importConsolidationValidation() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation2() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation3() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importShipment() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importShipment(any())).thenReturn("");
        // Test
        var responseEntity = entityTransferController.importShipment(ImportV3ShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importShipment2() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException());
        ImportV3ShipmentRequest  importV3ShipmentRequest = ImportV3ShipmentRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.importShipment(importV3ShipmentRequest));

    }

    @Test
    void importShipment3() throws RunnerException, JsonMappingException {
        // Mock
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        ImportV3ShipmentRequest  importV3ShipmentRequest = ImportV3ShipmentRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.importShipment(importV3ShipmentRequest));
    }

}
