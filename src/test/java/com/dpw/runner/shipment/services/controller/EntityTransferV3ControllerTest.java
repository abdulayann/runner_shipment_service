package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
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
import java.util.List;

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
    void sendShipment() throws RunnerException {
        when(entityTransferService.sendShipment(any())).thenReturn(new ArrayList<>());
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment2() throws RunnerException {
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException());
        SendShipmentRequest sendShipmentRequest = SendShipmentRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendShipment(sendShipmentRequest));
    }

    @Test
    void sendConsolidation() throws RunnerException {
        when(entityTransferService.sendConsolidation(any())).thenReturn(new ArrayList<>());
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation2() throws RunnerException {
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException());
        SendConsolidationRequest  sendConsolidationRequest = SendConsolidationRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendConsolidation(sendConsolidationRequest));
    }

    @Test
    void sendConsolidation3() throws RunnerException {
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        SendConsolidationRequest  sendConsolidationRequest = SendConsolidationRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.sendConsolidation(sendConsolidationRequest));
    }

    @Test
    void importConsolidationValidation() throws RunnerException, JsonMappingException {
        when(entityTransferService.importConsolidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation2() throws RunnerException, JsonMappingException {
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException());
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation3() throws RunnerException, JsonMappingException {
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        var responseEntity = entityTransferController.importConsolidation(ImportV3ConsolidationRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importShipment() throws RunnerException, JsonMappingException {
        when(entityTransferService.importShipment(any())).thenReturn("");
        var responseEntity = entityTransferController.importShipment(ImportV3ShipmentRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importShipment2() throws RunnerException, JsonMappingException {
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException());
        ImportV3ShipmentRequest  importV3ShipmentRequest = ImportV3ShipmentRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.importShipment(importV3ShipmentRequest));
    }

    @Test
    void importShipment3() throws RunnerException, JsonMappingException {
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        ImportV3ShipmentRequest  importV3ShipmentRequest = ImportV3ShipmentRequest.builder().build();
        assertThrows(RuntimeException.class, ()-> entityTransferController.importShipment(importV3ShipmentRequest));
    }

    @Test
    void validateSendShipment_Success() {
        ValidationResponse validationResponse = ValidationResponse.builder().success(true).build();
        when(entityTransferService.sendShipmentValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse(validationResponse));
        var responseEntity = entityTransferController.validateSendShipment(ValidateSendShipmentRequest.builder().shipId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateSendShipment_WithErrors() {
        SendShipmentValidationResponse validationResponse = SendShipmentValidationResponse.builder()
                .isError(true)
                .shipmentErrorMessage("Missing fields")
                .missingKeys(List.of("Flight number"))
                .build();
        when(entityTransferService.sendShipmentValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse(validationResponse));
        var responseEntity = entityTransferController.validateSendShipment(ValidateSendShipmentRequest.builder().shipId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateSendShipment_Exception() {
        when(entityTransferService.sendShipmentValidation(any())).thenThrow(new RuntimeException("Error"));
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(1L).build();
        assertThrows(RuntimeException.class, () -> entityTransferController.validateSendShipment(request));
    }

    @Test
    void validateAutomaticTransferShipment_Success() {
        SendShipmentValidationResponse validationResponse = SendShipmentValidationResponse.builder().isError(false).build();
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenReturn(validationResponse);
        var responseEntity = entityTransferController.validateAutomaticTransferShipment(ValidateSendShipmentRequest.builder().shipId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateAutomaticTransferShipment_WithErrors() {
        SendShipmentValidationResponse validationResponse = SendShipmentValidationResponse.builder()
                .isError(true)
                .shipmentErrorMessage("Missing flight number to retrigger the transfer")
                .missingKeys(List.of("Flight number"))
                .build();
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenReturn(validationResponse);
        var responseEntity = entityTransferController.validateAutomaticTransferShipment(ValidateSendShipmentRequest.builder().shipId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateAutomaticTransferShipment_Exception() {
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenThrow(new RuntimeException("Error"));
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(1L).build();
        assertThrows(RuntimeException.class, () -> entityTransferController.validateAutomaticTransferShipment(request));
    }

    @Test
    void validateSendConsolidation_Success() {
        ValidationResponse validationResponse = ValidationResponse.builder().success(true).build();
        when(entityTransferService.sendConsolidationValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse(validationResponse));
        var responseEntity = entityTransferController.validateSendConsolidation(ValidateSendConsolidationRequest.builder().consoleId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateSendConsolidation_WithErrors() {
        SendConsoleValidationResponse validationResponse = SendConsoleValidationResponse.builder()
                .isError(true)
                .consoleErrorMessage("Missing fields")
                .missingKeys(List.of("Flight number", "Eta"))
                .build();
        when(entityTransferService.sendConsolidationValidation(any())).thenReturn(ResponseHelper.buildSuccessResponse(validationResponse));
        var responseEntity = entityTransferController.validateSendConsolidation(ValidateSendConsolidationRequest.builder().consoleId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateSendConsolidation_Exception() {
        when(entityTransferService.sendConsolidationValidation(any())).thenThrow(new RuntimeException("Error"));
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(1L).build();
        assertThrows(RuntimeException.class, () -> entityTransferController.validateSendConsolidation(request));
    }

    @Test
    void validateAutomaticTransferConsolidation_Success() {
        SendConsoleValidationResponse validationResponse = SendConsoleValidationResponse.builder().isError(false).build();
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenReturn(validationResponse);
        var responseEntity = entityTransferController.validateAutomaticTransferConsolidation(ValidateSendConsolidationRequest.builder().consoleId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateAutomaticTransferConsolidation_WithErrors() {
        SendConsoleValidationResponse validationResponse = SendConsoleValidationResponse.builder()
                .isError(true)
                .consoleErrorMessage("Please enter the Flight number for the consolidation to retrigger the transfer")
                .missingKeys(List.of("Flight number"))
                .build();
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenReturn(validationResponse);
        var responseEntity = entityTransferController.validateAutomaticTransferConsolidation(ValidateSendConsolidationRequest.builder().consoleId(1L).build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateAutomaticTransferConsolidation_Exception() {
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenThrow(new RuntimeException("Error"));
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(1L).build();
        assertThrows(RuntimeException.class, () -> entityTransferController.validateAutomaticTransferConsolidation(request));
    }
}