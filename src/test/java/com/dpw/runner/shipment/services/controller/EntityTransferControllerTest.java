package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
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
@Execution(ExecutionMode.CONCURRENT)
class EntityTransferControllerTest {

    @Mock
    private IEntityTransferService entityTransferService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private EntityTransferController entityTransferController;

    @Test
    void sendShipment() throws RunnerException {
        // Mock
        when(entityTransferService.sendShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment2() throws RunnerException {
        // Mock
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendShipment3() throws RunnerException {
        // Mock
        when(entityTransferService.sendShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendShipment(SendShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation() throws RunnerException {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation2() throws RunnerException {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendConsolidation3() throws RunnerException {
        // Mock
        when(entityTransferService.sendConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendConsolidation(SendConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation() throws RunnerException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation2() throws RunnerException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportConsolidationRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importConsolidationValidation3() throws RunnerException {
        // Mock
        when(entityTransferService.importConsolidation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.importConsolidation(ImportConsolidationRequest.builder().build());
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
    void checkTaskExist() throws RunnerException {
        // Mock
        when(entityTransferService.checkTaskExist(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.checkTaskExist(CheckTaskExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkTaskExist2() throws RunnerException {
        // Mock
        when(entityTransferService.checkTaskExist(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.checkTaskExist(CheckTaskExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkTaskExist3() throws RunnerException {
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

    @Test
    void checkEntityExists() {
        // Mock
        when(entityTransferService.checkEntityExists(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.checkEntityExists(CheckEntityExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkEntityExists2() {
        // Mock
        when(entityTransferService.checkEntityExists(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.checkEntityExists(CheckEntityExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
    @Test
    void checkEntityExists3() {
        // Mock
        when(entityTransferService.checkEntityExists(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.checkEntityExists(CheckEntityExistRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importShipment() throws RunnerException {
        // Mock
        when(entityTransferService.importShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.importShipment(ImportShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importShipment2() throws RunnerException {
        // Mock
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.importShipment(ImportShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importShipment3() throws RunnerException {
        // Mock
        when(entityTransferService.importShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.importShipment(ImportShipmentRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkAcceptedFiles() {
        // Mock
        when(entityTransferService.checkAcceptedFiles(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.checkAcceptedFiles(AcceptedFileRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkAcceptedFiles2() {
        // Mock
        when(entityTransferService.checkAcceptedFiles(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.checkAcceptedFiles(AcceptedFileRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkAcceptedFiles3() {
        // Mock
        when(entityTransferService.checkAcceptedFiles(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.checkAcceptedFiles(AcceptedFileRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendFileToExternalSystem() throws RunnerException {
        // Mock
        when(entityTransferService.sendFileToExternalSystem(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = entityTransferController.sendFileToExternalSystem(SendFileToExternalRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sendFileToExternalSystem2() throws RunnerException {
        // Mock
        when(entityTransferService.sendFileToExternalSystem(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = entityTransferController.sendFileToExternalSystem(SendFileToExternalRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void sendFileToExternalSystem3() throws RunnerException {
        // Mock
        when(entityTransferService.sendFileToExternalSystem(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = entityTransferController.sendFileToExternalSystem(SendFileToExternalRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
