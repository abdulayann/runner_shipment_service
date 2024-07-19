package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {HblController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblControllerTest {

    @Mock
    private IHblService hblService;
    @InjectMocks
    private HblController hblController;

    @Test
    void generateHBL() throws RunnerException {
        // Mock
        when(hblService.generateHBL(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.generateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void generateHBL2() throws RunnerException {
        // Mock
        when(hblService.generateHBL(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.generateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void generateHBL3() throws RunnerException {
        // Mock
        when(hblService.generateHBL(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.generateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(hblService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.update(new HblRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(hblService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.update(new HblRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(hblService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.update(new HblRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentId() {
        // Mock
        when(hblService.retrieveByShipmentId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.retrieveByShipmentId(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentId2() {
        // Mock
        when(hblService.retrieveByShipmentId(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.retrieveByShipmentId(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentId3() {
        // Mock
        when(hblService.retrieveByShipmentId(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.retrieveByShipmentId(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void resetHbl() throws RunnerException {
        // Mock
        when(hblService.resetHbl(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.resetHbl(HblResetRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void resetHbl2() throws RunnerException {
        // Mock
        when(hblService.resetHbl(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.resetHbl(HblResetRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void resetHbl3() throws RunnerException {
        // Mock
        when(hblService.resetHbl(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.resetHbl(HblResetRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void saveV1Hbl() throws RunnerException {
        // Mock
        when(hblService.saveV1Hbl(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.saveV1Hbl(new HblRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void saveV1Hbl2() throws RunnerException {
        // Mock
        when(hblService.saveV1Hbl(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.saveV1Hbl(new HblRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void saveV1Hbl3() throws RunnerException {
        // Mock
        when(hblService.saveV1Hbl(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.saveV1Hbl(new HblRequestV2(), true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBL() throws RunnerException {
        // Mock
        when(hblService.partialUpdateHBL(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.partialUpdateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBL2() throws RunnerException {
        // Mock
        when(hblService.partialUpdateHBL(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = hblController.partialUpdateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBL3() throws RunnerException {
        // Mock
        when(hblService.partialUpdateHBL(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = hblController.partialUpdateHBL(HblGenerateRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(hblService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.delete(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(hblService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = hblController.retrieveById(111L, List.of());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
