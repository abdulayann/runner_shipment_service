package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.MawbStocksRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IMawbStocksService;
import com.dpw.runner.shipment.services.syncing.Entity.MawbStocksV2;
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
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {MawbStocksController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MawbStocksControllerTest {

    @Mock
    private IMawbStocksService mawbStocksService;
    @InjectMocks
    private MawbStocksController mawbStocksController;

    @Test
    void create() {
        // Mock
        when(mawbStocksService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.create(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(mawbStocksService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = mawbStocksController.create(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(mawbStocksService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = mawbStocksController.create(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(mawbStocksService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.update(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(mawbStocksService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = mawbStocksController.update(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(mawbStocksService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = mawbStocksController.update(MawbStocksRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1MawbStocks() throws RunnerException {
        // Mock
        when(mawbStocksService.createV1MawbStocks(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.createV1MawbStocks(new MawbStocksV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createV1MawbStocks2() throws RunnerException {
        // Mock
        when(mawbStocksService.createV1MawbStocks(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = mawbStocksController.createV1MawbStocks(new MawbStocksV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1MawbStocks3() throws RunnerException {
        // Mock
        when(mawbStocksService.createV1MawbStocks(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = mawbStocksController.createV1MawbStocks(new MawbStocksV2(), true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(mawbStocksService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.delete(11L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieve() {
        // Mock
        when(mawbStocksService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.retrieve(11L, List.of("containersList"));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        // Mock
        when(mawbStocksService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getNextMawbNumberByCarrier() {
        // Mock
        when(mawbStocksService.getNextMawbNumberByCarrier(anyString(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = mawbStocksController.getNextMawbNumberByCarrier("AIIIT", "org", false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }





}
