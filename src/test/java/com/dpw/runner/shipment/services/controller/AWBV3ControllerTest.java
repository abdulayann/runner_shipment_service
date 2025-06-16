package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.FetchAwbListRequest;
import com.dpw.runner.shipment.services.dto.request.ResetAwbRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
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
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AWBV3ControllerTest {

    @Mock
    private IAwbService awbService;
    @InjectMocks
    private AwbV3Controller awbV3Controller;

    @Test
    void list() {
        // Mock
        when(awbService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbV3Controller.list(new FetchAwbListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list1() {
        // Mock
        when(awbService.list(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbV3Controller.list(new FetchAwbListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails() {
        // Mock
        when(awbService.updateAwb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbV3Controller.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails2() {
        // Mock
        when(awbService.updateAwb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbV3Controller.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails3() {
        // Mock
        when(awbService.updateAwb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbV3Controller.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void reset() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbV3Controller.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void reset2() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbV3Controller.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void reset3() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbV3Controller.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void getAllMasterData() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbV3Controller.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData2() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbV3Controller.getAllMasterData(null, 123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData3() {
        // Mock
        // Test
        var responseEntity = awbV3Controller.getAllMasterData(null, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData4() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbV3Controller.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData5() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbV3Controller.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
}
