package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.NetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendFileToExternalRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferControllerTest {
    @Mock
    private INetworkTransferService networkTransferService;
    @InjectMocks
    private NetworkTransferController networkTransferController;
    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build());
    }

    @Test
    void list() {
        when(networkTransferService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = networkTransferController.list(ListCommonRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(networkTransferService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = networkTransferController.retrieveById(Optional.of(1L), Optional.empty());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void requestForTransfer1() {
        when(networkTransferService.requestForTransfer(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = networkTransferController.requestForTransfer(RequestForTransferRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void requestForTransfer2() {
        when(networkTransferService.requestForTransfer(any())).thenThrow(new RuntimeException());
        var responseEntity = networkTransferController.requestForTransfer(RequestForTransferRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void requestForTransfer3() {
        when(networkTransferService.requestForTransfer(any())).thenThrow(new RuntimeException("test"));
        var responseEntity = networkTransferController.requestForTransfer(RequestForTransferRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void requestForReassign1() {
        when(networkTransferService.requestForReassign(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = networkTransferController.requestForReassign(ReassignRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void requestForReassign2() {
        when(networkTransferService.requestForReassign(any())).thenThrow(new RuntimeException());
        var responseEntity = networkTransferController.requestForReassign(ReassignRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void requestForReassign3() {
        when(networkTransferService.requestForReassign(any())).thenThrow(new RuntimeException("test"));
        var responseEntity = networkTransferController.requestForReassign(ReassignRequest.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
    @Test
    void fetchEntityStatus() {
        when(networkTransferService.fetchEntityStatus(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = networkTransferController.fetchEntityStatus(UUID.randomUUID().toString());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createExternal() {
        // Mock
        when(networkTransferService.createExternal(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = networkTransferController.createExternal(NetworkTransferRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createExternal2() {
        // Mock
        when(networkTransferService.createExternal(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = networkTransferController.createExternal(NetworkTransferRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createExternal3() {
        // Mock
        when(networkTransferService.createExternal(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = networkTransferController.createExternal(NetworkTransferRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
}