package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import java.util.HashMap;
import java.util.List;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingV3ControllerTest {

    @Mock
    private IRoutingsV3Service routingService;

    @InjectMocks
    private RoutingV3Controller routingController;

    @Test
    void shipmentCreate() throws RunnerException {
        when(routingService.create(any(), any())).thenReturn(RoutingsResponse.builder().build());
        var response = routingController.shipmentCreate(RoutingsRequest.builder().build());
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shipmentDelete() throws RunnerException {
        var response = routingController.shipmentDelete(1L);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void fetchShipmentRoute() throws RunnerException {
        when(routingService.list(any(), eq(null))).thenReturn(RoutingListResponse.builder().totalPages(1).totalCount(2L).build());
        var response = routingController.fetchShipmentRoute(ListCommonRequest.builder().build(), null);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveById() throws RunnerException {
        when(routingService.retrieveById(any(), eq(null))).thenReturn(RoutingsResponse.builder().build());
        var response = routingController.retrieveById(1L, null);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shipmentUpdate() throws RunnerException {
        when(routingService.update(any(), any())).thenReturn(RoutingsResponse.builder().build());
        var response = routingController.shipmentUpdate(RoutingsRequest.builder().build());
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shipmentUpdateBulk() throws RunnerException {
        when(routingService.updateBulk(any(), any())).thenReturn(BulkRoutingResponse.builder().build());
        var response = routingController.shipmentUpdateBulk(List.of());
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void shipmentDeleteBulk() throws RunnerException {
        when(routingService.deleteBulk(any(), any())).thenReturn(BulkRoutingResponse.builder().build());
        var response = routingController.shipmentDeleteBulk(List.of());
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getAllMasterData() {
        when(routingService.getAllMasterData(any(), eq(null))).thenReturn(new HashMap<>());
        var response = routingController.getAllMasterData(1L, null);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}