package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentControllerV3Test {
    @Mock
    IShipmentServiceV3 shipmentService;
    @InjectMocks
    ShipmentControllerV3 shipmentControllerV3;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void getPendingNotificationCount() {
        // Mock
        when(shipmentService.getPendingNotificationCount()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentControllerV3.getPendingNotificationCount();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotificationCount1() {
        // Mock
        when(shipmentService.getPendingNotificationCount()).thenThrow(RuntimeException.class);
        // Test
        var responseEntity = shipmentControllerV3.getPendingNotificationCount();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotificationCount2() {
        // Mock
        when(shipmentService.getPendingNotificationCount()).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentControllerV3.getPendingNotificationCount();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void list(){
        when(shipmentService.listShipment(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentControllerV3.list(ListCommonRequest.builder().build(), true);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentAssignContainerTray() {
        when(shipmentService.getShipmentAndPacksForConsolidationAssignContainerTray(anyLong(), anyLong())).thenReturn(new ShipmentPacksAssignContainerTrayDto());
        var responseEntity = shipmentControllerV3.getShipmentAssignContainerTray(1L, 2L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}