package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {ShipmentControllerV3.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentControllerV3Test {
    @Mock
    IShipmentServiceV3 shipmentService;
    @Mock
    JsonHelper jsonHelper;
    @InjectMocks
    ShipmentControllerV3 shipmentControllerV3;

    private MockMvc mockMvc;
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(shipmentControllerV3).build();
    }

    @Test
    void getPendingNotificationCount() {
        when(shipmentService.getPendingNotificationCount()).thenReturn(NotificationCount.builder().build());
        var responseEntity = shipmentControllerV3.getPendingNotificationCount();
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
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

    @Test
    void pendingNotificationsData(){
        ShipmentPendingNotificationResponse shipmentPendingNotificationResponse = new ShipmentPendingNotificationResponse();
        when(shipmentService.getPendingNotificationData(any())).thenReturn(shipmentPendingNotificationResponse);
        var responseEntity = shipmentControllerV3.pendingNotificationsData(1L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}