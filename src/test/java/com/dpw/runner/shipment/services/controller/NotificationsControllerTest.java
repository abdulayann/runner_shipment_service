package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotificationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotificationsControllerTest {
    @Mock
    private INotificationService notificationService;
    @InjectMocks
    private NotificationsController notificationsController;

    @Test
    void list() {
        when(notificationService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = notificationsController.list(ListCommonRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(notificationService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = notificationsController.retrieveById(Optional.of(1L), Optional.empty());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void acceptNotification1() {
        when(notificationService.acceptNotification(anyLong())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = notificationsController.acceptNotification(anyLong());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void acceptNotification2() {
        when(notificationService.acceptNotification(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = notificationsController.acceptNotification(anyLong());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void acceptNotification3() {
        when(notificationService.acceptNotification(anyLong())).thenThrow(new RuntimeException("test"));
        var responseEntity = notificationsController.acceptNotification(anyLong());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void confirmationMessage1() {
        when(notificationService.confirmationMessage(anyLong())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = notificationsController.confirmationMessage(anyLong());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void confirmationMessage2() {
        when(notificationService.confirmationMessage(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = notificationsController.confirmationMessage(anyLong());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void confirmationMessage3() {
        when(notificationService.confirmationMessage(anyLong())).thenThrow(new RuntimeException("test"));
        var responseEntity = notificationsController.confirmationMessage(anyLong());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}