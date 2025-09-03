package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.response.NotificationConfirmationMsgResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.INotificationV3Service;
import org.apache.http.auth.AuthenticationException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotificationsV3ControllerTest {
    @Mock
    private INotificationV3Service notificationService;
    @InjectMocks
    private NotificationsV3Controller notificationsController;

    @Test
    void acceptNotification1() {
        doNothing().when(notificationService).acceptNotification(anyLong());
        var responseEntity = notificationsController.acceptNotification(anyLong());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void confirmationMessage1() throws AuthenticationException, RunnerException {
        when(notificationService.confirmationMessage(anyLong())).thenReturn(NotificationConfirmationMsgResponse.builder().build());
        var responseEntity = notificationsController.confirmationMessage(anyLong());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void confirmationMessage2() throws AuthenticationException, RunnerException {
        when(notificationService.confirmationMessage(anyLong())).thenThrow(new RuntimeException());
        Long id = anyLong();
        assertThrows(RuntimeException.class, ()->notificationsController.confirmationMessage(id));
    }

    @Test
    void confirmationMessage3() throws AuthenticationException, RunnerException {
        when(notificationService.confirmationMessage(anyLong())).thenThrow(new RuntimeException("test"));
        Long id = anyLong();
        assertThrows(RuntimeException.class, ()->notificationsController.confirmationMessage(id));
    }

}