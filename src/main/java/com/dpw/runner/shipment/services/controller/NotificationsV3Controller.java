package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.NotificationConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationConfirmationMsgResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotificationV3Service;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NotificationConstants.NOTIFICATION_V3_API_HANDLE)
public class NotificationsV3Controller {
    private INotificationV3Service notificationService;

    private class MyResponseClass extends RunnerResponse<NotificationResponse> {}

    @Autowired
    public NotificationsV3Controller(INotificationV3Service notificationService){
        this.notificationService = notificationService;
    }


    @ApiResponses(value = {@ApiResponse(code = 200, response = NotificationsV3Controller.MyResponseClass.class, message = NotificationConstants.NOTIFICATION_ACCEPT_SUCCESSFUL)})
    @PostMapping(NotificationConstants.NOTIFICATION_ACCEPT)
    public ResponseEntity<IRunnerResponse> acceptNotification(@ApiParam(value = NotificationConstants.NOTIFICATION_ID) @RequestParam Long id) {
        notificationService.acceptNotification(id);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NotificationsV3Controller.MyResponseClass.class, message = NotificationConstants.NOTIFICATION_CONFIRMATION_SUCCESSFUL)})
    @GetMapping(NotificationConstants.NOTIFICATION_CONFIRMATION_MSG_API)
    public ResponseEntity<IRunnerResponse> confirmationMessage(@ApiParam(value = NotificationConstants.NOTIFICATION_ID) @RequestParam Long id) throws AuthenticationException, RunnerException {
        NotificationConfirmationMsgResponse notificationConfirmationMsgResponse = notificationService.confirmationMessage(id);
        return ResponseHelper.buildSuccessResponse(notificationConfirmationMsgResponse);

    }
}
