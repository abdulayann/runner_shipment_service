package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NotificationConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.DeclineNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.NotificationListResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotificationService;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NotificationConstants.NOTIFICATION_API_HANDLE)
public class NotificationsController {
    private INotificationService notificationService;

    private class MyResponseClass extends RunnerResponse<NotificationResponse> {}
    private class MyListResponseClass extends RunnerListResponse<NotificationListResponse> {}

    @Autowired
    public NotificationsController(INotificationService notificationService){
        this.notificationService = notificationService;
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = NotificationsController.MyListResponseClass.class))), description = NotificationConstants.LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return notificationService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = NotificationsController.MyResponseClass.class)), description = NotificationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = NotificationConstants.NOTIFICATION_ID) @RequestParam Optional<Long> id, @Parameter(description = NotificationConstants.NOTIFICATION_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return notificationService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = NotificationsController.MyResponseClass.class)), description = NotificationConstants.NOTIFICATION_ACCEPT_SUCCESSFUL)})
    @PostMapping(NotificationConstants.NOTIFICATION_ACCEPT)
    public ResponseEntity<IRunnerResponse> acceptNotification(@Parameter(description = NotificationConstants.NOTIFICATION_ID) @RequestParam Long id) {
        String responseMsg;
        try {
            return notificationService.acceptNotification(id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = NotificationsController.MyResponseClass.class)), description = NotificationConstants.NOTIFICATION_REJECT_SUCCESSFUL)})
    @PostMapping(NotificationConstants.NOTIFICATION_REJECT)
    public ResponseEntity<IRunnerResponse> rejectNotification(@RequestBody @Valid DeclineNotificationRequest declineNotificationRequest) {
        String responseMsg;
        try {
            return notificationService.rejectNotification(CommonRequestModel.buildRequest(declineNotificationRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = NotificationsController.MyResponseClass.class)), description = NotificationConstants.NOTIFICATION_CONFIRMATION_SUCCESSFUL)})
    @GetMapping(NotificationConstants.NOTIFICATION_CONFIRMATION_MSG_API)
    public ResponseEntity<IRunnerResponse> confirmationMessage(@Parameter(description = NotificationConstants.NOTIFICATION_ID) @RequestParam Long id) {
        String responseMsg;
        try {
            return notificationService.confirmationMessage(id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
