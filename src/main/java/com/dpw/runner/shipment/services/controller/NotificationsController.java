package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.NotificationConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationListResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationResponse;
import com.dpw.runner.shipment.services.service.interfaces.INotificationService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.validation.Valid;
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

    @ApiResponses(value = {@ApiResponse(code = 200, response = NotificationsController.MyListResponseClass.class, message = NotificationConstants.LIST_SUCCESSFUL, responseContainer = NotificationConstants.RESPONSE_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return notificationService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = NotificationsController.MyResponseClass.class, message = NotificationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = NotificationConstants.NOTIFICATION_ID) @RequestParam Optional<Long> id, @ApiParam(value = NotificationConstants.NOTIFICATION_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return notificationService.retrieveById(CommonRequestModel.buildRequest(request));
    }
}
