package com.dpw.runner.shipment.services.notification.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.notification.request.CreateTagsRequest;
import com.dpw.runner.shipment.services.notification.request.GetLogsRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@Validated
@RequestMapping("/api/v2/notification")
public class NotificationController {
    @Autowired
    private INotificationService notificationService;

    @PostMapping(value = "/sendEmail", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<NotificationServiceResponse> sendEmail(@RequestBody SendEmailBaseRequest request) throws JsonProcessingException {
        NotificationServiceResponse response = notificationService.sendEmail(request);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PostMapping(value = "/getLogs")
    public ResponseEntity<IRunnerResponse> getLogs(@RequestBody GetLogsRequest request) {
        return notificationService.getLogs(request);
    }

    @PostMapping(value = "/createTags")
    public ResponseEntity<IRunnerResponse> createTags(@RequestBody CreateTagsRequest request) {
        return notificationService.createTags(request);
    }
}
