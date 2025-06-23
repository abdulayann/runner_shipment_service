package com.dpw.runner.shipment.services.notification.service;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.notification.request.CreateTagsRequest;
import com.dpw.runner.shipment.services.notification.request.GetLogsRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface INotificationService {
    NotificationServiceResponse sendEmail(SendEmailBaseRequest request) throws JsonProcessingException;
    void sendEmail(String body, String subject, List<String> emailIds, List<String> cc);
    ResponseEntity<IRunnerResponse> getLogs(GetLogsRequest request);
    ResponseEntity<IRunnerResponse> createTags(CreateTagsRequest request);
}
