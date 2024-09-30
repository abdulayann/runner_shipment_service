package com.dpw.runner.booking.services.notification.service;

import com.dpw.runner.booking.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.booking.services.notification.response.NotificationServiceResponse;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.util.List;

public interface INotificationService {
    NotificationServiceResponse sendEmail(SendEmailBaseRequest request) throws JsonProcessingException;
    void sendEmail(String body, String subject, List<String> emailIds, List<String> cc);
}
