package com.dpw.runner.shipment.services.notification.service;

import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.fasterxml.jackson.core.JsonProcessingException;

public interface INotificationService {
    NotificationServiceResponse sendEmail(SendEmailBaseRequest request) throws JsonProcessingException;
}
