package com.dpw.runner.shipment.services.notification.service.impl;


import com.dpw.runner.shipment.services.notification.config.NotificationConfig;
import com.dpw.runner.shipment.services.notification.config.NotificationRestClient;
import com.dpw.runner.shipment.services.notification.request.NotificationMetadata;
import com.dpw.runner.shipment.services.notification.request.NotificationServiceData;
import com.dpw.runner.shipment.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class NotificationServiceImpl implements INotificationService {

    @Autowired
    private NotificationConfig notificationConfig;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private NotificationRestClient restClient;

    @Override
    public NotificationServiceResponse sendEmail(SendEmailBaseRequest request) {
        long startTime = System.currentTimeMillis();
        NotificationServiceSendEmailRequest notificationServiceSendEmailRequest = null;
        try {
            notificationServiceSendEmailRequest = createNotificationServiceRequest(request);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }

        NotificationServiceResponse response = restClient.sendEmail(notificationServiceSendEmailRequest);

        try {
            log.info("Notification Service Response: {}", objectMapper.writeValueAsString(response));
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
        log.info("Total time taken from notification service to send email is {} ms", (System.currentTimeMillis() - startTime));

        return response;
    }

    private NotificationServiceSendEmailRequest createNotificationServiceRequest(SendEmailBaseRequest request) throws JsonProcessingException {
        NotificationServiceSendEmailRequest notificationServiceSendEmailRequest = new NotificationServiceSendEmailRequest();
        notificationServiceSendEmailRequest.setBccEmails(request.getBcc());
        notificationServiceSendEmailRequest.setModuleName(request.getModuleName());
        notificationServiceSendEmailRequest.setItem(request.getItem());
        notificationServiceSendEmailRequest.setHtmlBody(request.getHtmlBody());
        notificationServiceSendEmailRequest.setSubject(request.getSubject());
        notificationServiceSendEmailRequest.setCcEmails(request.getCc());
        notificationServiceSendEmailRequest.setRecipientEmails(request.getTo());
        notificationServiceSendEmailRequest.setApplicationId(notificationConfig.getApplicationId());
        notificationServiceSendEmailRequest.setOrganizationId(notificationConfig.getOrganizationId());

        NotificationMetadata metadata = new NotificationMetadata();
        metadata.setFrom(notificationConfig.getEmailFrom());
        metadata.setSubject(request.getSubject());

        NotificationServiceData data = new NotificationServiceData();
        data.setHtmlBody(request.getHtmlBody());

        metadata.setData(data);

        notificationServiceSendEmailRequest.setMetadata(objectMapper.writeValueAsString(metadata));
        notificationServiceSendEmailRequest.setTemplateName(request.getTemplateName());
        notificationServiceSendEmailRequest.setFiles(request.getFile());

        log.info("Notification Service Request: {}", objectMapper.writeValueAsString(notificationServiceSendEmailRequest));

        return notificationServiceSendEmailRequest;
    }
}
