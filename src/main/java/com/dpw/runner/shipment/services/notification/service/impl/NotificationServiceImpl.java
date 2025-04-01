package com.dpw.runner.shipment.services.notification.service.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.notification.config.NotificationConfig;
import com.dpw.runner.shipment.services.notification.config.NotificationRestClient;
import com.dpw.runner.shipment.services.notification.request.*;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.*;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Service
@Slf4j
public class NotificationServiceImpl implements INotificationService {

    @Autowired
    private NotificationConfig notificationConfig;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private NotificationRestClient restClient;

    @Override
    @Async
    public void sendEmail(String body, String subject, List<String> emailIds, List<String> cc) {
        cc.removeAll(emailIds);
        SendEmailBaseRequest request = SendEmailBaseRequest.builder()
                .htmlBody(body)
                .subject(subject)
                .to(String.join(",", emailIds))
                .cc(cc.isEmpty() ? null : String.join(",", cc))
                .build();
        sendEmail(request);
    }

    @Override
    public NotificationServiceResponse sendEmail(SendEmailBaseRequest request) {
        long startTime = System.currentTimeMillis();
        NotificationServiceSendEmailRequest notificationServiceSendEmailRequest = null;
        try {
            notificationServiceSendEmailRequest = createNotificationServiceRequest(request);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }

        NotificationServiceResponse response ;
        try{
            response = restClient.sendEmail(notificationServiceSendEmailRequest);
        }catch (Exception e){
            throw new ReportException(e.getMessage());
        }

        try {
            log.info("Notification Service Response: {}", jsonHelper.convertToJson(response));
        } catch (Exception e) {
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
        handleSendMeCopyCheck(request);
        notificationServiceSendEmailRequest.setCcEmails(request.getCc());
        notificationServiceSendEmailRequest.setRecipientEmails(request.getTo());
        notificationServiceSendEmailRequest.setApplicationId(notificationConfig.getApplicationId());
        notificationServiceSendEmailRequest.setOrganizationId(notificationConfig.getOrganizationId());
        if(!CommonUtils.listIsNullOrEmpty(request.getTags()))
            notificationServiceSendEmailRequest.setTags(jsonHelper.convertToJson(request.getTags()));
        if(!Objects.isNull(request.getAttachments()))
            notificationServiceSendEmailRequest.setAttachments(request.getAttachments());

        NotificationMetadata metadata = new NotificationMetadata();
        metadata.setFrom(notificationConfig.getEmailFrom());
        metadata.setSubject(request.getSubject());

        NotificationServiceData data = new NotificationServiceData();
        data.setHtmlBody(request.getHtmlBody());

        metadata.setData(data);

        notificationServiceSendEmailRequest.setMetadata(jsonHelper.convertToJson(metadata));
        notificationServiceSendEmailRequest.setTemplateName(request.getTemplateName());
        notificationServiceSendEmailRequest.setFiles(request.getFile());

        log.info("Notification Service Request: {}", jsonHelper.convertToJson(notificationServiceSendEmailRequest));

        return notificationServiceSendEmailRequest;
    }

    private void handleSendMeCopyCheck(SendEmailBaseRequest request) {
        if (Boolean.TRUE.equals(request.getSendMeCopy())) {
            String userEmail = UserContext.getUser().getEmail();
            if (!IsStringNullOrEmpty(userEmail)) {
                String cc = request.getCc();
                Set<String> emailList = new HashSet<>();
                if (!IsStringNullOrEmpty(cc)) {
                    emailList.addAll(Arrays.asList(cc.split("\\s*,\\s*")));
                }
                emailList.add(userEmail);
                request.setCc(String.join(",", emailList));
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getLogs(GetLogsRequest request) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(restClient.getLogs(request));
        } catch (Exception e) {
            log.error("Error while fetching logs from notification service with exception e {}", e.getMessage());
            responseMsg = e.getMessage() != null ? e.getMessage() : "Error fetching logs from notification service";
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @Override
    public ResponseEntity<IRunnerResponse> createTags(CreateTagsRequest request) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(restClient.createTags(request));
        } catch (Exception e) {
            log.error("Error while creating tags from notification service with exception e {}", e.getMessage());
            responseMsg = e.getMessage() != null ? e.getMessage() : "Error creating tags from notification service";
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
