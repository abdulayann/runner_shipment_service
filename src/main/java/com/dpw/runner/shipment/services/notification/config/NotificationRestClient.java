package com.dpw.runner.shipment.services.notification.config;

import com.dpw.runner.shipment.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Component
public class NotificationRestClient {

    @Autowired
    private NotificationConfig notificationConfig;

    @Autowired
    private RestTemplate restTemplate;

    public NotificationServiceResponse sendEmail(NotificationServiceSendEmailRequest params) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.set(notificationConfig.getNotificationApiKeyHeader(), notificationConfig.getNotificationApiKeyValue());


        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.add("templateName", params.getTemplateName());
        body.add("organizationId", params.getOrganizationId());
        body.add("applicationId", params.getApplicationId());
        body.add("recipientEmails", params.getRecipientEmails());
        body.add("ccEmails", params.getCcEmails());
        body.add("bccEmails", params.getBccEmails());
        body.add("priority", params.getPriority());
        body.add("trackEmailEvents", params.getTrackEmailEvents().toString());
        body.add("metadata", params.getMetadata());
        body.add("file", params.getFiles());

        HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(body, headers);

        String url = notificationConfig.getNotificationBaseUrl() + notificationConfig.getSendEmail();

        ResponseEntity<NotificationServiceResponse> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                NotificationServiceResponse.class
        );

        return responseEntity.getBody();
    }
}
