package com.dpw.runner.shipment.services.notification.config;

import com.dpw.runner.shipment.services.adapters.impl.ReportServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.reportService.MailAuditLogRequest;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.notification.request.CreateTagsRequest;
import com.dpw.runner.shipment.services.notification.request.GetLogsRequest;
import com.dpw.runner.shipment.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Component
@Slf4j
public class NotificationRestClient {

    @Autowired
    ReportServiceAdapter reportServiceAdapter;

    @Autowired
    private NotificationConfig notificationConfig;

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private JsonHelper jsonHelper;

    public NotificationServiceResponse sendEmail(NotificationServiceSendEmailRequest params) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.set(notificationConfig.getNotificationApiKeyHeader(), notificationConfig.getNotificationApiKeyValue());


        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.add("templateName", "EmailInvoice_Runner");
        body.add("organizationId", params.getOrganizationId());
        body.add("applicationId", params.getApplicationId());
        body.add("recipientEmails", params.getRecipientEmails());
        body.add("ccEmails", params.getCcEmails());
        body.add("bccEmails", params.getBccEmails());
        body.add("priority", params.getPriority());
        body.add("trackEmailEvents", params.getTrackEmailEvents().toString());
        body.add("metadata", params.getMetadata());
        body.add("file", params.getFiles());
        if(params.getTags() != null)
            body.add("tags", params.getTags());
        if(!Objects.isNull(params.getAttachments()))
            body.add("attachments", params.getAttachments());

        HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(body, headers);

        String url = notificationConfig.getNotificationBaseUrl() + notificationConfig.getSendEmail();

        ResponseEntity<NotificationServiceResponse> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                NotificationServiceResponse.class
        );

        //make rest client for v1 and

        String module = params.getModuleName();
        String sub = params.getSubject();
        String bodyEmail = params.getHtmlBody();
        String item = params.getItem();

        var subject = !StringUtility.isEmpty(sub) ? sub : "CargoRunner Notifications";
        try {
            List<String> toEmailsList = getEmailsListFromString(params.getRecipientEmails());
            List<String> ccEmailsList = getEmailsListFromString(params.getCcEmails());
            List<String> bccEmailsList = getEmailsListFromString(params.getBccEmails());
            var user = UserContext.getUser();
            MailAuditLogRequest request = MailAuditLogRequest.builder()
                    .tenantIds(List.of(user.TenantId))
                    .body(bodyEmail)
                    .userId(user.getId())
                    .sentBy(user.getUsername())
                    .subject(subject)
                    .to(toEmailsList)
                    .emailFrom(UserContext.getUser().Email)
                    .cc(ccEmailsList)
                    .moduleName(getModuleType(module))
                    .item(item)
                    .sentTime(LocalDateTime.now())
                    .emailAck((responseEntity != null && responseEntity.getBody() != null)
                            ? responseEntity.getBody().getAcknowledgementId() : null)
                    .build();
            reportServiceAdapter.postRequest(request, null);
        } catch (Exception ex) {
            log.error(ex.getMessage());
            log.error("CANNOT SEND MAIL AUDIT LOG REQUEST");
        }
        return responseEntity.getBody();
    }

    private String getModuleType(String module) {
        Pattern pattern = Pattern.compile("\\[(.+)\\]", Pattern.CASE_INSENSITIVE);

        if (module.contains("[dbo].")) {
            module = module.replace("[dbo].", "");
        }

        Matcher matcher = pattern.matcher(module);

        if (matcher.find()) {
            return matcher.group(1);
        }

        return module;
    }

    private List<String> getEmailsListFromString(String emailString) {
        List<String> emailsList = new ArrayList<>();
        if (emailString == null)
            return emailsList;
        String emailRegex = "^[a-zA-Z0-9+_.-]+@[a-zA-Z0-9.-]+$";
        for (String subAddress : emailString.split(",")) {
            String trimmedAddress = subAddress.trim();

            if (!Pattern.matches(emailRegex, trimmedAddress)) {
                throw new ValidationException("Some Email ID(s) are incorrect. Please enter comma(,) as a delimiter for entering multiple email IDs");
            }

            emailsList.add(trimmedAddress);
        }
        return emailsList;
    }

    public Object getLogs(GetLogsRequest request) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(notificationConfig.getNotificationApiKeyHeader(), notificationConfig.getNotificationApiKeyValue());
        headers.set(notificationConfig.getApiKeyVersionHeader(), notificationConfig.getApiKeyVersion());

        HttpEntity<MultiValueMap<String, Object>> requestEntity = new HttpEntity<>(headers);

        String tagsJson = jsonHelper.convertToJson(request.getTagsDataList());

        String url = UriComponentsBuilder.fromHttpUrl(notificationConfig.getNotificationBaseUrl() + notificationConfig.getGetLogs())
                .queryParam("_page", request.getPage())
                .queryParam("_limit", request.getLimit())
                .queryParam("_sort", request.getSort())
                .queryParam("_order", request.getOrder())
                .queryParam("_type", request.getType())
                .toUriString();

        url = url + "&tags=" + URLEncoder.encode(tagsJson, StandardCharsets.UTF_8);

        ResponseEntity<Object> responseBody = restTemplate.exchange(
                URI.create(url),
                HttpMethod.GET,
                requestEntity,
                new ParameterizedTypeReference<>() {}
        );
        return responseBody.getBody();
    }

    public Object createTags(CreateTagsRequest request) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(notificationConfig.getNotificationApiKeyHeader(), notificationConfig.getNotificationApiKeyValue());
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<Object> requestEntity = new HttpEntity<>(jsonHelper.convertToJson(request), headers);

        ResponseEntity<Object> responseBody = restTemplate.exchange(
                notificationConfig.getNotificationBaseUrl() + notificationConfig.getCreateTags(),
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {}
        );
        return responseBody.getBody();
    }
}
