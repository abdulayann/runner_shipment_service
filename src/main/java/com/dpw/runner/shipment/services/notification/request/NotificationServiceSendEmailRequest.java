package com.dpw.runner.shipment.services.notification.request;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@SuppressWarnings("java:S1948")
public class NotificationServiceSendEmailRequest implements Serializable {
    private String templateName;
    private String organizationId;
    private String applicationId;
    private String recipientEmails;
    private String ccEmails;
    private String bccEmails;
    private String priority = "high";
    private Boolean trackEmailEvents = true;
    @JsonIgnore
    private MultipartFile files;
    private String metadata;
    private String htmlBody;
    private String item;
    private String moduleName;
    private String subject;
    private String tags;
    private List<Object> attachments;
}
