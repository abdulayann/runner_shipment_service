package com.dpw.runner.shipment.services.notification.request;

import lombok.Getter;
import lombok.Setter;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;

@Getter
@Setter
public class SendEmailBaseRequest implements Serializable {
    private String to;
    private String cc;
    private String bcc;
    private String subject;
    private String htmlBody;
    private String templateName;
    private MultipartFile file;
    private String moduleName;
    private String item;
    private String userName;
    private String branchId;
    private String userId;
}
