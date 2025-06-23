package com.dpw.runner.shipment.services.notification.request;

import lombok.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@SuppressWarnings("java:S1948")
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
    private List<TagsData> tags;
    private List<Object> attachments;
    private Boolean sendMeCopy;
}
