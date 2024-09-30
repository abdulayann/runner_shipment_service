package com.dpw.runner.booking.services.notification.request;

import lombok.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;

@Getter
@Setter
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
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
