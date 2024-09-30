package com.dpw.runner.booking.services.notification.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
@Getter
@Setter
public class NotificationConfig {
    @Value("${notification.baseUrl}")
    private String notificationBaseUrl;
    @Value("${notification.apiKeyHeader:default}")
    private String notificationApiKeyHeader;
    @Value("${notification.apiKeyValue:default}")
    private String notificationApiKeyValue;
    @Value("${notification.sendEmail:default}")
    private String sendEmail;
    @Value("${notification.applicationId:default}")
    private String applicationId;
    @Value("${notification.organizationId:default}")
    private String organizationId;
    @Value("${notification.emailFrom:default}")
    private String emailFrom;
}
