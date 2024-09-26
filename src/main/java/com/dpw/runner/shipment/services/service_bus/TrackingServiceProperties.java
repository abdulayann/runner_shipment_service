package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
public class TrackingServiceProperties {
    @Value("${tracking-push-connection-string}")
    private String connectionString;
    @Value("${tracking-push-subscription-name}")
    private String subName;
    @Value("${tracking-push-topic-name}")
    private String topicName;
}
