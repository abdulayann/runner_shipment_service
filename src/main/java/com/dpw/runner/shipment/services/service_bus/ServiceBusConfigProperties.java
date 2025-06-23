package com.dpw.runner.shipment.services.service_bus;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@Data
@ConfigurationProperties(prefix = "servicebus")
public class ServiceBusConfigProperties {

    private TrackingService trackingService;

    @Data
    public static class TrackingService {
        private String connectionString;
        private String topicName;
        private String subscriptionName;
    }

}
