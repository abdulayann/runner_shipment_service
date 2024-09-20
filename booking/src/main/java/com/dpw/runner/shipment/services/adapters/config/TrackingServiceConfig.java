package com.dpw.runner.shipment.services.adapters.config;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
public class TrackingServiceConfig {

    @Value("${events-message-topic}")
    private String eventsMessageTopic;
    @Value("${runner-flow-topic}")
    private String runnerFlowMessageTopic;

}
