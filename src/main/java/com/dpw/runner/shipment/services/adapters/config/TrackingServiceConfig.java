package com.dpw.runner.shipment.services.adapters.config;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

@Data
@Configuration
public class TrackingServiceConfig {

    private final RestTemplate restTemplate;
    @Value("${events-message-topic}")
    private String eventsMessageTopic;
    @Value("${runner-flow-topic}")
    private String runnerFlowMessageTopic;

    public TrackingServiceConfig() {
        HttpComponentsClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory();
        requestFactory.setConnectTimeout(5000);
        requestFactory.setReadTimeout(5000);
        restTemplate = new RestTemplate(requestFactory);
    }

    @Bean
    public RestTemplate restTemplateForTrackingService() {
        return restTemplate;
    }

}
