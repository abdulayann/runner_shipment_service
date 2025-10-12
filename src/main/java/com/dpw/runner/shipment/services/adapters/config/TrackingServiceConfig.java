package com.dpw.runner.shipment.services.adapters.config;

import lombok.Data;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.core5.util.Timeout;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

@Data
@Configuration
public class TrackingServiceConfig {

    @Value("${events-message-topic}")
    private String eventsMessageTopic;
    @Value("${runner-flow-topic}")
    private String runnerFlowMessageTopic;
    private final RestTemplate restTemplate;

    public TrackingServiceConfig() {

        RequestConfig requestConfig = RequestConfig.custom()
                .setConnectTimeout(Timeout.ofMilliseconds(5000))
                .setResponseTimeout(Timeout.ofMilliseconds(5000))
                .build();

        CloseableHttpClient httpClient = HttpClientBuilder.create()
                .setDefaultRequestConfig(requestConfig)
                .build();

        HttpComponentsClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(httpClient);
        restTemplate = new RestTemplate(requestFactory);
    }

    @Bean
    public RestTemplate restTemplateForTrackingService() {
        return restTemplate;
    }

}
