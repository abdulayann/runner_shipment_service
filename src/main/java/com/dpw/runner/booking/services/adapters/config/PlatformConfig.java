package com.dpw.runner.booking.services.adapters.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Configuration
public class PlatformConfig {

    @Value("${platform.api.key}")
    private String apiKey;

    @Value("${platform.client_id}")
    private String clientId;

    @Bean
    public RestTemplate restTemplateForPlatform() {
        RestTemplate restTemplate = new RestTemplate();
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.set("X-Api-Key", apiKey);
            headers.set("client_id", clientId);
            headers.setContentType(MediaType.APPLICATION_JSON);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

}
