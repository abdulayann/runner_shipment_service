package com.dpw.runner.shipment.services.adapters.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Configuration
public class NPMConfig {

    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;

    @Bean
    public RestTemplate restTemplateForNPM() {
        RestTemplate restTemplate = new RestTemplate();
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-api-key-v2", xApikeyV2);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

}
