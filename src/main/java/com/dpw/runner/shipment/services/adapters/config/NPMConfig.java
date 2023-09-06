package com.dpw.runner.shipment.services.adapters.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

@Configuration
public class NPMConfig {

    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;

    @Value("${npm.exchange.rate.api.key}")
    private String xApiKey;

    @Bean
    public RestTemplate restTemplateForNPM() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-api-key-v2", xApikeyV2);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

    @Bean
    public RestTemplate restTemplateForExchangeRates() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-api-key", xApiKey);
            return execution.execute(request, body);
        });
        return restTemplate;
    }
}
