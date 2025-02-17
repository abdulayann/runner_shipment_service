package com.dpw.runner.shipment.services.adapters.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Configuration
public class FusionConfig {

    @Value("${fusion.P100.username}")
    private String usernameP100;
    @Value("${fusion.P100.password}")
    private String passwordP100;

    @Bean
    public RestTemplate restTemplateForCreditCheckP100() {

        RestTemplate restTemplate = new RestTemplate();
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setBasicAuth(usernameP100, passwordP100);
            headers.setContentType(MediaType.APPLICATION_JSON);
            return execution.execute(request, body);
        });

        return restTemplate;
    }

}
