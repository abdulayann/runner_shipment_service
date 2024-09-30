package com.dpw.runner.booking.services.adapters.config;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.booking.services.commons.constants.ApiConstants;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Configuration
public class MDMServiceConfig {

    @Value("${mdm.api.key}")
    private String apiKey;

    @Bean
    public RestTemplate restTemplateForMDM() {
        RestTemplate restTemplate = new RestTemplate();
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.set(ApiConstants.X_API_KEY, apiKey);
            headers.add("Authorization", RequestAuthContext.getAuthToken());
            headers.setContentType(MediaType.APPLICATION_JSON);
            return execution.execute(request, body);
        });

        return restTemplate;
    }
}
