package com.dpw.runner.booking.services.adapters.config;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.RequestAuthContext;
import lombok.Data;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Data
@Configuration
public class ReportServiceConfig {

    @Bean
    public RestTemplate restTemplateForReportService() {
        RestTemplate restTemplate = new RestTemplate();

        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.set("Authorization", "Bearer " + RequestAuthContext.getAuthToken());
            headers.set("X-Client-Type", "STOS");
            headers.setContentType(MediaType.APPLICATION_JSON);
            return execution.execute(request, body);
        });
        return restTemplate;
    }
}
