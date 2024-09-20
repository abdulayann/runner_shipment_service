package com.dpw.runner.shipment.services.adapters.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

@Configuration
public class CRPConfig {

    @Value("${crp.api.key}")
    private String apiKey;

    @Value("${crp.client.type.code}")
    private String clientTypeCode;

    @Value("${crp.service.provider.code}")
    private String serviceProviderCode;

    @Value("${crp.service.company.ref}")
    private String companyRef;

    @Bean
    public RestTemplate restTemplateForCRP() {
        RestTemplate restTemplate = new RestTemplate();
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.set("X-API-KEY", apiKey);
            headers.set("X-COMPANY-REFERENCE", companyRef);
            headers.set("X-CLIENT-TYPE-CODE", clientTypeCode);
            headers.set("X-SERVICE-PROVIDER-CODE", serviceProviderCode);
            headers.setContentType(MediaType.APPLICATION_JSON);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

}
