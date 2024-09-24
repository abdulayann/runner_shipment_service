package com.dpw.runner.shipment.services.adapters.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.Collections;

@Component
@ConfigurationProperties(prefix = "shipment")
@Data
public class ShipmentServiceConfig {
    private String baseUrl;
    private String xApiKey;
    private String createShipmentInV2Url;
    private String getByGuidUrl;
    private String createConsolidationFromBookingUrl;

    @Bean
    public RestTemplate restTemplateForShipmentService() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            headers.add("Authorization", RequestAuthContext.getAuthToken());
            return execution.execute(request, body);
        });
        return restTemplate;
    }
}
