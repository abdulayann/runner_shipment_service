package com.dpw.runner.shipment.services.adapters.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDateTime;

@Component
@ConfigurationProperties(prefix = "shipment")
@Data
public class ShipmentServiceConfig {
    private String baseUrl;
    private String xApiKey;
    private String createShipmentInV2Url;

    @Bean
    public RestTemplate restTemplateForShipment() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());

        // Create a custom ObjectMapper to handle LocalDateTime serialization
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());  // Handle LocalDateTime

        objectMapper.configOverride(LocalDateTime.class)
                .setFormat(JsonFormat.Value.forPattern("yyyy-MM-dd HH:mm:ss"));  // Example format, adjust if necessary

        // Create a message converter with the custom ObjectMapper
        MappingJackson2HttpMessageConverter messageConverter = new MappingJackson2HttpMessageConverter(objectMapper);

        // Replace the default converters with the customized one
        restTemplate.getMessageConverters().add(0, messageConverter);

        return restTemplate;

    }
}
