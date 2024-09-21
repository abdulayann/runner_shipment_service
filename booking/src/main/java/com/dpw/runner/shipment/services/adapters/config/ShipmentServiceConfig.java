package com.dpw.runner.shipment.services.adapters.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "shipment")
@Data
public class ShipmentServiceConfig {
    private String baseUrl;
    private String xApiKey;
    private String createShipmentInV2Url;
    private String getByGuidUrl;
}
