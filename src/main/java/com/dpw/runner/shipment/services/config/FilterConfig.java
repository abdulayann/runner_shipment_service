package com.dpw.runner.shipment.services.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "user.authservice")
public class FilterConfig {
    private String name;
}
