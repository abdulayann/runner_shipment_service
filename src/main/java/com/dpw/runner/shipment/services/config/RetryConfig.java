package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.support.RetryTemplate;

@Configuration
@Generated
public class RetryConfig {

    private RetryTemplate retryTemplate;

    @Bean
    public RetryTemplate getRetryTemplate() {
        retryTemplate = RetryTemplate.builder()
                .maxAttempts(3)
                .exponentialBackoff(1000, 2, 5000)
                .retryOn(Exception.class)
                .build();

        return retryTemplate;
    }

}
