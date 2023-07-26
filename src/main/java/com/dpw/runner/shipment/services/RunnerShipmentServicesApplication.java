package com.dpw.runner.shipment.services;

import io.micrometer.core.instrument.MeterRegistry;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.actuate.autoconfigure.metrics.MeterRegistryCustomizer;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.annotation.Bean;
import org.springframework.web.client.RestTemplate;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

@SpringBootApplication(scanBasePackages = "com.dpw.runner.shipment.services")
@EnableDiscoveryClient
@Slf4j
@EnableSwagger2
public class RunnerShipmentServicesApplication {

    public static void main(String[] args) {
        SpringApplication.run(RunnerShipmentServicesApplication.class, args);
        log.info("Shipment Service Started........");

    }


    @SuppressWarnings("rawtypes")
    @Bean
    MeterRegistryCustomizer CustomsMeterRegistryCustomizer(MeterRegistry meterRegistry) {
        return registry -> meterRegistry.config().commonTags("application", "runner-shipment-service");
    }

    @Bean
    RestTemplate restTemplate() {
        return new RestTemplate();
    }

    @Bean
    public ModelMapper createModelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setAmbiguityIgnored(true);
        return modelMapper;
    }



}






