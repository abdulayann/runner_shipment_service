package com.dpw.runner.shipment.services;

import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.filter.CommonsRequestLoggingFilter;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import java.util.TimeZone;


@SpringBootApplication(scanBasePackages = "com.dpw.runner.shipment.services")
@Slf4j
@EnableSwagger2
@EnableCaching(proxyTargetClass = true)
@EnableKafka
public class RunnerShipmentServicesApplication {

    public static void main(String[] args) {
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
        SpringApplication.run(RunnerShipmentServicesApplication.class, args);
        log.info("Shipment Service Started........");

    }


    @Bean
    RestTemplate restTemplate() {
        return new RestTemplate();
    }

    @Bean
    public ModelMapper createModelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setFieldMatchingEnabled(true);
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        modelMapper.getConfiguration().setAmbiguityIgnored(true);
        return modelMapper;
    }

    @Bean
    public CommonsRequestLoggingFilter requestLoggingFilter() {
        CommonsRequestLoggingFilter loggingFilter = new CommonsRequestLoggingFilter();
        loggingFilter.setIncludePayload(true);
        loggingFilter.setMaxPayloadLength(64000);
        return loggingFilter;
    }

}






