package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.web.client.RestTemplate;

import java.util.TimeZone;


@SpringBootApplication(scanBasePackages = "com.dpw.runner.shipment.services")
@Slf4j
@EnableCaching(proxyTargetClass = true)
@EnableKafka
@Generated
@ComponentScan(value={"com.dpw.runner.shipment.services", "com.dpw.api"})
public class RunnerShipmentServicesApplication {

    public static void main(String[] args) {
        log.info("Deploying Release-v3.1.0 from feature/310-working");
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
        SpringApplication.run(RunnerShipmentServicesApplication.class, args);
        log.info("--------==========Shipment Service Started==========--------");
        

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
        modelMapper.typeMap(CarrierDetails.class, CustomShipmentSyncRequest.class)
                .addMappings(mp -> mp.skip(CustomShipmentSyncRequest::setDestination));
        return modelMapper;
    }

}






