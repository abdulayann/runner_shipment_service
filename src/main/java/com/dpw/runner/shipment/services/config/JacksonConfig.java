package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.openapitools.jackson.nullable.JsonNullableModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.time.LocalDateTime;

@Configuration
@Generated
public class JacksonConfig {

    @Bean
    Jackson2ObjectMapperBuilder objectMapperBuilder() {
        Jackson2ObjectMapperBuilder builder = new Jackson2ObjectMapperBuilder();
        builder.serializationInclusion(JsonInclude.Include.ALWAYS)
                .featuresToDisable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
                .deserializerByType(LocalDateTime.class, new CustomLocalDateTimeDeserializer())
                .modulesToInstall(new JsonNullableModule());
        return builder;
    }

}