package com.dpw.runner.shipment.services.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.openapitools.jackson.nullable.JsonNullableModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.time.LocalDateTime;

@Configuration
public class JacksonConfig {

  @Bean
  Jackson2ObjectMapperBuilder objectMapperBuilder() {
    Jackson2ObjectMapperBuilder builder = new Jackson2ObjectMapperBuilder();
    builder.serializationInclusion(JsonInclude.Include.ALWAYS)
            .featuresToDisable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
            .deserializerByType(LocalDateTime.class,  new CustomLocalDateTimeDeserializer())
            .modulesToInstall(new JsonNullableModule());
    return builder;
  }



  @Bean
  public ObjectMapper objectMapper() {
    ObjectMapper objectMapper = new ObjectMapper();
    objectMapper. configure(DeserializationFeature. FAIL_ON_UNKNOWN_PROPERTIES, false);
    objectMapper. configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
    objectMapper.registerModule(new JavaTimeModule());
    return objectMapper;
  }
}