package com.dpw.runner.shipment.services.helpers;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import org.springframework.stereotype.Component;

@Component
public class MapperHelper {
    private final ObjectMapper objectMapper = new ObjectMapper();

    public ObjectMapper getMapper(){
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return objectMapper;
    }
}
