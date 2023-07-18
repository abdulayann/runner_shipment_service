package com.dpw.runner.shipment.services.helpers;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.json.JsonParseException;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class JsonHelper {

    @Autowired
    private ObjectMapper mapper;

    public <T> T readFromJson(String jsonString, Class<T> clazz) {
        try {
            return mapper.readValue(jsonString, clazz);
        } catch (JsonProcessingException e) {
            log.error("Failed to Parse given Json " + jsonString);
            throw new JsonParseException(e);
        }
    }

    public <T> String convertToJson(T object) {
        try {
            mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            log.error("Failed to Parse given Json");
            throw new JsonParseException(e);
        }
    }

    public <T,F> F convertValue(T object, Class<F> clazz) {
        return mapper.convertValue(object, clazz);
    }

}
