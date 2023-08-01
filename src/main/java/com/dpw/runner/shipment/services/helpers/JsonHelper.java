package com.dpw.runner.shipment.services.helpers;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.json.JsonParseException;
import org.springframework.stereotype.Component;

import java.util.List;

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
            mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            log.error("Failed to Parse given Json");
            throw new JsonParseException(e);
        }
    }

    public <T,F> F convertValue(T object, Class<F> clazz) {
        return mapper.convertValue(object, clazz);
    }

    public <T,F> List<F> convertValueToList(T object, Class<F> clazz) {
        return mapper.convertValue(object, mapper.getTypeFactory().constructCollectionType(List.class, clazz));
    }

}
