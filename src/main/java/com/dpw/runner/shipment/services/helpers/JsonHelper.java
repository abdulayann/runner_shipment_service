package com.dpw.runner.shipment.services.helpers;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.json.JsonParseException;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@Slf4j
public class JsonHelper {

    @Autowired
    private ObjectMapper mapper;

    private ObjectMapper mapper1 = new ObjectMapper();

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

    public <T> String convertToJsonIncludeNulls(T object) {
        try {
            mapper1.registerModule(new JavaTimeModule());
            mapper1.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
            return mapper1.writeValueAsString(object);
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

    public Map<String, Object> convertJsonToMap(String json) {
        Map<String, Object> map = null;
        try {
            map = mapper.readValue(json, Map.class);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        return map;
    }

}
