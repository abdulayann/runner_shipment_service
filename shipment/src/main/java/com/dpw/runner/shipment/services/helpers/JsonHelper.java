package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.objectMapperMixin.ShipmentMixIn;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeDeserializer;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.extern.slf4j.Slf4j;
import org.openapitools.jackson.nullable.JsonNullableModule;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.json.JsonParseException;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

@Component
@Slf4j
@Generated
public class JsonHelper {

    @Autowired
    private ObjectMapper mapper;


    private ObjectMapper mapper1 = new ObjectMapper();

    private ObjectMapper createMapper = new ObjectMapper();

    private final ObjectMapper mapper2 = new ObjectMapper();

    private ObjectMapper platformMapper = new ObjectMapper();

    @PostConstruct
    public void intializeMapper() {
        createMapper.registerModule(new JavaTimeModule());
        createMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        createMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        createMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        createMapper.addMixIn(ShipmentDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(Parties.class, ShipmentMixIn.class);
        createMapper.addMixIn(AdditionalDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(Containers.class, ShipmentMixIn.class);
        createMapper.addMixIn(CarrierDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(ELDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(Events.class, ShipmentMixIn.class);
        createMapper.addMixIn(FileRepo.class, ShipmentMixIn.class);
        createMapper.addMixIn(Packing.class, ShipmentMixIn.class);
        createMapper.addMixIn(ReferenceNumbers.class, ShipmentMixIn.class);
        createMapper.addMixIn(Routings.class, ShipmentMixIn.class);
        createMapper.addMixIn(ServiceDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(TruckDriverDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(PickupDeliveryDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(Jobs.class, ShipmentMixIn.class);
        createMapper.addMixIn(ConsolidationDetails.class, ShipmentMixIn.class);
        createMapper.addMixIn(BookingCarriage.class, ShipmentMixIn.class);
        createMapper.addMixIn(Notes.class, ShipmentMixIn.class);
        createMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        mapper.configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, false);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        platformMapper.setSerializationInclusion(JsonInclude.Include.ALWAYS);
        platformMapper.registerModule(new JavaTimeModule());
        platformMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        platformMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        platformMapper.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        platformMapper.configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, false);
        platformMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        SimpleModule module = new SimpleModule();
        module.addDeserializer(LocalDateTime.class,  new CustomLocalDateTimeDeserializer());
        mapper2.registerModule(new JsonNullableModule());
        mapper2.registerModule(new JavaTimeModule());
        mapper2.registerModule(module);
        mapper2.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        mapper2.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper2.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        mapper2.configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, false);
        mapper2.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    }

    public <T> T readFromJson(String jsonString, Class<T> clazz) {
        try {
            return mapper.readValue(jsonString, clazz);
        } catch (JsonProcessingException e) {
            log.error("Failed to Parse given Json " + jsonString);
            log.info("Exception thrown while parsing json: {}", e.toString());
            throw new JsonParseException(e);
        }
    }

    public <T> String convertToJson(T object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            log.error("Failed Parsed Object: {}", object.toString());
            log.error("Failed to Parse given Json: " + e.getMessage());
            log.info("Exception thrown while parsing json: {}", e.toString());
            throw new JsonParseException(e);
        }
    }

    public <T> String convertToJsonWithNulls(T object) {
        try {
            return platformMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new JsonParseException(e);
        }
    }

    public <T> String convertToJsonWithDateTimeFormatter(T object, DateTimeFormatter dateTimeFormatter) {
        try {
            ObjectMapper dateFormatMapper = new ObjectMapper();
            dateFormatMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
            dateFormatMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
            dateFormatMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
            JavaTimeModule javaTimeModule = new JavaTimeModule();
            LocalDateTimeSerializer localDateTimeSerializer = new LocalDateTimeSerializer(dateTimeFormatter);
            javaTimeModule.addSerializer(LocalDateTime.class, localDateTimeSerializer);
            dateFormatMapper.registerModule(javaTimeModule);
            return dateFormatMapper.writeValueAsString(object);
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

    public <T,F> F convertValueWithJsonNullable(T object, Class<F> clazz) {
        return mapper2.convertValue(object, clazz);
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

    public <T, F> F convertValue(T fromValue, TypeReference<F> toValueTypeRef) {
        return mapper.convertValue(fromValue, toValueTypeRef);
    }

    public <T,F> F convertCreateValue(T object, Class<F> clazz) {
        return createMapper.convertValue(object, clazz);
    }

}
