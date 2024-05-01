package com.dpw.runner.shipment.services.helper;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeDeserializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.entity.*;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.json.JsonParseException;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;


@Slf4j
public class JsonTestUtility {
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private String path = "src/test/java/com/dpw/runner/shipment/services/helper/payload.json";
    private Map<String, Object> payload;

    static {
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addDeserializer(LocalDateTime.class, new CustomLocalDateTimeDeserializer());
        objectMapper.registerModule(javaTimeModule);
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        objectMapper.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }
    public static ObjectMapper getMapper() {
        return objectMapper;
    }

    public JsonTestUtility() throws IOException {
        payload = objectMapper.readValue(new File(path), Map.class);
    }

    public Packing getTestPacking(){
        return objectMapper.convertValue(payload.get("PACKING") , Packing.class);
    }

    public List<Packing> getTestPackingList(){
        return convertValueToList(payload.get("PACKING_LIST"), Packing.class);
    }

    public PackSummaryResponse getTestPackSummaryResponse(){
        return objectMapper.convertValue(payload.get("PACK_SUMMARY_RESPONSE"), PackSummaryResponse.class);
    }

    public PackSummaryResponse getTestPackSummaryAirResponse(){
        return objectMapper.convertValue(payload.get("PACK_SUMMARY_RESPONSE_AIR"), PackSummaryResponse.class);
    }

    public VolumeWeightChargeable getTestVolWtChargeable(){
        return objectMapper.convertValue(payload.get("VOL_WT_CHARGEABLE"), VolumeWeightChargeable.class);
    }

    public ShipmentDetails getTestShipment() {
        ShipmentDetails shipmentDetails = objectMapper.convertValue(payload.get("NEW_SHIPMENT"), ShipmentDetails.class);
        return shipmentDetails;
    }

    public Containers getTestContainer() {
        return objectMapper.convertValue(payload.get("NEW_CONTAINER_CREATE"), Containers.class);
    }

    public ConsolidationDetails getTestConsolidation(){
        ConsolidationDetails consolidationDetails = objectMapper.convertValue(payload.get("CONSOLIDATION"), ConsolidationDetails.class);
        return consolidationDetails;
    }

    public ConsolidationDetails getTestNewConsolidation(){
        return objectMapper.convertValue(payload.get("NEW_CONSOLIDATION_CREATE"), ConsolidationDetails.class);
    }

    public Awb getTestHawb() {
        Awb awb = objectMapper.convertValue(payload.get("HAWB"), Awb.class);
        return awb;
    }

    public Awb getTestDmawb() {
        Awb awb = objectMapper.convertValue(payload.get("DMAWB"), Awb.class);
        return awb;
    }

    public Awb getTestMawb() {
        Awb awb = objectMapper.convertValue(payload.get("MAWB"), Awb.class);
        return awb;
    }

    public <T> T getJson(String key, Class<T> clazz) {
        return objectMapper.convertValue(payload.get(key), clazz);
    }

    public <T,F> List<F> convertValueToList(T object, Class<F> clazz) {
        return objectMapper.convertValue(object, objectMapper.getTypeFactory().constructCollectionType(List.class, clazz));
    }

    public <T> T getCopyObject(T t, Class<T> clazz) throws JsonProcessingException {
        String json = convertToJson(t);
        return objectMapper.readValue(json, clazz);
    }

    public <T> String convertToJson(T object) {
        try {
            return objectMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            log.error("Failed Parsed Object: {}", object.toString());
            log.error("Failed to Parse given Json: " + e.getMessage());
            log.info("Exception thrown while parsing json: {}", e.toString());
            throw new JsonParseException(e);
        }
    }

}
