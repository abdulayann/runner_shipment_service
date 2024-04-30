package com.dpw.runner.shipment.services.helper;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeDeserializer;
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

    public Allocations getTestAllocation() {
        return objectMapper.convertValue(payload.get("NEW_ALLOCATION_CREATE"), Allocations.class);
    }

    public AdditionalDetails getTestAdditionalDetails() {
        return objectMapper.convertValue(payload.get("NEW_ADDITIONAL_DETAIL") , AdditionalDetails.class);
    }

    public AchievedQuantities getTestAchievedQuantity() {
        return objectMapper.convertValue(payload.get("NEW_ACHIEVED_QUANTITY") , AchievedQuantities.class);
    }

    public ArrivalDepartureDetails getTestArrivalDepartureDetails() {
        return objectMapper.convertValue(payload.get("NEW_ARRIVAL_DEPART_DETAIL") , ArrivalDepartureDetails.class);
    }

    public Jobs getTestJob() {
        return objectMapper.convertValue(payload.get("NEW_JOB") , Jobs.class);
    }

    public MawbStocksLink getTestMawbStocksLink() {
        return objectMapper.convertValue(payload.get("MAWB_STOCK_LINK") , MawbStocksLink.class);
    }

    public MawbStocks getTestMawbStocks(){
        return objectMapper.convertValue(payload.get("MAWB_STOCK"), MawbStocks.class);
    }
    public MawbStocks getTestStockData() {
        return objectMapper.convertValue(payload.get("STOCK_DATA"), MawbStocks.class);
    }

    public IntegrationResponse getTestIntegrationResponse() {
        return objectMapper.convertValue(payload.get("INTEGRATION_PAYLOAD"), IntegrationResponse.class);
    }

    public ELDetails getTestELDetails() {
        return objectMapper.convertValue(payload.get("ELDETAILS") , ELDetails.class);
    }

    public Events getTestEventData() {
        return objectMapper.convertValue(payload.get("EVENT"), Events.class);
    }

    public FileRepo getTestFileRepoData() {
        return objectMapper.convertValue(payload.get("FILE_REPO"), FileRepo.class);
    }

    public AuditLog getTestAuditLog() {
        return objectMapper.convertValue(payload.get("AUDIT_LOG"), AuditLog.class);
    }

    public BookingCarriage getTestBookingCarriage() {
        return objectMapper.convertValue(payload.get("BOOKING_CARRIAGE") , BookingCarriage.class);
    }

    public CarrierDetails getTestCarrierDetails() {
        return objectMapper.convertValue(payload.get("CARRIER_DETAILS"), CarrierDetails.class);
    }

    public DefaultViews getTestDefaultView() {
        return objectMapper.convertValue(payload.get("DEFAULT_VIEWS"), DefaultViews.class);
    }

    public OrderNumber getTestOrderNumberDao() {
        return objectMapper.convertValue(payload.get("ORDER_NUMBER"), OrderNumber.class);
    }
}
