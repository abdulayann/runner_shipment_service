package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;

import java.time.LocalDateTime;
import java.util.Map;

public class PackingConstants {
    private PackingConstants(){}
    public static final String PACKING_API_HANDLE = "/api/v2/packing";
    public static final String PACKING_V3_API_HANDLE = "/api/v3/packing";
    public static final String PACKING_CREATE_SUCCESSFUL = "Successful Packing Data Creation";
    public static final String PACKING_UPDATE_SUCCESSFUL = "Successful Packing Data Update";
    public static final String PACKING_LIST_SUCCESSFUL = "Successful Packing Data List Retrieval";
    public static final String PACKING_SUMMARY_SUCCESSFUL = "Successful Retrieved Packing Summary";
    public static final String PACKING_DELETE_SUCCESSFUL = "Successful Packing Delete";
    public static final String NO_DATA = "Not Found!";
    public static final String PACKING_ID = "Packing Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String FLASH_POINT_INVALID_ERROR =  "FlashPoint is invalid at row: ";
    public static final String PACKS_NOT_VALID = "Packs is not valid at row: ";
    public static final String FAILURE_EXECUTING_REQUEST = "failure executing request ";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Packing Data Retrieval By Id";
    public static final String PACKING_RETRIEVE_BY_ID_ERROR = "Packing is null for Id {} with Request Id {}";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String PKG = "PKG";
    public static final Map<String, RunnerEntityMapping> TABLES_NAMES = Map.ofEntries(
            Map.entry(Constants.PACKS_TYPE, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.PACKS_TYPE).isContainsText(true).build()),
            Map.entry(Constants.COMMODITY_GROUP, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.COMMODITY_GROUP).isContainsText(true).build()),
            Map.entry(Constants.COMMODITY, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.COMMODITY).isContainsText(true).build()),
            Map.entry(Constants.HS_CODE, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.HS_CODE).isContainsText(true).build()),
            Map.entry(Constants.GOODS_DESCRIPTION, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.GOODS_DESCRIPTION).isContainsText(true).build()),
            Map.entry(Constants.HANDLING_INFO, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.HANDLING_INFO).isContainsText(true).build()),
            Map.entry(Constants.MARKS_N_NUMBERS, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.MARKS_N_NUMBERS).isContainsText(true).build()),
            Map.entry(Constants.PROPER_SHIPPING_NAME, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.PROPER_SHIPPING_NAME).isContainsText(true).build()),
            Map.entry(Constants.UN_NUMBER, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.UN_NUMBER).isContainsText(true).build()),
            Map.entry(Constants.PACKING_GROUP, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(String.class).fieldName(Constants.PACKING_GROUP).isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(Long.class).fieldName(Constants.SHIPMENT_ID).build()),
            Map.entry(Constants.CONSOLIDATION_ID, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(Long.class).fieldName(Constants.CONSOLIDATION_ID).build()),
            Map.entry(Constants.CONTAINER_ID, RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(Long.class).fieldName(Constants.CONTAINER_ID).build()),
            Map.entry("bookingId", RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(Long.class).fieldName("bookingId").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.PACKING_LC).dataType(LocalDateTime.class).fieldName("updatedAt").build())
    );
}
