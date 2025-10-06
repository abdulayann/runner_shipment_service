package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public class ShippingInstructionsConstants {

    private ShippingInstructionsConstants() {
    }

    public static final String SI_API_HANDLE = "/api/v3/si";
    public static final String CREATE_SUCCESSFUL = "Successful Shipping Instruction Creation";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Shipping Instruction Retrieval By Id";
    public static final String DELETE_SUCCESSFUL = "Successful Shipment Instruction Delete";
    public static final String UPDATE_SUCCESSFUL = "Successful Shipment Instruction Update";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String DATA_NOT_FOUND = "Shipping Instructions Not Found {}";
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of("freightDetailList", "commonPackagesList", "commonContainersList", "referenceNumbers");
    public static final String PAYMENT_TERM_PREPAID = "PPD";
    public static final String PAYMENT_TERM_COLLECT = "CCX";
    public static final String RETRIEVE_DEFAULT_SUCCESS = "Successful Default Shipping Instruction Retrieval";
    public static final String SUBMIT_SUCCESSFUL = "Shipping Instruction submitted successfully.";
    public static final String AMEND_SUCCESSFUL = "Shipping Instruction amend successful.";
    public static final String INVALID_ENTITY_TYPE = "Invalid value of Shipping Instruction Type";
    public static final String SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES = "SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES";
    public static final String SI_TABLE = "ShippingInstruction";
    public static final String SAILING_INFORMATION = "sailingInformation";
    public static final String VERIFIED_GROSS_MASS = "verifiedGrossMass";
    public static final String STATUS = "status";
    public static final String CANCELLED = "cancelled";
    public static final String SI_LIST_RESPONSE_SUCCESS = "SI list from db retrieved successfully for Request Id : {}";
    public static final String SI_LIST_REQUEST_EMPTY_ERROR = "Request is empty for SI list with Request Id {}";
    public static final String SI_LIST_REQUEST_NULL_ERROR = "SI List Request is Null";
    public static final String SHIPPING_INSTRUCTION = "SHIPPING_INSTRUCTION";
    public static final String APERAK_PREFIX = "APERAK_";
    public static final String CONTRLX_PREFIX = "CONTRLX_";
    public static final String XML_SUFFIX = ".xml";
    public static final String ACCEPTED = "Accepted";
    public static final String REJECTED = "Rejected";
    public static final String REJECTED_BY_CARRIER = "RejectedByCarrier";
    public static final String PROCESSED = "Processed";
    public static final String SHIPPING_INSTRUCTION_EMAIL_TEMPLATE = "SHIPPING_INSTRUCTION";
    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry(STATUS, RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(ShippingInstructionStatus.class)   // CarrierBookingStatus is an enum stored as string
                    .fieldName(STATUS)
                    .build()),

            Map.entry("carrierBookingNo", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBookingNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierBlNo", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBlNo")
                    .isContainsText(true)
                    .build()),
            Map.entry("shippingInstructionType", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(ShippingInstructionType.class)
                    .fieldName("shippingInstructionType")
                    .build()),
            Map.entry("entityType", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(EntityType.class)
                    .fieldName("entityType")
                    .build()),
            Map.entry("entityId", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(Long.class)
                    .fieldName("entityId")
                    .build()),

            Map.entry("entityNumber", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(String.class)
                    .fieldName("entityNumber")
                    .isContainsText(true)
                    .build()),

            Map.entry("serviceType", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(String.class)
                    .fieldName("serviceType")
                    .isContainsText(true)
                    .build()),

            Map.entry("updatedAt", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("updatedAt")
                    .isContainsText(false)
                    .build()),
            Map.entry("createdAt", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("createdAt")
                    .isContainsText(false)
                    .build()),
            Map.entry("createdBy", RunnerEntityMapping.builder()
                    .tableName(SI_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("createdBy")
                    .isContainsText(false)
                    .build()),
            Map.entry("verifiedGrossMassCutoff", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("verifiedGrossMassCutoff")
                    .isContainsText(true)
                    .build()),
            Map.entry("carrier", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("carrier")
                    .isContainsText(true)
                    .build()),
            Map.entry("pol", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("pol")
                    .isContainsText(true)
                    .build()),
            Map.entry("pod", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("pod")
                    .isContainsText(true)
                    .build()),
            Map.entry("siCutoff", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(LocalDateTime.class)
                    .fieldName("shipInstructionCutoff")
                    .isContainsText(false)
                    .build()),
            Map.entry("vgmCutoff", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(LocalDateTime.class)
                    .fieldName("verifiedGrossMassCutoff")
                    .isContainsText(false)
                    .build()),
            Map.entry(Constants.CONSIGNEE_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.SHIPPER_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.SHIPPER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build())

    );

}
