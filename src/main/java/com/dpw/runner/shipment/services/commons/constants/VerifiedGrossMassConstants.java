package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public class VerifiedGrossMassConstants {
    public static final String CR_BOOKING_ID = "ShipmentID";
    public static final String CARRIER_BL_NO = "BillOfLadingNumber";
    public static final String TRANSPORT_MODE_SEA = "Sea";
    public static final String TRANSPORT_MODE_ROAD = "Road";
    public static final String TRANSPORT_MODE_RAIL = "Rail";
    public static final String TRANSPORT_MODE_INLAND_WATER = "Inland";
    public static final String TRANSPORT_MODE_RAIL_WATER = "Rail Water";
    public static final String TRANSPORT_MODE_ROAD_WATER = "Road Water";
    public static final String FULL_DROP_OFF = "FullDropOFF";
    public static final String EMPTY_PICK_UP = "EmptyPickUp";
    public static final String CLOSING_DATE = "ClosingDate";
    public static final String EMPTY_PICKUP_DATE = "EmptyPickupDate";
    public static final String HAULAGE_PARTY = "haulageParty";
    public static final String INTTRA = "INTTRA";
    public static final String AMEND = "Amend";
    public static final String ORIGINAL = "Original";
    public static final String VGM_CREATE = "VGM_CREATE";
    public static final String VGM_AMEND = "VGM_AMEND";
    public static final String MAIN_CARRIAGE = "MainCarriage";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of("carrierRoutingList", "containersList");
    public static final String RETRIEVE_DEFAULT_SUCCESS = "Successful Default Verified Gross Mass Retrieval";
    public static final String VERIFIED_GROSS_MASS_BULK_UPDATE_SUCCESSFUL = "Bulk update successful";
    public static final String VERIFIED_GROSS_MASS = "VERIFIED_GROSS_MASS";

    private VerifiedGrossMassConstants() {
        // private constructor to prevent instantiation
    }

    public static final String VERIFIED_GROSS_MASS_API_HANDLE = "/api/v3/vgm";

    public static final String VERIFIED_GROSS_MASS_CREATE_SUCCESSFUL = "Successful Verified Gross Mass Data Creation";
    public static final String VERIFIED_GROSS_MASS_UPDATE_SUCCESSFUL = "Successful Verified Gross Mass Data Update";
    public static final String CARRIER_BOOKING_LIST_SUCCESSFUL = "Successful Verified Gross Mass Data List Retrieval";
    public static final String VERIFIED_GROSS_MASS_DELETE_SUCCESSFUL = "Successful Verified Gross Mass Delete";
    public static final String VERIFIED_GROSS_MASS_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Verified Gross Mass Data Retrieval By Id";
    public static final String VERIFIED_GROSS_MASS_SYNC_CONTAINERS_SUCCESSFUL = "Successful Verified Gross Mass Sync Containers From Consolidation";
    public static final String VERIFIED_GROSS_MASS_OPERATION_SUCCESSFUL = "Successful Verified Gross Mass Operation: ";

    public static final String CARRIER_BOOKING_ID = "Verified Gross Mass Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";

    public static final String CARRIER_BOOKING_RETRIEVE_ERROR = "Carrier Booking is null for Id {} with Request Id {}";
    public static final String VERIFIED_GROSS_MASS_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE = "Include Columns field is mandatory";
    public static final String VERIFIED_GROSS_MASS_LIST_REQUEST_EMPTY_ERROR = "Request is empty for Carrier list with Request Id {}";
    public static final String VERIFIED_GROSS_MASS_LIST_REQUEST_NULL_ERROR = "Carrier List Request is Null";
    public static final String VERIFIED_GROSS_MASS_LIST_RESPONSE_SUCCESS = "Carrier list from db retrieved successfully for Request Id : {}";
    public static final String GROSS_WEIGHT = "Gross Weight";
    public static final String VGM_WEIGHT = "VGM Weight";
    public static final String NET_WEIGHT = "Net Weight";
    public static final String TARE_WEIGHT = "Tare Weight";
    public static final String VERIFIED_GROSS_MASS_EMAIL_TEMPLATE = "VERIFIED_GROSS_MASS";

    private static final String VERIFIED_GROSS_MASS_TABLE = "VerifiedGrossMass";
    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("responsibleOrgCode", RunnerEntityMapping.builder()
                    .tableName("responsible").dataType(String.class)
                    .fieldName(Constants.ORG_CODE)
                    .isContainsText(true)
                    .build()),
            Map.entry("authorisedOrgCode", RunnerEntityMapping.builder()
                    .tableName("authorised")
                    .dataType(String.class)
                    .fieldName(Constants.ORG_CODE)
                    .isContainsText(true)
                    .build()),
            Map.entry("entityNumber", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)   // CarrierBookingStatus is an enum stored as string
                    .fieldName("entityNumber")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierBookingNo", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBookingNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierBlNo", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBlNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("entityType", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("entityType")
                    .isContainsText(true)
                    .build()),

            Map.entry("entityId", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(Long.class)
                    .fieldName("entityId")
                    .build()),

            Map.entry("status", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("status")
                    .isContainsText(true)
                    .build()),

            Map.entry("createByUserEmail", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("createByUserEmail")
                    .isContainsText(true)
                    .build()),

            Map.entry("updatedAt", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("updatedAt")
                    .isContainsText(false)
                    .build()),

            Map.entry("carrier", RunnerEntityMapping.builder()
                    .tableName(CarrierBookingConstants.SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("carrier")
                    .isContainsText(true)
                    .build()),

            Map.entry("vgmCutoff", RunnerEntityMapping.builder()
                    .tableName(CarrierBookingConstants.SAILING_INFORMATION)
                    .dataType(LocalDateTime.class)
                    .fieldName("verifiedGrossMassCutoff")
                    .isContainsText(false)
                    .build()),

            Map.entry("updatedBy", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("updatedBy")
                    .isContainsText(true)
                    .build()),

            Map.entry("createdBy", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(String.class)
                    .fieldName("createdBy")
                    .isContainsText(true)
                    .build()),
            Map.entry("createdAt", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("createdAt")
                    .isContainsText(false)
                    .build())
    );
    public static final List<String> serviceTypes = List.of("P2P", "D2D", "D2P", "P2D");
}
