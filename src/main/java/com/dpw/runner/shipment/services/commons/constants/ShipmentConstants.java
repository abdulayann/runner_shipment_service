package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class ShipmentConstants {
    public static final String HBL_NUMBER = "HBL Number";
    @SuppressWarnings("java:S2386")
    public static final List<String> SHIPMENT_HEADERS = Arrays.asList(
            "Shipment Clone", "Shipment Number", "Order Number", "Status", "Transport Mode",
            "Bill Status", "MBL Number", "Incoterm", "Service Type", "Release Type", "House Bill Type",
            "Delivery Mode", "Consolidation Type", "Activity Type", "Shipment Type", "Carrier",
            "Vessel Name/Flight", "Flight Number", "Voyage/Flight No.", "Paid Place Name",
            "Issued Place Name", "Source1", "Date of Issue", "Date of Receipt", "Country of Origin",
            "Notify Party Name", "Cargo Type", "Origin", "Destination", "Domestic", "Route",
            "Client Name", "Consignor Name", "Consignee Name", HBL_NUMBER, "BOE Number",
            "Screening Status", "BOE Date", "ETD", "ETA", "ATD", "ATA", "Estimated Delivery",
            "Actual Delivery", "Goods Description", "Gross Weight", "Gross Weight Unit", "Volume",
            "Volume Unit", "Chargeable Weight", "Volumetric Weight", "No. Of Packages", "Package Type",
            "No. Of Inner Packages", "IU", "Customer Booking Number", "Pickup Transporter",
            "Delivery Transporter", "Job Status", "Assigned To", "Created By", "Created Source",
            "Updated Date", "20RE", "20GP", "40RE", "40GP", "Container Number", "Created Date",
            "Estimated Cost", "Estimated Revenue", "Estimated Profit", "Estimated Profit %",
            "Captured Cost", "Captured Revenue", "Captured Profit", "Captured Profit %",
            "Invoiced Payable Cost", "Invoiced Receivable Revenue", "Invoiced Profit",
            "Invoiced Profit %", "20s Count", "40s Count", "TEU Count", "CreatedBy", "POL",
            "POD", "Waybill Number", "Additional Terms", "Reference Number", "POL Code", "POD Code", "Origin Code", "Destination Code"
    );
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of( "carrierDetails", "routingsList", "bookingCarriagesList", "packingList", "referenceNumbersList","servicesList", "containersList", "eventsList","triangulationPartnerList");
    public static final List<String> LIST_INCLUDE_COLUMNS_V3 = List.of( "carrierDetails", "referenceNumbersList", "triangulationPartnerList");
    public static final String ORIGIN_PORT_LOC_CODE = "originPortLocCode";
    public static final String PLACE_OF_ISSUE = "placeOfIssue";
    public static final String PAID_PLACE = "paidPlace";
    public static final String ORIGIN = "origin";
    public static final String DESTINATION = "destination";
    public static final String DESTINATION_PORT_LOC_CODE = "destinationPortLocCode";
    public static final String CONSOLIDATION_NUMBER = "consolidationNumber";
    public static final Map<String, RunnerEntityMapping> TABLES_NAMES = Map.ofEntries(
            Map.entry(Constants.CLIENT_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNER_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNEE_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CLIENT_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(Integer.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNER_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNEE_ADDRESS_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_MODE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.TRANSPORT_MODE).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry(Constants.DIRECTION, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.DIRECTION).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry(Constants.STATUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.STATUS).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry(Constants.JOB_TYPE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.JOB_TYPE).isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.SHIPMENT_ID).isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("screeningStatus").build()),
            Map.entry(PAID_PLACE, RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName(PAID_PLACE).build()),
            Map.entry(PLACE_OF_ISSUE, RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName(PLACE_OF_ISSUE).build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry(Constants.SHIPPING_LINE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.SHIPPING_LINE).isContainsText(true).build()),
            Map.entry(Constants.VESSEL, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VESSEL).build()),
            Map.entry(Constants.VOYAGE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VOYAGE).build()),
            Map.entry(ORIGIN, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ORIGIN).build()),
            Map.entry(DESTINATION, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(DESTINATION).build()),
            Map.entry(Constants.ORIGIN_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.ORIGIN_PORT).build()),
            Map.entry(Constants.DESTINATION_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.DESTINATION_PORT).build()),
            Map.entry("originLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originLocCode").build()),
            Map.entry("destinationLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationLocCode").build()),
            Map.entry(ORIGIN_PORT_LOC_CODE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(ORIGIN_PORT_LOC_CODE).build()),
            Map.entry(DESTINATION_PORT_LOC_CODE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(DESTINATION_PORT_LOC_CODE).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("id").build()),
            Map.entry(CONSOLIDATION_NUMBER, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(String.class).fieldName(CONSOLIDATION_NUMBER).build()),
            Map.entry(Constants.ORDER_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_NUMBER).build()),
            Map.entry(Constants.ORDER_MANAGEMENT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_NUMBER).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry(Constants.ORDER_MANAGEMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_ID).isContainsText(true).build()),
            Map.entry(Constants.FLIGHT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.FLIGHT_NUMBER).build()),
            Map.entry(Constants.CONSOLIDATION_ID, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(Long.class).fieldName("id").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("shipperRef", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(String.class).fieldName("shipperRef").build()),
            Map.entry(Constants.CONTAINS_HAZARDOUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).build()),
            Map.entry("shipmentPackStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(ShipmentPackStatus.class).build()),
            Map.entry(Constants.TENANT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.TENANT_ID).build()),
            Map.entry("cargoReadyDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoReadyDate").build()),
            Map.entry("cargoDeliveryDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoDeliveryDate").build()),
            Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(Constants.CREATED_AT).build()),
            Map.entry("sourceGuid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("sourceGuid").build()),
            Map.entry("routingPol", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName("pol").build()),
            Map.entry("routingPolCode", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName(ORIGIN_PORT_LOC_CODE).build()),
            Map.entry("routingPod", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName("pod").build()),
            Map.entry("routingPodCode", RunnerEntityMapping.builder().tableName(Constants.ROUTING_LIST).dataType(String.class).fieldName(DESTINATION_PORT_LOC_CODE).build())
    );
    public static final String LIST = "/list";
    public static final String SHIPMENT_LIST_V3_RESPONSE_SUCCESS = "Shipment list from db retrieved successfully for Request Id {}: {}";
    public static final String UPDATE_SAILING_SCHEDULE_SUCCESSFUL = "Sailing schedule data updated successfully";

    private ShipmentConstants() {
    }

    public static final String IMPLICATIONS_LIST_COLUMN = "implicationList";
    public static final String SHIPMENT_API_HANDLE = "/api/v2/shipment";

    public static final String HBL_NUMBER_CHECK_SUCCESSFUL = "HBL Number check successful";

    public static final String CREATE_SUCCESSFUL = "Successful Shipment Data Creation";
    public static final String FETCH_SUCCESSFUL = "Fetch Successful";

    public static final String UPDATE_SUCCESSFUL = "Successful Shipment Data Update";
    public static final String IMPORT_SUCCESSFUL = "Import Successful";
    public static final String IMPORT_SHIPMENT = "/import";

    public static final String LIST_SUCCESSFUL = "Successful Shipment List Retrieval";
    public static final String NON_BILLABLE_CUSTOMER_SUCCESSFUL = "Successful Created Non Billable Customer";

    public static final String TI_LIST_SUCCESSFUL = "Successful Transport Instruction List Retrieval";
    public static final String CONTAINER_LIST_SUCCESSFUL_NEW_TI = "Successful Container List Retrieval for Transport Instruction";

    public static final String DELETE_SUCCESSFUL = "Successful Shipment Delete";

    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Shipment Data Retrieval By Id";
    public static final String REQUESTED_INTER_BRANCH_CONSOLE = "Requested inter branch console";
    public static final String PENDING_NOTIFICATION_COUNT_SUCCESSFUL = "Pending Notification Count successful";

    public static final String SHIPMENT_ID = "Shipment Id";
    public static final String SHIPMENT_GUID = "Shipment Guid";
    public static final String CONSOLIDATION_ID = "Consolidation Id";

    public static final String MODULE_ID = "Module Id";
    public static final String CONSOLIDATION = "CONSOLIDATION";
    public static final String SHIPMENT = "SHIPMENT";


    public static final String RESPONSE_CONTAINER_LIST = "List";

    public static final String LOCK_TOGGLE_SUCCESSFUL = "Successfully toggled lock";

    public static final String SHIPMENT_LOCKED = "Current Shipment is Locked, try unlocking it first !";

    public static final String SHIPMENT_TYPE_DRT = "DRT";

    public static final String SHIPMENT_V1_CREATE = "/createV1Shipment";
    public static final String FETCH_ORG_INFO = "/fetchOrgInfoFromV1";

    public static final String SHIPMENT_SYNC_SUCCESSFUL = "Shipment synced successfully";
    public static final String EXPORT_SUCCESSFUL = "Export Successful";
    public static final String RETRIEVE_BY_ORDER_ID_SUCCESSFUL = "Successful Shipment Data Retrieval By Order Id";
    public static final String ORDER_ID = "Order Id";
    public static final String GENERATE_CUSTOM_HOUSE_BL = "/generate/customHouseBl";
    public static final String IMPORT_CONSOLIDATION = "/import-consolidation";

    public static final String DEFAULT_SHIPMENT_GENERATED_SUCCESSFULLY = "Default shipment generated successfully";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String CALCULATION_SUCCESSFUL = "calculation successful";
    public static final String ASSIGN_CONTAINERS_SUCCESSFUL = "Assign containers successful";
    public static final String LIST_SHIPMENT_FROM_CONSOLE_ID = "/list-shipment-by-console";
    public static final String GET_ACTIVE_INVOICES = "/get-active-invoices";
    public static final String SHOW_ASSIGN_ALL_CONTAINERS = "/show/assign/allContainers";
    public static final String CHECK_CREDIT_LIMIT_FROM_V1 = "/check-credit-limit";
    public static final String FETCH_CREDIT_LIMIT = "/creditLimit";
    public static final String FETCH_EMAILS = "/email";
    public static final String GET_DATETIME_CHANGES = "/get/dateTimeChanges";
    public static final String GET_CONTAINERS = "/get/ts/containers";
    public static final String API_RETRIEVE_MEASUREMENT_DATA = "/retrieve/measurement/data";
    public static final String API_SHIPMENT_RETRIEVE_FOR_NTE_SCREEN = "/retrieve/nte";

    // Shipment V3.0 API Endpoints
    public static final String SHIPMENT_API_HANDLE_V3 = "/api/v3/shipment";
    public static final String COUNT_PENDING_NOTIFICATION_API = "/count/pending/notification";


    public static final String SHIPMENT_CREATION = "SHPCR";

    public static final String ALL_SHIPMENT_COUNT = "All shipment count fetched successfully";
    public static final String LATEST_CARGO_DELIVERY_DATE = "Latest cargo delivery date out of shipments fetched successfully";
    public static final String UPDATE_SHIPMENT_STATUS = "Shipment status has been updated successfully";

    // Shipment Statuses
    public static final String PENDING = "PENDING";
    public static final String BOOKED = "BOOKED";
    public static final String CANCELLED = "CANCELLED";
    public static final String CONFIRMED = "CONFIRMED";
    public static final String SHIPMENT_RETRIEVE_BY_ID_ERROR = "Shipment Details is null for Id {} with Request Id {}";
    public static final String SHIPMENT_DETAILS_NULL_FOR_ID_ERROR = "Shipment Details is null for Id {}";
    public static final String SHIPMENT_LIST_REQUEST_EMPTY_ERROR = "Request is empty for Shipment list with Request Id {}";
    public static final String SHIPMENT_LIST_RESPONSE_SUCCESS = "Shipment list retrieved successfully for Request Id {} ";
    public static final String SHIPMENT_LIST_CRITERIA_PREPARING = "Shipment list criteria preparing for Request Id {} ";
    public static final String SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR = "Request is empty for Shipment retrieve with Request Id {}";
    public static final String NO_DATA_FOUND_FOR_ORG_CODE = "No Data found for org code {}";
    public static final String SHIPMENT_LIST_REQUEST_NULL_ERROR = "Shipment List Request is Null";
    public static final String CHECK_CREDIT_LIMIT_FAILED = "Check Credit Limit failed due to : ";
    public static final String NOTIFICATION_FETCHED_SUCCESSFULLY = "Notifications fetched successfully";

    public static final String OCEAN_DG_EMAIL_SEND_SUCCESS = "Ocean DG Email Sent Successfully";


    public static final String OCEAN_DG_APPROVAL_REQUEST_RESPONSE = "Ocean DG Approval Request Response";
    public static final String ATTACH_DETACH_ORDER_RESPONSE = "Order attached/detached successfully";
    public static final String PADDING_10_PX = "padding: 10px;";
    public static final String STYLE = "style";
    public static final String STALE_SHIPMENT_UPDATE_ERROR = "Consolidation request has been accepted, Please refresh the shipment for latest details.";

    public static final String SHIPMENT_LIST_SIMILAR_RESPONSE_SUCCESS = "Shipment similar list retrieved successfully for Request Id {} ";
    public static final String REQUIRED_PARAMETER_MISSING_ERROR = "Required parameter missing: {} for Request Id {} ";
    public static final String SHIPMENT_DETAILS_FOR_GUID_MISSING_ERROR = "Shipment Details is not present for Guid {} with Request Id {}";
    public static final String TENANT_DATA_RETRIEVAL = "Successful Tenant Data Retrieval";
    public static final String SHIPMENT_RETRIEVE_NULL_REQUEST = "Request Id is null for Shipment retrieve with Request Id {}";

    public static final String FETCH_MATCHING_RULES_SUCCESS = "Matching Rules Fetched Successfully";
    public static final String FETCH_MATCHING_RULES_WITH_EXECUTION_STATE_SUCCESS = "Matching Rules Fetched Successfully with Execution state";
    public static final String FULL_NAME = "FullName";
    public static final String SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE = "Request Id and Guid are null for Shipment retrieve with Request Id {}";
    public static final String ID_GUID_NULL_ERROR = "Id and GUID can't be null. Please provide any one !";
    public static final String SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR = "Shipment Details is null for Guid {} with Request Id {}";

    public static final String ATTACH_CONSOLIDATION_SUCCESSFUL = "Attach Consolidation Request Successful";

}
