package com.dpw.runner.shipment.services.commons.constants;

import java.util.Arrays;
import java.util.List;

public class ConsolidationConstants {
    private ConsolidationConstants(){}
    public static List<String> CONSOLIDATION_HEADER = Arrays.asList(
            "Consolidation Type", "Consolidation Number", "Transport Mode", "Cargo Type",
            "ETA", "ATA", "ETD", "ATD", "Domestic", "Created By", "Voyage/Flight No",
            "Payment Terms", "Carrier", "Cutoff Date", "HBL / HAWB", "Estimated Terminal Cutoff",
            "Terminal Cutoff", "Booking Cutoff", "Shipping Instruction Cutoff", "Hazardous Booking Cutoff",
            "VGM Cutoff", "Reefer Cutoff", "Booking Type", "Reference Number", "Carrier Booking Status",
            "Carrier Booking Number", "Container Count", "POL", "POD", "MBL / MAWB", "POL Code", "POD Code" , "Origin Code", "Destination Code",
            "Origin", "Destination"
    );
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of( "carrierDetails", "containersList", "routingsList", "PackingList", "triangulationPartnerList");
    public static final String CONSOLIDATION_API_HANDLE = "/api/v2/consolidation";
    public static final String MBL_NUMBER = "MBL Number";

    public static final String CREATE_SUCCESSFUL = "Successful Consolidation Data Creation";

    public static final String UPDATE_SUCCESSFUL = "Successful Consolidation Data Update";

    public static final String LIST_SUCCESSFUL = "Successful Consolidation List Retrieval";

    public static final String DELETE_SUCCESSFUL = "Successful Consolidation Delete";

    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Consolidation Data Retrieval By Id";

    public static final String CONSOLIDATION_ID = "Consolidation Id";
    public static final String CONSOLIDATION_GUID = "Consolidation Guid";

    public static final String RESPONSE_CONTAINER_LIST = "List";

    public static final String LOCK_TOGGLE_SUCCESSFUL = "Successfully toggled lock";

    public static final String CONSOLIDATION_LOCKED = "Current Consolidation is Locked, try unlocking it first !";

    public static final String CONSOLIDATION_CALCULATION_SUCCESSFUL = "Successful Required Calculation";
    public static final String CONSOLIDATION_V1_CREATE = "/createV1Consolidation";
    public static final String IMPORT_SHIPMENT = "/import-shipment";
    public static final String IMPORT_SUCCESSFUL = "Import Consolidation Successful";
    public static final String MBL_NUMBER_CHECK_SUCCESSFUL = "MBL Number check successful";
    public static final String ASSIGN_SUCCESSFUL = "Assign Shipments Successful";
    public static final String DETACH_SUCCESSFUL = "Detach Shipments Successful";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS = "Master Data Retrieve Successfully";
    public static final String DEFAULT_CONSOLIDATION_GENERATED_SUCCESSFULLY = "Default consolidation generated successfully";
    public static final String GENERATE_CUSTOM_HOUSE_BL = "/generate/customBol";
    public static final String CONSOLE_BOOKING_FIELD_UPDATE = "/update/console-booking-fields";

    public static final String SHOW_CREATE_BOOKING_SUCCESSFUL = "show or create bookingSuccessful";

    public static final String  API_RETRIEVE_SHOW_CREATE_BOOKING = "/retrieve/show_create_booking";
    public static final String API_CONSOLIDATION_RETRIEVE_FOR_NTE_SCREEN = "/retrieve/nte";

    public static final String SHOW_CREATE_BOOKING_OPERATION = "Operation Name (CREATE/VIEW)";
    public static final String CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR = "Consolidation Details is null for Id {}";
    public static final String CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID = "Consolidation Details is null for Id {} with Request Id {}";
    public static final String CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY = "Consolidation details fetched successfully for Id {} with Request Id {}";
    public static final String API_GET_SUCCESSFUL = "Get Custom Consolidation Request Successful";
    public static final String ATTACH_SHIPMENT_SUCCESSFUL = "Attach Shipment Request Successful";
    public static final String CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR = "Request is empty for Consolidation list with Request Id {}";
    public static final String CONSOLIDATION_LIST_REQUEST_NULL_ERROR = "Consolidation List Request is Null";
    public static final String NOTIFICATION_FETCHED_SUCCESSFULLY = "Notifications fetched successfully";
    public static final String CONSOLIDATION_RETRIEVE_EMPTY_REQUEST = "Request is empty for Consolidation retrieve with Request Id {}";
    public static final String CONSOLIDATION_DETAILS_NULL = "Consolidation Details is null for Guid {} with Request Id {}";
    public static final String PUSH_REQUESTED_SHIPMENT_VALIDATION_MESSAGE = "Existing Shipment Push Request already in place, Cannot initiate a new request.";
    public static final String CONSOLIDATION_RETRIEVE_NULL_REQUEST =  "Request Id is null for Consolidation retrieve with Request Id {}";
}
