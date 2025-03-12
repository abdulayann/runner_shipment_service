package com.dpw.runner.shipment.services.commons.constants;

import java.util.Arrays;
import java.util.List;

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
            "POD", "Waybill Number", "Additional Terms", "Reference Number","POL Code", "POD Code" , "Origin Code", "Destination Code"
    );

    private ShipmentConstants(){}
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
    public static final String STALE_SHIPMENT_UPDATE_ERROR= "Consolidation request has been accepted, Please refresh the shipment for latest details.";

    public static final String SHIPMENT_LIST_SIMILAR_RESPONSE_SUCCESS = "Shipment similar list retrieved successfully for Request Id {} ";
    public static final String REQUIRED_PARAMETER_MISSING_ERROR = "Required parameter missing: {} for Request Id {} ";
    public static final String SHIPMENT_DETAILS_FOR_GUID_MISSING_ERROR = "Shipment Details is not present for Guid {} with Request Id {}";
    public static final String TENANT_DATA_RETRIEVAL = "Successful Tenant Data Retrieval";
    public static final String SHIPMENT_RETRIEVE_NULL_REQUEST =  "Request Id is null for Shipment retrieve with Request Id {}";

    public static final String FETCH_MATCHING_RULES_SUCCESS = "Matching Rules Fetched Successfully";
    public static final String PAID_PLACE = "paidPlace";
    public static final String PLACE_OF_ISSUE = "placeOfIssue";
    public static final String ORIGIN = "origin";
    public static final String DESTINATION = "destination";
    public static final String ORIGIN_PORT_LOC_CODE = "originPortLocCode";
    public static final String DESTINATION_PORT_LOC_CODE = "destinationPortLocCode";
    public static final String FULL_NAME = "FullName";
    public static final String SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE = "Request Id and Guid are null for Shipment retrieve with Request Id {}";
    public static final String ID_GUID_NULL_ERROR = "Id and GUID can't be null. Please provide any one !";
    public static final String SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR = "Shipment Details is null for Guid {} with Request Id {}";

}
