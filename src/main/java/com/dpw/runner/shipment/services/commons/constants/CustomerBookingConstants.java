package com.dpw.runner.shipment.services.commons.constants;

public class CustomerBookingConstants {

    private CustomerBookingConstants(){}
    public static final String DATE_FORMAT = "yyyy/MM/dd";
    public static String ONE = "1";
    public static final String Customer_Booking_API_HANDLE = "/api/v2/customer-booking";
    public static final String PLATFORM_CREATE_BOOKING = "/platform-create-update";
    public static final String PLATFORM_UPDATE_BOOKING = "/platform-update";
    public static final String CREATE_SUCCESSFUL = "Successful Customer Booking Data Creation";
    public static final String CRP_LIST = "/external/crp-list";
    public static final String CRP_RETRIEVE = "/external/crp-retrieve";
    public static final String FUSION_CHECK_CREDIT_LIMIT = "/external/fusion-credit-limit";
    public static final String RETRY_FOR_BILLING = "/retry/bill";
    public static final String LIST_SUCCESSFUL = "Successful CRP List";
    public static final String RETRIEVE_SUCCESSFUL = "Successfully retrieved data";
    public static final String CREDIT_LIMIT_RETRIEVE_SUCCESSFUL = "Successfully retrieved credit limit";
    public static final String CREDIT_LIMIT_RETRIEVE_ERROR = "Error occurred while trying to fetch credit limit from fusion.";
    public static final String UPDATE_SUCCESSFUL = "Successful Customer Booking Data Update";
    public static final String DELETE_SUCCESSFUL = "Successful Customer Booking Delete";
    public static final String RETRIEVE_BY_ID_SUCCESSFUL = "Successful Customer Booking Data Retrieval By Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String BOOKING_ID = "Booking Id";
    public static final String BILLABLE_IDENTIFIER = "&identifierCode=Yes&identifierCodeType=BILLABLE_FLAG";
    public static final String STD = "STD";
    public static final String FCL = "FCL";
    public static final String ONLINE = "ONLINE";
    public static final String RUNNER = "RUNNER";
    public static final String REMOVE = "REMOVE";
    public static final String ADD = "ADD";
    public static final String SINGLE_USAGE = "SINGLE_USAGE";
    public static final String ENABLED = "ENABLED";
    public static final String DISABLED = "DISABLED";
    public static final String UNIT = "unit";
    public static final String YES = "YES";
    public static final String TENANT_ID = "TenantId";
    public static final String RUNNER_FUSION = "Runner";
    public static final String GCR_FUSION = "GCR";

    public static final String IMMEDIATE= "IMMEDIATE";
    public static final String DATE_TIME_FORMAT = "yyyy/MM/dd'T'hh:mm:ss";

    public static final String BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR = "Booking Details is null for Id {} with Request Id {}";
    public static final String CUSTOMER_BOOKING_STRING = "BOOKING";
    public static final String MDM_FINAL_STATUS_FIELD = "finalStatus";
    public static final String MDM_FINAL_STATUS_APPROVED = "Approved";
    public static final String BOOKING_GUID = "Booking Guid";

    public static final String RETRIEVE_BY_ORDER_ID_SUCCESSFUL = "Successful Booking Data Retrieval By Order Id";
    public static final String ORDER_ID = "Order Id";

    public static final String PLATFORM_FAILURE_EMAIL_SUBJECT = "Reverse Integration from Runner to Platform Failed on Env: %s for Booking Number: %s";
    public static final String PLATFORM_FAILURE_EMAIL_BODY = "Hi, \n\nReverse Integration from Runner to Platform Failed for Booking Number: %s. \n\nRequest Sent: %s \n\nResponse Received: %s \n\nRegards, \nCargoes Runner Team";

}
