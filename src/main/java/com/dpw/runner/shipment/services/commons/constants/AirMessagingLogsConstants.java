package com.dpw.runner.shipment.services.commons.constants;

public class AirMessagingLogsConstants {
    public static final String AIR_MESSAGING_LOGS_API_HANDLE = "/api/v2/air-messaging-logs";
    public static final String AIR_MESSAGING_LOGS_CREATE_SUCCESSFUL = "Successful Air Messaging Logs Data Creation";
    public static final String AIR_MESSAGING_LOGS_UPDATE_SUCCESSFUL = "Successful Air Messaging Logs Data Update";
    public static final String AIR_MESSAGING_LOGS_LIST_SUCCESSFUL = "Successful Air Messaging Logs Data List Retrieval";
    public static final String AIR_MESSAGING_LOGS_DELETE_SUCCESSFUL = "Successful Air Messaging Logs Delete";
    public static final String AIR_MESSAGING_LOGS_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Air Messaging Logs Data Retrieval By Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String AIR_MESSAGING_LOGS_ID = "Air Messaging Logs Id";
    public static final String AIR_MESSAGING_LOGS_RETRIEVE_BY_ID_ERROR = "Air Messaging Logs is null for Id {} with Request Id {}";
    // UserStory 144110
    public static final String SHIPMENT_FNM_FAILURE_ERROR = "FZB is not accepted due to %s, please rectify the data and resend using print button in MAWB";
    public static final String CONSOLIDATION_FNM_MAWB_SUCCESS_HAWB_FAILURE_ERROR = "FZB of the shipment/s %s is not accepted, please rectify the data as per the failure comments and resend using print button in MAWB";
    public static final String CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_SUCCESS_ERROR = "FWB is not accepted due to %s, please rectify the data and resend using print button in MAWB";
    public static final String CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_FAILURE_ERROR = "FWB and FZB of the shipment/s %s is not accepted, please rectify the data as per the failure comments and resend using print button in MAWB ";
    // Descartes Status
    public static final String PROCESSED = "PROCESSED";

    private AirMessagingLogsConstants() {
    }
}
