package com.dpw.runner.shipment.services.commons.constants;

public class EventConstants {

    private EventConstants(){}
    public static final String EVENT_API_HANDLE = "api/v2/events";
    public static final String EVENT_CREATE_SUCCESS = "Events created successfully !";
    public static final String EVENT_UPDATE_SUCCESS = "Events updated successfully !";
    public static final String EVENT_LIST_SUCCESS = "Events fetched successfully !";
    public static final String EVENT_DELETE_SUCCESS = "Events deleted successfully !";
    public static final String EVENTS_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Events Data Retrieval By Id";
    public static final String EVENT_ID = "Events Id";

    public static final String GENERATE_BL_EVENT_EXCLUSIVE_OF_DRAFT = "HBGNTD";
    public static final String SHIPPING_DOCUMENT_SENT_OR_NOT = "SDSUB";
    public static final String HBL_SURRENDERED_OR_NOT = "HBLSUR";
    public static final String MASTER_SEAWAY_BILL_OR_NOT = "MASSEA";
    public static final String SHIPPING_ADVISE_SENT_OR_NOT = "SHPSI";
    public static final String SR_SENT_OR_NOT = "SHPRSC";
    public static final String TRACK_EVENTS_FETCH_SUCCESSFUL = "Track Events fetched Successfully";
    public static final String TRACK_EVENT_DETAILS = "/trackEvents";
    public static final String EVENT_RETRIEVE_BY_ID_ERROR = "Event is null for Id {} with Request Id {}";

    public static final String EMPTY_REQUEST_ERROR = "Request can't be empty!";
    public static final String EMPTY_REQUEST_ID_ERROR = "Id can't be null in request";
    public static final String FWB_FZB_EVENT_CODE = "FWB/FZB";
    public static final String FWB_EVENT_CODE = "FWB";
}
