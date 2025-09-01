package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import java.util.List;
import java.util.Map;

public class CarrierBookingConstants {
    private CarrierBookingConstants() {
        // private constructor to prevent instantiation
    }

    public static final String CARRIER_BOOKING_API_HANDLE = "/api/v3/carrier-booking";

    public static final String CARRIER_BOOKING_CREATE_SUCCESSFUL = "Successful Carrier Booking Data Creation";
    public static final String CARRIER_BOOKING_UPDATE_SUCCESSFUL = "Successful Carrier Booking Data Update";
    public static final String CARRIER_BOOKING_LIST_SUCCESSFUL = "Successful Carrier Booking Data List Retrieval";
    public static final String CARRIER_BOOKING_DELETE_SUCCESSFUL = "Successful Carrier Booking Delete";
    public static final String CARRIER_BOOKING_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Carrier Booking Data Retrieval By Id";

    public static final String CARRIER_BOOKING_ID = "Carrier Booking Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";

    public static final String CARRIER_BOOKING_RETRIEVE_ERROR = "Carrier Booking is null for Id {} with Request Id {}";
    public static final String CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE = "Include Columns field is mandatory";
    public static final String CARRIER_LIST_REQUEST_EMPTY_ERROR = "Request is empty for Carrier list with Request Id {}";
    public static final String CARRIER_LIST_REQUEST_NULL_ERROR = "Carrier List Request is Null";
    public static final String CARRIER_LIST_RESPONSE_SUCCESS = "Carrier list from db retrieved successfully for Request Id : {}";

    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries();
    public static final List<String> serviceTypes = List.of("P2P","P2F","F2P","F2F");
}
