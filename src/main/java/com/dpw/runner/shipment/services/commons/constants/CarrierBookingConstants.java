package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public class CarrierBookingConstants {
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
    public static final String MAIN_CARRIAGE = "MainCarriage";
    public static final String MASTER_DATA_RETRIEVE_SUCCESS =  "Master Data Retrieve Successfully";
    public static final List<String> LIST_INCLUDE_COLUMNS = List.of("carrierRoutingList","containersList");

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
