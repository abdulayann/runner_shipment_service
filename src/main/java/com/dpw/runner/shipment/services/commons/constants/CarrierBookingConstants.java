package com.dpw.runner.shipment.services.commons.constants;

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
}
