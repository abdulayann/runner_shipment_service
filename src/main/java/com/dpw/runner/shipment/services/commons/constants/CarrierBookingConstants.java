package com.dpw.runner.shipment.services.commons.constants;

import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;

import java.time.LocalDateTime;
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
    public static final String RETRIEVE_DEFAULT_SUCCESS = "Successful Default Carrier Booking Retrieval";
    public static final String CARRIER_BOOKING_ADDITIONAL_PARTIES = "CARRIER_BOOKING_ADDITIONAL_PARTIES";
    public static final String SUBMIT_AMEND_SUCCESSFUL = "Carrier Booking submitted/Amended successfully.";
    public static final String CON = "CON";
    public static final String MESSAGE = "message";
    public static final String PAYLOAD = "payload";
    public static final String INTTRA_REFERENCE = "inttraReference";
    public static final String CARRIER_REFERENCE_NUMBER = "carrierReferenceNumber";
    public static final String BOOKING_DETAILS= "bookingDetails";
    public static final String SERVICE_RESPONSE= "SERVICE_RESPONSE";
    public static final String SERVICE_HTTP_STATUS_CODE= "SERVICE_HTTP_STATUS_CODE";

    private CarrierBookingConstants() {
        // private constructor to prevent instantiation
    }

    public static final String CARRIER_BOOKING_API_HANDLE = "/api/v3/carrier-booking";

    public static final String CARRIER_BOOKING_CREATE_SUCCESSFUL = "Successful Carrier Booking Data Creation";
    public static final String CARRIER_BOOKING_UPDATE_SUCCESSFUL = "Successful Carrier Booking Data Update";
    public static final String CARRIER_BOOKING_LIST_SUCCESSFUL = "Successful Carrier Booking Data List Retrieval";
    public static final String CARRIER_BOOKING_DELETE_SUCCESSFUL = "Successful Carrier Booking Delete";
    public static final String CARRIER_BOOKING_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Carrier Booking Data Retrieval By Id";
    public static final String CARRIER_BOOKING_SYNC_SUCCESSFUL = "Carrier Booking successfully sync to consolidation";

    public static final String CANCELLED = "CANCELLED";
    public static final String CARRIER_BOOKING_ID = "Carrier Booking Id";
    public static final String RESPONSE_CONTAINER_LIST = "List";
    public static final String CARRIER_BOOKING = "CARRIER_BOOKING";
    public static final String CARRIER_BOOKING_TABLE = "CarrierBooking";
    public static final String SHIPPING_INSTRUCTION = "shippingInstruction";
    public static final String VERIFIED_GROSS_MASS = "verifiedGrossMass";
    public static final String SAILING_INFORMATION = "sailingInformation";
    public static final String STATUS = "status";

    public static final String CARRIER_BOOKING_RETRIEVE_ERROR = "Carrier Booking is null for Id {} with Request Id {}";
    public static final String CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE = "Include Columns field is mandatory";
    public static final String CARRIER_LIST_REQUEST_EMPTY_ERROR = "Request is empty for Carrier list with Request Id {}";
    public static final String CARRIER_LIST_REQUEST_NULL_ERROR = "Carrier List Request is Null";
    public static final String CARRIER_LIST_RESPONSE_SUCCESS = "Carrier list from db retrieved successfully for Request Id : {}";
    public static final String INVALID_CARRIER_BOOKING_ID = "Invalid Carrier booking Id: ";

    public static final List<String> serviceTypes = List.of("P2P","P2F","F2P","F2F");
    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry(STATUS, RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(CarrierBookingStatus.class)   // CarrierBookingStatus is an enum stored as string
                    .fieldName(STATUS)
                    .isContainsText(true)
                    .build()),

            Map.entry("bookingNo", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("bookingNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierBookingNo", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBookingNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierBlNo", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierBlNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("entityType", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("entityType")
                    .isContainsText(true)
                    .build()),

            Map.entry("entityId", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(Long.class)
                    .fieldName("entityId")
                    .build()),

            Map.entry("entityNumber", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("entityNumber")
                    .isContainsText(true)
                    .build()),

            Map.entry("serviceType", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("serviceType")
                    .isContainsText(true)
                    .build()),

            Map.entry("bookingOffice", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("bookingOffice")
                    .isContainsText(true)
                    .build()),

            Map.entry("bookingComment", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("bookingComment")
                    .isContainsText(true)
                    .build()),

            Map.entry("carrierComment", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("carrierComment")
                    .isContainsText(true)
                    .build()),

            Map.entry("internalEmails", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("internalEmails")
                    .isContainsText(true)
                    .build()),

            Map.entry("externalEmails", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("externalEmails")
                    .isContainsText(true)
                    .build()),

            Map.entry("pickupFromReqEmptyPositioningDate", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("pickupFromReqEmptyPositioningDate")
                    .build()),

            Map.entry("pickupFromReqFullPickupDate", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("pickupFromReqFullPickupDate")
                    .build()),

            Map.entry("pickupFromContactName", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("pickupFromContactName")
                    .isContainsText(true)
                    .build()),

            Map.entry("pickupFromContactNo", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("pickupFromContactNo")
                    .isContainsText(true)
                    .build()),

            Map.entry("createByUserEmail", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("createByUserEmail")
                    .isContainsText(true)
                    .build()),

            Map.entry("deliveryToReqEmptyPositioningDate", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("deliveryToReqEmptyPositioningDate")
                    .build()),

            Map.entry("deliveryToReqFullPickupDate", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("deliveryToReqFullPickupDate")
                    .build()),

            Map.entry("deliveryToContactName", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("deliveryToContactName")
                    .isContainsText(true)
                    .build()),

            Map.entry("deliveryToContactNo", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("deliveryToContactNo")
                    .isContainsText(true)
                    .build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("updatedAt")
                    .isContainsText(false)
                    .build()),
            Map.entry("siStatus", RunnerEntityMapping.builder()
                    .tableName(SHIPPING_INSTRUCTION)
                    .dataType(ShippingInstructionStatus.class)
                    .fieldName(STATUS)
                    .isContainsText(true)
                    .build()),

            Map.entry("vgmStatus", RunnerEntityMapping.builder()
                    .tableName(VERIFIED_GROSS_MASS)
                    .dataType(VerifiedGrossMassStatus.class)
                    .fieldName(STATUS)
                    .isContainsText(true)
                    .build()),

            Map.entry("carrier", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("carrier")
                    .isContainsText(true)
                    .build()),

            Map.entry("siCutoff", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(LocalDateTime.class)
                    .fieldName("shipInstructionCutoff")
                    .isContainsText(false)
                    .build()),

            Map.entry("vgmCutoff", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(LocalDateTime.class)
                    .fieldName("verifiedGrossMassCutoff")
                    .isContainsText(false)
                    .build()),

            Map.entry("updatedBy", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(String.class)
                    .fieldName("updatedBy")
                    .isContainsText(true)
                    .build()),

            Map.entry("pol", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("pol")
                    .isContainsText(true)
                    .build()),

            Map.entry("pod", RunnerEntityMapping.builder()
                    .tableName(SAILING_INFORMATION)
                    .dataType(String.class)
                    .fieldName("pod")
                    .isContainsText(true)
                    .build()),
            Map.entry(Constants.SHIPPER_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.SHIPPER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.CONSIGNEE_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder()
                    .tableName(CARRIER_BOOKING_TABLE)
                    .dataType(LocalDateTime.class)
                    .fieldName("createdAt")
                    .isContainsText(false)
                    .build())
    );

    //ERROR messages
    public static final String ERR_INTTRA_MISSING_KEY = "Invalid inttraa response: %s is missing or empty";
}
