package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import lombok.Getter;

import java.util.List;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
@Getter
public enum CarrierBookingStatus {
    Draft(0, "Draft",  List.of(Constants.INTTRA)),
    Requested(1, "Booking Requested", List.of(Constants.CARRIER_PORTAL, Constants.INTTRA)),
    AcceptedByINTTRA(2, "Processed By INNTRA", List.of(Constants.INTTRA)),
    RejectedByINTTRA(3, "Rejected By INTTRA", List.of(Constants.INTTRA)),
    ConfirmedByCarrier(4, "Confirmed By Carrier", List.of(Constants.CARRIER_PORTAL, Constants.INTTRA)),
    DeclinedByCarrier(5, "Declined By Carrier", List.of(Constants.CARRIER_PORTAL, Constants.INTTRA)),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted", List.of(Constants.CARRIER_PORTAL, Constants.INTTRA)),
    PendingFromCarrier(7, "Pending From Carrier", List.of(Constants.INTTRA)),
    Cancelled(8, "Cancelled", List.of(Constants.CARRIER_PORTAL, Constants.INTTRA)),
    Changed(9, "Amend Requested", List.of(Constants.INTTRA)),
    ReplacedByCarrier(10, "Replaced By Carrier", List.of(Constants.INTTRA)),
    ChangeDraft(11, "Change Draft", List.of(Constants.INTTRA)),
    CancelledByCarrier(12, "Cancelled By Carrier", List.of(Constants.INTTRA)),
    AmendRejected(13, "Amend Rejected By INTTRA", List.of(Constants.INTTRA));

    private final int value;
    private final String description;
    private final List<String> allowedTypes;

    CarrierBookingStatus(int value, String description, List<String> allowedTypes) {
        this.value = value;
        this.description = description;
        this.allowedTypes = allowedTypes;
    }


}
