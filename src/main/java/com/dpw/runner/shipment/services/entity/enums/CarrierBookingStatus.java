package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

import java.util.List;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
@Getter
public enum CarrierBookingStatus {
    Draft(0, "Draft",  List.of("INTTRA")),
    Requested(1, "Booking Requested", List.of("Carrier Portal", "INTTRA")),
    AcceptedByINTTRA(2, "Processed By INNTRA", List.of("INTTRA")),
    RejectedByINTTRA(3, "Rejected By INTTRA", List.of("INTTRA")),
    ConfirmedByCarrier(4, "Confirmed By Carrier", List.of("Carrier Portal", "INTTRA")),
    DeclinedByCarrier(5, "Declined By Carrier", List.of("Carrier Portal", "INTTRA")),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted", List.of("Carrier Portal", "INTTRA")),
    PendingFromCarrier(7, "Pending From Carrier", List.of("INTTRA")),
    Cancelled(8, "Cancelled", List.of("Carrier Portal", "INTTRA")),
    Changed(9, "Amend Requested", List.of("INTTRA")),
    ReplacedByCarrier(10, "Replaced By Carrier", List.of("INTTRA")),
    ChangeDraft(11, "Change Draft", List.of("INTTRA")),
    CancelledByCarrier(12, "Cancelled By Carrier", List.of("INTTRA")),
    AmendRejected(13, "Amend Rejected By INTTRA", List.of("INTTRA"));
    
    private final int value;
    private final String description;
    private final List<String> allowedTypes;

    CarrierBookingStatus(int value, String description, List<String> allowedTypes) {
        this.value = value;
        this.description = description;
        this.allowedTypes = allowedTypes;
    }


}
