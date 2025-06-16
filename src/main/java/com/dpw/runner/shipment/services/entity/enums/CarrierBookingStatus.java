package com.dpw.runner.shipment.services.entity.enums;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum CarrierBookingStatus {
    Draft(0, "Draft"),
    Requested(1, "Booking Requested"),
    AcceptedByINTTRA(2, "Processed By INNTRA"),
    RejectedByINTTRA(3, "Rejected By INTTRA"),
    ConfirmedByCarrier(4, "Confirmed By Carrier"),
    DeclinedByCarrier(5, "Declined By Carrier"),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted"),
    PendingFromCarrier(7, "Pending From Carrier"),
    Cancelled(8, "Cancelled"),
    Changed(9, "Amend Requested"),
    ReplacedByCarrier(10, "Replaced By Carrier"),
    ChangeDraft(11, "Change Draft"),
    CancelledByCarrier(12, "Cancelled By Carrier"),
    AmendRejected(13, "Amend Rejected By INTTRA");

    private final int value;
    private final String description;

    CarrierBookingStatus(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }
}
