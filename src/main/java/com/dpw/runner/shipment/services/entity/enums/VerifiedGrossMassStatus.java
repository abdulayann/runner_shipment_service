package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum VerifiedGrossMassStatus {
    Draft(0, "Draft"),
    Requested(1, "VGM Requested"),
    AcceptedByINTTRA(2, "Accepted By INTTRA"),
    RejectedByINTTRA(3, "Rejected By INTTRA"),
    ConfirmedByCarrier(4, "Confirmed By Carrier"),
    DeclinedByCarrier(5, "Declined By Carrier"),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted"),
    PendingFromCarrier(7, "Pending From Carrier"),
    Cancelled(8, "Cancelled"),
    Changed(9, "VGM Amend Submitted"),
    ReplacedByCarrier(10, "Replaced By Carrier"),
    ChangeDraft(11, "Change Draft"),
    CancelledByCarrier(12, "Cancelled By Carrier"),
    AmendRejected(13, "Amend Rejected By INTTRA"),
    Acknowledged(14, "Acknowledged"),
    CancelledRequested(15, "Cancelled Requested");

    private final int value;
    private final String description;

    VerifiedGrossMassStatus(int value, String description) {
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
