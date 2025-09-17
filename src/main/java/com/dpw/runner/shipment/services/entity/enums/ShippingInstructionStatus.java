package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
@SuppressWarnings("java:S115")
public enum ShippingInstructionStatus {
    Draft(0, "Draft"),
    Requested(1, "Booking Requested"),
    SiAccepted(2, "SI accepted"),
    RejectedByCarrier(3, "Rejected By INTTRA"),
    ConfirmedByCarrier(4, "Confirmed By Carrier"),
    DeclinedByCarrier(5, "Declined By Carrier"),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted"),
    SiAmendRequested(7, "SI Amend Requested"),
    Cancelled(8, "Cancelled"),
    SiSubmitted(10, "SI Submitted");
    private final int value;
    private final String description;

    ShippingInstructionStatus(int value, String description) {
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
