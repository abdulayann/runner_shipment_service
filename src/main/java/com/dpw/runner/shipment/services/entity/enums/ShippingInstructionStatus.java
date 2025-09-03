package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum ShippingInstructionStatus {
    Draft(0, "Draft"),
    Requested(1, "Booking Requested"),
    AcceptedByCarrier(2, "Processed By INNTRA"),
    RejectedByCarrier(3, "Rejected By INTTRA"),
    ConfirmedByCarrier(4, "Confirmed By Carrier"),
    DeclinedByCarrier(5, "Declined By Carrier"),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted"),
    PendingFromCarrier(7, "Pending From Carrier"),
    Cancelled(8, "Cancelled"),
    ChangedSI(9, "Amend Requested");
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
