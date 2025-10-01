package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
@SuppressWarnings("java:S115")
public enum ShippingInstructionStatus {
    Draft(0, "Draft"),
    Requested(1, "SI Requested"),
    AcceptedByCarrier(2, "SI Accepted By INTTRA"),
    RejectedByCarrier(3, "SI Rejected By INTTRA"),
    ConfirmedByCarrier(4, "SI Confirmed By Carrier"),
    DeclinedByCarrier(5, "SI Declined By Carrier"),
    ConditionallyAccepted(6, "Confirmed - Conditionally Accepted"),
    PendingFromCarrier(7, "Pending From Carrier"),
    Cancelled(8, "Cancelled"),
    Changed(9,"Changed"),
    ReplacedByCarrier(10, "Replaced By Carrier"),
    ChangeSI(11, "Change SI");

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
