package com.dpw.runner.shipment.services.entity.enums;

public enum SIStatus {
    DRAFT(0, "Draft"),
    REQUESTED(1, "SI Submitted"),
    ACCEPTED_BY_CARRIER(2, "Processed By INNTRA"),
    REJECTED_BY_CARRIER(3, "Rejected By INTTRA"),
    CONFIRMED_BY_CARRIER(4, "Accepted By Carrier"),
    DECLINED_BY_CARRIER(5, "Declined By Carrier"),
    CONDITIONALLY_ACCEPTED(6, "Accepted By Carrier"),
    PENDING_FROM_CARRIER(7, "Pending From Carrier"),
    CANCELLED(8, "Cancelled"),
    CHANGED(9, "Changed"),
    DESCRIPTION(10, "Replaced By Carrier"),
    CHANGE_SI(11, "Change SI");

    private final int value;
    private final String description;

    SIStatus(int value, String description) {
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
