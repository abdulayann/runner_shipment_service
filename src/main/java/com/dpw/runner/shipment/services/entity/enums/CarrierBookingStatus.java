package com.dpw.runner.shipment.services.entity.enums;

public enum CarrierBookingStatus {
    DRAFT(0, "Draft"),
    REQUESTED(1, "Booking Requested"),
    ACCEPTED_BY_INTTRA(2, "Processed By INNTRA"),
    REJECTED_BY_INTTRA(3, "Rejected By INTTRA"),
    CONFIRMED_BY_CARRIER(4, "Confirmed By Carrier"),
    DECLINED_BY_CARRIER(5, "Declined By Carrier"),
    CONDITIONALLY_ACCEPTED(6, "Confirmed - Conditionally Accepted"),
    PENDING_FROM_CARRIER(7, "Pending From Carrier"),
    CANCELLED(8, "Cancelled"),
    CHANGED(9, "Amend Requested"),
    REPLACED_BY_CARRIER(10, "Replaced By Carrier"),
    CHANGE_DRAFT(11, "Change Draft"),
    CANCELLED_BY_CARRIER(12, "Cancelled By Carrier"),
    AMEND_REJECTED(13, "Amend Rejected By INTTRA");

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
