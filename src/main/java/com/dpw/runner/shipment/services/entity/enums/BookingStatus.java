package com.dpw.runner.shipment.services.entity.enums;

public enum BookingStatus {
    PENDING_FOR_KYC(1, "Pending KYC"),
    PENDING_FOR_CREDIT_LIMIT(2, "Pending Credit Check"),
    READY_FOR_SHIPMENT(3, "Ready for Shipment"),
    CANCELLED(4, "Booking Cancelled"),
    PENDING_FOR_REVIEW(5, "Pending for Review");

    private final int value;
    private final String description;

    BookingStatus(int value, String description) {
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

