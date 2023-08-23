package com.dpw.runner.shipment.services.entity.enums;

public enum BookingStatus {
    PENDING_FOR_KYC(1, "KYC Pending"),
    PENDING_FOR_CREDIT_LIMIT(2, "Credit Check Pending"),
    READY_FOR_SHIPMENT(3, "Ready for Shipment"),

    CANCELLED(4, "Booking Cancelled");

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

