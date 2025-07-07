package com.dpw.runner.shipment.services.entity.enums;

public enum BookingStatus {
    PENDING_FOR_KYC(1, "Booking Created"),
    PENDING_FOR_CREDIT_LIMIT(2, "Credit Check Approved"),
    READY_FOR_SHIPMENT(3, "Shipment Created"),
    CANCELLED(4, "Booking Cancelled"),
    PENDING_FOR_REVIEW(5, "Pending for Review"),
    REJECTED(6, "Rejected");

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

