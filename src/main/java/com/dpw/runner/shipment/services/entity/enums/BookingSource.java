package com.dpw.runner.shipment.services.entity.enums;

public enum BookingSource {
    RUNNER(1, "Runner"),
    PLATFORM(2, "Platform"),
    EXTERNAL(3, "External"),
    B2B(4, "B2b");

    private final int value;
    private final String description;

    BookingSource(int value, String description) {
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

