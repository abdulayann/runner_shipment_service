package com.dpw.runner.booking.services.entity.enums;

public enum LGDStatus {
    INITIATED(0, "Initiated"),
    SUCCESS(1, "Success"),
    FAILED(2, "Failed"),
    RESENT(3, "Resent");

    private final int value;
    private final String description;

    LGDStatus(int value, String description) {
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

