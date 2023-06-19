package com.dpw.runner.shipment.services.entity.enums;

public enum LGDStatus {
    Initiated(0, "Initiated"),
    Success(1, "Success"),
    Failed(2, "Failed"),
    Resent(3, "Resent");

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

