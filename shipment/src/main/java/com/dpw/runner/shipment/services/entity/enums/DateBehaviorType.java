package com.dpw.runner.shipment.services.entity.enums;

public enum DateBehaviorType {
    ACTUAL(0, "Actual"),
    ESTIMATED(1, "Estimated");

    private final int value;
    private final String description;

    DateBehaviorType(int value, String description) {
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
