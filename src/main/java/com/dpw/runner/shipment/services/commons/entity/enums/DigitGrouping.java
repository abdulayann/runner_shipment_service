package com.dpw.runner.shipment.services.commons.entity.enums;

public enum DigitGrouping {

    TWO(0, "2"),
    THREE(1, "3");

    private final int value;
    private final String description;

    DigitGrouping(int value, String description) {
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
