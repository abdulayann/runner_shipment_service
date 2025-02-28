package com.dpw.runner.shipment.services.entity.enums;

public enum InstructionType {
    PICKUP(1, "Pickup"),
    DELIVERY(2, "Delivery"),
    CROSS_BORDER(3, "CrossBorder");

    private final int value;
    private final String description;

    InstructionType(int value, String description) {
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
