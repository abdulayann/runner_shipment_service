package com.dpw.runner.shipment.services.entity.enums;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum InstructionType {
    Pickup(1, "Pickup"),
    Delivery(2, "Delivery"),
    CrossBorder(3, "CrossBorder");

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
