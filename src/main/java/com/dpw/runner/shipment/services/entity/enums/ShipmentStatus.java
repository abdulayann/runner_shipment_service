package com.dpw.runner.shipment.services.entity.enums;

public enum ShipmentStatus {
    Created (0, "Created"),
    Booked (1, "Booked"),
    Completed (2, "Completed"),
    Cancelled (3, "Cancelled"),
    Confirmed (4, "Confirmed"),
    InTransit (5, "In Transit"),
    GenerateHBL (6, "HBL Generated"),
    GenerateHAWB (7, "HAWB Generated"),
    Arrived (8, "Arrived"),
    FinanceClosure (9 , "Finance Closure"),
    NonMovement (10, "Non-Movement");

    private final int value;
    private final String description;

    ShipmentStatus(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }

    public static ShipmentStatus fromValue(int value) {
        for (ShipmentStatus enumValue : ShipmentStatus.values()) {
            if (enumValue.getValue() == value) {
                return enumValue;
            }
        }
        throw new IllegalArgumentException("No enum constant with value " + value);
    }
}
