package com.dpw.runner.shipment.services.entity.enums;

public enum ShipmentStatus {
    CREATED(0, "Created"),
    BOOKED(1, "Booked"),
    COMPLETED(2, "Completed"),
    CANCELLED(3, "Cancelled"),
    CONFIRMED(4, "Confirmed"),
    IN_TRANSIT(5, "In Transit"),
    GENERATE_HBL(6, "HBL Generated"),
    GENERATE_HAWB(7, "HAWB Generated"),
    ARRIVED(8, "Arrived"),
    FINANCE_CLOSURE(9 , "Finance Closure"),
    NON_MOVEMENT(10, "Non-Movement");

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
