package com.dpw.runner.shipment.services.entity.enums;

public enum TypeOfHblPrint {

    DRAFT(0, "Draft"),
    eHBL(1, "eHBL"),
    ORIGINAL(2, "Original"),
    SURRENDER(2, "Surrender"),
    ALL(10, "All");

    private final int value;
    private final String description;

    TypeOfHblPrint(int value, String description) {
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
