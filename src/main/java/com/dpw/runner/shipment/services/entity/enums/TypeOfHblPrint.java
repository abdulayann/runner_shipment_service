package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public enum TypeOfHblPrint {

    DRAFT(0, "Draft"),
    E_HBL(1, "eHBL"),
    ORIGINAL(2, "Original"),
    SURRENDER(3, "Surrender"),
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
