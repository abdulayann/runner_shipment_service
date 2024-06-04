package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public enum TypeOfHblPrint {

    Draft(0, "Draft"),
    eHBL(1, "eHBL"),
    Original(2, "Original"),
    Surrender(3, "Surrender"),
    All(10, "All");

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
