package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum SourceSystem {

    CargoRunner(0, "Cargo Runner"),
    Carrier(1, "Carrier"),
    INTTRA(2, "INTTRA"),
    Other(3, "Other");

    private final int value;
    private final String description;

    SourceSystem(int value, String description) {
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
