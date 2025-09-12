package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum SourceSystem {

    CARGO_RUNNER(0, "Cargo Runner"),
    CARRIER(1, "Carrier"),
    INTTRA(2, "INTTRA"),
    OTHER(3, "Other");

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
