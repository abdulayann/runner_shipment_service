package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum DateType {
    ATA(1, "ATA"),
    ATD(2, "ATD"),
    ETA(3, "ETA"),
    ETD(4, "ETD");

    private final int id;
    private final String description;

    DateType(int id, String description) {
        this.id = id;
        this.description = description;
    }
}
