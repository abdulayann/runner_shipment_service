package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum DateType {
    ATA(1, "ATA"),
    ATD(2, "ATD"),
    ETA(3, "ETA"),
    ETD(4, "ETD");

    private int id;
    private String description;

    DateType(int id, String description) {
        this.id = id;
        this.description = description;
    }
}
