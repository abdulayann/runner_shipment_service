package com.dpw.runner.shipment.services.entity.enums;

public enum AirAuthorisingEntity {
    AO (1, "Airport Authority"),
    RA (2, "Regulated Agent"),
    KC (3, "Known Consignor");

    private final int value;
    private final String description;
    AirAuthorisingEntity(int value, String description) {
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
