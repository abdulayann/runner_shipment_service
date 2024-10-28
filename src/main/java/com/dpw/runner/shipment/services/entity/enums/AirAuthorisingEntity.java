package com.dpw.runner.shipment.services.entity.enums;

public enum AirAuthorisingEntity {
    Airport_Authority ("AO", "Airport Authority"),
    Regulated_Agent ("RA", "Regulated Agent"),
    Known_Consignor ("KC", "Known Consignor");

    private final String value;
    private final String description;
    AirAuthorisingEntity(String value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String getValue() {
        return value;
    }
}
