package com.dpw.runner.shipment.services.entity.enums;

public enum LocationTypeCode {
    O("Origin"),
    T("Transit"),
    D("Destination");

    private final String description;

    LocationTypeCode(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
