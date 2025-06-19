package com.dpw.runner.shipment.services.commons.enums;

public enum TILegType {
    EMPTY("Empty"), FULL("Full");
    private String description;

    TILegType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
