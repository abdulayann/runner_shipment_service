package com.dpw.runner.shipment.services.entity.enums;

public enum TaskType {
    SHIPMENT_IMPORTER(1, "ShipmentImporter"),
    CONSOLIDATION_IMPORTER(9, "ConsolidationImporter");


    private final int value;
    private final String description;

    TaskType(int value, String description) {
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

