package com.dpw.runner.shipment.services.entity.enums;

public enum FileStatus {
    WRK(0, "Working"),
    INV(1, "Invoiced"),
    CLS(2, "Closed");
    private final int value;
    private final String description;

    FileStatus(int value, String description) {
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
