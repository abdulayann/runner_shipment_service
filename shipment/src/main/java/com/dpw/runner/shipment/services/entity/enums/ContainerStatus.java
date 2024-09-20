package com.dpw.runner.shipment.services.entity.enums;

public enum ContainerStatus {
    ON_DOCK(1, "On Dock"),
    LOADED_ON_BARGE(2, "Loaded on Barge"),
    DISCHARGED_FROM_BARGE(3, "Discharged from Barge");

    private final int value;
    private final String description;

    ContainerStatus(int value, String description) {
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

