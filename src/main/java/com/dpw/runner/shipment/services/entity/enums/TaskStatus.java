package com.dpw.runner.shipment.services.entity.enums;

public enum TaskStatus {
    PENDING_ACTION(0, "PendingAction"),
    APPROVED(1, "Approved"),
    REJECTED(2, "Rejected");

    private final int value;
    private final String description;

    TaskStatus(int value, String description) {
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

