package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum TaskStatus {
    PENDING_ACTION(0, "Pending Action", "PendingAction"),
    APPROVED(1, "Approved", "Approved"),
    REJECTED(2, "Rejected", "Rejected");

    private final int value;
    private final String description;
    private final String name;

    TaskStatus(int value, String description, String name) {
        this.value = value;
        this.description = description;
        this.name = name;
    }
}

