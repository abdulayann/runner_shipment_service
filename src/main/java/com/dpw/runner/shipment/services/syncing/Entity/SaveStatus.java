package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Getter;

@Getter
public enum SaveStatus {
    CREATE(0, "CREATE"),
    UPDATE(1, "UPDATE"),
    RESET(2, "RESET");

    private final int value;
    private final String description;

    SaveStatus(int value, String description) {
        this.value = value;
        this.description = description;
    }
}
