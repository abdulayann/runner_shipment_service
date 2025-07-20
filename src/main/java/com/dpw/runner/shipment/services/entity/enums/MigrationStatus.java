package com.dpw.runner.shipment.services.entity.enums;

public enum MigrationStatus {
    MIGRATED_FROM_V2(1, "migrated from v2"),
    MIGRATED_FROM_V3(2, "migrated from v3"),
    CREATED_IN_V2(3, "create in v2"),
    CREATED_IN_V3(4, "create in v3"),
    NT_CREATED(5, "nt created"),
    NT_PROCESSED_FOR_V3(6, "nt processed for v3"),
    NT_PROCESSED_FOR_V2(7, "nt processed for v2");
    private final int value;
    private final String description;
    MigrationStatus(int value, String description) {
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
