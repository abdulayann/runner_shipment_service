package com.dpw.runner.shipment.services.entity.enums;

public enum ShipmentPackStatus {
    BOOKED(0, "Booked"),
    PARTIAL_CARGO_GATE_IN(1, "Partial Cargo Gate In"),
    CARGO_GATED_IN(0, "Cargo Gated In"),
    DRAFT_LOAD_PLAN(1, "Draft Load Plan"),
    PARTIALLY_ASSIGNED(0, "Partially Assigned"),
    ASSIGNED(1, "Assigned"),
    LOADED(0, "Loaded"),
    SAILED(1, "Sailed");

    private final int value;
    private final String description;

    ShipmentPackStatus(int value, String description) {
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
