package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum FlowType {

    INBOUND(0, "Inbound"),
    OUTBOUND(1, "Outbound");

    private final int value;
    private final String description;

    FlowType(int value, String description) {
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
