package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum FlowType {

    Inbound(0, "Inbound"),
    Outbound(1, "Outbound");

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
