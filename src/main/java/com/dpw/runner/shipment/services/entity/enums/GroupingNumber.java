package com.dpw.runner.shipment.services.entity.enums;

public enum GroupingNumber {

    DOT_AND_COMMA(0, "Use . and ,"),
    COMMA_AND_DOT(1, "Use , and .");
    private final int value;
    private final String description;

    GroupingNumber(int value, String description) {
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
