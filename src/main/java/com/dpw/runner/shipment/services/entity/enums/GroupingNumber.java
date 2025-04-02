package com.dpw.runner.shipment.services.entity.enums;

@SuppressWarnings("java:S115")
public enum GroupingNumber {

    DotAndComma(0, "Use . and ,"),
    CommaAndDot(1, "Use , and .");
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
