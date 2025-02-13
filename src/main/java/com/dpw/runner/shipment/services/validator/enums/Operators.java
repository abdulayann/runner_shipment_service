package com.dpw.runner.shipment.services.validator.enums;

public enum Operators {
    GREATER_THAN("greater-than"),
    GREATER_THAN_EQUALS("greater-than-equals"),
    LESSER_THAN("lesser-than"),
    LESSER_THAN_EQUALS("lesser-than-equals"),
    EQUALS("equals"),
    NOT_EQUALS("not-equals"),
    IN("in");
    private final String value;

    Operators(String value) {
        this.value = value;
    }

    public static Operators fromValue(String value) {
        for (Operators operator : Operators.values()) {
            if (operator.getValue().equalsIgnoreCase(value)) {
                return operator;
            }
        }
        throw new IllegalArgumentException("Invalid Operator value: " + value);
    }

    public String getValue() {
        return value;
    }


}