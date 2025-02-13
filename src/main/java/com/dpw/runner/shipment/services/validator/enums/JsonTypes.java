package com.dpw.runner.shipment.services.validator.enums;

public enum JsonTypes {
    OBJECT("object"),
    ARRAY("array"),
    STRING("string"),
    NUMBER("number"),
    DATE("date"),
    DATE_TIME("date-time"),
    BOOLEAN("boolean"),
    NULL("null");
    private final String value;

    JsonTypes(String value) {
        this.value = value;
    }

    public static JsonTypes fromValue(String value) {
        for (JsonTypes jsonTypes : JsonTypes.values()) {
            if (jsonTypes.getValue().equalsIgnoreCase(value)) {
                return jsonTypes;
            }
        }
        throw new IllegalArgumentException("Invalid JsonType value: " + value);
    }

    public String getValue() {
        return value;
    }


}