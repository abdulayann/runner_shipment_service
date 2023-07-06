package com.dpw.runner.shipment.services.entity.enums;

import org.springframework.util.StringUtils;

public enum GenerationType {

    RANDOM(0, "Generation Type Random"),
    SERIAL(1, "Generation Type Serial");


    private final int value;
    private final String description;

    GenerationType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int getValue() {
        return value;
    }
    public String getDescription() {
        return description;
    }


    public static GenerationType fromValue(int value) {
        for (GenerationType generationType : GenerationType.values()) {
            if (generationType.getValue() == value) {
                return generationType;
            }
        }
        throw new IllegalArgumentException("Invalid Generation Type value: " + value);
    }
}
