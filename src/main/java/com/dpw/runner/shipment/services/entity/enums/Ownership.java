package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.util.StringUtils;

@Generated
public enum Ownership {
    SELF(1, "Self"),
    THIRD_PARTY(2, "3rd Party");

    private final int value;
    private final String description;

    Ownership(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static Ownership fromValue(int value) {
        for (Ownership ownership : Ownership.values()) {
            if (ownership.getValue() == value) {
                return ownership;
            }
        }
        throw new IllegalArgumentException("Invalid Ownership value: " + value);
    }

    public String getDescriptionFormatted() {
        return StringUtils.capitalize(description);
    }
}
