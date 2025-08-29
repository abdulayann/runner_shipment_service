package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
@SuppressWarnings("java:S115")
public enum CarrierBookingGenerationType {

    Random(1, "Generation Type Random"),
    Serial(2, "Generation Type Serial");


    private final int value;
    private final String description;

    CarrierBookingGenerationType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int getValue() {
        return value;
    }
    public String getDescription() {
        return description;
    }


    public static CarrierBookingGenerationType fromValue(int value) {
        for (CarrierBookingGenerationType generationType : CarrierBookingGenerationType.values()) {
            if (generationType.getValue() == value) {
                return generationType;
            }
        }
        throw new IllegalArgumentException("Invalid Generation Type value: " + value);
    }
}
