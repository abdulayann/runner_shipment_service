package com.dpw.runner.shipment.services.entity.enums;

public enum GenericKafkaMsgType {

    SI(0, "Shipping Instruction"),
    CB(1, "Carrier Booking"),
    VGM(2, "VGM");


    private final int value;
    private final String description;

    GenericKafkaMsgType(int value, String description) {
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
