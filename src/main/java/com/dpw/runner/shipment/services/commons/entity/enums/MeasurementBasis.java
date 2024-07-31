package com.dpw.runner.shipment.services.commons.entity.enums;

public enum MeasurementBasis {

    ContainerCount(1, "ContainerCount"),
    Weight(1, "Weight"),
    Volume(1, "Volume"),
    Chargeable(1, "Chargeable"),
    LowestBill(1, "LowestBill"),
    Package(1, "Package"),
    Shipment(1, "Shipment"),
    TEU(1, "TEU"),
    ChargePercentage(1, "ChargePercentage"),
    Custom(1, "Custom"),
    ContainerType(11, "ContainerType");

    private final int value;
    private final String description;

    MeasurementBasis(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }

    public static MeasurementBasis getByValue(int value) {
        for (MeasurementBasis measurementBasis : values()) {
            if (measurementBasis.value == value) {
                return measurementBasis;
            }
        }
        throw new IllegalArgumentException("No enum constant with value " + value);
    }
}
