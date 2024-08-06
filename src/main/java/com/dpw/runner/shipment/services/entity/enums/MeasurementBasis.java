package com.dpw.runner.shipment.services.entity.enums;

public enum MeasurementBasis {

    ContainerCount(1, "ContainerCount"),
    Weight(2, "Weight"),
    Volume(3, "Volume"),
    Chargeable(4, "Chargeable"),
    LowestBill(5, "LowestBill"),
    Package(6, "Package"),
    Shipment(7, "Shipment"),
    TEU(8, "TEU"),
    ChargePercentage(9, "ChargePercentage"),
    Custom(10, "Custom"),
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
