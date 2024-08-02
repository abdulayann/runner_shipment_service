package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum MeasurementBasis {

    ContainerCount(1, "ContainerCount", "Container_Count"),
    Weight(1, "Weight", "Weight"),
    Volume(1, "Volume", "Volume"),
    Chargeable(1, "Chargeable", "Chargeable"),
    LowestBill(1, "LowestBill", "Lowest_Bill"),
    Package(1, "Package", "Package"),
    Shipment(1, "Shipment", "Shipment"),
    TEU(1, "TEU", "TEU"),
    ChargePercentage(1, "ChargePercentage", "Charge_Percentage"),
    Custom(1, "Custom", "Custom"),
    ContainerType(11, "ContainerType", "Container_Type");

    private final int value;
    private final String description;
    private final String billingValue;

    public static MeasurementBasis getByValue(int value) {
        for (MeasurementBasis measurementBasis : values()) {
            if (measurementBasis.value == value) {
                return measurementBasis;
            }
        }
        throw new IllegalArgumentException("No enum constant with value " + value);
    }
}
