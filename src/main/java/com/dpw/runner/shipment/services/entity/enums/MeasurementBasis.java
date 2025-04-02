package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum MeasurementBasis {

    ContainerCount(1, "ContainerCount", "Container_Count"),
    Container_Count(1, "ContainerCount", "Container_Count"),
    Weight(2, "Weight", "Weight"),
    Volume(3, "Volume", "Volume"),
    Chargeable(4, "Chargeable", "Chargeable"),
    LowestBill(5, "LowestBill", "Lowest_Bill"),
    Package(6, "Package", "Package"),
    Shipment(7, "Shipment", "Shipment"),
    TEU(8, "TEU", "TEU"),
    ChargePercentage(9, "ChargePercentage", "Charge_Percentage"),
    Custom(10, "Custom", "Custom"),
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
