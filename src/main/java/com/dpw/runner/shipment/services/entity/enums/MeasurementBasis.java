package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum MeasurementBasis {

    CONTAINERCONT(1, "ContainerCount", "Container_Count"),
    CONTAINER_COUNT(1, "ContainerCount", "Container_Count"),
    WEIGHT(2, "Weight", "Weight"),
    VOLUME(3, "Volume", "Volume"),
    CHARGEABLE(4, "Chargeable", "Chargeable"),
    LOWEST_BILL(5, "LowestBill", "Lowest_Bill"),
    PACKAGE(6, "Package", "Package"),
    SHIPMENT(7, "Shipment", "Shipment"),
    TEU(8, "TEU", "TEU"),
    CHARGE_PERCENTAGE(9, "ChargePercentage", "Charge_Percentage"),
    CUSTOM(10, "Custom", "Custom"),
    CONTAINER_TYPE(11, "ContainerType", "Container_Type");

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
