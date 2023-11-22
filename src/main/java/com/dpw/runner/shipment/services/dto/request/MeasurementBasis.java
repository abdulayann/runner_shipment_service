package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum MeasurementBasis {
    Container_Count( "Container Count", 1),
    Weight( "Weight", 2),
    Volume("Volume", 3),
    Chargeable( "Chargeable", 4),
    Lowest_Bill( "Lowest Bill", 5),
    Package("Package", 6),
    Shipment("Shipment", 7),
    TEU( "TEU", 7),
    Charge_Percentage("Charge Percentage", 8),
    Custom( "Custom", 9),
    Container_Type("Container Type", 10);
    private final String description;
    private final Integer count;
}
