package com.dpw.runner.shipment.services.commons.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ModuleValidationFieldType {
    CARRIER("Carrier Details missing"),
    CARRIER_ETA("Carrier ETA Details missing"),
    CARRIER_ETD("Carrier ETD Details missing"),
    CONTAINER_DETAILS("Container Details missing"),
    MAWB_DETAILS("MAWB Details missing"),
    MBL_DETAILS("MBL Details missing"),
    SHIPMENT_ORIGIN_AGENT("Shipment origin agent details missing"),
    SHIPMENT_DESTINATION_AGENT("Shipment Destination agent details missing");

    private final String description;
}
