package com.dpw.runner.shipment.services.entity.enums;

public enum OceanChargeType {
    OCEAN_FREIGHT(1, "OceanFreight"),
    DESTINATION_HAULAGE_CHARGES(2, "DestinationHaulageCharges"),
    DESTINATION_TERMINAL_HANDLING(3, "DestinationTerminalHandling"),
    ORIGIN_HAULAGE_CHARGES(4, "OriginHaulageCharges"),
    ORIGIN_TERMINAL_HANDLING(5, "OriginTerminalHandling"),
    ADDITIONAL_CHARGES(6, "AdditionalCharges");

    private final int value;
    private final String description;

    OceanChargeType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int getValue() {
        return value;
    }
    public String getDescription() {
        return description;
    }

}
