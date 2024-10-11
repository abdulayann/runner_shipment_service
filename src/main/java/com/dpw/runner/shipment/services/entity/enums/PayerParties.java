package com.dpw.runner.shipment.services.entity.enums;

public enum PayerParties {
    BOOKER(1, "Booker"),
    FORWARDER(2, "Forwarder"),
    SHIPPER(3, "Shipper"),
    CONSIGNEE(4, "Consignee"),
    CONSIGNOR(5, "Consignor"),
    CHA(6, "Cha");

    private final int value;
    private final String description;

    PayerParties(int value, String description) {
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

