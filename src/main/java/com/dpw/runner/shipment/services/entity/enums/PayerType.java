package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum PayerType {
    SHIPPER("Shipper"),
    CONSIGNEE("Consignee");

    private final String description;
    PayerType(String description){
        this.description = description;
    }
}
