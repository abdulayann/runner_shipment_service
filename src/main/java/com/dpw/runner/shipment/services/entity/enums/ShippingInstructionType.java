package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum ShippingInstructionType {
    ORIGINAL("Original"),
    EXPRESS("Seaway/Express");

    private final String description;

    ShippingInstructionType(String description) {
        this.description = description;
    }
}
