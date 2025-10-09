package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PackCategory {
    SHIPMENT(0, "Shipment Package"),
    PURCHASE_ORDER(1, "Purchase Order Package");

    private final int value;
    private final String description;
}
