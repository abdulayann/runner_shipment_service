package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum WeightDeterminationMethodType {
    METHOD1("Method 1"),
    METHOD2("Method 2");
    private final String description;

    WeightDeterminationMethodType(String description) {
        this.description = description;
    }
}
