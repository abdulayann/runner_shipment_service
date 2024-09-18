package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum RatingTypeIndicator {
    F("F"), P("P"), A("A");

    private final String value;

}
