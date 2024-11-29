package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum TimeUnit {
    HOUR(0, "Hour"),
    DAY(1, "Day");
    private final int id;
    private final String description;

    TimeUnit(Integer id, String description) {
        this.id = id;
        this.description = description;
    }
}
