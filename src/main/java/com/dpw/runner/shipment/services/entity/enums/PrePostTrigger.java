package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum PrePostTrigger {
    PRE(0, "-"),
    POST(1, "+");

    private final int id;
    private final String description;

    PrePostTrigger(Integer id, String description) {
        this.id = id;
        this.description = description;
    }
}
