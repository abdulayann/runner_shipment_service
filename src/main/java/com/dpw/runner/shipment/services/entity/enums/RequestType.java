package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum RequestType {
    REASSIGN(0, "Reassign"),
    REQUEST_TRANSFER(1, "Request Transfer");
    private final int value;
    private final String description;

    RequestType(int value, String description) {
        this.value = value;
        this.description = description;
    }

}
