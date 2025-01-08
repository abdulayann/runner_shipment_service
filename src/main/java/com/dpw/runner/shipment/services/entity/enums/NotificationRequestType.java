package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum NotificationRequestType {
    REASSIGN(0, "Reassign"),
    REQUEST_TRANSFER(1, "Request Transfer");
    private final int value;
    private final String description;

    NotificationRequestType(int value, String description) {
        this.value = value;
        this.description = description;
    }

}
