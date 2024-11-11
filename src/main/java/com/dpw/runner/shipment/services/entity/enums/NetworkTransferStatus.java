package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum NetworkTransferStatus {
    SCHEDULED(0 , "Scheduled"),
    REQUESTED_TO_TRANSFER(1 , "Requested to Transfer"),
    TRANSFERRED(2 , "Transferred"),
    REASSIGNED(3 , "Reassigned"),
    ACCEPTED(4 , "Accepted");
    private final int value;
    private final String description;

    NetworkTransferStatus(int value, String description) {
        this.value = value;
        this.description = description;
    }

}
