package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum FileTransferCriteriaFields {
    ETA(0,"ETA"),
    ETD(1,"ETD"),
    ATA(2,"ATA"),
    ATD(3, "ATD");

    private final int id;
    private final String description;

    FileTransferCriteriaFields(Integer id, String description) {
        this.id = id;
        this.description = description;
    }
}
