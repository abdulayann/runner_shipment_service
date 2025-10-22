package com.dpw.runner.shipment.services.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum EventProgressStatus {
    TO_BE_PLANNED("To be planned"),
    PLANNED("Planned"),
    COMPLETED("Completed"),
    DELAYED_COMPLETION("Delayed completion");

    private final String description;
}

