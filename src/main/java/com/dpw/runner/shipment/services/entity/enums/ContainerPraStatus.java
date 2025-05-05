package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum ContainerPraStatus {
    ACCEPTED,
    FAILED,
    SUCCESS,
    REJECTED,
    FILED,
    CANCELLATION_SUBMITTED,
    CANCELLATION_REJECTED,
    CANCELLATION_ACCEPTED
}
