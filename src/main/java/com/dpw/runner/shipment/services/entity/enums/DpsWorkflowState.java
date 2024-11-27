package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum DpsWorkflowState {
    HOLD, UN_HOLD, TEMP_BLOCKED, PER_BLOCKED, UN_HOLD_WITH_CONDITION;
}