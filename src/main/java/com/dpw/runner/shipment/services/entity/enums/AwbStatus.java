package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum AwbStatus {
    AWB_GENERATED,
    AIR_MESSAGE_SENT,
    AIR_MESSAGE_FAILED,
    AIR_MESSAGE_SUCCESS,
    AWB_ORIGINAL_PRINTED,
    AWB_FSU_LOCKED;
}
