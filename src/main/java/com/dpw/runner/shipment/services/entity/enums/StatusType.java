package com.dpw.runner.shipment.services.entity.enums;

public enum StatusType {
    INTERNAL_VALIDATION_ERROR(true), EXTERNAL_VALIDATION_ERROR(true), INTERNAL_ERROR(true), INITIATED(false), SUBMITTED(false), RECEIVED(false), PROCESSED(false), REJECTED(true);

    private final boolean isError;


    StatusType(boolean isError) {
        this.isError = isError;
    }

    public boolean isError() {
        return isError;
    }
}
