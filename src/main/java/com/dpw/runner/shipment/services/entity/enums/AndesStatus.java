package com.dpw.runner.shipment.services.entity.enums;

public enum AndesStatus {
    SEND_TO_ANDES(1, "Send To Customs"),
    SUCCESS_FULL_INTEGRATED(2, "Submitted"),
    FAILED_TO_GET_RESPONSE(3, "Failed To Get Response"),
    SOMETHING_WENT_WRONG(4, "Something Went Wrong");

    private final int value;
    private final String description;

    AndesStatus(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }
}

