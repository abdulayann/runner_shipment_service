package com.dpw.runner.shipment.services.entity.enums;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
public enum AndesStatus {
    SendToAndes(1, "Send To Customs"),
    SuccessFullIntegrated(2, "Submitted"),
    FailedToGetResponse(3, "Failed To Get Response"),
    SomethingWentWrong(4, "Something Went Wrong");

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

