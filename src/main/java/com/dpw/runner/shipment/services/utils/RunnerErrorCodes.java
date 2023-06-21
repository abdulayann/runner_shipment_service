package com.dpw.runner.shipment.services.utils;

public enum RunnerErrorCodes {
    FORBIDDEN("R403", "FORBIDDEN"),
    SOMETHING_WENT_WRONG("R001", "SOMETHING_WENT_WRONG"),
    REQUEST_VALIDATION("R002", "REQUEST_VALIDATION"),
    UNAUTHORIZED("R003", "UNAUTHORIZED"),
    ENTITY_CREATION_EXCEPTION("R004", "Entity Creation Failed"),
    ENTITY_FETCHING_EXCEPTION("R005", "Entity Fetch Failed"),
    HBL_DETAILS_NOT_FOUND("B210", "HBL details not found"),
    MORE_THAN_ONE_HBL_EXISTS("B211", "More than one HBL details found")
    ;
    private String code;
    private String msg;

    RunnerErrorCodes(String code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    public String getCode() {
        return code;
    }

    public String getMsg() {
        return msg;
    }
}
