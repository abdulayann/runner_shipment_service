package com.dpw.runner.shipment.services.entitytransfer.enums;

public enum RemoteIdTypes {
    PASS_THROUGH_ID(1),
    INTRA_COMPANY_ID(2);
    private final int id;

    RemoteIdTypes(int id) {
        this.id = id;
    }
}
