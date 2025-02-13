package com.dpw.runner.shipment.services.entitytransfer.enums;

public enum AddressTypes {
    BILLING(1),
    GENERAL(2),
    LOAD(3),
    UNLOAD(4);

    private final int id;

    AddressTypes(int id) {
        this.id = id;
    }
}
