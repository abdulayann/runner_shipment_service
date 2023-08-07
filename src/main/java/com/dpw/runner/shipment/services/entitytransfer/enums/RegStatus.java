package com.dpw.runner.shipment.services.entitytransfer.enums;

public enum RegStatus {
    REGISTERED(1),
    UNREGISTERED(2);
    private int id;
    RegStatus(int id) {
            this.id = id;
        }
}
