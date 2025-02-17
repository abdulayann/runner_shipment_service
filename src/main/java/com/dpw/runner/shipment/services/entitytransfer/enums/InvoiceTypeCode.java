package com.dpw.runner.shipment.services.entitytransfer.enums;

public enum InvoiceTypeCode {
    B2B(0),
    B2C(1),
    SEZWP(2),
    SEZWOP(3),
    EXPWP(4),
    EXPWOP(5),
    NA(11);
    private final int id;

    InvoiceTypeCode(int id) {
        this.id = id;
    }
}
