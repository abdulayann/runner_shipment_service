package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

@Generated
@Getter
public enum OrderPartiesPartyType {
    CONSIGNOR(0, "Consignor"),
    CONSIGNEE(1, "Consignee"),
    NOTIFY_PARTY(2, "Notify Party"),
    BUYER(3, "Buyer"),
    SUPPLIER(4, "Supplier");

    private final int value;
    private final String description;

    OrderPartiesPartyType(int value, String description) {
        this.value = value;
        this.description = description;
    }
}
