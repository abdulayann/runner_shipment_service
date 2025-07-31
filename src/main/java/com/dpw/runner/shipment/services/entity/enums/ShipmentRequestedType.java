package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

@Getter
@Generated
public enum ShipmentRequestedType {

    SHIPMENT_PULL_ACCEPTED(1, "Shipment Pull Accepted"),
    SHIPMENT_PUSH_ACCEPTED(2, "Shipment Push Accepted"),
    SHIPMENT_PULL_REQUESTED(3, "Shipment Pull Requested"),
    SHIPMENT_PUSH_REQUESTED(4, "Shipment Push Requested"),
    SHIPMENT_PULL_REJECTED(5, "Shipment Pull Rejected"),
    SHIPMENT_PUSH_REJECTED(6, "Shipment Push Rejected"),

    SHIPMENT_PULL_WITHDRAW(7, "Shipment Pull Withdraw"),
    SHIPMENT_PUSH_WITHDRAW(8, "Shipment Push Withdraw"),
    APPROVE(9, "Shipment Accepted"),
    REJECT(10, "Shipment Rejected"),
    WITHDRAW(11, "Shipment Withdrawn"),
    SHIPMENT_DETACH(12, "Shipment Detach");
    private final int value;
    private final String description;

    ShipmentRequestedType(int value, String description) {
        this.value = value;
        this.description = description;
    }


    public static ShipmentRequestedType fromValue(int value) {
        for (ShipmentRequestedType shipmentRequestedType : ShipmentRequestedType.values()) {
            if (shipmentRequestedType.getValue() == value) {
                return shipmentRequestedType;
            }
        }
        throw new IllegalArgumentException("Invalid Shipment Requested Type value: " + value);
    }
}
