package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

@Getter
@Generated
public enum ShipmentRequestedType {

    SHIPMENT_PULL_ACCEPTED(1, "Shipment Pull Accepted", "Pull Accepted"),
    SHIPMENT_PUSH_ACCEPTED(2, "Shipment Push Accepted", "Push Accepted"),
    SHIPMENT_PULL_REQUESTED(3, "Shipment Pull Requested", "Pull Requested"),
    SHIPMENT_PUSH_REQUESTED(4, "Shipment Push Requested", "Push Requested"),
    SHIPMENT_PULL_REJECTED(5, "Shipment Pull Rejected", "Pull Rejected"),
    SHIPMENT_PUSH_REJECTED(6, "Shipment Push Rejected", "Push Rejected"),

    SHIPMENT_PULL_WITHDRAW(7, "Shipment Pull Withdraw", "Pull Withdraw"),
    SHIPMENT_PUSH_WITHDRAW(8, "Shipment Push Withdraw", "Push Withdraw"),
    APPROVE(9, "Shipment Accepted", "Accepted"),
    REJECT(10, "Shipment Rejected", "Rejected"),
    WITHDRAW(11, "Shipment Withdrawn", "Withdrawn"),
    SHIPMENT_DETACH(12, "Shipment Detach", "Detach");
    private final int value;
    private final String description;
    private final String v3Description;

    ShipmentRequestedType(int value, String description, String v3Description) {
        this.value = value;
        this.description = description;
            this.v3Description = v3Description;
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
