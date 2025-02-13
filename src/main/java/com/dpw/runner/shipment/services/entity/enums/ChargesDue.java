package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum ChargesDue {
    AGENT(1, "Agent"),
    CARRIER(2, "Carrier");

    int id;
    String description;

    ChargesDue(int id, String description) {
        this.id = id;
        this.description = description;
    }

    public static ChargesDue getById(int id) {
        for (ChargesDue chargesDue : ChargesDue.values()) {
            if (chargesDue.id == id) {
                return chargesDue;
            }
        }
        return null;
    }
}
