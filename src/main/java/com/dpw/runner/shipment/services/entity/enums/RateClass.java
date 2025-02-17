package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum RateClass {
    M(1, "M"),
    N(2, "N"),
    Q(3, "Q"),
    B(4, "B"),
    C(5, "C"),
    U(6, "U"),
    X(7, "X"),
    E(8, "E");

    int id;
    String description;

    RateClass(int id, String description) {
        this.id = id;
        this.description = description;
    }

    public static RateClass getById(int id) {
        for (RateClass rateClass : RateClass.values()) {
            if (rateClass.id == id) {
                return rateClass;
            }
        }
        return null;
    }
}
