package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

@Getter
@Generated
public enum RAKCType {
    REGULATED_AGENT(1, "Regulated Agent"),
    KNOWN_CONSIGNOR(2, "Known Consignor");

    int id;
    String description;

    RAKCType(int id, String description) {
        this.id = id;
        this.description = description;
    }

    public static RAKCType getById(int id) {
        for (RAKCType rakcType : RAKCType.values()) {
            if (rakcType.id == id) {
                return rakcType;
            }
        }
        return null;
    }
}
