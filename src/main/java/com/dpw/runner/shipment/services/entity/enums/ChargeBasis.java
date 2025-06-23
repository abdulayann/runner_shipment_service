package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;

@Getter
@Generated
public enum ChargeBasis {
    FLAT_AMOUNT(1, "Flat Amount"),
    CHARGEABLE_WEIGHT(2, "Chargeable Weight"),
    GROSS_WEIGHT(3, "Gross Weight");

    int id;
    String description;

    ChargeBasis(int id, String description) {
        this.id = id;
        this.description = description;
    }

    public static ChargeBasis getById(int id) {
        for (ChargeBasis chargeBasis : ChargeBasis.values()) {
            if(chargeBasis.id == id) {
                return chargeBasis;
            }
        }
        return null;
    }
}
