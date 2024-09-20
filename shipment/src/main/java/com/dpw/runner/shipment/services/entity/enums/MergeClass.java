package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.util.StringUtils;

@Generated
public enum MergeClass {

    A(0);

    private final int value;

    MergeClass(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static MergeClass fromValue(int value) {
        for (MergeClass mergeClass : MergeClass.values()) {
            if (mergeClass.getValue() == value) {
                return mergeClass;
            }
        }
        throw new IllegalArgumentException("Invalid MergeClass value: " + value);
    }

    public String getDescription() {
        return StringUtils.capitalize(name());
    }
}
