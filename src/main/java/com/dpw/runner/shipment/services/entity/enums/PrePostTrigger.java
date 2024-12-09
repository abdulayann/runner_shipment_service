package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum PrePostTrigger {
    PRE(0, "-"),
    POST(1, "+");

    private final int id;
    private final String description;

    PrePostTrigger(Integer id, String description) {
        this.id = id;
        this.description = description;
    }

    // Static method to get description by id
    public static String getDescriptionById(int id) {
        for (PrePostTrigger field : PrePostTrigger.values()) {
            if (field.getId() == id) {
                return field.getDescription();
            }
        }
        throw new IllegalArgumentException("No matching field found for id: " + id);
    }
}
