package com.dpw.runner.shipment.services.entity.enums;

import lombok.Getter;

@Getter
public enum TimeUnit {
    HOUR(0, "Hour"),
    DAY(1, "Day");
    private final int id;
    private final String description;

    TimeUnit(Integer id, String description) {
        this.id = id;
        this.description = description;
    }

    // Static method to get description by id
    public static String getDescriptionById(int id) {
        for (TimeUnit field : TimeUnit.values()) {
            if (field.getId() == id) {
                return field.getDescription();
            }
        }
        throw new IllegalArgumentException("No matching field found for id: " + id);
    }
}
