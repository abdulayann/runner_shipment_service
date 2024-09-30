package com.dpw.runner.booking.services.masterDataObjects.enums;

import com.fasterxml.jackson.annotation.JsonCreator;

public enum RegStatus {
    REGISTERED(1),
    UNREGISTERED(2);
    private int id;
    RegStatus(int id) {
            this.id = id;
        }

    @JsonCreator
    public static RegStatus getNameByValue(final int value) {
        for (final RegStatus s: RegStatus.values()) {
            if (s.id == value) {
                return s;
            }
        }
        return null;
    }
}
