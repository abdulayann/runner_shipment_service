package com.dpw.runner.booking.services.commons.objectMapperMixin;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.UUID;

public abstract class BookingMixIn {
    @JsonIgnore
    abstract Long getId();

    @JsonIgnore
    abstract UUID getGuid();
}
