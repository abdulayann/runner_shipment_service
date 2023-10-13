package com.dpw.runner.shipment.services.commons.objectMapperMixin;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.UUID;

public abstract class ShipmentMixIn {
    @JsonIgnore
    abstract Long getId();

    @JsonIgnore
    abstract UUID getGuid();
}
