package com.dpw.runner.shipment.services.commons.mixin;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Set;

public abstract class ConsoleShipmentMixIn {
    @JsonIgnore
    abstract Set<ShipmentDetails> getShipmentsList();
}
