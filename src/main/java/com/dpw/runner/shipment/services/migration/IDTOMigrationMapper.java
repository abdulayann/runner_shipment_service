package com.dpw.runner.shipment.services.migration;

public interface IDTOMigrationMapper<D1, D2> {

    D2 forwardMap(D1 d1);

    D1 reverseMap(D2 d2);
}
