package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.Packing;

import java.util.List;

public interface IPackingSync {
    void sync(List<Packing> request, Long consolidationId, Long shipmentId);
}
