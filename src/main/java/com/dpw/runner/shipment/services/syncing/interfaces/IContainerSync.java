package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.entity.Containers;

import java.util.List;

public interface IContainerSync {
    void sync(List<Containers> request, Long consolidationId, Long shipmentId);
}
