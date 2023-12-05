package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IPackingSync {
    ResponseEntity<?> sync(List<Packing> request, Long consolidationId, Long shipmentId);
}
