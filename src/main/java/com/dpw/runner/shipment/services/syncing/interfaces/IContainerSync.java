package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IContainerSync {
    ResponseEntity<?> sync(List<Containers> request, Long consolidationId, Long shipmentId);
}
