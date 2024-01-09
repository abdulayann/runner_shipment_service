package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.UUID;

public interface IShipmentSync {
    ResponseEntity<?> sync(ShipmentDetails shipmentDetails, List<UUID> deletedContGuids);
    ResponseEntity<?> syncById(Long shipmentId);
    void syncLockStatus(ShipmentDetails shipmentDetails);
}
