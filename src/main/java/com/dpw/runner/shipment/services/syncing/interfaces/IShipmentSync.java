package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

public interface IShipmentSync {
    ResponseEntity<?> sync(ShipmentDetails shipmentDetails);
    ResponseEntity<?> syncById(Long shipmentId);
    void syncLockStatus(ShipmentDetails shipmentDetails);
}
