package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import org.springframework.http.ResponseEntity;

public interface IShipmentSync {
    ResponseEntity<?> sync(ShipmentDetails shipmentDetails);
    ResponseEntity<?> reverseSync(CustomShipmentSyncRequest cs);
}
