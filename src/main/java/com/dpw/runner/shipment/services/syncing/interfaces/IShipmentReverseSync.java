package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import org.springframework.http.ResponseEntity;

public interface IShipmentReverseSync {
    ResponseEntity<?> reverseSync(CustomShipmentSyncRequest cs);
}
