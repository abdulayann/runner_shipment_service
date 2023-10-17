package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import org.springframework.http.ResponseEntity;

public interface IShipmentReverseSync {
    ResponseEntity<?> reverseSync(ShipmentSettingsSyncRequest request);
}
