package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsSync {
    ResponseEntity<?> sync(ShipmentSettingsDetails request);
    ResponseEntity<?> syncSettings();
}
