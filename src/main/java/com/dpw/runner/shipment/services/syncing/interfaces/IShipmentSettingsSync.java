package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsSync {
    ResponseEntity<?> sync(ShipmentSettingsDetails request);
    ResponseEntity<?> syncProductSequence(ProductSequenceConfig productSequenceConfig, HttpHeaders headers);
    ResponseEntity<?> syncSettings();
}
