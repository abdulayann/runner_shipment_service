package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsSync {
    ResponseEntity<?> sync(ShipmentSettingRequest request);
}
