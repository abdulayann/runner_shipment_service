package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsSync {
    ResponseEntity<?> sync(ShipmentSettingRequest request);
}
