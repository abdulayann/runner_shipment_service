package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsSync {
    ResponseEntity<IRunnerResponse> sync(ShipmentSettingsDetails request);
    ResponseEntity<IRunnerResponse> syncProductSequence(ProductSequenceConfig productSequenceConfig, HttpHeaders headers);
    ResponseEntity<IRunnerResponse> syncSettings();
}
