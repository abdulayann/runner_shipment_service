package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.http.ResponseEntity;

import java.util.UUID;

public interface IShipmentSettingsService extends ICommonService{
    ResponseEntity<?> retrieveByGuid(UUID guid);
}
