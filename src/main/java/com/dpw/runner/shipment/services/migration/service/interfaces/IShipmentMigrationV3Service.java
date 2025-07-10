package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.Map;
import java.util.UUID;

public interface IShipmentMigrationV3Service {
    ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails, Map<UUID, UUID> packingVsContainerGuid) throws RunnerException;

    ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) throws RunnerException;
}
