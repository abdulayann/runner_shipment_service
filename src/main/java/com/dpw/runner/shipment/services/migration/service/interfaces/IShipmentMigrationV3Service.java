package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;

public interface IShipmentMigrationV3Service {
    ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails);

    ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails);
}
