package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ShipmentMigrationV3Service implements IShipmentMigrationV3Service {

    @Override
    public ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails) {


        mapShipmentV2ToV3(shipmentDetails);

        // update Shipment

        return null;
    }

    public ShipmentDetails mapShipmentV2ToV3(ShipmentDetails shipmentDetails) {
        // Business Logic for transformation

        return shipmentDetails;
    }

    @Override
    public ShipmentDetails migrateShipmentV3ToV2(ShipmentDetails shipmentDetails) {
        return null;
    }
}
