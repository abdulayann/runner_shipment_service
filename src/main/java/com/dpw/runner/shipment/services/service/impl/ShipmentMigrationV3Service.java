package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentMigrationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ShipmentMigrationV3Service implements IShipmentMigrationV3Service {
    @Override
    public ShipmentDetails migrateShipmentV2ToV3(ShipmentDetails shipmentDetails){

        // createNotes


        // Transform Container and packs based on Auto update Weight Volume flag


        return shipmentDetails;
    }
}
