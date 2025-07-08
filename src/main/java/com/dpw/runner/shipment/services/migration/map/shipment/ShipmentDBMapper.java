package com.dpw.runner.shipment.services.migration.map.shipment;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.IDBMigrationMapper;

public abstract class ShipmentDBMapper<D> implements IDBMigrationMapper<ShipmentDetails, D> {
    @Override
    public abstract ShipmentDetails mapToEntity(D d) ;

    @Override
    public abstract D mapFromEntity(ShipmentDetails d) ;
}
