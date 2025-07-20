package com.dpw.runner.shipment.services.migration.dao.interfaces;

import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;

import java.util.List;

public interface IShipmentBackupDao {
    ShipmentBackupEntity findByShipmentId(Long shipmentId);
}
