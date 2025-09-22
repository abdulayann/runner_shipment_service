package com.dpw.runner.shipment.services.migration.dao.interfaces;

import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;

import java.util.Set;

public interface IShipmentBackupDao {
    ShipmentBackupEntity findByShipmentId(Long shipmentId);
    Set<Long> findShipmentIdsByTenantId(Integer tenantId);
    Set<Long> findNonAttachedShipmentIdsByTenantId(Integer tenantId);

    void makeIsDeleteTrueToMarkRestoreSuccessful(Long id);
}
