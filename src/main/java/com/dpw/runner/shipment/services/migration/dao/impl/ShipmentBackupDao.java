package com.dpw.runner.shipment.services.migration.dao.impl;

import com.dpw.runner.shipment.services.migration.dao.interfaces.IConsolidationBackupDao;
import com.dpw.runner.shipment.services.migration.dao.interfaces.IShipmentBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
import com.dpw.runner.shipment.services.migration.repository.IShipmentBackupRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

@SuppressWarnings("java:S3776")
@Repository
@Slf4j
public class ShipmentBackupDao implements IShipmentBackupDao {

    @Autowired
    private IShipmentBackupRepository shipmentBackupRepository;

    @Override
    public ShipmentBackupEntity findByShipmentId(Long shipmentId) {
        return shipmentBackupRepository.findByShipmentId(shipmentId);
    }

    @Override
    public Set<Long> findShipmentIdsByTenantId(Integer tenantId) {
        return shipmentBackupRepository.findShipmentIdsByTenantId(tenantId);
    }

    @Override
    public Set<Long> findNonAttachedShipmentIdsByTenantId(Integer tenantId) {
        return shipmentBackupRepository.findNonAttachedShipmentIdsByTenantId(tenantId);
    }
}
