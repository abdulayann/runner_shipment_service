package com.dpw.runner.shipment.services.migration.repository.interfaces;

import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShipmentBackupRepository extends JpaRepository<ShipmentBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
