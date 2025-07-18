package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IShipmentBackupRepository extends JpaRepository<ShipmentBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
