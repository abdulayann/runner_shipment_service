package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface IShipmentBackupRepository extends JpaRepository<ShipmentBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);

    @Query(value = "SELECT * FROM shipment_backup s WHERE s.shipment_id = ?1", nativeQuery = true)
    ShipmentBackupEntity findByShipmentId(Long shipmentId);

    @Query(value = "SELECT c.shipment_id FROM shipment_backup c WHERE c.tenant_id = ?1 and c.is_deleted = false", nativeQuery = true)
    Set<Long> findShipmentIdsByTenantId(Integer tenantId);

    @Query(value = "SELECT c.shipment_id FROM shipment_backup c WHERE c.tenant_id = ?1 and c.is_shipment_attached = false", nativeQuery = true)
    Set<Long> findNonAttachedShipmentIdsByTenantId(Integer tenantId);

    @Modifying
    @Query(value = "UPDATE shipment_backup SET is_deleted = true WHERE id = ?1", nativeQuery = true)
    void makeIsDeleteTrueToMarkRestoreSuccessful(Long id);
}
