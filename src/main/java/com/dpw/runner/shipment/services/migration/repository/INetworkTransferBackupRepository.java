package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.NetworkTransferBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;
import java.util.UUID;

@Repository
public interface INetworkTransferBackupRepository extends JpaRepository<NetworkTransferBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);

    @Query(value = "SELECT * FROM network_transfer_backup c WHERE c.tenant_id = ?1 and c.is_deleted = false", nativeQuery = true)
    @Transactional
    List<NetworkTransferBackupEntity> findNetworkTransferIdsByTenantId(Integer tenantId);

    @Query(value = "SELECT * FROM network_transfer_backup c WHERE c.network_transfer_id = ?1 and c.is_deleted = false", nativeQuery = true)
    @Transactional
    NetworkTransferBackupEntity findNetworkTransferDetailsById(Long networkTransferId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE network_transfer_backup SET is_deleted = true WHERE id = ?1", nativeQuery = true)
    void makeIsDeleteTrueToMarkRestoreSuccessful(Long backupId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE network_transfer_backup SET is_deleted = true WHERE network_transfer_id = ?1 and tenant_id = ?2", nativeQuery = true)
    void deleteBackupByTenantIdAndNetworkTransferId(Long networkTransferId, Integer tenantId);

    @Modifying
    @Query(value = "UPDATE network_transfer_backup SET is_deleted = true WHERE network_transfer_guid IN ?1 and tenant_id = ?2", nativeQuery = true)
    void deleteDuplicateBackupByTenantIdAndNetworkTransferIds(Set<UUID> networkTransferIds, Integer tenantId);
}
