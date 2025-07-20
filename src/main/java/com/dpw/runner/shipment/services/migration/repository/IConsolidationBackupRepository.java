package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IConsolidationBackupRepository extends JpaRepository<ConsolidationBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);

    @Query(value = "SELECT c.consolidation_id FROM consolidation_backup c WHERE c.tenant_id = ?1", nativeQuery = true)
    List<Long> findConsolidationIdsByTenantId(Integer tenantId);

    @Query(value = "SELECT * FROM consolidation_backup c WHERE c.consolidation_id = ?1", nativeQuery = true)
    ConsolidationBackupEntity findConsolidationsById(Long consolidationId);
}
