package com.dpw.runner.shipment.services.migration.repository.interfaces;

import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ConsolidationBackupRepository extends JpaRepository<ConsolidationBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
