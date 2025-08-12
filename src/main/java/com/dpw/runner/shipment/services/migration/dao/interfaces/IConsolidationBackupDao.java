package com.dpw.runner.shipment.services.migration.dao.interfaces;

import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;

import java.util.List;

public interface IConsolidationBackupDao {
    List<ConsolidationBackupEntity> findConsolidationIdsByTenantId(Integer tenantId);

    void makeIsDeleteTrueToMarkRestoreSuccessful(Long id);
}
