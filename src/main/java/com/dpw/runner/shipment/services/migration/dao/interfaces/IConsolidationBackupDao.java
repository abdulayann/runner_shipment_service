package com.dpw.runner.shipment.services.migration.dao.interfaces;

import java.util.List;

public interface IConsolidationBackupDao {
    List<Long> findConsolidationIdsByTenantId(Integer tenantId);

    void makeIsDeleteTrueToMarkRestoreSuccessful(Long id);
}
