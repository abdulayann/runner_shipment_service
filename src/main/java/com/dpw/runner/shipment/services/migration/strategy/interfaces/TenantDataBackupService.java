package com.dpw.runner.shipment.services.migration.strategy.interfaces;

public interface TenantDataBackupService {

    void backupTenantData(Integer tenantId);
    void deleteBackupForTenant(Integer tenantId);
}
