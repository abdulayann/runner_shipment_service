package com.dpw.runner.shipment.services.migration.strategy.interfaces;

public interface TenantDataBackupService {

    void backupTenantData(Integer tenantId);
}
