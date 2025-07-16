package com.dpw.runner.shipment.services.migration.strategy.interfaces;

public interface BackupService {

    void backupTenantData(Integer tenantId);
    void removeTenantData(Integer tenantId);
}
