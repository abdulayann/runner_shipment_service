package com.dpw.runner.shipment.services.migration.strategy.interfaces;

public interface BackupRestoreStrategy {

    void backup(Integer tenantId);
    void delete(Integer tenantId);
    void restore(Integer tenantId);
}
