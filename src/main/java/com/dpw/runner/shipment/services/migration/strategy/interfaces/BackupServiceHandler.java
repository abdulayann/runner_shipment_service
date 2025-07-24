package com.dpw.runner.shipment.services.migration.strategy.interfaces;


public interface BackupServiceHandler {

    void backup(Integer tenantId);
    void rollback(Integer tenantId);
}
