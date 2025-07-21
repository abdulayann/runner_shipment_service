package com.dpw.runner.shipment.services.migration.strategy.interfaces;


public interface BackupHandler {

    void backup(Integer tenantId);
    void rollback(Integer tenantId);

}
