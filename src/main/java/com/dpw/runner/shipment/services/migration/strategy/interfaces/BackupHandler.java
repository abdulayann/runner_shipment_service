package com.dpw.runner.shipment.services.migration.strategy.interfaces;

import java.util.List;

public interface BackupHandler {

    void backup(Integer tenantId);
    void rollback(Integer tenantId);

}
