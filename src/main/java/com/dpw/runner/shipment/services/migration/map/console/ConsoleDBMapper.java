package com.dpw.runner.shipment.services.migration.map.console;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.migration.IDBMigrationMapper;

public abstract class ConsoleDBMapper<D> implements IDBMigrationMapper<ConsolidationDetails, D> {
    @Override
    public abstract ConsolidationDetails mapToEntity(D d) ;

    @Override
    public abstract D mapFromEntity(ConsolidationDetails d) ;
}
