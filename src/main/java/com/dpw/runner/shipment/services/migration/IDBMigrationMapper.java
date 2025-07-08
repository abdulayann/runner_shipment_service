package com.dpw.runner.shipment.services.migration;

public interface IDBMigrationMapper<E, D> {

    E mapToEntity(D d);

    D mapFromEntity(E d);
}
