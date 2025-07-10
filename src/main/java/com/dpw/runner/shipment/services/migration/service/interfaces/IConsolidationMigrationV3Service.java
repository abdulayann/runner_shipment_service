package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

public interface IConsolidationMigrationV3Service {
    ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails);
    ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException;
}
