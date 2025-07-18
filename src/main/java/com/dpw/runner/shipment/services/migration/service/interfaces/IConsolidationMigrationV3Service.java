package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.Map;

public interface IConsolidationMigrationV3Service {
    ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails);
    ConsolidationDetails migrateConsolidationV3ToV2(Long consolidationId) throws RunnerException;

    Map<String, Integer> migrateConsolidationsV3ToV2ForTenant(Integer tenantId);
}
