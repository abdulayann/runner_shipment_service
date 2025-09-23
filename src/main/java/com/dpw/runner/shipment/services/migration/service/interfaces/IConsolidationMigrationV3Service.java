package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

public interface IConsolidationMigrationV3Service {

    ConsolidationDetails migrateConsolidationV2ToV3(Long consolidationId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal);
    ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails, Map<UUID, UUID> packingVsContainerGuid, Boolean canUpdateTransportInstructions, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal);
    ConsolidationDetails mapConsoleV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException;
    ConsolidationDetails migrateConsolidationV3ToV2(Long consolidationId) throws RunnerException;

    Map<String, Integer> migrateConsolidationsV3ToV2ForTenant(Integer tenantId);
}
