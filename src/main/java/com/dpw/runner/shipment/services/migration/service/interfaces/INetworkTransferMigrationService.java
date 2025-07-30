package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.Map;


public interface INetworkTransferMigrationService {
    NetworkTransfer migrateNteFromV2ToV3(Long networkTransferId) throws RunnerException;
    NetworkTransfer migrateNteFromV3ToV2(Long networkTransferId) throws RunnerException;
    Map<String, Integer> migrateNetworkTransferV3ToV2ForTenant(Integer tenantId);
    Map<String, Integer> migrateNetworkTransferV2ToV3ForTenant(Integer tenantId);
}
