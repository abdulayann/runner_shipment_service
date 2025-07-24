package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataBackupService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class TenantDataBackupServiceImpl implements TenantDataBackupService {

    private final ShipmentBackupHandler shipmentBackupHandler;
    private final ConsolidationBackupHandler consolidationBackupHandler;
    private final CustomerBookingBackupHandler customerBookingBackupHandler;

    @Autowired
    @Lazy
    TenantDataBackupServiceImpl self;

    @Override
    public void backupTenantData(Integer tenantId) {
        try {
            CompletableFuture<Void> shipmentFuture = shipmentBackupHandler.backupAsync(tenantId);
            CompletableFuture<Void> consolidationFuture = consolidationBackupHandler.backupAsync(tenantId);
            CompletableFuture<Void> bookingFuture = customerBookingBackupHandler.backupAsync(tenantId);
            CompletableFuture.allOf(shipmentFuture, consolidationFuture, bookingFuture).join();
            log.info("Tenant backup successful for tenantId: {}", tenantId);
        } catch (Exception e) {
            log.error("Error occurred during backup for tenantId: {}. Rolling back.", tenantId, e);
            self.cleanupFailedBackup(tenantId);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    @Transactional
    public void cleanupFailedBackup(Integer tenantId) {

        log.info("Cleaning up failed backup for tenantId: {}", tenantId);
        shipmentBackupHandler.rollback(tenantId);
        consolidationBackupHandler.rollback(tenantId);
        customerBookingBackupHandler.rollback(tenantId);
        log.info("Cleanup completed for tenantId: {}", tenantId);
    }
}
