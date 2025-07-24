package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupServiceHandler;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataBackupService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

@Service
@RequiredArgsConstructor
@Slf4j
public class TenantDataBackupServiceImpl implements TenantDataBackupService {

    private final List<BackupServiceHandler> backupHandlers;
    private final ThreadPoolTaskExecutor asyncBackupServiceExecutor;

    @Override
    public void backupTenantData(Integer tenantId) {

        log.info("Starting backup for tenantId: {}", tenantId);
        try {
            List<CompletableFuture<Void>> futures = initiateBackupTasks(tenantId);
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Successfully completed backup for tenantId: {}", tenantId);
        } catch (CompletionException e) {
            log.error("Backup failed for tenantId: {}. Error: {}. Initiating rollback.", tenantId, e.getMessage(), e);
            rollbackBackupForTenant(tenantId);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    private void rollbackBackupForTenant(Integer tenantId) {

        log.info("Initiating rollback for tenantId: {}", tenantId);
        backupHandlers.forEach(handler -> {
            try {
                log.debug("Executing rollback with handler class: {}", handler.getClass());
                handler.rollback(tenantId);
            } catch (Exception ex) {
                log.error("Rollback failed for {}", handler.getClass().getSimpleName(), ex);
            }
        });
        log.info("Successfully executed rollback foe tenantId: {}", tenantId);
    }

    private List<CompletableFuture<Void>> initiateBackupTasks(Integer tenantId) {

        return backupHandlers.stream()
                .map(handler -> CompletableFuture.runAsync(() ->
                    handler.backup(tenantId), asyncBackupServiceExecutor)).toList();
    }
}
