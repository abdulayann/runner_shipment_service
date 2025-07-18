package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;
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

    private final List<BackupHandler> backupHandlers;
    private final ThreadPoolTaskExecutor asyncBackupServiceExecutor;

    @Override
    public void backupTenantData(Integer tenantId) {

        log.info("Starting backup for tenantId: {}", tenantId);
        List<CompletableFuture<Void>> futures = initiateBackupTasks(tenantId);

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        } catch (CompletionException e) {
            log.error("Backup failed for tenant: {}. Initiating rollback.", tenantId, e);
            rollbackBackupForTenant(tenantId);
        }
        log.info("Completed backup for tenantId: {}", tenantId);
    }


    @Override
    public void deleteBackupForTenant(Integer tenantId) {
        rollbackBackupForTenant(tenantId);
    }

    private void rollbackBackupForTenant(Integer tenantId) {

        backupHandlers.forEach(handler -> {
            try {
                handler.rollback(tenantId);
            } catch (Exception ex) {
                log.error("Rollback failed for {}", handler.getClass().getSimpleName(), ex);
            }
        });
    }

    private List<CompletableFuture<Void>> initiateBackupTasks(Integer tenantId) {
        return backupHandlers.stream()
                .map(handler -> CompletableFuture.runAsync(() -> {
                    log.info("Starting execution of {}", handler.getClass().getSimpleName());
                    handler.backup(tenantId);
                }, asyncBackupServiceExecutor)
                        .exceptionally(ex -> {
                            log.error("Async backup failed for handler: {}", handler.getClass().getSimpleName(), ex);
                            return null;
                        })).toList();

    }
}
