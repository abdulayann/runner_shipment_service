package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

@Service
@RequiredArgsConstructor
@Slf4j
public class BackupServiceImpl implements BackupService {

    private final List<BackupHandler> backupHandlers;
    private final ThreadPoolTaskExecutor asyncExecutor;

    @Override
    public void backupTenantData(Integer tenantId) {

        log.info("Starting backup for tenantId: {}", tenantId);
        List<CompletableFuture<Void>> futures = new ArrayList<>();

        for (BackupHandler handler : backupHandlers) {
            CompletableFuture<Void> future = CompletableFuture.runAsync(() ->
                    handler.backup(tenantId), asyncExecutor).exceptionally(ex -> {
                throw new CompletionException(ex);
            });
            futures.add(future);
        }

        try {
            // Wait for all handlers to complete or throw immediately on any failure
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Backup process completed successfully for tenant: {}", tenantId);
        } catch (CompletionException e) {
            log.error("Backup process failed for tenant: {}. Triggering rollback for all handlers.", tenantId);
            backupHandlers.forEach(handler -> {
                try {
                    handler.rollback(tenantId);
                } catch (Exception rollbackEx) {
                    //If one rollback failed, continue with other rollbacks.
                    log.error("Rollback failed for handler: {} and tenant: {}", handler.getClass().getSimpleName(), tenantId, rollbackEx);
                }
            });

            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e.getCause());
        }
    }
}
