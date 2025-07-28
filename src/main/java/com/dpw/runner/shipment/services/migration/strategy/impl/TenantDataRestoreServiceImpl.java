package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataRestoreService;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.internal.util.stereotypes.Lazy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

@Service
@RequiredArgsConstructor
@Slf4j
@Generated
public class TenantDataRestoreServiceImpl implements TenantDataRestoreService {

    @Lazy
    @Autowired
    private TenantDataRestoreServiceImpl self;
    private final List<RestoreServiceHandler> restoreHandlers;
    private final TransactionTemplate transactionTemplate;
    private final ThreadPoolTaskExecutor asyncRestoreHandlerExecutor;

    @Override
    public void restoreTenantData(Integer tenantId) {
        transactionTemplate.execute(status -> {
            try {
                // Execute all handlers in parallel
                List<CompletableFuture<Void>> futures = restoreHandlers.stream()
                        .map(handler -> CompletableFuture.runAsync(
                                () -> executeHandler(handler, tenantId),
                                asyncRestoreHandlerExecutor
                        ))
                        .toList();

                // Wait for all handlers to complete
                CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
                return null;
            } catch (Exception e) {
                status.setRollbackOnly();
                log.error("Restore failed for tenant: {}", tenantId, e);
                throw new RestoreFailureException("Restore failed for tenant: " + tenantId, e);
            }
        });
    }

    private void executeHandler(RestoreServiceHandler handler, Integer tenantId) {
        try {
            log.info("Starting serial execution of {} for tenant {}",
                    handler.getClass().getSimpleName(), tenantId);
            // Each handler executes its work serially within this thread
            handler.restore(tenantId);
            log.info("Completed serial execution of {} for tenant {}",
                    handler.getClass().getSimpleName(), tenantId);
        } catch (Exception e) {
            log.error("Handler {} failed during serial execution",
                    handler.getClass().getSimpleName(), e);
            throw new CompletionException(e);
        }
    }
}
