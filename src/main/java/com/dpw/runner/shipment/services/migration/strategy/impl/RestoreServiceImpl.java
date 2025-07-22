package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreHandler;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.internal.util.stereotypes.Lazy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

@Service
@RequiredArgsConstructor
@Slf4j
public class RestoreServiceImpl implements RestoreService {

    private final List<RestoreHandler> restoreHandlers;
    private final ThreadPoolTaskExecutor asyncExecutor;

    @Lazy
    @Autowired
    private RestoreServiceImpl self;

    @Override
    public void restoreTenantData(Integer tenantId) {

        log.info("Starting restore for tenantId: {}", tenantId);
        List<CompletableFuture<Void>> futures = restoreHandlers.stream()
                .map(handler -> CompletableFuture.runAsync(
                        () -> self.executeHandlerSafely(handler, tenantId),
                        asyncExecutor
                ))
                .toList();

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        } catch (Exception e) {
            log.error("Restore failed for tenant : {}", tenantId, e);
            throw new RestoreFailureException("Restore failed : ", e);
        }
    }

    private void executeHandlerSafely(RestoreHandler handler, Integer tenantId) {

        try {
            log.info("Executing {} for tenant {}",
                    handler.getClass().getSimpleName(), tenantId);
            handler.restore(tenantId);
        } catch (Exception e) {
            log.error("Handler {} failed", handler.getClass().getSimpleName(), e);
            throw new CompletionException(e);
        }
    }
}
