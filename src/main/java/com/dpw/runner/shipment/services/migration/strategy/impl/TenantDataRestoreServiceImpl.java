package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataRestoreService;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.internal.util.stereotypes.Lazy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
@Generated
public class TenantDataRestoreServiceImpl implements TenantDataRestoreService {

    @Lazy
    @Autowired
    private TenantDataRestoreServiceImpl self;
    private final List<RestoreServiceHandler> restoreHandlers;
    private final EmailServiceUtility emailServiceUtility;
    private final HelperExecutor trxExecutor;

    @Override
    public ResponseEntity<String> restoreTenantDataAsync(Integer tenantId) {

        trxExecutor.runInAsync(() ->  {
            try {
                long startTime = System.currentTimeMillis();
                restoreTenantData(tenantId);
                log.info("Restore from V3 to V2 completed for tenantId: {}", tenantId);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, "completed successfully : ", "Restore From V3 to V2",  false);
            } catch (Exception e) {
                log.error("Restore from V3 to V2 failed for tenantId: {} due to : {}", tenantId, e.getMessage());
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, e.getMessage(), "Restore From V3 to V2", true);
                throw new IllegalArgumentException(e);
            }
            return null;
        });
        return ResponseEntity.ok("Restore activity submitted successfully for tenant: " + tenantId);
    }


    @Override
    public void restoreTenantData(Integer tenantId) {
            try {
                executeHandler(getHandler(NetworkTransferRestoreHandler.class), tenantId);
                executeHandler(getHandler(ConsolidationRestoreHandler.class), tenantId);
                executeHandler(getHandler(ShipmentRestoreHandler.class), tenantId);
                executeHandler(getHandler(CustomerBookingRestoreHandler.class), tenantId);
            } catch (Exception e) {
                log.error("Restore failed for tenant: {}", tenantId, e);
                throw new IllegalArgumentException(e);
            }
    }

    private RestoreServiceHandler getHandler(Class<? extends RestoreServiceHandler> clazz) {
        return restoreHandlers.stream()
                .filter(clazz::isInstance)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Handler not found: " + clazz.getSimpleName()));
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
            throw new IllegalArgumentException(e);
        }
    }
}
