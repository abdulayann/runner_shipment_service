package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
import java.util.Optional;

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
    private final ShipmentSettingsDao shipmentSettingsDao;

    @Override
    public ResponseEntity<String> restoreTenantDataAsync(Integer tenantId, Integer count) throws RunnerException {
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.checkRestoreRunning();
        if (shipmentSettingsDetails.isPresent()){
            throw new RunnerException("Another restore is in progress. Please try again later.");
        }
        shipmentSettingsDao.updateIsRestoreRunningFlag(true, tenantId);

        trxExecutor.runInAsync(() ->  {
            try {
                long startTime = System.currentTimeMillis();
                restoreTenantData(tenantId, count);
                log.info("Restore from V3 to V2 completed for tenantId: {}", tenantId);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, "completed successfully : " + (System.currentTimeMillis()-startTime), "Restore From V3 to V2",  false);
                shipmentSettingsDao.updateIsRestoreRunningFlag(false, tenantId);
            } catch (Exception e) {
                log.error("Restore from V3 to V2 failed for tenantId: {} due to : {}", tenantId, e.getMessage());
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, e.getMessage(), "Restore From V3 to V2", true);
                shipmentSettingsDao.updateIsRestoreRunningFlag(false, tenantId);
                throw new IllegalArgumentException(e);
            }
            return null;
        });
        return ResponseEntity.ok("Restore activity submitted successfully for tenant: " + tenantId);
    }


    @Override
    public void restoreTenantData(Integer tenantId, Integer count) {
            try {
                if ((count & 2) > 0)
                    executeHandler(getHandler(NetworkTransferRestoreHandler.class), tenantId);
                if ((count & 4) > 0)
                    executeHandler(getHandler(ConsolidationRestoreHandler.class), tenantId);
                if ((count & 8) > 0)
                    executeHandler(getHandler(ShipmentRestoreHandler.class), tenantId);
                if ((count & 16) > 0)
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
