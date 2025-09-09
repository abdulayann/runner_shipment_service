package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
    private final JsonHelper jsonHelper;

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
                var response =   restoreTenantData(tenantId, count);
                log.info("Restore from V3 to V2 completed for tenantId: {}", tenantId);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, jsonHelper.convertToJson(response) + "took : " + (System.currentTimeMillis()-startTime) / (60000.0) + " min", "Restore From V3 to V2",  false);
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
    public Map<String, Object> restoreTenantData(Integer tenantId, Integer count) {
        Map<String, Object> map = new HashMap<>();

        if ((count & 2) > 0) {
            Map<String, Object> nt = executeHandler(getHandler(NetworkTransferRestoreHandler.class), tenantId);
            map.put("Network Transfer :", nt);
        }
        if ((count & 4) > 0) {
            Map<String, Object> console = executeHandler(getHandler(ConsolidationRestoreHandler.class), tenantId);
            map.put("Consolidation :", console);
        }
        if ((count & 8) > 0) {
            Map<String, Object> shipment = executeHandler(getHandler(ShipmentRestoreHandler.class), tenantId);
            map.put("Shipment :", shipment);
        }
        if ((count & 16) > 0) {
            Map<String, Object> booking = executeHandler(getHandler(CustomerBookingRestoreHandler.class), tenantId);
            map.put("Booking :", booking);
        }
    return map;
    }

    private RestoreServiceHandler getHandler(Class<? extends RestoreServiceHandler> clazz) {
        return restoreHandlers.stream()
                .filter(clazz::isInstance)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Handler not found: " + clazz.getSimpleName()));
    }

    private Map<String, Object> executeHandler(RestoreServiceHandler handler, Integer tenantId) {
        try {
            log.info("Starting serial execution of {} for tenant {}",
                    handler.getClass().getSimpleName(), tenantId);
            // Each handler executes its work serially within this thread
           return handler.restore(tenantId);
        } catch (Exception e) {
            log.error("Handler {} failed during serial execution",
                    handler.getClass().getSimpleName(), e);
            throw new IllegalArgumentException(e);
        }
    }
}
