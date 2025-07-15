package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class MigrationV3Service implements IMigrationV3Service {

    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private IConsolidationMigrationV3Service consolidationMigrationV3Service;

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Autowired
    private NotesUtil notesUtil;

    @Autowired
    private V1ServiceImpl v1Service;

    @Override
    public Map<String, Integer> migrateV2ToV3(Integer tenantId, Long consolId) {

        Map<String, Integer> map = new HashMap<>();
//        TenantContext.setCurrentTenant(tenantId);
//        List<ConsolidationDetails> consolidationDetails = fetchConsoleFromDB(false, tenantId);
        List<ConsolidationDetails> consolidationDetails = List.of(consolidationDetailsDao.findConsolidationsById(consolId));

        map.put("Total Consolidation", consolidationDetails.size());
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} consolidation for Migrations", consolidationDetails.size());

        consolidationDetails.forEach(cos -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {

                            ConsolidationDetails response = consolidationMigrationV3Service.migrateConsolidationV2ToV3(cos);
                            return response.getId();
                        } catch (Exception e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            queue.add(future);

        });


        List<Long> processed = collectAllProcessedIds(queue);
        map.put("Total Consolidation Migrated", processed.size());

//        List<ShipmentDetails> shipmentDetailsList = fetchShipmentFromDB(false, tenantId);
//        map.put("Total Shipment", shipmentDetailsList.size());
//        List<Future<Long>> shipmentQueue = new ArrayList<>();
//        log.info("fetched {} shipments for Migrations", shipmentDetailsList.size());
//        for (ShipmentDetails ship : shipmentDetailsList) {
//            // execute async
//            Future<Long> future = trxExecutor.runInAsync(() ->
//                    trxExecutor.runInTrx(() -> {
//                        ShipmentDetails response = null;
//                        try {
//                            v1Service.setAuthContext();
//                            TenantContext.setCurrentTenant(tenantId);
//                            response = shipmentMigrationV3Service.migrateShipmentV2ToV3(ship);
//                        } catch (Exception e) {
//                            throw new IllegalArgumentException(e);
//                        } finally {
//                            v1Service.clearAuthContext();
//                        }
//                        return response.getId();
//                    })
//            );
//            shipmentQueue.add(future);
//        }
//        List<Long> shipmentProcessed = collectAllProcessedIds(shipmentQueue);
//        map.put("Total Shipment Migrated", shipmentProcessed.size());

        return map;

    }

    private List<ConsolidationDetails> fetchConsoleFromDB(boolean isMigratedToV3, Integer tenantId) {
        return consolidationDetailsDao.findAllByIsMigratedToV3(isMigratedToV3, tenantId);
    }

    private List<ShipmentDetails> fetchShipmentFromDB(boolean isMigratedToV3, Integer tenantId) {
        return shipmentDao.findShipmentByIsMigratedToV3(isMigratedToV3, tenantId);
    }

    @Override
    public Map<String, Integer> migrateV3ToV2(Integer tenantId) {
        TenantContext.setCurrentTenant(tenantId);
        Map<String, Integer> map = new HashMap<>();
        List<ConsolidationDetails> consolidationDetails = fetchConsoleFromDB(true, tenantId);
        map.put("Total Consolidation", consolidationDetails.size());
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} consolidation for Migrations", consolidationDetails.size());
        for (ConsolidationDetails cos : consolidationDetails) {// execute async
            Future<Long> future = trxExecutor.runInAsync(() ->
                    // execute in trx
                    trxExecutor.runInTrx(() -> {
                        ConsolidationDetails response = null;
                        try {
                            response = consolidationMigrationV3Service.migrateConsolidationV3ToV2(cos);
                        } catch (RunnerException e) {
                            throw new RuntimeException(e);
                        }
                        return response.getId();
                    })
            );
            queue.add(future);
        }
        List<Long> processed = collectAllProcessedIds(queue);
        map.put("Total Consolidation Migrated", processed.size());

        List<ShipmentDetails> shipmentDetailsList = fetchShipmentFromDB(true, tenantId);
        map.put("Total Shipment", shipmentDetailsList.size());
        List<Future<Long>> shipmentQueue = new ArrayList<>();
        log.info("fetched {} shipments for Migrations", shipmentDetailsList.size());
        for (ShipmentDetails ship : shipmentDetailsList) {// execute async
            Future<Long> future = trxExecutor.runInAsync(() ->
                    // execute in trx
                    trxExecutor.runInTrx(() -> {
                        ShipmentDetails response = null;
                        try {
                            response = shipmentMigrationV3Service.migrateShipmentV3ToV2(ship);
                        } catch (RunnerException e) {
                            throw new RuntimeException(e);
                        }
                        return response.getId();
                    })
            );
            shipmentQueue.add(future);
        }
        List<Long> shipmentProcessed = collectAllProcessedIds(shipmentQueue);
        map.put("Total Shipment Migrated", shipmentProcessed.size());
        TenantContext.removeTenant();
        return map;
    }

    private List<Long> collectAllProcessedIds(List<Future<Long>> queue) {
        List<Long> ids = new ArrayList<>();
        for (Future<Long> future : queue) {
            try {
                ids.add(future.get());
            } catch (Exception er) {
                log.error("Error in trx", er);
            }
        }
        return ids;
    }
}
