package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.INetworkTransferMigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
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
    private ICustomerBookingV3MigrationService customerBookingV3MigrationService;

    @Autowired
    private INetworkTransferMigrationService networkTransferMigrationService;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Autowired
    private NotesUtil notesUtil;

    @Autowired
    private V1ServiceImpl v1Service;

    @Override
    public Map<String, Integer> migrateV2ToV3(Integer tenantId, Long consolId) {
        Map<String, Integer> map = new HashMap<>();
        // Step 1: Fetch all V2 consolidations for tenant
        List<Long> consolIds = fetchConsoleFromDB(List.of(MigrationStatus.CREATED_IN_V2.name(), MigrationStatus.MIGRATED_FROM_V3.name()), tenantId);
        map.put("Total Consolidation", consolIds.size());
        log.info("Starting V2 to V3 migration for tenant [{}]. Found {} consolidation(s).", tenantId, consolIds.size());
        // Queue for async processing of consolidation migrations
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} consolidation for Migrations", consolIds.size());

        consolIds.forEach(id -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating Consolidation [id={}]", id);
                            ConsolidationDetails migrated = consolidationMigrationV3Service.migrateConsolidationV2ToV3(id);
                            log.info("Successfully migrated Consolidation [oldId={}, newId={}]", id, migrated.getId());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("Consolidation migration failed [id={}]: {}", id, e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during consolidation setup [id={}]", id, e);
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            queue.add(future);

        });

        List<Long> migratedConsolIds = collectAllProcessedIds(queue);
        map.put("Total Consolidation Migrated", migratedConsolIds.size());
        log.info("Consolidation migration complete: {}/{} migrated for tenant [{}]", migratedConsolIds.size(), consolIds.size(), tenantId);

        // Step 2: Fetch all V2 shipments for tenant
        List<ShipmentDetails> shipmentDetailsList = fetchShipmentFromDB(List.of(MigrationStatus.CREATED_IN_V2.name(), MigrationStatus.MIGRATED_FROM_V3.name()), tenantId);
//        Optional<ShipmentDetails> shipmentByIdWithQuery = shipmentDao.findShipmentByIdWithQuery(consolId);
//        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentByIdWithQuery.get());
        map.put("Total Shipment", shipmentDetailsList.size());
        log.info("Starting Shipment migration for tenant [{}]. Found {} shipment(s).", tenantId, shipmentDetailsList.size());

        List<Future<Long>> shipmentFutures = new ArrayList<>();
        log.info("fetched {} shipments for Migrations", shipmentDetailsList.size());
        for (ShipmentDetails ship : shipmentDetailsList) {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating Shipment [id={}]", ship.getId());
                            ShipmentDetails migrated = shipmentMigrationV3Service.migrateShipmentV2ToV3(ship);
                            log.info("Successfully migrated Shipment [oldId={}, newId={}]", ship.getId(), migrated.getId());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("Shipment migration failed [id={}]: {}", ship.getId(), e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during shipment setup [id={}]", ship.getId(), e);
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            shipmentFutures.add(future);
        }
        List<Long> migratedShipmentIds = collectAllProcessedIds(shipmentFutures);
        map.put("Total Shipment Migrated", migratedShipmentIds.size());
        log.info("Shipment migration complete: {}/{} migrated for tenant [{}]", migratedShipmentIds.size(), shipmentDetailsList.size(), tenantId);

        Map<String, Integer> nteStats = networkTransferMigrationService.migrateNetworkTransferV2ToV3ForTenant(tenantId);
        map.putAll(nteStats);

        return map;
    }

    private List<CustomerBooking> fetchBookingFromDB(boolean isMigratedToV3, Integer tenantId) {
        return customerBookingDao.findAllByIsMigratedToV3(isMigratedToV3, tenantId);
    }

    @Override
    public Map<String, Integer> migrateV3ToV2(Integer tenantId) {
        Map<String, Integer> result = new HashMap<>();

        log.info("[Migration] Initiating full V3 to V2 migration for tenant [{}]", tenantId);

        Map<String, Integer> consolidationStats = consolidationMigrationV3Service.migrateConsolidationsV3ToV2ForTenant(tenantId);
        result.putAll(consolidationStats);

        Map<String, Integer> shipmentStats = shipmentMigrationV3Service.migrateShipmentsV3ToV2ForTenant(tenantId);
        result.putAll(shipmentStats);

        Map<String, Integer> nteStats = networkTransferMigrationService.migrateNetworkTransferV3ToV2ForTenant(tenantId);
        result.putAll(nteStats);

        log.info("[Migration] Completed migration for tenant [{}]: {}", tenantId, result);
        return result;
    }

    @Override
    public Map<String, Integer> bookingV2ToV3Migration(Integer tenantId, Long bookingId) {
        Map<String, Integer> map = new HashMap<>();
//        List<CustomerBooking> customerBookingList = fetchBookingFromDB(false, tenantId);
        List<CustomerBooking> customerBookingList = List.of(customerBookingDao.findById(bookingId).get());
        map.put("Total Bookings", customerBookingList.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookings for Migrations", customerBookingList.size());
        customerBookingList.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = customerBookingV3MigrationService.getCodeTeuMapping();
                    return trxExecutor.runInTrx(() -> {
                        try {

                            CustomerBooking response = customerBookingV3MigrationService.migrateBookingV2ToV3(booking, codeTeuMap);
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
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
        return map;
    }

    @Override
    public Map<String, Integer> bookingV3ToV2Migration(Integer tenantId, Long bookingId) {
        Map<String, Integer> map = new HashMap<>();
//        List<CustomerBooking> customerBookingList = fetchBookingFromDB(false, tenantId);
        List<CustomerBooking> customerBookingList = List.of(customerBookingDao.findById(bookingId).get());
        map.put("Total Bookings", customerBookingList.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        log.info("fetched {} bookings for Migrations", customerBookingList.size());
        customerBookingList.forEach(booking -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    Map<String, BigDecimal> codeTeuMap = customerBookingV3MigrationService.getCodeTeuMapping();
                    return trxExecutor.runInTrx(() -> {
                        try {

                            CustomerBooking response = customerBookingV3MigrationService.migrateBookingV3ToV2(booking, codeTeuMap);
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
            bookingQueue.add(future);
        });

        List<Long> bookingsProcessed = collectAllProcessedIds(bookingQueue);
        map.put("Total Bookings Migrated", bookingsProcessed.size());
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

    private List<Long> fetchConsoleFromDB(List<String> migrationStatuses, Integer tenantId) {
        return consolidationDetailsDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

    private List<ShipmentDetails> fetchShipmentFromDB(List<String> migrationStatuses, Integer tenantId) {
        return shipmentDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

}
