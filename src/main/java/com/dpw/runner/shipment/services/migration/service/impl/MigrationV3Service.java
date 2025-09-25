package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.exception.exceptions.MdmException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
import com.dpw.runner.shipment.services.migration.repository.IShipmentBackupRepository;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.INetworkTransferMigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataBackupService;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import static com.dpw.runner.shipment.services.migration.utils.MigrationUtil.collectAllProcessedIds;

@Service
@Slf4j
@Generated
@SuppressWarnings("java:S112")
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

    @Autowired
    private TenantDataBackupService backupService;

    @Autowired
    private MigrationUtil migrationUtil;

    @Autowired
    private IConsolidationBackupRepository consolidationBackupRepository;

    @Autowired
    private IShipmentBackupRepository shipmentBackupRepository;

    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private MDMServiceAdapter mdmServiceAdapter;

    @Autowired
    private ShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private CommonUtils commonUtils;


    private Map<String, BigDecimal> initCodeTeuMap() {
        return migrationUtil.initCodeTeuMap();
    }


    @Override
    public ResponseEntity<IRunnerResponse> migrateV2Tov3Async(Integer tenantId, Long consolId, Long bookingId, Integer count, Integer weightDecimal, Integer volumeDecimal) throws RunnerException {
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.checkMigrationRunning();
        if (shipmentSettingsDetails.isPresent()){
            throw new RunnerException("Another migration is in progress. Please try again later.");
        }
        shipmentSettingsDao.updateMigrationRunningFlag(true, tenantId);

        trxExecutor.runInAsync(() -> {
            try {
                var response = migrateV2ToV3(tenantId, consolId, bookingId, count, weightDecimal, volumeDecimal);
                log.info("Migration from V2 to V3 completed for tenantId: {}. Result: {}", tenantId, response);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, jsonHelper.convertToJson(response), "Migration From V2 to V3", false);
                shipmentSettingsDao.updateMigrationRunningFlag(false, tenantId);
            } catch (Exception e) {
                log.error("Migration V2 to V3 failed for tenantId: {} due to : {}", tenantId, e.getMessage());
                shipmentSettingsDao.updateMigrationRunningFlag(false, tenantId);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, e.getMessage(), "Migration From V2 to V3", true);
                throw new IllegalArgumentException(e);
            }
            return null;
        });
        return ResponseHelper.buildSuccessResponse("Migration v2 to v3 request submitted successfully for tenantId: " + tenantId);
    }

    @Override
    public ResponseEntity<IRunnerResponse> migrateV3ToV2Async(Integer tenantId, Long bookingId, Integer count, Integer weightDecimal, Integer volumeDecimal) throws RunnerException {
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.checkMigrationRunning();
        if (shipmentSettingsDetails.isPresent()){
            throw new RunnerException("Another migration is in progress. Please try again later.");
        }
        shipmentSettingsDao.updateMigrationRunningFlag(true, tenantId);

        trxExecutor.runInAsync(() -> {
            try {
                var response = migrateV3ToV2(tenantId, bookingId, count, weightDecimal, volumeDecimal);
                log.info("Migration from V3 to V2 completed for tenantId: {}. Result: {}", tenantId, response);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, jsonHelper.convertToJson(response), "Migration From V3 to V2", false);
                shipmentSettingsDao.updateMigrationRunningFlag(false, tenantId);
            } catch (Exception e) {
                log.error("Migration V3 to V2 failed for tenantId: {}", tenantId, e);
                emailServiceUtility.sendMigrationAndRestoreEmail(tenantId, e.getMessage(), "Migration From V3 to V2", true);
                shipmentSettingsDao.updateMigrationRunningFlag(false, tenantId);
                throw new IllegalArgumentException(e);
            }
            return null;
        });
        return ResponseHelper.buildSuccessResponse("Migration from V3 to V2 request submitted successfully for tenantId: " + tenantId);
    }


    @Override
    public Map<String, Object> migrateV2ToV3(Integer tenantId, Long consolId, Long bookingId, Integer count, Integer weightDecimal, Integer volumeDecimal) {

        // Taking json backup for respective tenantID.
        v1Service.setAuthContext();
        Map<String, BigDecimal> codeTeuMap = initCodeTeuMap();
        if ((count & 2) > 0)
            backupService.backupTenantData(tenantId);
        Map<String, Object> map = new HashMap<>();

        if ((count & 4) > 0) {
            Map<String, Object> bookingStats = customerBookingV3MigrationService.migrateBookingV2ToV3ForTenant(tenantId, codeTeuMap, weightDecimal, volumeDecimal);
            map.put("Booking :", bookingStats);
        }

        if ((count & 8) > 0)
            map.put("Consolidation : " , this.migrateConsolidation(tenantId,codeTeuMap, weightDecimal, volumeDecimal));

        if ((count & 16) > 0)
            map.put("Shipment :", this.migrateShipment(tenantId));

        if ((count & 32) > 0) {
            Map<String, Object> nteStats = networkTransferMigrationService.migrateNetworkTransferV2ToV3ForTenant(tenantId, codeTeuMap, weightDecimal, volumeDecimal);
            map.put("Network Transfer :", nteStats);
        }

        return map;
    }

    @Override
    public Map<String, Integer> migrateV3ToV2(Integer tenantId, Long bookingId, Integer count, Integer weightDecimal, Integer volumeDecimal) {
        Map<String, Integer> result = new HashMap<>();

        log.info("[Migration] Initiating full V3 to V2 migration for tenant [{}]", tenantId);

        if ((count & 2) > 0) {
            Map<String, Integer> bookingStats = customerBookingV3MigrationService.migrateBookingV3ToV2ForTenant(tenantId, weightDecimal, volumeDecimal);
            result.putAll(bookingStats);
        }

        if ((count & 4) > 0) {
            Map<String, Integer> consolidationStats = consolidationMigrationV3Service.migrateConsolidationsV3ToV2ForTenant(tenantId);
            result.putAll(consolidationStats);
        }

        if ((count & 8) > 0) {
            Map<String, Integer> shipmentStats = shipmentMigrationV3Service.migrateShipmentsV3ToV2ForTenant(tenantId);
            result.putAll(shipmentStats);
        }

        if ((count & 16) > 0) {
            Map<String, Integer> nteStats = networkTransferMigrationService.migrateNetworkTransferV3ToV2ForTenant(tenantId);
            result.putAll(nteStats);
        }

        log.info("[Migration] Completed migration for tenant [{}]: {}", tenantId, result);
        return result;
    }

    private List<Long> fetchConsoleFromDB(List<String> migrationStatuses, Integer tenantId) {
        return consolidationDetailsDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }

    private List<Long> fetchShipmentFromDB(List<String> migrationStatuses, Integer tenantId) {
        return shipmentDao.findAllByMigratedStatuses(migrationStatuses, tenantId);
    }


    private Map<String, Object> migrateShipment(Integer tenantId) {
        Map<String, Object> map = new HashMap<>();
        // Step 2: Fetch all V2 shipments for tenant
        List<Long> shipmentIds = fetchShipmentFromDB(List.of(MigrationStatus.CREATED_IN_V2.name(), MigrationStatus.MIGRATED_FROM_V3.name()), tenantId);
        map.put("Total Shipment", shipmentIds.size());
        log.info("Starting Shipment migration for tenant [{}]. Found {} shipment(s).", tenantId, shipmentIds.size());
        Map<Long, String>  failureMap = new HashMap<>();

        List<Future<Long>> shipmentFutures = new ArrayList<>();
        boolean isUnLocationLocCodeRequired = commonUtils.getBooleanConfigFromAppConfig("ENABLE_CARRIER_ROUTING_MIGRATION_FOR_LOC_CODE");
        log.info("fetched {} shipments for Migrations", shipmentIds.size());
        shipmentIds.forEach(id -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating Shipment [id={}] and start time: {}", id, System.currentTimeMillis());
                            ShipmentDetails migrated = shipmentMigrationV3Service.migrateShipmentV2ToV3(id);
                            log.info("Successfully migrated Shipment [oldId={}, newId={}] and end time: {}", id, migrated.getId(), System.currentTimeMillis());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("Shipment migration failed [id={}]: {}", id, e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during shipment setup [id={}]", id, e);
                    migrationUtil.saveErrorResponse(id, Constants.SHIPMENT, IntegrationType.V2_TO_V3_DATA_SYNC, Status.FAILED, e.getMessage());
                    shipmentBackupRepository.deleteBackupByTenantIdAndShipmentId(id, tenantId);
                    failureMap.put(id, e.getMessage());
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            shipmentFutures.add(future);
        });
        List<Long> migratedShipmentIds = collectAllProcessedIds(shipmentFutures);
        map.put("Total Shipment Migrated " , migratedShipmentIds.size());
        if(!failureMap.isEmpty()) {
            map.put("Failed Shipment Migration: ", failureMap);
        }
        log.info("Shipment migration complete: {}/{} migrated for tenant [{}]", migratedShipmentIds.size(), shipmentIds.size(), tenantId);
        return map;
    }

    private Map<String, Object> migrateConsolidation(Integer tenantId, Map<String, BigDecimal> codeTeuMap, Integer weightDecimal, Integer volumeDecimal) {
        // Step 1: Fetch all V2 consolidations for tenant
        Map<String, Object> map = new HashMap<>();
        List<Long> consolIds = fetchConsoleFromDB(List.of(MigrationStatus.CREATED_IN_V2.name(), MigrationStatus.MIGRATED_FROM_V3.name()), tenantId);
        map.put("Total Consolidation", consolIds.size());
        Map<Long, String>  failureMap = new HashMap<>();
        log.info("Starting V2 to V3 migration for tenant [{}]. Found {} consolidation(s).", tenantId, consolIds.size());
        // Queue for async processing of consolidation migrations
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} consolidation for Migrations", consolIds.size());
        boolean isUnLocationLocCodeRequired = commonUtils.getBooleanConfigFromAppConfig("ENABLE_CARRIER_ROUTING_MIGRATION_FOR_LOC_CODE");
            consolIds.forEach(id -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {

                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating Consolidation [id={}] and start time: {}", id, System.currentTimeMillis());
                            ConsolidationDetails migrated = consolidationMigrationV3Service.migrateConsolidationV2ToV3(id, codeTeuMap, weightDecimal, volumeDecimal, isUnLocationLocCodeRequired);
                            log.info("Successfully migrated Consolidation [oldId={}, newId={}] and end time: {}", id, migrated.getId(), System.currentTimeMillis());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("Consolidation migration failed [id={}]: {}", id, e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during consolidation setup [id={}]", id, e);
                    migrationUtil.saveErrorResponse(id, Constants.CONSOLIDATION, IntegrationType.V2_TO_V3_DATA_SYNC, Status.FAILED, e.getMessage());
                    consolidationBackupRepository.deleteBackupByTenantIdAndConsolidationId(id, tenantId);
                    failureMap.put(id, e.getMessage());
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            queue.add(future);

        });

        List<Long> migratedConsolIds = collectAllProcessedIds(queue);
        map.put("Total Consolidation Migrated", migratedConsolIds.size());
        if (!failureMap.isEmpty()) {
            map.put("Failed Consolidation Migration: ", failureMap);
        }
        log.info("Consolidation migration complete: {}/{} migrated for tenant [{}]", migratedConsolIds.size(), consolIds.size(), tenantId);
        return map;
    }
}
