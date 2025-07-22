package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.INetworkTransferMigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.Future;

@Slf4j
@Service
public class NetworkTransferMigrationService implements INetworkTransferMigrationService {

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

    @Autowired
    private IConsolidationMigrationV3Service consolidationMigrationV3Service;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private NotesUtil notesUtil;

    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private V1ServiceImpl v1Service;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Override
    public NetworkTransfer migrateNteFromV2ToV3(Long networkTransferId) throws RunnerException {
        log.info("Starting V2 to V3 migration for Network Transfer [id={}]", networkTransferId);
        Optional<NetworkTransfer> networkTransferOptional = networkTransferDao.findById(networkTransferId);
        if(networkTransferOptional.isEmpty()) {
            throw new DataRetrievalFailureException("No NetworkTransfer found with given id: " + networkTransferId);
        }
        NetworkTransfer networkTransfer = networkTransferOptional.get();
        Map<String, Object> entityPayload = networkTransfer.getEntityPayload();
        if(Objects.equals(networkTransfer.getEntityType(), Constants.SHIPMENT)){
            return migrateShipmentV2ToV3(networkTransfer, entityPayload);
        }else{
            return migrateConsolidationV2ToV3(networkTransfer, entityPayload);
        }
    }

    private NetworkTransfer migrateShipmentV2ToV3(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) throws RunnerException {
        Long shipmentId = networkTransfer.getEntityId();
        ShipmentDetails shipmentDetails = shipmentDao.findShipmentByIdWithQuery(shipmentId).orElse(null);
        if(shipmentDetails==null) {
            log.info("No Shipment Details present for Network Transfer with [id={}]", networkTransfer.getId());
            return null;
        }
        ShipmentDetails v2Shipment = jsonHelper.convertValue(shipmentDetails, ShipmentDetails.class);
        ShipmentDetails v3Shipment = shipmentMigrationV3Service.mapShipmentV2ToV3(v2Shipment, null);
        log.info("Mapping completed for Network Transfer -> Shipment [id={}]", networkTransfer.getId());
        StringBuilder text = notesUtil.getShipmentNotes(v2Shipment);
        Notes notes = notesUtil.getNotes(v2Shipment.getId(), "SHIPMENT", text);
        List<Notes> notesList = shipmentDetails.getNotesList()!=null && !shipmentDetails.getNotesList().isEmpty() ? shipmentDetails.getNotesList(): new ArrayList<>();
        notesList.add(notes);
        log.info("Notes added for Network Transfer Shipment [id={}]", networkTransfer.getId());
        v3Shipment.setNotesList(notesList);
        Map<String, Object> stringObjectMap = getCommonShipmentPayload(v3Shipment, entityPayload);
        log.info("New payload created for Network Transfer -> Shipment [id={}]", networkTransfer.getId());
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setMigrationStatus(MigrationStatus.NT_PROCESSED_FOR_V2);
        networkTransferDao.updateWithCustomMigrationStatus(networkTransfer);
        log.info("Migration V2 to V3 complete for Network Transfer for Shipment with [id={}]", networkTransfer.getId());
        return networkTransfer;
    }

    private NetworkTransfer migrateConsolidationV2ToV3(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) {
        Long consolidationId = networkTransfer.getEntityId();
        EntityTransferV3ConsolidationDetails existingPayload = jsonHelper.convertValue(entityPayload, EntityTransferV3ConsolidationDetails.class);
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolidationId);
        log.info("Consolidation Details present for Network Transfer with [id={}]", networkTransfer.getId());
        ConsolidationDetails v2Consol = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        if(v2Consol.getShipmentsList()!=null && !v2Consol.getShipmentsList().isEmpty()){
            for(ShipmentDetails shipmentDetails: v2Consol.getShipmentsList()){
                StringBuilder text = notesUtil.getShipmentNotes(shipmentDetails);
                Notes notes = notesUtil.getNotes(shipmentDetails.getId(), "SHIPMENT", text);
                List<Notes> notesList = shipmentDetails.getNotesList()!=null && !shipmentDetails.getNotesList().isEmpty() ? shipmentDetails.getNotesList(): new ArrayList<>();
                notesList.add(notes);
                shipmentDetails.setNotesList(notesList);
            }
        }
        log.info("Notes added for Network Transfer Consolidation [id={}]", networkTransfer.getId());
        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        ConsolidationDetails v3Consol = consolidationMigrationV3Service.mapConsoleV2ToV3(v2Consol, packingVsContainerGuid);
        EntityTransferV3ConsolidationDetails newPayload = jsonHelper.convertValue(v3Consol, EntityTransferV3ConsolidationDetails.class);
        log.info("Mapping completed for Network Transfer -> Consolidation [id={}]", networkTransfer.getId());
        prepareCommonConsolePayload(v3Consol, existingPayload, newPayload, consolidationDetails);
        var etPackingContainerGuidMap = newPayload.getPackingVsContainerGuid();
        if(!packingVsContainerGuid.isEmpty() && !etPackingContainerGuidMap.isEmpty()) {
            for (Map.Entry<UUID, UUID> packingContainerMap : packingVsContainerGuid.entrySet()) {
                UUID packingGuid = packingContainerMap.getKey();
                UUID containerGuid = packingContainerMap.getValue();
                etPackingContainerGuidMap.put(packingGuid, containerGuid);
            }
        }
        newPayload.setPackingVsContainerGuid(etPackingContainerGuidMap);

        var v3Payload = jsonHelper.convertValue(newPayload, EntityTransferV3ConsolidationDetails.class);
        v3Payload.setSendToBranch(existingPayload.getSendToBranch());
        v3Payload.setShipmentType(existingPayload.getShipmentType());
        String payloadString = jsonHelper.convertToJson(v3Payload);
        Map<String, Object> stringObjectMap = jsonHelper.convertJsonToMap(payloadString);
        log.info("New payload created for Network Transfer -> Consolidation [id={}]", networkTransfer.getId());
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setMigrationStatus(MigrationStatus.NT_PROCESSED_FOR_V2);
        networkTransferDao.updateWithCustomMigrationStatus(networkTransfer);
        log.info("Migration V2 to V3 complete for Network Transfer for Consolidation with [id={}]", networkTransfer.getId());
        return networkTransfer;
    }


    @Override
    public NetworkTransfer migrateNteFromV3ToV2(Long networkTransferId) throws RunnerException {
        log.info("Starting V3 to V2 migration for Network Transfer [id={}]", networkTransferId);
        Optional<NetworkTransfer> networkTransferOptional = networkTransferDao.findById(networkTransferId);
        if(networkTransferOptional.isEmpty()) {
            throw new DataRetrievalFailureException("No NetworkTransfer found with given id: " + networkTransferId);
        }
        NetworkTransfer networkTransfer = networkTransferOptional.get();
        Map<String, Object> entityPayload = networkTransfer.getEntityPayload();
        if(Objects.equals(networkTransfer.getEntityType(), Constants.SHIPMENT)){
            Long shipmentId = networkTransfer.getEntityId();
            ShipmentDetails shipmentDetails = shipmentDao.findShipmentByIdWithQuery(shipmentId).orElse(null);
            if(shipmentDetails==null) {
                log.info("No Shipment Details present for Network Transfer with [id={}]", networkTransfer.getId());
                return null;
            }
            ShipmentDetails v2Shipment = jsonHelper.convertValue(shipmentDetails, ShipmentDetails.class);
            ShipmentDetails v3Shipment = shipmentMigrationV3Service.mapShipmentV3ToV2(v2Shipment, null);
            log.info("Mapping completed for Network Transfer for V3 to V2 -> Shipment [id={}]", networkTransfer.getId());
            Map<String, Object> stringObjectMap = getCommonShipmentPayload(v3Shipment, entityPayload);
            networkTransfer.setEntityPayload(stringObjectMap);
            networkTransfer.setMigrationStatus(MigrationStatus.NT_PROCESSED_FOR_V3);
            log.info("Migration V3 to V2 complete for Network Transfer for Shipment with [id={}]", networkTransfer.getId());
            networkTransferDao.updateWithCustomMigrationStatus(networkTransfer);
            return networkTransfer;
        }else{
            return migrateConsolidationV3ToV2(networkTransfer, entityPayload);
        }
    }

    private Map<String, Object> getCommonShipmentPayload(ShipmentDetails v3Shipment, Map<String, Object> entityPayload) {
        EntityTransferV3ShipmentDetails newPayload = jsonHelper.convertValue(v3Shipment, EntityTransferV3ShipmentDetails.class);
        EntityTransferV3ShipmentDetails existingPayload = jsonHelper.convertValue(entityPayload, EntityTransferV3ShipmentDetails.class);
        newPayload.setMasterData(existingPayload.getMasterData());
        newPayload.setSourceBranchTenantName(existingPayload.getSourceBranchTenantName());
        newPayload.setAdditionalDocs(existingPayload.getAdditionalDocs());
        var v3Payload = jsonHelper.convertValue(newPayload, EntityTransferV3ShipmentDetails.class);
        v3Payload.setSendToBranch(existingPayload.getSendToBranch());
        v3Payload.setDirection(existingPayload.getDirection());
        String payloadString = jsonHelper.convertToJson(v3Payload);
        return jsonHelper.convertJsonToMap(payloadString);
    }

    private NetworkTransfer migrateConsolidationV3ToV2(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) throws RunnerException {
        Long consolidationId = networkTransfer.getEntityId();
        EntityTransferV3ConsolidationDetails existingPayload = jsonHelper.convertValue(entityPayload, EntityTransferV3ConsolidationDetails.class);
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolidationId);
        ConsolidationDetails v3Consol = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);
        ConsolidationDetails v2Consol = consolidationMigrationV3Service.mapConsoleV3ToV2(v3Consol);
        log.info("Mapping completed for Network Transfer for V3 to V2 -> Consolidation [id={}]", networkTransfer.getId());
        EntityTransferV3ConsolidationDetails newPayload = jsonHelper.convertValue(v2Consol, EntityTransferV3ConsolidationDetails.class);

        prepareCommonConsolePayload(v2Consol, existingPayload, newPayload, consolidationDetails);
        var etPackingContainerGuidMap = newPayload.getPackingVsContainerGuid();
        newPayload.setPackingVsContainerGuid(etPackingContainerGuidMap);

        var v3Payload = jsonHelper.convertValue(newPayload, EntityTransferV3ConsolidationDetails.class);
        v3Payload.setSendToBranch(existingPayload.getSendToBranch());
        v3Payload.setShipmentType(existingPayload.getShipmentType());
        String payloadString = jsonHelper.convertToJson(v3Payload);
        Map<String, Object> stringObjectMap = jsonHelper.convertJsonToMap(payloadString);
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setMigrationStatus(MigrationStatus.NT_PROCESSED_FOR_V3);
        log.info("Migration V3 to V2 complete for Network Transfer for Consolidation with [id={}]", networkTransfer.getId());
        networkTransferDao.updateWithCustomMigrationStatus(networkTransfer);
        return networkTransfer;
    }

    private void prepareCommonConsolePayload(ConsolidationDetails consolDetails, EntityTransferV3ConsolidationDetails existingPayload, EntityTransferV3ConsolidationDetails newPayload, ConsolidationDetails consolidationDetails) {
        processConsoleShipments(consolDetails, existingPayload, newPayload);

        newPayload.setMasterData(existingPayload.getMasterData());
        newPayload.setSourceBranchTenantName(existingPayload.getSourceBranchTenantName());
        newPayload.setAdditionalDocs(existingPayload.getAdditionalDocs());

        Map<UUID, UUID> packsVsContainerGuid = new HashMap<>();
        if(consolidationDetails.getContainersList() != null) {
            consolidationDetails.getContainersList().forEach(container -> {
                if(container.getPacksList() != null)
                    container.getPacksList().forEach(pack -> packsVsContainerGuid.put(pack.getGuid(), container.getGuid()));
            });
        }
        newPayload.setPackingVsContainerGuid(packsVsContainerGuid);
    }

    private void processConsoleShipments(ConsolidationDetails consolDetails, EntityTransferV3ConsolidationDetails existingPayload, EntityTransferV3ConsolidationDetails newPayload) {
        List<EntityTransferV3ShipmentDetails> transferShipmentDetails = new ArrayList<>();
        Map<UUID, List<UUID>> containerVsShipmentGuid = new HashMap<>();
        List<EntityTransferV3ShipmentDetails> etConsoleShipments = existingPayload.getShipmentsList();
        if (!consolDetails.getShipmentsList().isEmpty() && etConsoleShipments != null) {
            for (var shipment : consolDetails.getShipmentsList()) {
                // Try to find a matching shipment in etConsoleShipments
                var matchedShipment = etConsoleShipments.stream()
                        .filter(etShipment -> Objects.equals(etShipment.getShipmentId(), shipment.getShipmentId()))
                        .findFirst()
                        .orElse(null);

                if (matchedShipment == null) {
                    // No match found, so this is an extra shipment
                    processExtraShipment(existingPayload, shipment, transferShipmentDetails);
                } else {
                    // Match found, convert and copy necessary fields
                    EntityTransferV3ShipmentDetails entityTransferShipment = jsonHelper.convertValue(shipment, EntityTransferV3ShipmentDetails.class);
                    entityTransferShipment.setMasterData(matchedShipment.getMasterData());
                    entityTransferShipment.setSourceBranchTenantName(matchedShipment.getSourceBranchTenantName());
                    entityTransferShipment.setAdditionalDocs(matchedShipment.getAdditionalDocs());
                    entityTransferShipment.setDirection(matchedShipment.getDirection());
                    entityTransferShipment.setSendToBranch(matchedShipment.getSendToBranch());
                    transferShipmentDetails.add(entityTransferShipment);
                }
                processContainerVsShipmentGuidMap(shipment, containerVsShipmentGuid);
            }
        } else if (!consolDetails.getShipmentsList().isEmpty()) {
            for (var shipment : consolDetails.getShipmentsList()) {
                processExtraShipment(existingPayload, shipment, transferShipmentDetails);
                processContainerVsShipmentGuidMap(shipment, containerVsShipmentGuid);
            }
        }


        newPayload.setShipmentsList(transferShipmentDetails);
        newPayload.setContainerVsShipmentGuid(containerVsShipmentGuid);
    }

    private void processExtraShipment(EntityTransferV3ConsolidationDetails existingPayload, ShipmentDetails shipment, List<EntityTransferV3ShipmentDetails> transferShipmentDetails) {
        EntityTransferV3ShipmentDetails entityTransferShipment = jsonHelper.convertValue(shipment, EntityTransferV3ShipmentDetails.class);
        entityTransferShipment.setSourceBranchTenantName(existingPayload.getSourceBranchTenantName());
        entityTransferShipment.setDirection(existingPayload.getShipmentType());
        entityTransferShipment.setSendToBranch(existingPayload.getSendToBranch());
        transferShipmentDetails.add(entityTransferShipment);
    }

    private void processContainerVsShipmentGuidMap(ShipmentDetails shipment, Map<UUID, List<UUID>> containerVsShipmentGuid) {
        if(shipment.getContainersList() != null) {
            shipment.getContainersList().stream().map(Containers::getGuid).forEach(
                    containerGuid -> {
                        if(!containerVsShipmentGuid.containsKey(containerGuid)) {
                            containerVsShipmentGuid.put(containerGuid, new ArrayList<>());
                        }
                        containerVsShipmentGuid.get(containerGuid).add(shipment.getGuid());
                    }
            );
        }
    }


    public  Map<String, Integer> migrateNetworkTransferV3ToV2ForTenant(Integer tenantId) {
        Map<String, Integer> map = new HashMap<>();
        List<Long> networkTransferList = fetchNteFromDB(List.of(MigrationStatus.NT_CREATED.name(), MigrationStatus.NT_PROCESSED_FOR_V2.name()), tenantId);
        map.put("Total networkTransfer", networkTransferList.size());
        log.info("Starting V3 to V2 networkTransfer migration for tenant [{}]. Found {} shipment(s).", tenantId, networkTransferList.size());

        List<Future<Long>> networkTransferFutures = new ArrayList<>();
        log.info("fetched {} NetworkTransfer for V3 to V2 Migrations", networkTransferList.size());
        for (Long nteId : networkTransferList) {// execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating NetworkTransfer with [id={}]", nteId);
                            NetworkTransfer migrated = migrateNteFromV3ToV2(nteId);
                            log.info("Successfully migrated the NetworkTransfer [oldId={}, newId={}]", nteId, migrated.getId());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("networkTransferFutures migration failed [id={}]: {}", nteId, e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during networkTransferFutures setup [id={}]", nteId, e);
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            networkTransferFutures.add(future);
        }
        List<Long> migratedNetworkTransferIds = MigrationUtil.collectAllProcessedIds(networkTransferFutures);
        map.put("Total networkTransfer Migrated", migratedNetworkTransferIds.size());
        log.info("Network Transfer migration completed: {}/{} migrated for tenant [{}]", migratedNetworkTransferIds.size(), networkTransferList.size(), tenantId);
        return map;
    }

    @Override
    public Map<String, Integer> migrateNetworkTransferV2ToV3ForTenant(Integer tenantId) {
        Map<String, Integer> map = new HashMap<>();
        List<Long> networkTranferList = fetchNteFromDB(List.of(MigrationStatus.NT_CREATED.name(), MigrationStatus.NT_PROCESSED_FOR_V3.name()), tenantId);
        map.put("Total NetworkTransfer", networkTranferList.size());
        log.info("Starting NetworkTransfer migration for tenant [{}]. Found {} NetworkTransfer(s).", tenantId, networkTranferList.size());

        List<Future<Long>> networkTransferFutures = new ArrayList<>();
        log.info("fetched {} networkTransfer for Migrations", networkTranferList.size());
        for (Long nteId : networkTranferList) {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());

                    return trxExecutor.runInTrx(() -> {
                        try {
                            log.info("Migrating NetworkTransfer [id={}]", nteId);
                            NetworkTransfer migrated = migrateNteFromV2ToV3(nteId);
                            log.info("Successfully migrated NetworkTransfer [oldId={}, newId={}]", nteId, migrated.getId());
                            return migrated.getId();
                        } catch (Exception e) {
                            log.error("NetworkTransfer migration failed [id={}]: {}", nteId, e.getMessage(), e);
                            throw new IllegalArgumentException(e);
                        }
                    });
                } catch (Exception e) {
                    log.error("Async failure during NetworkTransfer setup [id={}]", nteId, e);
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            networkTransferFutures.add(future);
        }
        List<Long> migratedNteIds = MigrationUtil.collectAllProcessedIds(networkTransferFutures);
        map.put("Total Network Transfer Migrated", migratedNteIds.size());
        log.info("Network Transfer migration complete: {}/{} migrated for tenant [{}]", migratedNteIds.size(), networkTranferList.size(), tenantId);
        return map;
    }

    private List<Long> fetchNteFromDB(List<String> migrationStatuses, Integer tenantId) {
        return networkTransferDao.findNteForMigrationStatuses(migrationStatuses, tenantId);
    }


}
