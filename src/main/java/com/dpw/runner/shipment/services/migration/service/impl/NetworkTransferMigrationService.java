package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.INetworkTransferMigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

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

    @Override
    public NetworkTransfer migrateNteFromV2ToV3(NetworkTransfer networkTransfer) throws RunnerException {
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
        if(shipmentDetails==null)
            return null;
        ShipmentDetails v2Shipment = jsonHelper.convertValue(shipmentDetails, ShipmentDetails.class);
////        notesUtil.addNotesForShipment(v2Shipment);
        ShipmentDetails v3Shipment = shipmentMigrationV3Service.mapShipmentV2ToV3(v2Shipment, null);
        Map<String, Object> stringObjectMap = getCommonShipmentPayload(v3Shipment, entityPayload);
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setIsMigratedToV3(true);
        networkTransferDao.save(networkTransfer);
        return networkTransfer;
    }

    private NetworkTransfer migrateConsolidationV2ToV3(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) {
        Long consolidationId = networkTransfer.getEntityId();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolidationId);
        ConsolidationDetails v2Consol = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);
////        notesUtil.addNotesForConsolidation(v2Consol);
        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        ConsolidationDetails v3Consol = consolidationMigrationV3Service.mapConsoleV2ToV3(v2Consol, packingVsContainerGuid);
        EntityTransferV3ConsolidationDetails newPayload = jsonHelper.convertValue(v3Consol, EntityTransferV3ConsolidationDetails.class);
        EntityTransferV3ConsolidationDetails existingPayload = jsonHelper.convertValue(entityPayload, EntityTransferV3ConsolidationDetails.class);
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
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setIsMigratedToV3(true);
        networkTransferDao.save(networkTransfer);
        return networkTransfer;
    }



    @Override
    public NetworkTransfer migrateNteFromV3ToV2(NetworkTransfer networkTransfer) throws RunnerException {
        Map<String, Object> entityPayload = networkTransfer.getEntityPayload();
        if(Objects.equals(networkTransfer.getEntityType(), Constants.SHIPMENT)){
            Long shipmentId = networkTransfer.getEntityId();
            ShipmentDetails shipmentDetails = shipmentDao.findShipmentByIdWithQuery(shipmentId).orElse(null);
            if(shipmentDetails==null)
                return null;
            ShipmentDetails v2Shipment = jsonHelper.convertValue(shipmentDetails, ShipmentDetails.class);
            ShipmentDetails v3Shipment = shipmentMigrationV3Service.mapShipmentV3ToV2(v2Shipment, null);
            Map<String, Object> stringObjectMap = getCommonShipmentPayload(v3Shipment, entityPayload);
            networkTransfer.setEntityPayload(stringObjectMap);
            networkTransfer.setIsMigratedToV3(true);
            networkTransferDao.save(networkTransfer);
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
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolidationId);
        ConsolidationDetails v3Consol = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);
        ConsolidationDetails v2Consol = consolidationMigrationV3Service.mapConsoleV3ToV2(v3Consol);
        EntityTransferV3ConsolidationDetails newPayload = jsonHelper.convertValue(v2Consol, EntityTransferV3ConsolidationDetails.class);
        EntityTransferV3ConsolidationDetails existingPayload = jsonHelper.convertValue(entityPayload, EntityTransferV3ConsolidationDetails.class);
        prepareCommonConsolePayload(v2Consol, existingPayload, newPayload, consolidationDetails);
        var etPackingContainerGuidMap = newPayload.getPackingVsContainerGuid();
        newPayload.setPackingVsContainerGuid(etPackingContainerGuidMap);


        var v3Payload = jsonHelper.convertValue(newPayload, EntityTransferV3ConsolidationDetails.class);
        v3Payload.setSendToBranch(existingPayload.getSendToBranch());
        v3Payload.setShipmentType(existingPayload.getShipmentType());
        String payloadString = jsonHelper.convertToJson(v3Payload);
        Map<String, Object> stringObjectMap = jsonHelper.convertJsonToMap(payloadString);
        networkTransfer.setEntityPayload(stringObjectMap);
        networkTransfer.setIsMigratedToV3(false);
        networkTransferDao.save(networkTransfer);
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
        if(!consolDetails.getShipmentsList().isEmpty() && etConsoleShipments!=null) {
                for (var shipment : consolDetails.getShipmentsList()) {
                    for(var etShipment: etConsoleShipments) {
                        if(!Objects.equals(etShipment.getShipmentId(), shipment.getShipmentId()))
                            continue;
                        EntityTransferV3ShipmentDetails entityTransferShipment = jsonHelper.convertValue(shipment, EntityTransferV3ShipmentDetails.class);
                        // populate master data and other fields
                        entityTransferShipment.setMasterData(etShipment.getMasterData());
                        entityTransferShipment.setSourceBranchTenantName(etShipment.getSourceBranchTenantName());
                        entityTransferShipment.setAdditionalDocs(etShipment.getAdditionalDocs());
                        entityTransferShipment.setDirection(etShipment.getDirection());
                        entityTransferShipment.setSendToBranch(etShipment.getSendToBranch());
                        transferShipmentDetails.add(entityTransferShipment);
                        processContainerVsShipmentGuidMap(shipment, containerVsShipmentGuid);
                    }
                }
            }

        newPayload.setShipmentsList(transferShipmentDetails);
        newPayload.setContainerVsShipmentGuid(containerVsShipmentGuid);
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


}
