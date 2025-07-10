package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

@Service
@Slf4j
public class ConsolidationMigrationV3Service implements IConsolidationMigrationV3Service {

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        // Convert V2 Console and Attached shipment to V3
        ConsolidationDetails console = mapConsoleV2ToV3(consolidationDetails1.get());

        // ContainerSave
        // Attach Container to shipment
        // PackingSave
        // ShipmentSave
        // ConsoleSave


        return console;
    }

    public ConsolidationDetails mapConsoleV2ToV3(ConsolidationDetails consolidationDetails) {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        // Container splitting and creation (With new Guids)

        // Container Attachment to shipment (add containers in shipment)


        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();
        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {
                    shipmentMigrationV3Service.migrateShipmentV2ToV3(ship, packingVsContainerGuid);
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }

        // Update Container from attached packages (based on packingVsContainerGuid)

        // Console summary update

        // Console to shipment cutoff fields update, Agents, etc (Refer excel)

        return console;
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) throws RunnerException {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }

        // Convert V3 Console and Attached shipment to V2
        ConsolidationDetails console = mapConsoleV3ToV2(consolidationDetails1.get());

        // ContainerSave
        // PackingSave
        // ShipmentSave
        // ConsoleSave

        return console;
    }

    public ConsolidationDetails mapConsoleV3ToV2(ConsolidationDetails consolidationDetails) {
        ConsolidationDetails console = jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Map<String, EntityTransferContainerType> containerTypeMap = shipmentMigrationV3Service.fetchContainerTypeDetails(console.getContainersList());

        List<ShipmentDetails> shipmentDetailsList = console.getShipmentsList().stream().toList();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> {
                try {
                    shipmentMigrationV3Service.migrateShipmentV3ToV2(ship, containerTypeMap);
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }

        // Console utilisation update

        return console;
    }
}
