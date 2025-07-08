package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;

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
    private IShipmentDao shipmentDao;

    @Override
    @Transactional
    public ConsolidationDetails migrateConsolidationV2ToV3(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }
        List<ShipmentDetails> shipmentDetailsList = consolidationDetails1.get().getShipmentsList().stream().toList();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> shipmentMigrationV3Service.migrateShipmentV2ToV3(ship));
        }




        return consolidationDetails1.get();
    }

    @Override
    public ConsolidationDetails migrateConsolidationV3ToV2(ConsolidationDetails consolidationDetails) {
        Optional<ConsolidationDetails> consolidationDetails1 = consolidationDetailsDao.findById(consolidationDetails.getId());
        if(consolidationDetails1.isEmpty()) {
            throw new DataRetrievalFailureException("No Console found with given id: " + consolidationDetails.getId());
        }
        List<ShipmentDetails> shipmentDetailsList = consolidationDetails1.get().getShipmentsList().stream().toList();
        if(CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentDetailsList.forEach(ship -> shipmentMigrationV3Service.migrateShipmentV3ToV2(ship));
        }

        return consolidationDetails1.get();
    }
}
