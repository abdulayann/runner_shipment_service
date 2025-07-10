package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.service.impl.ConsolidationService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

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

    @Override
    public Map<String, Integer> migrateV2ToV3(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest) {

        Map<String, Integer> map = new HashMap<>();
        List<ConsolidationDetails> consolidationDetails = fetchConsoleFromDB(consoleRequest);
        map.put("Total Consolidation", consolidationDetails.size());
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} consolidation for Migrations", consolidationDetails.size());
        consolidationDetails.forEach(cos -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() ->
                    // execute in trx
                    trxExecutor.runInTrx(() -> {
                        ConsolidationDetails response = consolidationMigrationV3Service.migrateConsolidationV2ToV3(cos);
                        return response.getId();
                    })
            );
            queue.add(future);
        });
        List<Long> processed = collectAllProcessedIds(queue);
        map.put("Total Migrated", processed.size());
        return map;

    }

    private List<ConsolidationDetails> fetchConsoleFromDB(ListCommonRequest listCommonRequest) {
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class, ConsolidationService.tableNames);
        Page<ConsolidationDetails> consolPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        return consolPage.getContent();
    }

    private List<ShipmentDetails> fetchShipmentFromDB(ListCommonRequest listCommonRequest) {
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentConstants.TABLES_NAMES);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return shipmentDetailsPage.getContent();
    }

    @Override
    public Map<String, Integer> migrateV3ToV2(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest) {

        Map<String, Integer> map = new HashMap<>();
        List<ConsolidationDetails> consolidationDetails = fetchConsoleFromDB(consoleRequest);
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

        List<ShipmentDetails> shipmentDetailsList = fetchShipmentFromDB(shipmentRequest);
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
