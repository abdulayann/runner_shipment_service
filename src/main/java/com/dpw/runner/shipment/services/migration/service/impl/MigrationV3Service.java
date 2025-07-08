package com.dpw.runner.shipment.services.migration.service.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;

@Service
@Slf4j
public class MigrationV3Service implements IMigrationV3Service {

    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private IConsolidationMigrationV3Service consolidationMigrationV3Service;

    @Autowired
    private IShipmentMigrationV3Service shipmentMigrationV3Service;

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
        List<Long> processed = collectAllConsolidation(queue);
        map.put("Total Migrated", processed.size());
        return map;

    }

    private List<ConsolidationDetails> fetchConsoleFromDB(ListCommonRequest listCommonRequest) {
        List<ConsolidationDetails> details = new ArrayList<>();
        for (long i = 0; i < 10; i++) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setId(i);
            details.add(consolidationDetails);
        }
        return details;
    }

    @Override
    public Map<String, Integer> migrateV3ToV2(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest) {

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
                        ConsolidationDetails response = consolidationMigrationV3Service.migrateConsolidationV3ToV2(cos);
                        return response.getId();
                    })
            );
            queue.add(future);
        });
        List<Long> processed = collectAllConsolidation(queue);
        map.put("Total Migrated", processed.size());
        return map;

    }

    private List<Long> collectAllConsolidation(List<Future<Long>> queue) {
        List<Long> console = new ArrayList<>();
        for (Future<Long> future : queue) {
            try {
                console.add(future.get());
            } catch (Exception er) {
                log.error("Error in trx", er);
            }
        }
        return console;
    }
}
