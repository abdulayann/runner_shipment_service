package com.dpw.runner.shipment.services.migration.map.console;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.AbstractShipmentMigration;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;

@Slf4j
public abstract class AbstractConsolidationMigration<D1, D2, S1, S2> {


    @Autowired
    private HelperExecutor trxExecutor;


    public Map<String, Integer> migrate(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest) {
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
                        D1 fromDTO = getConsoleDBMapperFrom().mapFromEntity(cos);
                        D2 toDTO = getConsoleDTOMapper().forwardMap(fromDTO);
                        ConsolidationDetails toConsole = getConsoleDBMapperTo().mapToEntity(toDTO);
                        this.updateInDB(toConsole);
                        getShipmentMapping().migrate(appendConsoleIdInShipment(shipmentRequest, toConsole.getId()));
                        return toConsole.getId();
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


    private List<ConsolidationDetails> fetchConsoleFromDB(ListCommonRequest listCommonRequest) {
        List<ConsolidationDetails> details = new ArrayList<>();
        for (long i = 0; i < 10; i++) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setId(i);
            details.add(consolidationDetails);
        }
        return details;
    }

    private void updateInDB(ConsolidationDetails details) {
        log.info("Saving Consolidation for Id {}", details.getId());
    }

    private ListCommonRequest appendConsoleIdInShipment(ListCommonRequest shipmentRequest, Long consoleId) {
        return shipmentRequest;
    }

    protected abstract ConsoleDBMapper<D1> getConsoleDBMapperFrom();

    protected abstract ConsoleDBMapper<D2> getConsoleDBMapperTo();

    protected abstract IDTOMigrationMapper<D1, D2> getConsoleDTOMapper();

    protected abstract AbstractShipmentMigration<S1, S2> getShipmentMapping();
}
