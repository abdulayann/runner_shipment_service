package com.dpw.runner.shipment.services.migration.map.shipment;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

@Slf4j
public abstract class AbstractShipmentMigration<S1, S2> {

    @Autowired
    private HelperExecutor trxExecutor;


    public List<Long> migrate(ListCommonRequest shipmentRequest) {
        List<ShipmentDetails> shipmentDetailsList = fetchShipmentFromDB(shipmentRequest);
        List<Future<Long>> queue = new ArrayList<>();
        log.info("fetched {} Shipment for Migrations", shipmentDetailsList.size());
        shipmentDetailsList.forEach(cos -> {
            // execute async
            Future<Long> future = trxExecutor.runInAsync(() ->
                    // execute in trx
                    trxExecutor.runInTrx(() -> {
                        S1 fromDTO = getShipmentDBMapperFrom().mapFromEntity(cos);
                        S2 toDTO = getShipmentDTOMapper().forwardMap(fromDTO);
                        ShipmentDetails shipmentId = getShipmentDBMapperTo().mapToEntity(toDTO);
                        this.updateInDB(shipmentId);
                        return shipmentId.getId();
                    })
            );
            queue.add(future);
        });
        return collectAll(queue);
    }


    private List<Long> collectAll(List<Future<Long>> queue) {
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


    private List<ShipmentDetails> fetchShipmentFromDB(ListCommonRequest listCommonRequest) {
        List<ShipmentDetails> list = new ArrayList<>();
        for (long i = 100; i < 110; i++) {
            ShipmentDetails shipmentDetails = new ShipmentDetails();
            shipmentDetails.setId(i);
            list.add(shipmentDetails);
        }
        return list;
    }

    private void updateInDB(ShipmentDetails details) {
        log.info("Saving Shipment for Id {}", details.getId());
    }

    protected abstract ShipmentDBMapper<S1> getShipmentDBMapperFrom();

    protected abstract ShipmentDBMapper<S2> getShipmentDBMapperTo();

    protected abstract IDTOMigrationMapper<S1, S2> getShipmentDTOMapper();

}
