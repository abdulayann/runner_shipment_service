package com.dpw.runner.shipment.services.migration.strategy;

import com.dpw.runner.shipment.services.migration.entity.enums.EntityType;
import com.dpw.runner.shipment.services.migration.strategy.impl.BookingStrategy;
import com.dpw.runner.shipment.services.migration.strategy.impl.ConsolidationStrategy;
import com.dpw.runner.shipment.services.migration.strategy.impl.ShipmentStrategy;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupRestoreStrategy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class BackupStrategyFactory {

    @Autowired
    private ShipmentStrategy shipmentStrategy;

    @Autowired
    private BookingStrategy bookingStrategy;

    @Autowired
    private ConsolidationStrategy consolidationStrategy;


    public BackupRestoreStrategy getStrategy(EntityType entityType) {

        return (BackupRestoreStrategy) switch (entityType) {
            case SHIPMENT -> shipmentStrategy;
            case BOOKING -> bookingStrategy;
            case CONSOLIDATION -> consolidationStrategy;
        };
    }
}
