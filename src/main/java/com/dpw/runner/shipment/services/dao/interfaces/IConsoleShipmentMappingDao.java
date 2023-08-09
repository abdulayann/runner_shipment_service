package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;

import java.util.List;

public interface IConsoleShipmentMappingDao {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);

    void assignShipments(Long consolidationId, List<Long> shipIds);

    void detachShipments(Long consolidationId, List<Long> shipIds);

    void updateShipmentsMappings(Long consolidationId, List<Long> shipIds);
}
