package com.dpw.runner.shipment.services.dao.interfaces;

import java.util.List;

public interface IConsolidationShipmentsMappingDao {
    void assignShipments(Long consolidationId, List<Long> shipIds);
    void detachShipments(Long consolidationId, List<Long> shipIds);
    void updateShipmentsMappings(Long consolidationId, List<Long> shipIds);
}
