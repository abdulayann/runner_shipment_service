package com.dpw.runner.shipment.services.dao.interfaces;

import java.util.List;

public interface IShipmentsContainersMappingDao {

    void assignShipments(Long containerId, List<Long> shipIds);
    void detachShipments(Long containerId, List<Long> shipIds);
    void updateShipmentsMappings(Long containerId, List<Long> shipIds);
}
