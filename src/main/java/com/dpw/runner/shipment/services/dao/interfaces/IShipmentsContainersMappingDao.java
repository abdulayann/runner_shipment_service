package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;

import java.util.List;

public interface IShipmentsContainersMappingDao {

    void assignShipments(Long containerId, List<Long> shipIds);

    void detachShipments(Long containerId, List<Long> shipIds);

    void updateShipmentsMappings(Long containerId, List<Long> shipIds);

    List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId);
}
