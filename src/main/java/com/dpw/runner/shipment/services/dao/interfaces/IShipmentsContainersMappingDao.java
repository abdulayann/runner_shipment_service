package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;

import java.util.List;

public interface IShipmentsContainersMappingDao {

    List<ShipmentsContainersMapping> findByContainerId(Long containerId);

    List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId);

    void assignShipments(Long containerId, List<Long> shipIds);

    void detachShipments(Long containerId, List<Long> shipIds);

    void updateShipmentsMappings(Long containerId, List<Long> shipIds);

}
