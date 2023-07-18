package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;

public interface IShipmentsContainersMappingRepository extends JpaRepository <ShipmentsContainersMapping, Long> {
    List<ShipmentsContainersMapping> findByContainerId(Long containerId);
    List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId);
}
