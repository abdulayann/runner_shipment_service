package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import java.util.Set;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IShipmentsContainersMappingDao {

    Page<ShipmentsContainersMapping> findAll(Specification<ShipmentsContainersMapping> spec, Pageable pageable);
    Page<ShipmentsContainersMapping> findAllByContainerIds(List<Long> containerIds);
    List<ShipmentsContainersMapping> findByContainerId(Long containerId);
    List<ShipmentsContainersMapping> findByContainerIdIn(List<Long> containerIds);

    List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId);

    void assignContainers(Long shipmentId, List<Long> containerIds, String transactionId);
    void assignShipments(Long containerId, Set<Long> shipIds, boolean fromV1);

    void detachShipments(Long containerId, List<Long> shipIds, boolean fromV1);
    void detachListShipments(List<Long> containerId, List<Long> shipIds, boolean fromV1);
    void updateShipmentsMappings(Long containerId, List<Long> shipIds);

}
