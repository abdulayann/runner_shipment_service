package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
@Generated
public interface IShipmentsContainersMappingRepository extends JpaRepository <ShipmentsContainersMapping, Long> {
    List<ShipmentsContainersMapping> findByContainerId(Long containerId);
    List<ShipmentsContainersMapping> findByContainerIdIn(List<Long> containerId);

    @ExcludeTenantFilter
    default List<ShipmentsContainersMapping> findByContainerIdInWithoutTenantFilter(List<Long> containerId){
        return findByContainerIdIn(containerId);
    }

    List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId);
    Page<ShipmentsContainersMapping> findAll(Specification<ShipmentsContainersMapping> spec, Pageable pageable);
}
