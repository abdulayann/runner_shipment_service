package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentsContainersMappingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

@Repository
public class ShipmentsContainersMappingDao implements IShipmentsContainersMappingDao {

    @Autowired
    private IShipmentsContainersMappingRepository shipmentsContainersMappingRepository;

    private List<ShipmentsContainersMapping> findByContainerId(Long containerId) {
        return shipmentsContainersMappingRepository.findByContainerId(containerId);
    }

    public List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId) {
        return shipmentsContainersMappingRepository.findByShipmentId(shipmentId);
    }

    private ShipmentsContainersMapping save(ShipmentsContainersMapping shipmentsContainersMapping) {
        return shipmentsContainersMappingRepository.save(shipmentsContainersMapping);
    }

    private void delete(ShipmentsContainersMapping shipmentsContainersMapping) {
        shipmentsContainersMappingRepository.delete(shipmentsContainersMapping);
    }

    @Override
    public void assignShipments(Long containerId, List<Long> shipIds) {
        List<ShipmentsContainersMapping> mappings = findByContainerId(containerId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        if (mappings != null && mappings.size() > 0) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                if (shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    shipmentIds.remove(shipmentsContainersMappings.getShipmentId());
                }
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long id : shipmentIds) {
                ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
                entity.setShipmentId(id);
                entity.setContainerId(containerId);
                save(entity);
            }
        }
    }

    @Override
    public void detachShipments(Long containerId, List<Long> shipIds) {
        List<ShipmentsContainersMapping> mappings = findByContainerId(containerId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ShipmentsContainersMapping> deleteMappings = new ArrayList<>();
        if (mappings != null && mappings.size() > 0) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                if (shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    deleteMappings.add(shipmentsContainersMappings);
                }
            }
        }
        if (!deleteMappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMapping : deleteMappings) {
                delete(shipmentsContainersMapping);
            }
        }
    }

    @Override
    public void updateShipmentsMappings(Long containerId, List<Long> shipIds) {
        List<ShipmentsContainersMapping> mappings = findByContainerId(containerId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ShipmentsContainersMapping> deleteMappings = new ArrayList<>();
        for (ShipmentsContainersMapping shipmentsContainersMapping : mappings) {
            if (shipmentIds.contains(shipmentsContainersMapping.getShipmentId())) {
                shipmentIds.remove(shipmentsContainersMapping.getShipmentId());
            } else {
                deleteMappings.add(shipmentsContainersMapping);
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long shipmentId : shipmentIds) {
                ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
                entity.setShipmentId(shipmentId);
                entity.setContainerId(containerId);
                save(entity);
            }
        }
        if (!deleteMappings.isEmpty()) {
            for (ShipmentsContainersMapping mapping : deleteMappings) {
                delete(mapping);
            }
        }
    }
}
