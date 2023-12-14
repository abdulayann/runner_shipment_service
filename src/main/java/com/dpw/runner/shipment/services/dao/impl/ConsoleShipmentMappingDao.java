package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.repository.interfaces.IConsoleShipmentsMappingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Repository
public class ConsoleShipmentMappingDao implements IConsoleShipmentMappingDao {
    @Autowired
    private IConsoleShipmentsMappingRepository consoleShipmentsMappingRepository;

    @Override
    public List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId) {
        return consoleShipmentsMappingRepository.findByConsolidationId(consolidationId);
    }

    private void deleteByConsolidationIdAndShipmentIdIn(Long consolidationId, List<Long> shipmentIds) {
        consoleShipmentsMappingRepository.deleteByConsolidationIdAndShipmentIdIn(consolidationId, shipmentIds);
    }

    @Override
    public List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId) {
        return consoleShipmentsMappingRepository.findByShipmentId(shipmentId);
    }

    private ConsoleShipmentMapping save(ConsoleShipmentMapping consoleShipmentMapping) {
        return consoleShipmentsMappingRepository.save(consoleShipmentMapping);
    }

    private void delete(ConsoleShipmentMapping consoleShipmentMapping) {
        consoleShipmentsMappingRepository.delete(consoleShipmentMapping);
    }

    @Override
    public List<Long> assignShipments(Long consolidationId, List<Long> shipIds) {
        List<ConsoleShipmentMapping> mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        if (mappings != null && mappings.size() > 0) {
            for (ConsoleShipmentMapping consoleShipmentMapping : mappings) {
                if (shipmentIds.contains(consoleShipmentMapping.getShipmentId())) {
                    shipmentIds.remove(consoleShipmentMapping.getShipmentId());
                }
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long id : shipmentIds) {
                ConsoleShipmentMapping entity = new ConsoleShipmentMapping();
                entity.setShipmentId(id);
                entity.setConsolidationId(consolidationId);
                save(entity);
            }
        }
        return new ArrayList<>(shipmentIds);
    }

    @Override
    @Transactional
    public List<Long> detachShipments(Long consolidationId, List<Long> shipIds) {
        deleteByConsolidationIdAndShipmentIdIn(consolidationId, shipIds);
        return shipIds;
    }

    @Override
    public void updateShipmentsMappings(Long consolidationId, List<Long> shipIds) {
        List<ConsoleShipmentMapping> mappings = Optional.ofNullable(findByConsolidationId(consolidationId)).orElse(Collections.emptyList());
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ConsoleShipmentMapping> deleteMappings = new ArrayList<>();
        for (ConsoleShipmentMapping consoleShipmentMapping : mappings) {
            if (shipmentIds.contains(consoleShipmentMapping.getShipmentId())) {
                shipmentIds.remove(consoleShipmentMapping.getShipmentId());
            } else {
                deleteMappings.add(consoleShipmentMapping);
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long shipmentId : shipmentIds) {
                ConsoleShipmentMapping entity = new ConsoleShipmentMapping();
                entity.setShipmentId(shipmentId);
                entity.setConsolidationId(consolidationId);
                save(entity);
            }
        }
        if (!deleteMappings.isEmpty()) {
            for (ConsoleShipmentMapping mapping : deleteMappings) {
                delete(mapping);
            }
        }
    }
}
