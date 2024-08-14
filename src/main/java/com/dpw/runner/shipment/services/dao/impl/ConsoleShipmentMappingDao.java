package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.repository.interfaces.IConsoleShipmentsMappingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Repository
public class ConsoleShipmentMappingDao implements IConsoleShipmentMappingDao {
    @Autowired
    private IConsoleShipmentsMappingRepository consoleShipmentsMappingRepository;

    @Override
    public Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable) {
        return consoleShipmentsMappingRepository.findAll(spec, pageable);
    }

    @Override
    public List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId) {
        return consoleShipmentsMappingRepository.findByConsolidationIdByQuery(consolidationId);
    }

    private void deleteByConsolidationIdAndShipmentIdIn(Long consolidationId, List<Long> shipmentIds) {
        consoleShipmentsMappingRepository.deleteByConsolidationIdAndShipmentIdIn(consolidationId, shipmentIds);
    }

    @Override
    public List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId) {
        return consoleShipmentsMappingRepository.findByShipmentIdByQuery(shipmentId);
    }

    @Override
    public List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId) {
        return consoleShipmentsMappingRepository.findByConsolidationIdByQuery(consolidationId);
    }

    @Override
    public List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId) {
        return consoleShipmentsMappingRepository.findByShipmentIdByQuery(shipmentId);
    }

    @Override
    public List<ConsoleShipmentMapping> findByConsolidationIdAll(Long consolidationId) {
        return consoleShipmentsMappingRepository.findByConsolidationId(consolidationId);
    }

    @Override
    public ConsoleShipmentMapping save(ConsoleShipmentMapping consoleShipmentMapping) {
        return consoleShipmentsMappingRepository.save(consoleShipmentMapping);
    }

    private void delete(ConsoleShipmentMapping consoleShipmentMapping) {
        consoleShipmentsMappingRepository.delete(consoleShipmentMapping);
    }

    @Override
    public HashSet<Long> assignShipments(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<Long> shipIds, List<ConsoleShipmentMapping> mappings, Set<Long> interBranchShipIds) {
        if(mappings == null)
            mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        if (mappings != null && mappings.size() > 0) {
            for (ConsoleShipmentMapping consoleShipmentMapping : mappings) {
                if(Objects.equals(consoleShipmentMapping.getConsolidationId(), consolidationId))
                    shipmentIds.remove(consoleShipmentMapping.getShipmentId());
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long id : shipmentIds) {
                ConsoleShipmentMapping entity = new ConsoleShipmentMapping();
                entity.setShipmentId(id);
                entity.setConsolidationId(consolidationId);
                if(interBranchShipIds.contains(id)) {
                    entity.setIsAttachmentDone(false);
                    entity.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED);
                } else {
                    entity.setIsAttachmentDone(true);
                }
                if(shipmentRequestedType != null) {
                    consoleShipmentsMappingRepository.updateConsoleShipmentStatus(shipmentRequestedType.getValue(), consolidationId, id);
                } else {
                    save(entity);
                }
            }
        }
        return shipmentIds;
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

    @Override
    @Transactional
    public void updateConsoleShipments(ShipmentRequestedType shipmentRequestedType, Long consoleId, Long shipmentId) {
        Integer shipmentRequestedTypeValue = shipmentRequestedType.getValue();
        consoleShipmentsMappingRepository.updateConsoleShipmentStatus(shipmentRequestedTypeValue, consoleId, shipmentId);
    }

    @Override
    @Transactional
    public void deletePendingStateByConsoleId(Long consoleId) {
        consoleShipmentsMappingRepository.deletePendingStateByConsoleId(consoleId);
    }

    @Override
    @Transactional
    public void deletePendingStateByShipmentId(Long shipmentId) {
        consoleShipmentsMappingRepository.deletePendingStateByShipmentId(shipmentId);
    }

    @Override
    @Transactional
    public void deletePendingStateByConsoleIdAndShipmentId(Long consoleId, Long shipmentId) {
        consoleShipmentsMappingRepository.deletePendingStateByConsoleIdAndShipmentId(consoleId, shipmentId);
    }
}
