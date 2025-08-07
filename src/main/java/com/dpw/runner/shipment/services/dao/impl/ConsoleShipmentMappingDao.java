package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.repository.interfaces.IConsoleShipmentsMappingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
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

    @Autowired
    @Lazy
    private ShipmentDao shipmentDao;

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
    public List<ConsoleShipmentMapping> findByShipmentIds(Set<Long> shipmentIds) {
        return consoleShipmentsMappingRepository.findByShipmentIdsByQuery(shipmentIds);
    }

    @Override
    public Integer countAllStateMappings(Long shipmentId) {
        return consoleShipmentsMappingRepository.countByShipmentIdAndIsAttachmentDoneNotTrue(shipmentId);
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
    public List<ConsoleShipmentMapping> findByShipmentIdAll(Long shipmentId) {
        return consoleShipmentsMappingRepository.findByShipmentId(shipmentId);
    }

    @Override
    public ConsoleShipmentMapping save(ConsoleShipmentMapping consoleShipmentMapping) {
        return consoleShipmentsMappingRepository.save(consoleShipmentMapping);
    }
    @Override
    public List<ConsoleShipmentMapping> saveAll(List<ConsoleShipmentMapping> consoleShipmentMappingList) {
        return consoleShipmentsMappingRepository.saveAll(consoleShipmentMappingList);
    }

    private void delete(ConsoleShipmentMapping consoleShipmentMapping) {
        consoleShipmentsMappingRepository.delete(consoleShipmentMapping);
    }

    @Override
    public HashSet<Long> assignShipments(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<Long> shipIds, List<ConsoleShipmentMapping> mappings, Set<Long> interBranchRequestedShipIds, Set<Long> interBranchApprovedShipIds, Map<Long, ShipmentDetails> interBranchImportShipmentMap) {
        if(mappings == null)
            mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        Map<Long, ConsoleShipmentMapping> consoleShipmentMappingMap = getLongConsoleShipmentMappingMap(shipmentRequestedType, consolidationId, mappings, shipmentIds);
        if (!shipmentIds.isEmpty()) {
            for (Long id : shipmentIds) {
                ConsoleShipmentMapping entity;
                if(shipmentRequestedType != null && consoleShipmentMappingMap.containsKey(id))
                    entity = consoleShipmentMappingMap.get(id);
                else
                    entity = new ConsoleShipmentMapping();
                entity.setShipmentId(id);
                entity.setConsolidationId(consolidationId);
                processInterBranchCase(interBranchRequestedShipIds, interBranchImportShipmentMap, id, entity);
                if(shipmentRequestedType != null && (interBranchApprovedShipIds.contains(id) || interBranchImportShipmentMap.containsKey(id))) {
                    entity.setRequestedType(shipmentRequestedType);
                }
                save(entity);
            }
        }
        return shipmentIds;
    }

    private void processInterBranchCase(Set<Long> interBranchRequestedShipIds, Map<Long, ShipmentDetails> interBranchImportShipmentMap, Long id, ConsoleShipmentMapping entity) {
        if(interBranchRequestedShipIds.contains(id) && !interBranchImportShipmentMap.containsKey(id))
        {
            entity.setIsAttachmentDone(false);
            entity.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED);
        } else {
            entity.setIsAttachmentDone(true);
        }
    }

    private Map<Long, ConsoleShipmentMapping> getLongConsoleShipmentMappingMap(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<ConsoleShipmentMapping> mappings, HashSet<Long> shipmentIds) {
        Map<Long, ConsoleShipmentMapping> consoleShipmentMappingMap = new HashMap<>();
        if (mappings != null && !mappings.isEmpty()) {
            for (ConsoleShipmentMapping consoleShipmentMapping : mappings) {
                if(Objects.equals(consolidationId, consoleShipmentMapping.getConsolidationId())) {
                    if(Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone()))
                        shipmentIds.remove(consoleShipmentMapping.getShipmentId());
                    if(shipmentRequestedType != null)
                        consoleShipmentMappingMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping);
                }
            }
        }
        return consoleShipmentMappingMap;
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
    public void deletePendingStateByShipmentIds(List<Long> shipmentIds) {
        consoleShipmentsMappingRepository.deletePendingStateByShipmentIds(shipmentIds);
    }

    @Override
    @Transactional
    public void deletePendingStateByConsoleIdAndShipmentId(Long consoleId, Long shipmentId) {
        consoleShipmentsMappingRepository.deletePendingStateByConsoleIdAndShipmentId(consoleId, shipmentId);
    }

    @Override
    public Map<Long, Integer> pendingStateCountBasedOnShipmentId(List<Long> shipmentIds, Integer requestedType) {
        List<Object[]> results = consoleShipmentsMappingRepository.pendingStateCountBasedOnShipmentId(shipmentIds, requestedType);
        return this.convertResponseToMap(results);
    }

    @Override
    public Integer pendingStateCountBasedOnRequestType(Integer requestType, Integer tenantId) {
        return consoleShipmentsMappingRepository.pendingStateCountBasedOnRequestType(requestType, tenantId);
    }

    @Override
    public List<ConsoleShipmentMapping> findByConsolidationIdsByQuery(Set<Long> consolidationIds) {
        return consoleShipmentsMappingRepository.findByConsolidationIdsByQuery(consolidationIds);
    }

    @Override
    public Map<Long, Integer> pendingStateCountBasedOnConsolidation(List<Long> consoleIds, Integer requestedType) {
        List<Object[]> results = consoleShipmentsMappingRepository.pendingStateCountBasedOnConsolidation(consoleIds, requestedType);
        return this.convertResponseToMap(results);
    }

    private Map<Long, Integer> convertResponseToMap(List<Object[]> results) {
        Map<Long, Integer> responseMap = new HashMap<>();

        for (Object[] result : results) {
            Long key = ((Number) result[0]).longValue();
            int count = ((Number) result[1]).intValue();
            responseMap.put(key, count);
        }

        return responseMap;
    }
}
