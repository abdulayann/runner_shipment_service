package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface IConsoleShipmentMappingDao {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);
    ConsoleShipmentMapping save(ConsoleShipmentMapping consoleShipmentMapping);
    List<ConsoleShipmentMapping> saveAll(List<ConsoleShipmentMapping> consoleShipmentMappingList);

    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);
    List<ConsoleShipmentMapping> findByShipmentIds(Set<Long> shipmentId);

    Integer countAllStateMappings(Long shipmentId);

    HashSet<Long> assignShipments(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<Long> shipIds, List<ConsoleShipmentMapping> consoleShipmentMappings, Set<Long> interBranchRequestedShipIds, Set<Long> interBranchApprovedShipIds, Map<Long, ShipmentDetails> interBranchImportShipmentMap);

    List<Long> detachShipments(Long consolidationId, List<Long> shipIds);

    void updateShipmentsMappings(Long consolidationId, List<Long> shipIds);

    Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable);

    List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId);
    List<ConsoleShipmentMapping> findByConsolidationIdAll(Long consolidationId);
    List<ConsoleShipmentMapping> findByShipmentIdAll(Long shipmentId);
    void deletePendingStateByConsoleId(Long consoleId);
    void deletePendingStateByShipmentId(Long shipmentId);
    void deletePendingStateByShipmentIds(List<Long> shipmentIds);
    void deletePendingStateByConsoleIdAndShipmentId(Long consoleId, Long shipmentId);
    Map<Long, Integer> pendingStateCountBasedOnShipmentId(List<Long> shipmentIds, Integer requestedType);
    Map<Long, Integer> pendingStateCountBasedOnConsolidation(List<Long> consoleIds, Integer requestedType);
    Integer pendingStateCountBasedOnRequestType(Integer requestType, Integer tenantId);
    List<ConsoleShipmentMapping> findByConsolidationIdsByQuery(Set<Long> consolidationIds);
}
