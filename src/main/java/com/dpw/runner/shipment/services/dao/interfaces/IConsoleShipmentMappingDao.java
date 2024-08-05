package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public interface IConsoleShipmentMappingDao {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);

    HashSet<Long> assignShipments(Long consolidationId, List<Long> shipIds, List<ConsoleShipmentMapping> consoleShipmentMappings, Set<Long> interBranchShipIds);

    List<Long> detachShipments(Long consolidationId, List<Long> shipIds);

    void updateShipmentsMappings(Long consolidationId, List<Long> shipIds);

    Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable);

    List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId);
    List<ConsoleShipmentMapping> findByConsolidationIdAll(Long consolidationId);

    void updateConsoleShipments(ShipmentRequestedType shipmentRequestedType, Long consoleId, Long shipmentId);
}
