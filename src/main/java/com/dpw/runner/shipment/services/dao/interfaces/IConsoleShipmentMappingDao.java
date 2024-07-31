package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.commons.entity.ConsoleShipmentMapping;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IConsoleShipmentMappingDao {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);

    List<Long> assignShipments(Long consolidationId, List<Long> shipIds, List<ConsoleShipmentMapping> consoleShipmentMappings);

    List<Long> detachShipments(Long consolidationId, List<Long> shipIds);

    void updateShipmentsMappings(Long consolidationId, List<Long> shipIds);

    Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable);

    List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId);
}
