package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;

public interface IConsoleShipmentsMappingRepository extends JpaRepository<ConsoleShipmentMapping, Long> {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);
    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);
}
