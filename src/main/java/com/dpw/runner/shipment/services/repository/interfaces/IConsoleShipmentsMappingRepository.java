package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface IConsoleShipmentsMappingRepository extends JpaRepository<ConsoleShipmentMapping, Long> {
    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);
    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);
    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.consolidation_id = ?1 AND csm.shipment_id IN (?2)", nativeQuery = true)
    void deleteByConsolidationIdAndShipmentIdIn(Long consolidationId, List<Long> shipmentIds);

}
