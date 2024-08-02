package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
@Generated
public interface IConsoleShipmentsMappingRepository extends JpaRepository<ConsoleShipmentMapping, Long> {
    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.consolidation_id = ?1 AND csm.shipment_id IN (?2)", nativeQuery = true)
    void deleteByConsolidationIdAndShipmentIdIn(Long consolidationId, List<Long> shipmentIds);
    Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable);

    @Modifying
    @Query(value = "SELECT * FROM console_shipment_mapping WHERE consolidation_id = ?1 AND is_attachment_done = true", nativeQuery = true)
    List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId);

    @Modifying
    @Query(value = "SELECT * FROM console_shipment_mapping WHERE shipment_id = ?1 AND is_attachment_done = true", nativeQuery = true)
    List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE console_shipment_mapping SET request_type = ?1 WHERE consolidation_id = ?2 AND shipment_id = ?3", nativeQuery = true)
    void updateConsoleShipmentStatus(Integer requestedType, Long consoleId, Long shipmentId);

}
