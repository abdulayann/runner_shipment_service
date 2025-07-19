package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;
import java.util.Set;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;


@Generated
public interface IConsoleShipmentsMappingRepository extends JpaRepository<ConsoleShipmentMapping, Long> {
    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.consolidation_id = ?1 AND csm.shipment_id IN (?2)", nativeQuery = true)
    void deleteByConsolidationIdAndShipmentIdIn(Long consolidationId, List<Long> shipmentIds);
    Page<ConsoleShipmentMapping> findAll(Specification<ConsoleShipmentMapping> spec, Pageable pageable);

    @Query(value = "SELECT * FROM console_shipment_mapping WHERE consolidation_id = ?1 AND is_attachment_done = true", nativeQuery = true)
    List<ConsoleShipmentMapping> findByConsolidationIdByQuery(Long consolidationId);

    @Query(value = "SELECT * FROM console_shipment_mapping WHERE shipment_id = ?1 AND is_attachment_done = true", nativeQuery = true)
    List<ConsoleShipmentMapping> findByShipmentIdByQuery(Long shipmentId);

    @Query(value = "SELECT * FROM console_shipment_mapping WHERE shipment_id in ?1 AND is_attachment_done = true", nativeQuery = true)
    List<ConsoleShipmentMapping> findByShipmentIdsByQuery(Set<Long> shipmentIds);

    List<ConsoleShipmentMapping> findByConsolidationId(Long consolidationId);

    List<ConsoleShipmentMapping> findByShipmentId(Long shipmentId);

    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.consolidation_id = ?1 and csm.is_attachment_done = false", nativeQuery = true)
    void deletePendingStateByConsoleId(Long consoleId);

    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.shipment_id = ?1 and csm.is_attachment_done = false", nativeQuery = true)
    void deletePendingStateByShipmentId(Long shipmentId);

    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.shipment_id IN (:shipmentIds) AND csm.is_attachment_done = false", nativeQuery = true)
    void deletePendingStateByShipmentIds(@Param("shipmentIds") List<Long> shipmentIds);


    @Modifying
    @Query(value = "DELETE FROM console_shipment_mapping csm WHERE csm.consolidation_id = ?1 and csm.shipment_id = ?2 and csm.is_attachment_done = false", nativeQuery = true)
    void deletePendingStateByConsoleIdAndShipmentId(Long consoleId, Long shipmentId);

    @Query(value = "SELECT COUNT(*) FROM console_shipment_mapping WHERE shipment_id = ?1 AND is_attachment_done <> true", nativeQuery = true)
    int countByShipmentIdAndIsAttachmentDoneNotTrue(Long shipmentId);

    @Query(value = "SELECT shipment_id, COUNT(*) FROM console_shipment_mapping WHERE shipment_id IN ?1 AND is_attachment_done = false AND request_type = ?2 group by shipment_id", nativeQuery = true)
    List<Object[]> pendingStateCountBasedOnShipmentId(List<Long> shipmentIds, Integer requestType);

    @Query(value = "SELECT consolidation_id, COUNT(*) FROM console_shipment_mapping WHERE consolidation_id IN ?1 AND is_attachment_done = false AND request_type = ?2 group by consolidation_id", nativeQuery = true)
    List<Object[]> pendingStateCountBasedOnConsolidation(List<Long> consoleIds, Integer requestType);

    @Query(value = "SELECT COUNT(*) FROM console_shipment_mapping csm join shipment_details sd on csm.shipment_id = sd.id WHERE csm.is_attachment_done = false AND csm.request_type = ?1 AND sd.tenant_id = ?2", nativeQuery = true)
    Integer pendingStateCountBasedOnRequestType(Integer requestType, Integer tenantId);

    @Query(value = "SELECT * FROM console_shipment_mapping WHERE consolidation_id in ?1", nativeQuery = true)
    List<ConsoleShipmentMapping> findByConsolidationIdsByQuery(Set<Long> consolidationsIds);
}
