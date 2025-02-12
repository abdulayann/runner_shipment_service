package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;


@Repository @Generated
@InterBranchEntity
public interface IShipmentRepository extends MultiTenancyRepository<ShipmentDetails> {
    List<ShipmentDetails> findAll();
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    default Optional<ShipmentDetails> findById(Long id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    default Optional<ShipmentDetails> findByGuid(UUID id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM shipment_details WHERE guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findAllByGuids(List<UUID> guids);

    @Query(value = "SELECT * FROM shipment_details where house_bill = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ShipmentDetails> findByHouseBill(String hbl, Integer tenantId);
    List<ShipmentDetails> findAllByHouseBill(String Hbl);
    @Query(value = "SELECT * FROM shipment_details where booking_reference = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ShipmentDetails> findByBookingReference(String ref, Integer tenantId);

    @Query(value = "SELECT MAX(c.id) FROM consolidation_details c", nativeQuery = true)
    Long findMaxId();

    @Modifying @Transactional
    @Query(value = "Update shipment_details set job_status = ?2 Where id = ?1", nativeQuery = true)
    void saveJobStatus(Long id, String jobStatus);

    @Modifying @Transactional @ExcludeTenantFilter
    @Query(value = "Update shipment_details set status = ?2 Where id = ?1", nativeQuery = true)
    void saveStatus(Long id, Integer status);

    @Modifying @Transactional
    @Query(value = "Update shipment_details set created_by = ?2, created_at = ?3 Where id = ?1", nativeQuery = true)
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1", nativeQuery = true)
    List<ShipmentDetails> getShipmentNumberFromId(List<Long> shipmentIds);

    @Modifying @Transactional
    @Query(value = "Update shipment_details set entity_transfer = ?2 Where id = ?1", nativeQuery = true)
    void saveEntityTransfer(Long id, Boolean entityTransfer);

    @Query(value = "SELECT * FROM shipment_details WHERE guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsByGuids(Set<UUID> guids);

    @Query(value = "SELECT * FROM shipment_details WHERE source_guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsBySourceGuids(Set<UUID> sourceGuid);

    @Query(value = "SELECT * FROM shipment_details WHERE source_guid = ?1 and tenant_id = ?2 and is_deleted = false", nativeQuery = true)
    List<ShipmentDetails> findShipmentBySourceGuidAndTenantId(UUID sourceGuid, Integer tenantId);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsByIds(Set<Long> id);

    @Query(value = "SELECT * FROM shipment_details WHERE id = ?1", nativeQuery = true)
    Optional<ShipmentDetails> findShipmentByIdWithQuery(Long id);

    List<ShipmentDetails> findBySourceGuid(UUID guid);

    @Query(value = "SELECT distinct s.id from ShipmentDetails s inner join ConsoleShipmentMapping csm " +
            "on s.id = csm.shipmentId " +
            "where csm.isAttachmentDone = false and csm.requestedType = ?1")
    Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable);

    @Query(value = "SELECT "
            + " tenant_id as tenantId, "
            + " house_bill as hblNumber, "
            + " shipment_id as shipmentId "
            + " FROM shipment_details "
            + " WHERE house_bill = ?1 "
            + " AND (?2 IS NULL OR shipment_id != CAST(?2 AS VARCHAR))", nativeQuery = true)
    List<ShipmentDetailsProjection> findByHblNumberAndExcludeShipmentId(String hblNumber, String shipmentId);

    @Modifying @Transactional
    @Query(value = "Update shipment_details set is_transferred_to_receiving_branch = ?2 Where id = ?1", nativeQuery = true)
    void saveIsTransferredToReceivingBranch(Long id, Boolean entityTransferred);

    @Modifying @Transactional
    @Query(value = "Update triangulation_partner_shipment set is_accepted = ?3 where shipment_id = ?1 AND partner_id = ?2", nativeQuery = true)
    void updateIsAcceptedTriangulationPartner(Long shipmentId, Long triangulationPartner, Boolean isAccepted);

    @ExcludeTenantFilter
    default Page<ShipmentDetails> findAllWithoutTenantFilter(Specification<ShipmentDetails> spec, Pageable pageable) {
        return findAll(spec, pageable);
    }

    @ExcludeTenantFilter
    List<ShipmentDetails> findByShipmentId(String shipmentNumber);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "UPDATE shipment_additional_details a " +
            "SET empty_container_returned = ?2 " +
            "FROM shipment_details s " +
            "WHERE s.additional_details_id = a.id AND s.id = ?1", nativeQuery = true)
    void updateAdditionalDetailsByShipmentId(Long id, Boolean emptyContainerReturned);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1 AND contains_hazardous = ?2", nativeQuery = true)
    List<ShipmentDetails> findByShipmentIdInAndContainsHazardous(List<Long> shipmentIdList, boolean containsHazardous);
    List<ShipmentDetails> findByShipmentIdIn(List<String> shipmentIds);
    
    @Modifying
    @Query(value = "update shipment_additional_details set fcr_number = fcr_number + 1 where id in (select additional_details_id from shipment_details where id = ?1)", nativeQuery = true)
    void updateFCRNo(Long id);

    @Query(value = "SELECT * FROM shipment_details WHERE guid = ?1", nativeQuery = true)
    Optional<ShipmentDetails> findShipmentByGuidWithQuery(UUID guid);

    @ExcludeTenantFilter
    @Query(value = "SELECT receiving_branch FROM shipment_details WHERE guid = ?1", nativeQuery = true)
    Integer findReceivingByGuid(UUID guid);
}
