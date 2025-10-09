package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated @InterBranchEntity
public interface IPackingRepository extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);

    List<Packing> findByShipmentIdInAndContainerId(List<Long> shipmentIds, Long containerId);

    default Optional<Packing> findById(Long id) {
        Specification<Packing> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Packing> findByOrderLineGuidIn(List<String> orderLineGuidList);

    List<Packing> findAll();

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    default Optional<Packing> findByGuid(UUID id) {
        Specification<Packing> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<Packing> findByConsolidationId(Long consolidationId);

    List<Packing> findByContainerIdIn(List<Long> deleteContainerIds);

    @ExcludeTenantFilter
    default List<Packing> findByContainerIdInWithoutTenantFilter(List<Long> deleteContainerIds){
        return findByContainerIdIn(deleteContainerIds);
    }

    List<Packing> findByIdIn(List<Long> packingIds);

    @Query(value = "SELECT * FROM packing WHERE id = ?1", nativeQuery = true)
    Optional<Packing> findByIdWithQuery(Long id);

    @Query(value = "SELECT * FROM packing WHERE guid = ?1", nativeQuery = true)
    Optional<Packing> findByGuidWithQuery(UUID guid);

    @ExcludeTenantFilter
    default Page<Packing> findAllWithoutTenantFilter(Specification<Packing> spec, Pageable pageable) {
        return findAll(spec, pageable);
    }

    @Query(value = """
    SELECT
        SUM(CASE
                WHEN p.container_id IS NOT NULL AND p.container_id > 0
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS assignedCount,
        SUM(CASE
                WHEN p.container_id IS NULL
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS unassignedCount
    FROM packing p
    WHERE p.shipment_id = ?1 and p.is_deleted = false
    """, nativeQuery = true)
    PackingAssignmentProjection getPackingAssignmentCountByShipment(@Param("shipmentId") Long shipmentId);

    @Query(value = """
    SELECT
        SUM(CASE
                WHEN p.container_id IS NOT NULL AND p.container_id > 0
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS assignedCount,
        SUM(CASE
                WHEN p.container_id IS NULL
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS unassignedCount
    FROM packing p
    WHERE p.shipment_id = ?1 and p.is_deleted = false and p.tenant_id = ?2
    """, nativeQuery = true)
    PackingAssignmentProjection getPackingAssignmentCountByShipmentAndTenant(@Param("shipmentId") Long shipmentId, @Param("tenantId") Integer tenantId);

    @Query(value = """
    SELECT
        SUM(CASE
                WHEN p.container_id IS NOT NULL AND p.container_id > 0
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS assignedCount,
        SUM(CASE
                WHEN p.container_id IS NULL
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS unassignedCount
    FROM packing p
    WHERE p.shipment_id IN ?1 and is_deleted = false
    """, nativeQuery = true)
    PackingAssignmentProjection getPackingAssignmentCountByShipmentIn(@Param("shipmentIds") List<Long> shipmentIds);

    @ExcludeTenantFilter
    default PackingAssignmentProjection getPackingAssignmentCountByShipmentWithoutTenantFilter(@Param("shipmentId") Long shipmentId){
        return getPackingAssignmentCountByShipment(shipmentId);
    }

    List<Packing> findByShipmentIdIn(List<Long> shipmentIds);
    List<Packing> findByBookingIdIn(List<Long> bookingIds);

    @Modifying
    @Query("UPDATE Packing p SET p.containerId = :containerId WHERE p.id IN :packingIds")
    void setPackingIdsToContainer(@Param("packingIds") List<Long> packingIds, @Param("containerId") Long containerId);

    @Query(value = """
    SELECT
        SUM(CASE
                WHEN p.container_id IS NOT NULL AND p.container_id > 0
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS assignedCount,
        SUM(CASE
                WHEN p.container_id IS NULL
                THEN COALESCE(CAST(NULLIF(p.packs, '') AS INTEGER), 0)
                ELSE 0
            END) AS unassignedCount
    FROM packing p
    WHERE p.shipment_id IN ?1 and p.is_deleted = false and p.tenant_id = ?2
    """, nativeQuery = true)
    PackingAssignmentProjection getPackingAssignmentCountByShipmentInAndTenant(List<Long> shipmentIds, Integer tenantId);

    @Query(value = "SELECT EXISTS (SELECT 1 FROM packing WHERE shipment_id = :shipmentId and is_deleted = false)", nativeQuery = true)
    boolean existsPackingByShipmentId(@Param("shipmentId") Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = true WHERE id NOT IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void deleteAdditionalPackingByConsolidationId(List<Long> packingIds, Long consolidationId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = false WHERE id IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void revertSoftDeleteByPackingIdsAndConsolidationId(List<Long> packingIds, Long consolidationId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = true WHERE id NOT IN (?1) and booking_id = ?2", nativeQuery = true)
    void deleteAdditionalPackingByCustomerBookingId(List<Long> packingIds, Long bookingId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = false WHERE id IN (?1) and booking_id = ?2", nativeQuery = true)
    void revertSoftDeleteByPackingIdsAndBookingId(List<Long> packingIds, Long bookingId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalPackingByShipmentId(List<Long> packingIds, Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE packing SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByPackingIdsAndShipmentId(List<Long> packingIds, Long shipmentId);

    @Query(value = "SELECT p.container_id FROM packing p " +
            "JOIN containers c ON c.id = p.container_id " +
            "WHERE p.is_deleted = false AND c.is_deleted = false AND " +
            "c.container_number ILIKE CONCAT('%', :containerNumber, '%') AND " +
            "CASE " +
            "   WHEN :idType = 'SHIPMENT' THEN p.container_id in (select container_id from shipments_containers_mapping where shipment_id = :id)" +
            "   WHEN :idType = 'CONSOLIDATION' THEN c.consolidation_id = :id " +
            "   WHEN :idType = 'BOOKING' THEN c.booking_id = :id " +
            "END",
            nativeQuery = true)
    List<Long> getContainerIdByContainerNumberAndType(@Param("containerNumber") String containerNumber, @Param("id") Long id, @Param("idType") String idType);
}

