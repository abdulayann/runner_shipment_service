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
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated @InterBranchEntity
public interface IPackingRepository extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);

    default Optional<Packing> findById(Long id) {
        Specification<Packing> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

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

    @Query("""
    SELECT
        SUM(CASE WHEN p.containerId IS NOT NULL AND p.containerId > 0 THEN 1 ELSE 0 END) AS assignedCount,
        SUM(CASE WHEN p.containerId IS NULL THEN 1 ELSE 0 END) AS unassignedCount
    FROM Packing p
    WHERE p.shipmentId = :shipmentId
    """)
    PackingAssignmentProjection getPackingAssignmentCountByShipment(@Param("shipmentId") Long shipmentId);

    @ExcludeTenantFilter
    default PackingAssignmentProjection getPackingAssignmentCountByShipmentWithoutTenantFilter(@Param("shipmentId") Long shipmentId){
        return getPackingAssignmentCountByShipment(shipmentId);
    }

    List<Packing> findByShipmentIdIn(List<Long> shipmentIds);
}

