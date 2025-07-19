package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;


import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated
@InterBranchEntity
public interface IRoutingsRepository extends MultiTenancyRepository<Routings> {
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

    List<Routings> findByShipmentId(Long shipmentId);

    default Optional<Routings> findById(Long id) {
        Specification<Routings> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Routings> findAll();

    default Optional<Routings> findByGuid(UUID id) {
        Specification<Routings> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<Routings> findByConsolidationId(Long consolidationId);

    List<Routings> findByIdIn(List<Long> routingIds);

    List<Routings> findByShipmentIdAndCarriage(Long shipmentId, RoutingCarriage routingCarriage);

    @Query(value = "SELECT * FROM routings WHERE id = ?1", nativeQuery = true)
    Optional<Routings> findByIdWithQuery(Long id);

    @Query(value = "SELECT * FROM routings WHERE guid = ?1", nativeQuery = true)
    Optional<Routings> findByGuidWithQuery(UUID guid);

    @ExcludeTenantFilter
    default Page<Routings> findAllWithoutTenantFilter(Specification<Routings> spec, Pageable pageable) {
        return findAll(spec, pageable);
    }

    @Modifying
    @Query(value = "UPDATE routings SET is_deleted = true WHERE id NOT IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void deleteAdditionalDataByRoutingsIdsConsolidationId(List<Long> routingsIds, Long consolidationId);

    @Modifying
    @Query(value = "UPDATE routings SET is_deleted = false WHERE id IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void revertSoftDeleteByRoutingsIdsAndConsolidationId(List<Long> routingsIds, Long consolidationId);
}
