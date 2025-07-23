package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository @Generated
public interface IPartiesRepository extends MultiTenancyRepository<Parties> {
    Page<Parties> findAll(Specification<Parties> spec, Pageable pageable);

    List<Parties> findByEntityIdAndEntityType(Long entityId, String entityType);

    default Optional<Parties> findById(Long id) {
        Specification<Parties> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Parties> findAll();

    List<Parties> findByIdIn(List<Long> ids);

    @Modifying
    @Transactional
    @Query(value = "UPDATE parties SET is_deleted = true WHERE id NOT IN (?1) and entity_id = ?2 and entity_type = ?3", nativeQuery = true)
    void deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(List<Long> addressIds, Long entityId, String entityType);

    @Modifying
    @Transactional
    @Query(value = "UPDATE parties SET is_deleted = false WHERE id IN (?1)", nativeQuery = true)
    void revertSoftDeleteByPartiesIds(List<Long> partiesIds);

    @Modifying
    @Transactional
    @Query(value = "UPDATE parties SET is_deleted = true WHERE id NOT IN (?1) and entity_id = ?2 and shipment_id = ?3 and entity_type = ?4", nativeQuery = true)
    void deleteAdditionalPartiesInPickupDeliveryDetailsByShipmentId(List<Long> partiesIds, List<Long> pickupDeliveryDetailsIds, Long shipmentId, String entityType);
}
