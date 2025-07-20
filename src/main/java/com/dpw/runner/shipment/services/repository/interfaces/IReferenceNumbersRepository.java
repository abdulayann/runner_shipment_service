package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
@Generated
public interface IReferenceNumbersRepository extends MultiTenancyRepository<ReferenceNumbers> {
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);

    @ExcludeTenantFilter
    default Page<ReferenceNumbers> findAllWithoutTenantFilter(Specification<ReferenceNumbers> spec, Pageable pageable){
        return findAll(spec, pageable);
    }

    List<ReferenceNumbers> findByShipmentId(Long shipmentId);
    default Optional<ReferenceNumbers> findById(Long id) {
        Specification<ReferenceNumbers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<ReferenceNumbers> findAll();

    @Modifying
    @Transactional
    @Query(value = "UPDATE reference_numbers SET is_deleted = true WHERE id NOT IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void deleteAdditionalDataByReferenceNumberIdsConsolidationId(List<Long> referenceNumberIds, Long consolidationId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE reference_numbers SET is_deleted = false WHERE id IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void revertSoftDeleteByReferenceNumberIdsAndConsolidationId(List<Long> referenceNumberIds, Long consolidationId);

    @Modifying
    @Query(value = "UPDATE reference_numbers SET is_deleted = true WHERE id NOT IN (?1) and booking_id = ?2", nativeQuery = true)
    void deleteAdditionalDataByReferenceNumberIdsBookingId(List<Long> referenceNumberIds, Long bookingId);

    @Modifying
    @Query(value = "UPDATE reference_numbers SET is_deleted = false WHERE id IN (?1) and booking_id = ?2", nativeQuery = true)
    void revertSoftDeleteByReferenceNumberIdsAndBookingId(List<Long> referenceNumberIds, Long bookingId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE reference_numbers SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalreferenceNumbersByShipmentId(List<Long> referenceNumbersIds, Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE reference_numbers SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByreferenceNumbersIdsAndShipmentId(List<Long> referenceNumbersIds, Long shipmentId);
}
