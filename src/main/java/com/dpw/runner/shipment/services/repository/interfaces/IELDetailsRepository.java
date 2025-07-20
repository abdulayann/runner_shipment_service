package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated
public interface IELDetailsRepository extends MultiTenancyRepository<ELDetails> {
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);

    Optional<ELDetails> findByElNumber(String elNumber);

    List<ELDetails> findByShipmentId(Long shipmentId);

    default Optional<ELDetails> findById(Long id) {
        Specification<ELDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<ELDetails> findByGuid(UUID id) {
        Specification<ELDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<ELDetails> findAll();

    @Modifying
    @Transactional
    @Query(value = "UPDATE el_details SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalElDetailsByShipmentId(List<Long> elDetailsIds, Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE el_details SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByElDetailsIdsAndShipmentId(List<Long> elDetailsIds, Long shipmentId);
}
