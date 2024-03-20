package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
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
import java.util.UUID;


@Repository
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
    List<ShipmentDetails> findByHouseBill(String Hbl);
    List<ShipmentDetails> findAllByHouseBill(String Hbl);
    List<ShipmentDetails> findByBookingReference(String ref);

    @Query(value = "SELECT MAX(c.id) FROM consolidation_details c", nativeQuery = true)
    Long findMaxId();

    @Modifying @Transactional
    @Query(value = "Update shipment_details set job_status = ?2 Where id = ?1", nativeQuery = true)
    void saveJobStatus(Long id, String jobStatus);

    @Modifying @Transactional
    @Query(value = "Update shipment_details set created_by = ?2, created_at = ?3 Where id = ?1", nativeQuery = true)
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);

    @Modifying @Transactional
    @Query(value = "Update shipment_details set job_status = ?2 Where guid = ?1", nativeQuery = true)
    void saveJobStatusByGuid(String guid, String jobStatus);
}
