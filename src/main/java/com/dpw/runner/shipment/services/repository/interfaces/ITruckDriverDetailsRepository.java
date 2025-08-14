package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
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
public interface ITruckDriverDetailsRepository extends MultiTenancyRepository<TruckDriverDetails> {
    Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable);
    default Optional<TruckDriverDetails> findById(Long id) {
        Specification<TruckDriverDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<TruckDriverDetails> findAll();
  List<TruckDriverDetails> findByShipmentId(Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE truck_driver_details SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalTruckDriverDetailsByShipmentId(List<Long> truckDriverDetailsIds, Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE truck_driver_details SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByTruckDriverDetailsIdsAndShipmentId(List<Long> truckDriverDetailsIds, Long shipmentId);
}
