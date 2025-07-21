package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Generated
public interface IPickupDeliveryDetailsRepository extends MultiTenancyRepository<PickupDeliveryDetails> {
    List<PickupDeliveryDetails> findByShipmentId(Long shipmentId);
    default Optional<PickupDeliveryDetails> findById(Long id) {
        Specification<PickupDeliveryDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<PickupDeliveryDetails> findAll();
    List<PickupDeliveryDetails> findByIdIn(List<Long> ids);
    @Query(value = "SELECT count(*) from pickup_delivery_details where shipment_id = :shipmentId and is_deleted in(true,false)", nativeQuery = true)
    Long getTotalTransportInstructionCountIncludeDeleted(@Param("shipmentId") Long shipmentId);

    @Query(value = "SELECT * FROM pickup_delivery_details WHERE shipment_id IN ?1", nativeQuery = true)
    List<PickupDeliveryDetails> findByShipmentIdIn(Set<Long> shipmentIds);

    @Modifying
    @Query(value = "UPDATE pickup_delivery_details SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalPickupDeliveryDetailsByShipmentId(List<Long> pickupDeliveryDetailsIds, Long shipmentId);

    @Modifying
    @Query(value = "UPDATE pickup_delivery_details SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByPickupDeliveryDetailsIdsAndShipmentId(List<Long> pickupDeliveryDetailsIds, Long shipmentId);
}
