package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
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
}
