package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.domain.Specification;

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

}
