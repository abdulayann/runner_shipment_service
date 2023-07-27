package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;

import java.util.List;
import java.util.Optional;

public interface IPickupDeliveryDetailsRepository extends MultiTenancyRepository<PickupDeliveryDetails> {
    List<PickupDeliveryDetails> findByShipmentId(Long shipmentId);
    Optional<PickupDeliveryDetails> findById(Long id);
    List<PickupDeliveryDetails> findAll();

}
