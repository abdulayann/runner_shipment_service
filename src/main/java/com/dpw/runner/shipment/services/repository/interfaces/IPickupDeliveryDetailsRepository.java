package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IPickupDeliveryDetailsRepository extends MultiTenancyRepository<PickupDeliveryDetails> {
    List<PickupDeliveryDetails> findByShipmentId(Long shipmentId);
    Optional<PickupDeliveryDetails> findByGuid(UUID guid);

}
