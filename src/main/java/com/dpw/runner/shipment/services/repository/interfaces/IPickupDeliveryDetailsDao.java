package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;

public interface IPickupDeliveryDetailsDao extends MultiTenancyRepository<PickupDeliveryDetails> {
}
