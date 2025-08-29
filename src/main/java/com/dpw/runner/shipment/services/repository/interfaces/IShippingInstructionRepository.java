package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;

public interface IShippingInstructionRepository extends MultiTenancyRepository<ShippingInstruction> {
}
