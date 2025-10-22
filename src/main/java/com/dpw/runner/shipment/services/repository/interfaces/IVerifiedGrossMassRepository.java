package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.EntityType;

public interface IVerifiedGrossMassRepository extends MultiTenancyRepository<VerifiedGrossMass> {
    VerifiedGrossMass findByCarrierBookingNo(String carrierBookingNo);
    VerifiedGrossMass findByEntityTypeAndEntityId(EntityType entityType, Long entityId);
}
