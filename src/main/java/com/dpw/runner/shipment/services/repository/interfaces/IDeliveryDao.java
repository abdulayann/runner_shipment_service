package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DeliveryDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IDeliveryDao extends MultiTenancyRepository<DeliveryDetails> {



}
