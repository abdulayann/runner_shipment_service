package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Events;
import org.springframework.stereotype.Repository;

@Repository
public interface IEventRepository extends MultiTenancyRepository<Events> {
}
