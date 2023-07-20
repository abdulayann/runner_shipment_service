package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.Events;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface IEventRepository extends MultiTenancyRepository<Events> {
    Optional<Events> findByGuid(UUID guid);
}
