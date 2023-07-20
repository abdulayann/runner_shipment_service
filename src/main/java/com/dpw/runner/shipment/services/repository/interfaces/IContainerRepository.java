package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Containers;

import java.util.List;
import java.util.Optional;
import java.util.UUID;


public interface IContainerRepository extends MultiTenancyRepository<Containers> {
    Optional<Containers> findByGuid(UUID guid);
}
