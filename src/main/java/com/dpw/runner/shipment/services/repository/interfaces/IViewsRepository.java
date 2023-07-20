package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.Views;

import java.util.Optional;
import java.util.UUID;

public interface IViewsRepository extends MultiTenancyRepository<Views> {
    Optional<Views> findByGuid(UUID guid);
}
