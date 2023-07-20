package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IPackingRepository extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);
    Optional<Packing> findByGuid(UUID guid);
}
