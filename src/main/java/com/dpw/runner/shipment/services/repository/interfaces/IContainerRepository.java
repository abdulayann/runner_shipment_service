package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Containers;

import java.util.List;


public interface IContainerRepository extends MultiTenancyRepository<Containers> {

    List<Containers> findByShipmentId(Long shipmentId);
}
