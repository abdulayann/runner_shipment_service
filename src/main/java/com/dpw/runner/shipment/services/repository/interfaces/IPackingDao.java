package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Packing;

import java.util.List;

public interface IPackingDao extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);
}
