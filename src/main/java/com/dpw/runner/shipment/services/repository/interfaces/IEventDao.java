package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Events;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IEventDao extends MultiTenancyRepository<Events> {
    List<Events> findByShipmentId(Long shipmentId);
}
