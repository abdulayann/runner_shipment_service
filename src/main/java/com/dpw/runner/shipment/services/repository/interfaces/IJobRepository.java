package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Jobs;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IJobRepository extends MultiTenancyRepository<Jobs> {
    List<Jobs> findByShipmentId(Long id);
}
