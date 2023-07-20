package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.entity.Jobs;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface IJobRepository extends MultiTenancyRepository<Jobs> {
    List<Jobs> findByShipmentId(Long id);
    Optional<Jobs> findByGuid(UUID guid);
}
