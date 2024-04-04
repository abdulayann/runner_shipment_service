package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface IAirMessagingLogsRepository extends MultiTenancyRepository<AirMessagingLogs> {
    List<AirMessagingLogs> findAll();
    Page<AirMessagingLogs> findAll(Specification<AirMessagingLogs> spec, Pageable pageable);
    List<AirMessagingLogs> findByEntityGuid(UUID guid);
}
