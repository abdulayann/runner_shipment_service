package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Jobs;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IJobRepository extends MultiTenancyRepository<Jobs> {
    List<Jobs> findByShipmentId(Long id);
    Optional<Jobs> findById(Long id);
    List<Jobs> findAll();
    Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable);
}
