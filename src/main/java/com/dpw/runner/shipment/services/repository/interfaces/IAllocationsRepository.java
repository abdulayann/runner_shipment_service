package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Allocations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface IAllocationsRepository extends MultiTenancyRepository<Allocations> {
    List<Allocations> findAll();
    Page<Allocations> findAll(Specification<Allocations> spec, Pageable pageable);
    Optional<Allocations> findById(Long id);
}
