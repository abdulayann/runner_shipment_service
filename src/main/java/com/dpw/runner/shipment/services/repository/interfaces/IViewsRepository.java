package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Views;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IViewsRepository extends MultiTenancyRepository<Views> {
    List<Views> findAll();
    Page<Views> findAll(Specification<Views> spec, Pageable pageable);
    Optional<Views> findById(Long id);
}
