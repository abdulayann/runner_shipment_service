package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Routings;
import org.springframework.data.domain.Page;


import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IRoutingsRepository extends MultiTenancyRepository<Routings> {
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

    List<Routings> findByShipmentId(Long shipmentId);
    Optional<Routings> findById(Long id);
    List<Routings> findAll();

}
