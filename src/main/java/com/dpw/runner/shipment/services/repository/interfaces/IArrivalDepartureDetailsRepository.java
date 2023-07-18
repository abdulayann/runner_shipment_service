package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsRepository;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;


@Repository
public interface IArrivalDepartureDetailsRepository extends MultiTenancyRepository<ArrivalDepartureDetails>, PermissionsRepository<ArrivalDepartureDetails> {
    Page<ArrivalDepartureDetails> findAll(Specification<ArrivalDepartureDetails> spec, Pageable pageable);
}
