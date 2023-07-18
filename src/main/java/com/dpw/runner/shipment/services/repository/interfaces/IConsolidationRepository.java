package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsRepository;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;


@Repository
public interface IConsolidationRepository extends MultiTenancyRepository<ConsolidationDetails>, PermissionsRepository<ConsolidationDetails> {
    Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable);
}
