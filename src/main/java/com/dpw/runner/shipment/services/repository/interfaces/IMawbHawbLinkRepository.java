package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

@Repository
public interface IMawbHawbLinkRepository extends MultiTenancyRepository<MawbHawbLink> {
    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);
}
