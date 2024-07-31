package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.commons.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository @Generated
public interface IMawbHawbLinkRepository extends MultiTenancyRepository<MawbHawbLink> {
    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);

    List<MawbHawbLink> findByMawbId(Long mawbId);
    List<MawbHawbLink> findByHawbId(Long hawbId);
}
