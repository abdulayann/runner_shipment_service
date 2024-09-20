package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository @Generated
public interface IMawbHawbLinkRepository extends MultiTenancyRepository<MawbHawbLink> {
    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);
    @Query(value = "SELECT * FROM mawb_hawb_link WHERE mawb_id = ?1", nativeQuery = true)
    List<MawbHawbLink> findByMawbId(Long mawbId);
    List<MawbHawbLink> findByHawbId(Long hawbId);
}
