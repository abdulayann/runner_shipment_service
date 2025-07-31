package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ISectionDetailsRepository extends MultiTenancyRepository<SectionDetails> {
    boolean existsBySectionName(String sectionName);

    @Query("SELECT sd FROM SectionDetails sd JOIN sd.sectionFields sf WHERE sf.id = :fieldId")
    @ExcludeTenantFilter
    List<SectionDetails> findAllBySectionFieldId(@Param("fieldId") Long fieldId);
}
