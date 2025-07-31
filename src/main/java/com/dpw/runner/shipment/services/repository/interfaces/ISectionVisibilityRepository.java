package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.SectionVisibility;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ISectionVisibilityRepository extends
        MultiTenancyRepository<SectionVisibility> {

    boolean existsByBranchCodeAndModeAndDirection(String branch, String mode, String direction);

    SectionVisibility findByBranchCodeAndModeAndDirection(String branch, String mode, String direction);

    List<SectionVisibility> findByBranchCode(String code);

    @Query("SELECT sv FROM SectionVisibility sv JOIN sv.sections sd WHERE sd.id = :sectionDetailsId")
    List<SectionVisibility> findBySectionDetailsId(@Param("sectionDetailsId") Long sectionDetailsId);
}
