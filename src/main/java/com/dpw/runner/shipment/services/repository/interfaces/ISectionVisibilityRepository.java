package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SectionVisibility;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ISectionVisibilityRepository extends JpaRepository<SectionVisibility, Long> {

  boolean existsByBranchAndModeAndDirection(String branch, String mode, String direction);
  SectionVisibility findByTenantIdAndModeAndDirection(Integer tenantId, String mode, String direction);

}
