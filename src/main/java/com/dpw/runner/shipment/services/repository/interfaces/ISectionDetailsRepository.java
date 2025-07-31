package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SectionDetails;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ISectionDetailsRepository extends JpaRepository<SectionDetails, Long> {
  boolean existsBySectionName(String sectionName);
}
