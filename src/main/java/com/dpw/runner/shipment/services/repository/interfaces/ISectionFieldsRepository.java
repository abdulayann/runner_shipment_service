package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SectionFields;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ISectionFieldsRepository extends JpaRepository<SectionFields, Long> {

  boolean existsByFieldName(String fieldName);
}
