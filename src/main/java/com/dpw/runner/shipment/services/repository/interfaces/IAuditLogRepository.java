package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

@Generated
public interface IAuditLogRepository extends MultiTenancyRepository<AuditLog> {
  List<AuditLog> findByOperationAndParentId(String operation, Long parentId);

  @ExcludeTenantFilter
  default Page<AuditLog> findAllWithoutTenantFilter(Specification<AuditLog> spec, Pageable pageable) {
    return findAll(spec, pageable);
  }
}
