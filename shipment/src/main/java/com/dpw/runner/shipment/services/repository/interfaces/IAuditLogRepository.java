package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;

@Generated
public interface IAuditLogRepository extends MultiTenancyRepository<AuditLog> {
  List<AuditLog> findByOperationAndParentId(String operation, Long parentId);
}
