package com.dpw.runner.booking.services.repository.interfaces;


import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.booking.services.entity.AuditLog;
import com.dpw.runner.booking.services.utils.Generated;
import java.util.List;

@Generated
public interface IAuditLogRepository extends MultiTenancyRepository<AuditLog> {
  List<AuditLog> findByOperationAndParentId(String operation, Long parentId);
}
