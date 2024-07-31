package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.commons.entity.AuditLog;
import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public interface IAuditLogRepository extends MultiTenancyRepository<AuditLog> {
}
