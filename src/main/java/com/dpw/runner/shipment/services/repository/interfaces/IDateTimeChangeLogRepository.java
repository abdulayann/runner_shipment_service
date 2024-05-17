package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import org.springframework.stereotype.Repository;

@Repository
public interface IDateTimeChangeLogRepository extends MultiTenancyRepository<DateTimeChangeLog> {
}
