package com.dpw.runner.booking.services.repository.interfaces;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.booking.services.entity.DateTimeChangeLog;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IDateTimeChangeLogRepository extends MultiTenancyRepository<DateTimeChangeLog> {

    List<DateTimeChangeLog> findByShipmentId(Long shipmentId);
}
