package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface ILogsHistoryRepository extends MultiTenancyRepository<LogsHistory> {
    Optional<LogsHistory> findByEntityGuid(UUID entityGuid);
}
