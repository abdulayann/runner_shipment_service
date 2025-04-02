package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Generated
public interface ILogsHistoryRepository extends MultiTenancyRepository<LogsHistory> {
    @Query(value = "SELECT * FROM logs_history WHERE entity_guid = ?1 and created_at <= ?2 order By created_at desc limit 1", nativeQuery = true)
    Optional<LogsHistory> findByEntityGuidAndTimeStamp(UUID entityGuid, LocalDateTime timeStamp);

    @Query(value = " SELECT DISTINCT ON (lh.entity_guid) lh.* FROM logs_history lh WHERE lh.entity_guid IN ?1 AND lh.created_at < ?2 ORDER BY lh.entity_guid, lh.created_at DESC", nativeQuery = true)
    List<LogsHistory> findByEntityGuidsAndTimeStamp(List<UUID> entityGuids, LocalDateTime timeStamp);
}
