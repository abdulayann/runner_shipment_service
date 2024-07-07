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

    @Query(value = "WITH LatestLogs AS (" +
            "    SELECT entity_guid, MAX(created_at) AS latest_created_at" +
            "    FROM logs_history" +
            "    WHERE entity_guid IN ?1" +
            "      AND created_at < ?2" +
            "    GROUP BY entity_guid" +
            ") " +
            "SELECT lh.* " +
            "FROM logs_history lh " +
            "JOIN LatestLogs ll " +
            "ON lh.entity_guid = ll.entity_guid AND lh.created_at = ll.latest_created_at " +
            "ORDER BY lh.created_at DESC",
            nativeQuery = true)
    List<LogsHistory> findByEntityGuidsAndTimeStamp(List<UUID> entityGuids, LocalDateTime timeStamp);
}
