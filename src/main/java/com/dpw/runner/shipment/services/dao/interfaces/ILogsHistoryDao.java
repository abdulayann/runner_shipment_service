package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.LogsHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ILogsHistoryDao {
    LogsHistory save(LogsHistory logsHistory);

    List<LogsHistory> saveAll(List<LogsHistory> auditLogs);

    Page<LogsHistory> findAll(Specification<LogsHistory> spec, Pageable pageable);

    Optional<LogsHistory> findById(Long id);

    void delete(LogsHistory logsHistory);

    Optional<LogsHistory> findByEntityGuidAndTimeStamp(UUID entityGuid, LocalDateTime timeStamp);

    List<LogsHistory> findByEntityGuidsAndTimeStamp(List<UUID> entityGuids, LocalDateTime timeStamp);
}
