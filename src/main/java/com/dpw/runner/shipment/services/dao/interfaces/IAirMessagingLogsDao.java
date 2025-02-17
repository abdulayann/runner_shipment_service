package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IAirMessagingLogsDao {
    AirMessagingLogs save(AirMessagingLogs airMessagingLogs) throws RunnerException;

    Page<AirMessagingLogs> findAll(Specification<AirMessagingLogs> spec, Pageable pageable);

    Optional<AirMessagingLogs> findById(Long id);

    void delete(AirMessagingLogs airMessagingLogs);

    List<AirMessagingLogs> findByEntityGuid(UUID guid);

    List<AirMessagingLogs> findByEntityGuidByQuery(UUID guid);

    void createAirMessagingLogs(UUID guid, UUID entityGuid, String errorMessage, String messageType, String xmlPayload, String status, Integer tenantId, LocalDateTime createdAt);
}
