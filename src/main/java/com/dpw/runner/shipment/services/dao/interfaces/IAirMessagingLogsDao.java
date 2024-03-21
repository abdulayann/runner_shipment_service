package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAirMessagingLogsDao {
    AirMessagingLogs save(AirMessagingLogs airMessagingLogs) throws RunnerException;
    Page<AirMessagingLogs> findAll(Specification<AirMessagingLogs> spec, Pageable pageable);
    Optional<AirMessagingLogs> findById(Long id);
    void delete(AirMessagingLogs airMessagingLogs);
}
