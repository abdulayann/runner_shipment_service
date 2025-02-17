package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAirMessagingLogsDao;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.repository.interfaces.IAirMessagingLogsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class AirMessagingLogsDao implements IAirMessagingLogsDao {
    private final IAirMessagingLogsRepository airMessagingLogsRepository;

    @Autowired
    public AirMessagingLogsDao(IAirMessagingLogsRepository airMessagingLogsRepository) {
        this.airMessagingLogsRepository = airMessagingLogsRepository;
    }

    @Override
    public AirMessagingLogs save(AirMessagingLogs airMessagingLogs) {
        return airMessagingLogsRepository.save(airMessagingLogs);
    }

    @Override
    public Page<AirMessagingLogs> findAll(Specification<AirMessagingLogs> spec, Pageable pageable) {
        return airMessagingLogsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<AirMessagingLogs> findById(Long id) {
        return airMessagingLogsRepository.findById(id);
    }

    @Override
    public void delete(AirMessagingLogs airMessagingLogs) {
        airMessagingLogsRepository.delete(airMessagingLogs);
    }

    @Override
    public List<AirMessagingLogs> findByEntityGuid(UUID guid) {
        return airMessagingLogsRepository.findByEntityGuid(guid);
    }

    @Override
    public List<AirMessagingLogs> findByEntityGuidByQuery(UUID guid) {
        return airMessagingLogsRepository.findByEntityGuidByQuery(guid);
    }

    @Override
    public void createAirMessagingLogs(UUID guid, UUID entityGuid, String errorMessage, String messageType, String xmlPayload, String status, Integer tenantId, LocalDateTime createdAt) {
        airMessagingLogsRepository.createAirMessagingLogs(guid, entityGuid, errorMessage, messageType, xmlPayload, status, tenantId, createdAt);
    }

}
