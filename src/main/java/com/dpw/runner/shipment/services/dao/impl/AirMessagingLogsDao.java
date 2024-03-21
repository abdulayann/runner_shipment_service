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

import java.util.Optional;

@Repository
@Slf4j
public class AirMessagingLogsDao implements IAirMessagingLogsDao {
    private IAirMessagingLogsRepository airMessagingLogsRepository;

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
}
