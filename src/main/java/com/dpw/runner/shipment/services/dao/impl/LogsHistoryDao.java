package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ILogsHistoryDao;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.repository.interfaces.ILogsHistoryRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class LogsHistoryDao implements ILogsHistoryDao {

    @Autowired
    private ILogsHistoryRepository logsHistoryRepository;
    @Override
    public LogsHistory save(LogsHistory logsHistory) {
        return logsHistoryRepository.save(logsHistory);
    }

    @Override
    public List<LogsHistory> saveAll(List<LogsHistory> logsHistories) {
        return logsHistoryRepository.saveAll(logsHistories);
    }

    @Override
    public Page<LogsHistory> findAll(Specification<LogsHistory> spec, Pageable pageable) {
        return logsHistoryRepository.findAll(spec,pageable);
    }

    @Override
    public Optional<LogsHistory> findById(Long id) {
        return logsHistoryRepository.findById(id);
    }

    @Override
    public void delete(LogsHistory logsHistory) {
        logsHistoryRepository.delete(logsHistory);
    }

    @Override
    public Optional<LogsHistory> findByEntityGuid(UUID entityGuid){
        return logsHistoryRepository.findByEntityGuid(entityGuid);
    }
}
