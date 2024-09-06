package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.repository.interfaces.IAuditLogRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class AuditLogDao implements IAuditLogDao {
    @Autowired
    private IAuditLogRepository auditLogRepository;

    @Override
    public AuditLog save(AuditLog auditLog) {
        return auditLogRepository.save(auditLog);
    }

    @Override
    public List<AuditLog> saveAll(List<AuditLog> auditLogs) {
        return auditLogRepository.saveAll(auditLogs);
    }

    @Override
    public Page<AuditLog> findAll(Specification<AuditLog> spec, Pageable pageable) {
        return auditLogRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<AuditLog> findById(Long id) {
        return auditLogRepository.findById(id);
    }

    @Override
    public void delete(AuditLog auditLog) {
        auditLogRepository.delete(auditLog);
    }


    @Override
    public List<AuditLog> findByOperationAndParentId(String operation, Long parentId) {
        return auditLogRepository.findByOperationAndParentId(operation, parentId);
    }

}
