package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.repository.interfaces.IAuditLogRepository;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.jpa.TypedParameterValue;
import org.hibernate.type.StandardBasicTypes;
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

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public AuditLog save(AuditLog auditLog) {
        log.info("AuditLog Save : preparing event save native query");

        Query query = entityManager.createNativeQuery(
                "INSERT INTO audit_log (id, operation, entity, entity_id, changes, parent_type, parent_id, tenant_id, guid, is_deleted, created_at, updated_at) " +
                    "VALUES (?, ?, ?, ?, CAST(? AS jsonb), ?, ?, ?, ?, ?, ?, ?)")
            .setParameter(1, auditLog.getId())
            .setParameter(2, auditLog.getOperation())
            .setParameter(3, auditLog.getEntity())
            .setParameter(4, auditLog.getEntityId())
            .setParameter(5, auditLog.getChanges() != null ? auditLog.getChanges().toString() : null) // Convert map to JSON string
            .setParameter(6, auditLog.getParentType())
            .setParameter(7, auditLog.getParentId())
            .setParameter(8, auditLog.getTenantId()) // Assuming `tenant_id` is inherited from `MultiTenancy`
            .setParameter(9, auditLog.getGuid())
            .setParameter(10, auditLog.getIsDeleted())
            .setParameter(11, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, auditLog.getCreatedAt()))
            .setParameter(12, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, auditLog.getUpdatedAt()));

        log.info("Executing native query for AuditLog save");
        query.executeUpdate();
        log.info("Native query execution for AuditLog save completed");
        return auditLog;
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
