package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAuditLogRepository;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
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
import org.springframework.transaction.annotation.Transactional;

@Repository
@Slf4j
public class AuditLogDao implements IAuditLogDao {
    @Autowired
    private IAuditLogRepository auditLogRepository;

    @Autowired
    private JsonHelper jsonHelper;

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    @Transactional
    public AuditLog save(AuditLog auditLog) {
        log.info("AuditLog Save : preparing event save native query");

        Query query = entityManager.createNativeQuery(
                "INSERT INTO audit_log (operation, entity, parent_type, parent_id, tenant_id, is_deleted, updated_by, updated_at, changes, created_at, created_by, flow, data_type, is_integration_log) " +
                    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, CAST(? AS jsonb), ?, ?, ?, ?, ?)")
            .setParameter(1, auditLog.getOperation())
            .setParameter(2, auditLog.getEntity())
            .setParameter(3, auditLog.getParentType())
            .setParameter(4, auditLog.getParentId())
            .setParameter(5, auditLog.getTenantId())
            .setParameter(6, auditLog.getIsDeleted())
            .setParameter(7, auditLog.getUpdatedBy())
            .setParameter(8, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(auditLog.getUpdatedAt())))
            .setParameter(9, jsonHelper.convertToJson(auditLog.getChanges()))
            .setParameter(10, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(auditLog.getCreatedAt())))
            .setParameter(11, auditLog.getCreatedBy())
            .setParameter(12, auditLog.getFlow())
            .setParameter(13, auditLog.getDataType())
            .setParameter(14, auditLog.getIsIntegrationLog());

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
