package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAuditLogDao {
    AuditLog save(AuditLog auditLog);
    Page<AuditLog> findAll(Specification<AuditLog> spec, Pageable pageable);
    Optional<AuditLog> findById(Long id);
    void delete(AuditLog auditLog);
}
