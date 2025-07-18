package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ICustomerBookingBackupRepository extends JpaRepository<CustomerBookingBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
