package com.dpw.runner.shipment.services.migration.repository.interfaces;

import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CustomerBookingRepository extends JpaRepository<CustomerBookingBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
