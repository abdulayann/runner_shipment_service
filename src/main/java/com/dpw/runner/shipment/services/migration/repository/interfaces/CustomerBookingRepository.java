package com.dpw.runner.shipment.services.migration.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CustomerBookingRepository extends JpaRepository<CustomerBookingBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);
}
