package com.dpw.runner.shipment.services.migration.repository;

import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

@Repository
public interface ICustomerBookingBackupRepository extends JpaRepository<CustomerBookingBackupEntity, Long> {
    void deleteByTenantId(Integer tenantId);

    @Query(value = "SELECT c.booking_id FROM customer_booking_backup c WHERE c.tenant_id = ?1 and c.is_deleted = false", nativeQuery = true)
    @Transactional
    Set<Long> findCustomerBookingIdsByTenantId(Integer tenantId);

    @Query(value = "SELECT * FROM customer_booking_backup c WHERE c.booking_id = ?1", nativeQuery = true)
    @Transactional
    CustomerBookingBackupEntity findCustomerBookingDetailsById(Long bookingId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE customer_booking_backup SET is_deleted = true WHERE id = ?1", nativeQuery = true)
    void makeIsDeleteTrueToMarkRestoreSuccessful(Long backupId);
}
