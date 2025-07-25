package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SyncQueue;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;


@org.springframework.stereotype.Repository @Generated
public interface IOrderManagementRepository extends JpaRepository<SyncQueue, Long> {
    @Modifying
    @Transactional
    @Query(value = "Update customer_booking set updated_at = now(), order_management_number = ?4, order_management_id = ?3 where booking_number = ?1 and tenant_id = ?2", nativeQuery = true)
    void saveOrderIdAndOrderManagementNumber(String bookingNumber, Long tenantId, String orderId, String orderManagementNumber);
}
