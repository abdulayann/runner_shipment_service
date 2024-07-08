package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SyncQueue;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@org.springframework.stereotype.Repository @Generated
public interface IOrderManagementRepository extends JpaRepository<SyncQueue, Long> {
    @Modifying
    @Transactional
    @Query(value = "Update customer_booking set order_management_number = ?3 and order_id = ?2 Where guid = ?1", nativeQuery = true)
    void saveOrderIdAndOrderManagementNumber(UUID guid, String orderId, String orderManagementNumber);
}
