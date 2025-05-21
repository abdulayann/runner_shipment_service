package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.InternalEvent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Repository
public interface InternalEventRepository extends JpaRepository<InternalEvent, Long> {

    @Modifying
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Query(value = "UPDATE internal_events SET published_status = :status, published_timestamp = :timestamp WHERE id = :Id", nativeQuery = true)
    int updatePublishedStatus(@Param("Id") Long Id, @Param("status") String status, @Param("timestamp") LocalDateTime timestamp);

    @Modifying
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Query(value = "UPDATE internal_events SET consumed_status = :status, consumed_timestamp = :timestamp WHERE id = :id", nativeQuery = true)
    int updateConsumedStatus(@Param("id") Long id, @Param("status") String status, @Param("timestamp") LocalDateTime timestamp);

}
