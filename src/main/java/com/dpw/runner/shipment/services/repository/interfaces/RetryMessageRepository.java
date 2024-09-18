package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.RetryMessageEntity;
import com.dpw.runner.shipment.services.entity.enums.EventMessageStatusEnum;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface RetryMessageRepository extends JpaRepository<RetryMessageEntity, UUID> {


    List<RetryMessageEntity> findByStatusAndRetryCountLessThan(EventMessageStatusEnum eventMessageStatusEnum, int retryCount);
}
