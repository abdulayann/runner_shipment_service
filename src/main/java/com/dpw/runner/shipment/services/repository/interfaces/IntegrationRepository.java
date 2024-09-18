package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.IntegrationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface IntegrationRepository extends JpaRepository<IntegrationEntity, UUID> {
    IntegrationEntity findByUniqueId(String conversationID);

    List<IntegrationEntity> findByAwbNumber(String awbNumber);

    IntegrationEntity findFirstByEntityIdOrderByCreatedAtDesc(String entityId);
}
