package com.dpw.runner.shipment.services.repositoryTO;

import com.dpw.runner.shipment.services.entityTO.IntegrationEntity;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
@Qualifier("secondaryEntityManagerFactory")
public interface IntegrationRepository extends JpaRepository<IntegrationEntity, UUID> {
    IntegrationEntity findByUniqueId(String conversationID);

    List<IntegrationEntity> findByAwbNumber(String awbNumber);

    IntegrationEntity findFirstByEntityIdOrderByCreatedAtDesc(String entityId);
}
