package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
@Generated
public interface IAirMessagingLogsRepository extends MultiTenancyRepository<AirMessagingLogs> {
    List<AirMessagingLogs> findAll();

    Page<AirMessagingLogs> findAll(Specification<AirMessagingLogs> spec, Pageable pageable);

    List<AirMessagingLogs> findByEntityGuid(UUID guid);

    @Modifying
    @Query(value = "SELECT * FROM air_messaging_logs WHERE entity_guid = ?1", nativeQuery = true)
    List<AirMessagingLogs> findByEntityGuidByQuery(UUID guid);

    @Modifying
    @Transactional
    @Query(value = "insert into air_messaging_logs (guid, entity_guid, error_message, message_type, xml_payload, status, tenant_id, created_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)", nativeQuery = true)
    void createAirMessagingLogs(UUID guid, UUID entityGuid, String errorMessage, String messageType, String xmlPayload, String status, Integer tenantId, LocalDateTime createdAt);

}
