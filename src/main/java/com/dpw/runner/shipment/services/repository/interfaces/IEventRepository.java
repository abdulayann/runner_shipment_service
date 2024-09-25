package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository @Generated
@InterBranchEntity
public interface IEventRepository extends MultiTenancyRepository<Events> {
    Page<Events> findAll(Specification<Events> spec, Pageable pageable);

    List<Events> findAll();

    default Optional<Events> findById(Long id) {
        Specification<Events> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<Events> findByGuid(UUID id) {
        Specification<Events> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @Modifying
    @Transactional
    @Query(value = "insert into events (guid, entity_id, entity_type, event_code, description, estimated, actual, source, tenant_id, status, created_at, updated_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12)", nativeQuery = true)
    void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt);

    @Modifying @Transactional
    @Query(value = "insert into events (guid, entity_id, entity_type, event_code, description, source, tenant_id, pieces, total_pieces, weight, total_weight, is_partial, received_date, scheduled_date, created_at, updated_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15, ?16)", nativeQuery = true)
    void createEventForAirMessagingEvent(UUID guid, Long entityId, String entityType, String eventCode, String description, String source, Integer tenantId, Integer pieces, Integer totalPieces, BigDecimal weight, BigDecimal totalWeight, Boolean isPartial, LocalDateTime receivedDate, LocalDateTime scheduledDate, LocalDateTime createdAt, LocalDateTime updatedAt);
}
