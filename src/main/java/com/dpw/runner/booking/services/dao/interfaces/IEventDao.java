package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.Events;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IEventDao {
    Events save(Events events);

    List<Events> saveAll(List<Events> eventsList);

    Optional<Events> findByGuid(UUID id);

    Page<Events> findAll(Specification<Events> spec, Pageable pageable);

    Optional<Events> findById(Long id);

    void delete(Events events);

    void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt);

    void createEventForAirMessagingEvent(Events events);
}
