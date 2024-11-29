package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface IEventDao {
    Events save(Events events);

    List<Events> saveAll(List<Events> eventsList);

    Optional<Events> findByGuid(UUID id);

    Page<Events> findAll(Specification<Events> spec, Pageable pageable);

    Optional<Events> findById(Long id);

    void delete(Events events);

    List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType) throws RunnerException;
    List<Events> saveEntityFromOtherEntity(List<Events> events, Long entityId, String entityType, Map<Long, Events> oldEntityMap);

    List<Events> saveEntityFromOtherEntity(List<Events> events, Long entityId, String entityType);

    List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType, List<Events> oldEntityList) throws RunnerException;

    void autoGenerateEvents(CustomAutoEventRequest request);

    void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt);

    void createEventForAirMessagingEvent(Events events);

    void updateEventDetails(Events event);

    boolean shouldSendEventFromShipmentToConsolidation(Events events, String transportMode);

    void updateFieldsForShipmentGeneratedEvents(List<Events> eventsList, ShipmentDetails shipmentDetails);
}
