package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.commons.requests.ListCommonRequest;
import com.dpw.runner.booking.services.dao.interfaces.IEventDao;
import com.dpw.runner.booking.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.booking.services.entity.Events;
import com.dpw.runner.booking.services.entity.enums.LifecycleHooks;
import com.dpw.runner.booking.services.exception.exceptions.ValidationException;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.repository.interfaces.IEventRepository;
import com.dpw.runner.booking.services.service.interfaces.IAuditLogService;
import com.dpw.runner.booking.services.utils.CommonUtils;
import com.dpw.runner.booking.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.jpa.TypedParameterValue;
import org.hibernate.type.StandardBasicTypes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static com.dpw.runner.booking.services.helpers.DbAccessHelper.fetchData;

@Repository
@Slf4j
public class EventDao implements IEventDao {
    @Autowired
    private IEventRepository eventRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public Events save(Events events) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(events), Constants.EVENTS, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return eventRepository.save(events);
    }

    @Override
    public List<Events> saveAll(List<Events> eventsList) {
        for (var events: eventsList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(events), Constants.EVENTS, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return eventRepository.saveAll(eventsList);
    }

    @Override
    public Page<Events> findAll(Specification<Events> spec, Pageable pageable) {
        return eventRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Events> findById(Long id) {
        return eventRepository.findById(id);
    }

    @Override
    public Optional<Events> findByGuid(UUID id) {
        return eventRepository.findByGuid(id);
    }

    @Override
    public void delete(Events events) {
        eventRepository.delete(events);
    }

    public List<Events> getTheDataFromEntity(String EntityType, long EntityID, boolean publicEvent) {
        ListCommonRequest listCommonRequest;
        if (publicEvent) {
            listCommonRequest = CommonUtils.andCriteria("entityId", EntityID, "=", null);
            CommonUtils.andCriteria("entityType", EntityType, "=", listCommonRequest);
            CommonUtils.andCriteria("isPublicTrackingEvent", true, "=", listCommonRequest);
        } else {
            listCommonRequest = CommonUtils.andCriteria("entityId", EntityID, "=", null);
            CommonUtils.andCriteria("entityType", EntityType, "=", listCommonRequest);
            CommonUtils.andCriteria("isPublicTrackingEvent", false, "=", listCommonRequest);
        }


        Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
        Page<Events> events = findAll(pair.getLeft(), pair.getRight());
        if (events.getContent().size() > 0) {
            return events.getContent();
        }
        return null;
    }

    public boolean checkIfEventsRowExistsForEntityTypeAndEntityId(CustomAutoEventRequest request) {
        List<Events> eventsRowList = getTheDataFromEntity(request.entityType, request.entityId, true);
        if (eventsRowList != null && eventsRowList.size() > 0) {
            for (Events eventsRow : eventsRowList) {
                if (eventsRow.getEventCode().equalsIgnoreCase(request.eventCode)) {
                    log.info("Event already exists for given id: " + request.entityId + " and type: " + request.entityType + " and event code : " + request.eventCode);
                    return true;
                }
            }
        }
        return false;
    }


    @Override
    public void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt) {
        eventRepository.createEventForAirMessagingStatus(guid, entityId, entityType, eventCode, description, estimated, actual, source, tenantId, status, createdAt, updatedAt);
    }

    @Override
    @Transactional
    public void createEventForAirMessagingEvent(Events events) {
        Query query = entityManager.createNativeQuery("insert into events (guid, entity_id, entity_type, event_code, description, source, tenant_id, pieces, total_pieces, weight, total_weight, is_partial, received_date, scheduled_date, created_at, updated_at, estimated, actual, place_name, place_description, longitude, latitude) VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
                .setParameter(1, events.getGuid())
                .setParameter(2, events.getEntityId())
                .setParameter(3, events.getEntityType())
                .setParameter(4, events.getEventCode())
                .setParameter(5, events.getDescription())
                .setParameter(6, events.getSource())
                .setParameter(7, events.getTenantId())
                .setParameter(8, new TypedParameterValue(StandardBasicTypes.INTEGER, events.getPieces()))
                .setParameter(9, new TypedParameterValue(StandardBasicTypes.INTEGER, events.getTotalPieces()))
                .setParameter(10, new TypedParameterValue(StandardBasicTypes.BIG_DECIMAL, events.getWeight()))
                .setParameter(11, new TypedParameterValue(StandardBasicTypes.BIG_DECIMAL, events.getTotalWeight()))
                .setParameter(12, events.getIsPartial())
                .setParameter(15, events.getCreatedAt())
                .setParameter(16, events.getUpdatedAt())
                .setParameter(19, events.getPlaceName())
                .setParameter(20, events.getPlaceDescription())
                .setParameter(21, events.getLongitude())
                .setParameter(22, events.getLatitude());

        if(events.getReceivedDate() != null) {
            query.setParameter(13, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getReceivedDate())));
        } else {
            query.setParameter(13, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if(events.getScheduledDate() != null) {
            query.setParameter(14, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getScheduledDate())));
        } else {
            query.setParameter(14, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if(events.getEstimated() != null) {
            query.setParameter(17, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getEstimated())));
        } else {
            query.setParameter(17, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if(events.getActual() != null) {
            query.setParameter(18, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getActual())));
        } else {
            query.setParameter(18, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        query.executeUpdate();

//        eventRepository.createEventForAirMessagingEvent(guid, entityId, entityType, eventCode, description, source, tenantId, pieces, totalPieces, weight, totalWeight, partial, receivedDate, scheduledDate, createdAt, updatedAt);
    }
}
