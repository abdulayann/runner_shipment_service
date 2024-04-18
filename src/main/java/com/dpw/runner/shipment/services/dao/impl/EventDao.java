package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.jpa.TypedParameterValue;
import org.hibernate.type.StandardBasicTypes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;

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
    private IEventsSync eventsSync;

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

    @Override
    public List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType) throws RunnerException {
        String responseMsg;
        List<Events> responseEvents = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Events> hashMap;
//            if(!Objects.isNull(eventsIdList) && !eventsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entityId, entityType);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> events = findAll(pair.getLeft(), pair.getRight());
                hashMap = events.stream()
                        .collect(Collectors.toMap(Events::getId, Function.identity()));
//            }
            Map<Long, Events> copyHashMap = new HashMap<>(hashMap);
            List<Events> eventsRequestList = new ArrayList<>();
            if (eventsList != null && eventsList.size() != 0) {
                for (Events request : eventsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    eventsRequestList.add(request);
                }
                responseEvents = saveEntityFromOtherEntity(eventsRequestList, entityId, entityType, copyHashMap);
            }
            deleteEvents(hashMap, entityType, entityId);
            return responseEvents;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<Events> saveEntityFromOtherEntity(List<Events> events, Long entityId, String entityType) {
        List<Events> res = new ArrayList<>();
        for (Events req : events) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Events> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Events is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Events.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Events> saveEntityFromOtherEntity(List<Events> events, Long entityId, String entityType, Map<Long, Events> oldEntityMap) {
        List<Events> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (Events req : events) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Events is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Events.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    private void deleteEvents(Map<Long, Events> hashMap, String entityType, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(event -> {
                String json = jsonHelper.convertToJson(event);
                delete(event);
                if(entityType != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Events.class))
                                        .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType, List<Events> oldEntityList) throws RunnerException {
        String responseMsg;
        Map<UUID, Events> eventsMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Events entity :
                    oldEntityList) {
                eventsMap.put(entity.getGuid(), entity);
            }
        }
        List<Events> responseEvents = new ArrayList<>();
        try {
            Events oldEntity;
            List<Events> eventsRequestList = new ArrayList<>();
            if (eventsList != null && eventsList.size() != 0) {
                for (Events request : eventsList) {
                    oldEntity = eventsMap.get(request.getGuid());
                    if (oldEntity != null) {
                        eventsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    request.setEntityId(entityId);
                    request.setEntityType(entityType);
                    eventsRequestList.add(request);
                }
                responseEvents = saveEntityFromOtherEntity(eventsRequestList, entityId, entityType);
            }
            Map<Long, Events> hashMap = new HashMap<>();
            eventsMap.forEach((s, events) -> hashMap.put(events.getId(), events));
            deleteEvents(hashMap, entityType, entityId);
            return responseEvents;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public void autoGenerateEvents(CustomAutoEventRequest request) {
        try {
            if (request.createDuplicate || !checkIfEventsRowExistsForEntityTypeAndEntityId(request)) {
                createAutomatedEventRequest(request.entityType, request.entityId, request.eventCode, request.isEstimatedRequired, request.isActualRequired, request.placeName, request.placeDesc);
            }
        } catch (Exception e) {
            log.error("Error occured while trying to auto create runner event, Request recieved is = " + request + ". Exception raised is: " + e);
            throw new ValidationException("Error occured while trying to auto create runner event, Request recieved is = " + request, e);
        }
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

    public void createAutomatedEventRequest(String entityType, long entityId, String eventCode, boolean isEstimatedRequired, boolean isActualRequired, String placeName, String placeDesc) {
        try {
            Events eventsRow = new Events();
            if (isActualRequired) {
                eventsRow.setActual(LocalDate.now().atStartOfDay());
            }

            if (isEstimatedRequired) {
                eventsRow.setEstimated(LocalDate.now().atStartOfDay());
            }
            eventsRow.setSource(Constants.CARGO_RUNNER);
            eventsRow.setIsPublicTrackingEvent(true);
            eventsRow.setEntityType(entityType);
            eventsRow.setEntityId(entityId);
            eventsRow.setEventCode(eventCode);
            eventsRow.setPlaceName(placeName);
            eventsRow.setPlaceDescription(placeDesc);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(eventsRow)
                                .prevData(null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(DBOperationType.CREATE.name()).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                log.error(e.getMessage());
            }
            eventsRow = eventRepository.save(eventsRow);
            try {
                eventsSync.sync(List.of(eventsRow));
            } catch (Exception e) {
                log.error("Error performing sync on event entity, {}", e);
            }
        } catch (Exception e) {
            log.error("Error occured while trying to create runner event, Exception raised is: " + e);
        }
    }


    @Override
    public void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt) {
        eventRepository.createEventForAirMessagingStatus(guid, entityId, entityType, eventCode, description, estimated, actual, source, tenantId, status, createdAt, updatedAt);
    }

    @Override
    @Transactional
    public void createEventForAirMessagingEvent(UUID guid, Long entityId, String entityType, String eventCode, String description, String source, Integer tenantId, Integer pieces, Integer totalPieces, BigDecimal weight, BigDecimal totalWeight, String partial, LocalDateTime receivedDate, LocalDateTime scheduledDate, LocalDateTime createdAt, LocalDateTime updatedAt) {
        Query query = entityManager.createNativeQuery("insert into events (guid, entity_id, entity_type, event_code, description, source, tenant_id, pieces, total_pieces, weight, total_weight, partial, received_date, scheduled_date, created_at, updated_at) VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
                .setParameter(1, guid)
                .setParameter(2, entityId)
                .setParameter(3, entityType)
                .setParameter(4, eventCode)
                .setParameter(5, description)
                .setParameter(6, source)
                .setParameter(7, tenantId)
                .setParameter(8, new TypedParameterValue(StandardBasicTypes.INTEGER, pieces))
                .setParameter(9, new TypedParameterValue(StandardBasicTypes.INTEGER, totalPieces))
                .setParameter(10, new TypedParameterValue(StandardBasicTypes.BIG_DECIMAL, weight))
                .setParameter(11, new TypedParameterValue(StandardBasicTypes.BIG_DECIMAL, totalWeight))
                .setParameter(12, partial)
                .setParameter(13, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(receivedDate)))
                .setParameter(14, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(scheduledDate)))
                .setParameter(15, createdAt)
                .setParameter(16, updatedAt);
        query.executeUpdate();

//        eventRepository.createEventForAirMessagingEvent(guid, entityId, entityType, eventCode, description, source, tenantId, pieces, totalPieces, weight, totalWeight, partial, receivedDate, scheduledDate, createdAt, updatedAt);
    }
}
