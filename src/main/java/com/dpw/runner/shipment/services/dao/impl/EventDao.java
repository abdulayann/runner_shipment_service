package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
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
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.hibernate.jpa.TypedParameterValue;
import org.hibernate.type.StandardBasicTypes;
import org.slf4j.MDC;
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
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
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

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private CommonUtils commonUtils;

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public Events save(Events events) {
        updateUserFieldsInEvent(events, false);
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(events), Constants.EVENTS, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return eventRepository.save(events);
    }

    @Override
    public List<Events> saveAll(List<Events> eventsList) {
        for (var events : eventsList) {
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
            if (entityType.equalsIgnoreCase(Constants.CONSOLIDATION)) {
                listCommonRequest = CommonUtils.constructListCommonRequest("consolidationId", entityId, "=");
            }
            Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
            Page<Events> eventsPage = findAll(pair.getLeft(), pair.getRight());
            hashMap = eventsPage.stream()
                    .collect(Collectors.toMap(Events::getId, Function.identity()));
//            }
            Map<Long, Events> copyHashMap = new HashMap<>(hashMap);
            List<Events> eventsRequestList = new ArrayList<>();
            eventsList = filterStaleEvents(eventsList, eventsPage.getContent());
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
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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
            if (req.getEntityId() == null)
                req.setEntityId(entityId);
            if (req.getEntityType() == null)
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
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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
                if (entityType != null) {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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
                eventsRow.setActual(LocalDateTime.now());
            }

            if (isEstimatedRequired) {
                eventsRow.setEstimated(LocalDateTime.now());
            }

            eventsRow.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
            eventsRow.setIsPublicTrackingEvent(true);
            eventsRow.setEntityType(entityType);
            eventsRow.setEntityId(entityId);
            eventsRow.setEventCode(eventCode);
            eventsRow.setPlaceName(placeName);
            eventsRow.setPlaceDescription(placeDesc);

            updateEventDetails(eventsRow);

            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(eventsRow)
                                .prevData(null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(DBOperationType.CREATE.name()).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException e) {
                log.error(e.getMessage());
            }
            eventRepository.save(eventsRow);
        } catch (Exception e) {
            log.error("Error occured while trying to create runner event, Exception raised is: " + e);
        }
    }


    @Override
    @Transactional
    public void createEventForAirMessagingStatus(UUID guid, Long entityId, String entityType, String eventCode, String description, LocalDateTime estimated, LocalDateTime actual, String source, Integer tenantId, String status, LocalDateTime createdAt, LocalDateTime updatedAt) {
        Events events = new Events();
        events.setGuid(guid);
        events.setEntityId(entityId);
        events.setEntityType(entityType);
        events.setEventCode(eventCode);
        events.setDescription(description);
        events.setEstimated(estimated);
        events.setActual(actual);
        events.setSource(source);
        events.setTenantId(tenantId);
        events.setStatus(status);
        events.setCreatedAt(createdAt);
        events.setUpdatedAt(updatedAt);
        updateEventDetails(events);
        saveEventByEntityManager(events);
    }

    @Override
    @Transactional
    public void createEventForAirMessagingEvent(Events events) {
        updateEventDetails(events);
        saveEventByEntityManager(events);
    }

    private void saveEventByEntityManager(Events events) {
        log.info("Air-messaging : preparing event save native query");
        Query query = entityManager.createNativeQuery(
                        "insert into events (guid, entity_id, entity_type, event_code, description, source, tenant_id, " +
                                "pieces, total_pieces, weight, total_weight, is_partial, received_date, scheduled_date, " +
                                "created_at, updated_at, estimated, actual, place_name, place_description, longitude, latitude, " +
                                "consolidation_id, shipment_number, status) " +
                                "VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
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
                .setParameter(12, new TypedParameterValue(StandardBasicTypes.BOOLEAN, events.getIsPartial()))
                .setParameter(15, events.getCreatedAt())
                .setParameter(16, events.getUpdatedAt())
                .setParameter(19, events.getPlaceName())
                .setParameter(20, events.getPlaceDescription())
                .setParameter(21, events.getLongitude())
                .setParameter(22, events.getLatitude())
                .setParameter(24, events.getShipmentNumber())
                .setParameter(25, events.getStatus());

        if (events.getReceivedDate() != null) {
            query.setParameter(13, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getReceivedDate())));
        } else {
            query.setParameter(13, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if (events.getScheduledDate() != null) {
            query.setParameter(14, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getScheduledDate())));
        } else {
            query.setParameter(14, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if (events.getEstimated() != null) {
            query.setParameter(17, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getEstimated())));
        } else {
            query.setParameter(17, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if (events.getActual() != null) {
            query.setParameter(18, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, Timestamp.valueOf(events.getActual())));
        } else {
            query.setParameter(18, new TypedParameterValue(StandardBasicTypes.TIMESTAMP, null));
        }
        if (events.getConsolidationId() != null) {
            query.setParameter(23, events.getConsolidationId());
        } else {
            query.setParameter(23, new TypedParameterValue(StandardBasicTypes.BIG_INTEGER, null));
        }

        log.info("Air-messaging : executing event save native query");
        query.executeUpdate();
        log.info("Air-messaging : native query execution complete");
    }

    @Override
    public void updateEventDetails(Events event) {
        log.info("event-entity : populating consolidationId, shipmentNumber if available...");

        Long entityId = event.getEntityId();
        String entityType = event.getEntityType();

        if (Constants.SHIPMENT.equals(entityType)) {
            // Fetch shipment details and console mappings
            List<ShipmentDetails> shipmentDetailsList = shipmentDao.getShipmentNumberFromId(List.of(entityId));
            List<ConsoleShipmentMapping> consoleMappings = consoleShipmentMappingDao.findByShipmentId(entityId);

            // Set consolidationId if required
            shipmentDetailsList.stream().findFirst().ifPresent(shipmentDetails -> {
                if (ObjectUtils.isNotEmpty(consoleMappings) &&
                        shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                    event.setConsolidationId(consoleMappings.get(0).getConsolidationId());
                }
                // Set shipment number
                event.setShipmentNumber(shipmentDetails.getShipmentId());
                event.setEntityId(shipmentDetails.getId());
                if (event.getDirection() == null) {
                    event.setDirection(shipmentDetails.getDirection());
                }
            });

        } else if (Constants.CONSOLIDATION.equals(entityType)) {
            event.setConsolidationId(entityId);
        }

        updateUserFieldsInEvent(event, false);
    }

    @Override
    public void updateAllEventDetails(List<Events> events) {
        log.info("event-entity : populating consolidationId, shipmentNumber if available for multiple events...");

        Set<Long> shipmentIds = events.stream()
                .filter(event -> Constants.SHIPMENT.equals(event.getEntityType()))
                .map(Events::getEntityId)
                .collect(Collectors.toSet());

        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDao.getShipmentNumberFromId(shipmentIds.stream().toList()).stream()
                .collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));

        Map<Long, Long> shipmentToConsolidationMap = consoleShipmentMappingDao.findByShipmentIds(shipmentIds).stream()
                .collect(Collectors.toMap(
                        ConsoleShipmentMapping::getShipmentId,
                        ConsoleShipmentMapping::getConsolidationId,
                        (existing, replacement) -> existing  // Keep first occurrence
                ));

        Boolean automaticTransfer = isAutomaticTransfer();
        for (Events event : events) {
            if (Constants.SHIPMENT.equals(event.getEntityType())) {
                ShipmentDetails shipmentDetails = shipmentDetailsMap.get(event.getEntityId());

                if (shipmentDetails != null) {
                    event.setShipmentNumber(shipmentDetails.getShipmentId());
                    event.setEntityId(shipmentDetails.getId());

                    if (event.getDirection() == null) {
                        event.setDirection(shipmentDetails.getDirection());
                    }

                    Long consolidationId = shipmentToConsolidationMap.get(event.getEntityId());
                    if (consolidationId != null &&
                            shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                        event.setConsolidationId(consolidationId);
                    }
                }
            } else if (Constants.CONSOLIDATION.equals(event.getEntityType())) {
                event.setConsolidationId(event.getEntityId());
            }

            if (Boolean.FALSE.equals(automaticTransfer)) {
                updateUserFieldsInEvent(event, false);
            }
        }
    }


    /**
     * Determines if an event should be sent from shipment to consolidation based on the transport mode
     * and the event code associated with the shipment event.
     *
     * <p>This method checks if the provided transport mode has a corresponding set of event codes
     * that warrant sending the event to consolidation. The event codes for each mode are predefined
     * in a mapping for efficient lookup.</p>
     *
     * @param events        the event details containing the event code
     * @param transportMode the mode of transport, e.g., "SEA" or "AIR"
     * @return {@code true} if the event should be sent to consolidation, {@code false} otherwise
     */
    @Override
    public boolean shouldSendEventFromShipmentToConsolidation(Events events, String transportMode) {
        String eventCode = events.getEventCode().toUpperCase();

        // Map each transport mode to its corresponding event codes
        Map<String, Set<String>> eventCodesByMode = Map.of(
                TRANSPORT_MODE_SEA, Set.of(
                        EventConstants.BOCO, EventConstants.ECPK, EventConstants.FCGI, EventConstants.VSDP,
                        EventConstants.FHBL, EventConstants.FNMU, EventConstants.PRST, EventConstants.ARDP,
                        EventConstants.DOGE, EventConstants.DOTP, EventConstants.FUGO, EventConstants.CAFS,
                        EventConstants.PRDE, EventConstants.EMCR, EventConstants.ECCC, EventConstants.BLRS,
                        EventConstants.COOD, EventConstants.INGE, EventConstants.SISC, EventConstants.VGMS,
                        EventConstants.BBCK, EventConstants.DORC, EventConstants.DNMU
                ),
                TRANSPORT_MODE_AIR, Set.of(
                        EventConstants.BOCO, EventConstants.FLDR, EventConstants.PRST, EventConstants.DOGE,
                        EventConstants.DOTP, EventConstants.CAFS, EventConstants.PRDE, EventConstants.HAWB,
                        EventConstants.TRCF, EventConstants.TNFD, EventConstants.TRCS, EventConstants.ECCC,
                        EventConstants.BLRS, EventConstants.COOD, EventConstants.FNMU, EventConstants.INGE,
                        EventConstants.DNMU
                )
        );

        // Check if the transport mode exists in the map and if the event code is present in its set
        boolean shouldSend = eventCodesByMode.getOrDefault(transportMode.toUpperCase(), Set.of()).contains(eventCode);

        // Log the decision result
        log.info("Result of shouldSendEventFromShipmentToConsolidation for transportMode '{}' and eventCode '{}': {}",
                transportMode, eventCode, shouldSend);

        return shouldSend;
    }

    @Override
    public void updateFieldsForShipmentGeneratedEvents(List<Events> eventsList, ShipmentDetails shipmentDetails) {
        // update events with consolidation id with condition
        Set<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
        AtomicReference<Long> consolidationId = new AtomicReference<>(null);
        if (ObjectUtils.isNotEmpty(consolidationList)) {
            consolidationId.set(consolidationList.iterator().next().getId());
        }
        eventsList.stream()
                .map(vEvent -> updateUserFieldsInEvent(vEvent, false))
                .filter(event -> shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode()))
                .forEach(event -> event.setConsolidationId(consolidationId.get()));
    }

    @Override
    public Events updateUserFieldsInEvent(Events event, Boolean forceUpdate) {
        if (forceUpdate || ObjectUtils.isEmpty(event.getUserName())) {
            event.setUserName(Optional.ofNullable(UserContext.getUser()).map(UsersDto::getDisplayName).orElse(null));
        }
        if (forceUpdate || ObjectUtils.isEmpty(event.getUserEmail())) {
            event.setUserEmail(Optional.ofNullable(UserContext.getUser()).map(UsersDto::getEmail).orElse(null));
        }
        if (forceUpdate || ObjectUtils.isEmpty(event.getBranch())) {
            event.setBranch(Optional.ofNullable(UserContext.getUser()).map(UsersDto::getCode).orElse(null));
        }
        if (forceUpdate || ObjectUtils.isEmpty(event.getBranchName())) {
            event.setBranchName(Optional.ofNullable(UserContext.getUser()).map(UsersDto::getTenantDisplayName).orElse(null));
        }

        if (Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING.equals(event.getSource())) {
            event.setUserName(EventConstants.SYSTEM_GENERATED);
            event.setUserEmail(null);
            event.setBranch(null);
            event.setBranchName(null);
        }
        return event;
    }

    @Override
    @ExcludeTenantFilter
    public List<Events> updateEventsList(List<Events> shipmentEvents) {
        return eventRepository.saveAll(shipmentEvents);
    }

    private List<Events> filterStaleEvents(List<Events> requestList, List<Events> dbList) {
        // requestList : remove any clashing key from dbList if id of request event < id of dbList event
        Map<String, Events> dbEventsMap = dbList.stream().collect(Collectors.toMap(i -> commonUtils.getTrackingEventsUniqueKey(
                i.getEventCode(),
                i.getContainerNumber(),
                i.getShipmentNumber(),
                i.getSource(),
                i.getPlaceName()
        ), Function.identity(), (existing, replacement) -> existing.getId() > replacement.getId() ? existing : replacement));

        List<Events> newEventList = new ArrayList<>();
        List<Events> filteredEvents = new ArrayList<>(requestList.stream().filter(e -> {
            String uniqueKey = commonUtils.getTrackingEventsUniqueKey(
                    e.getEventCode(),
                    e.getContainerNumber(),
                    e.getShipmentNumber(),
                    e.getSource(),
                    e.getPlaceName()
            );
            if (dbEventsMap.containsKey(uniqueKey)) {
                Events dbEvent = dbEventsMap.get(uniqueKey);
                if (e.getId() < dbEvent.getId()) {
                    newEventList.add(dbEvent);
                    return false;
                }
            }

            return true;
        }).toList());
        filteredEvents.addAll(newEventList);
        return filteredEvents;
    }

    private boolean isAutomaticTransfer() {
        String automaticTransfer = MDC.get(LoggingConstants.AUTOMATIC_TRANSFER);
        return automaticTransfer != null && automaticTransfer.equals("true");
    }

}
