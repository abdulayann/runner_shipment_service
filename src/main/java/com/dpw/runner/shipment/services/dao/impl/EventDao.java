package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
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

    @Override
    public Events save(Events events) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(events), Constants.EVENTS, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(errors.toString());
        return eventRepository.save(events);
    }

    @Override
    public List<Events> saveAll(List<Events> containers) {
        List<Events> res = new ArrayList<>();
        for (Events req : containers) {
            req = save(req);
            res.add(req);
        }
        return res;
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
    public List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType) throws Exception {
        String responseMsg;
        List<Events> responseEvents = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entityId, entityType);
            Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
            Page<Events> events = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Events> hashMap = events.stream()
                    .collect(Collectors.toMap(Events::getId, Function.identity()));
            List<Events> eventsRequestList = new ArrayList<>();
            if (eventsList != null && eventsList.size() != 0) {
                for (Events request : eventsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    eventsRequestList.add(request);
                }
                responseEvents = saveEntityFromOtherEntity(eventsRequestList, entityId, entityType);
            }
            deleteEvents(hashMap);
            return responseEvents;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Events> saveEntityFromOtherEntity(List<Events> events, Long entityId, String entityType) {
        List<Events> res = new ArrayList<>();
        for (Events req : events) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Events> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Events is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteEvents(Map<Long, Events> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<Events> updateEntityFromOtherEntity(List<Events> eventsList, Long entityId, String entityType, List<Events> oldEntityList) throws Exception {
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
            deleteEvents(hashMap);
            return responseEvents;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public void autoGenerateEvents(CustomAutoEventRequest request) {
        try {
            if (!checkIfEventsRowExistsForEntityTypeAndEntityId(request)) {
                createAutomatedEventRequest(request.entityType, request.entityId, request.eventCode, request.isEstimatedRequired, request.isActualRequired);
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

    public void createAutomatedEventRequest(String entityType, long entityId, String eventCode, boolean isEstimatedRequired, boolean isActualRequired) {
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
            eventRepository.save(eventsRow);
        } catch (Exception e) {
            log.error("Error occured while trying to create runner event, Exception raised is: " + e);
        }
    }
}
