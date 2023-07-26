package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;

@Repository
@Slf4j
public class EventDao implements IEventDao {
    @Autowired
    private IEventRepository eventRepository;

    @Override
    public Events save(Events events) {
        return eventRepository.save(events);
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
}
