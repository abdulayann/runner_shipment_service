package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

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

    public List<Events> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<Events> responseEvents = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
            Page<Events> events = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Events> hashMap = events.stream()
                    .collect(Collectors.toMap(Events::getId, Function.identity()));
            List<EventsRequest> eventsRequestList = new ArrayList<>();
            List<EventsRequest> requestList = (List<EventsRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (EventsRequest request : requestList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    eventsRequestList.add(request);
                }
                responseEvents = saveEvents(eventsRequestList);
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

    private List<Events> saveEvents(List<EventsRequest> events) {
        List<Events> res = new ArrayList<>();
        for(EventsRequest req : events){
            Events saveEntity = convertToClass(req, Events.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<Events> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
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
