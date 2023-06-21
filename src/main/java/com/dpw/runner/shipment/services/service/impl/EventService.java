package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.MapperHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventDao;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class EventService implements IEventService {

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private MapperHelper mapperHelper;


    @Override
    public ResponseEntity<?> list(EventsRequest request) {
        List<Events> events = eventDao.findAll();
        List<EventsResponse> response = events.stream()
                .map(this::generateEventResponseFromEntity)
                .collect(Collectors.toList());
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @Override
    public ResponseEntity<EventsResponse> create(EventsRequest request) {
        Events event = generateEntityMappingFromRequest(request);
        event.setGuid(UUID.randomUUID());
        event = eventDao.save(event);
        return ResponseEntity.status(HttpStatus.OK).body(generateEventResponseFromEntity(event));
    }

    @Override
    public ResponseEntity<?> update(List<EventsRequest> request) {
        List<Events> events = request.stream()
                .map(this::generateEntityMappingFromRequest)
                .collect(Collectors.toList());
        eventDao.saveAll(events);
        return ResponseEntity.status(HttpStatus.OK).body(EventConstants.EVENT_UPDATE_SUCCESS);
    }

    @Override
    public ResponseEntity<?> delete(List<EventsRequest> request) {
        List<Events> events = request.stream()
                .map(this::generateEntityMappingFromRequest)
                .collect(Collectors.toList());
        eventDao.deleteAll(events);
        return ResponseEntity.status(HttpStatus.OK).body(EventConstants.EVENT_DELETE_SUCCESS);
    }


    private Events generateEntityMappingFromRequest(EventsRequest request){
        return mapperHelper.getObjectMapper().convertValue(request, Events.class);
    }

    private EventsResponse generateEventResponseFromEntity(Events event){
        return mapperHelper.getObjectMapper().convertValue(event,EventsResponse.class);
    }

}
