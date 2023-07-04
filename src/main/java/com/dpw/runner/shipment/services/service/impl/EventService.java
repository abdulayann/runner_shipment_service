package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventDao;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class EventService implements IEventService {

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private ModelMapper modelMapper;


    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        List<Events> events = eventDao.findAll();
        List<IRunnerResponse> response = events.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
        return ResponseHelper.buildListSuccessResponse(response);
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<Events>, Pageable> tuple = fetchData(request, Events.class);
            Page<Events> eventsPage  = eventDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(eventsPage.getContent()),
                                    eventsPage.getTotalPages(),
                                    eventsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<EventsResponse> create(EventsRequest request) {
        Events event = generateEntityMappingFromRequest(request);
        event.setGuid(UUID.randomUUID());
        event = eventDao.save(event);
        return ResponseEntity.status(HttpStatus.OK).body(convertEntityToDto(event));
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
        return modelMapper.map(request, Events.class);
    }

    private EventsResponse convertEntityToDto(Events event){
        return modelMapper.map(event,EventsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Events> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(event -> {
            responseList.add(convertEntityToDto(event));
        });
        return responseList;
    }

}
