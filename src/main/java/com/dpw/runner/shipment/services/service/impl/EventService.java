package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventDao;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class EventService implements IEventService {

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        EventsRequest request = null;
        request = (EventsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Event create");
        }
        Events bookingCarriage = convertRequestToEntity(request);
        try {
            bookingCarriage = eventDao.save(bookingCarriage);
            log.info("Event Details created successfully for Id {}", bookingCarriage.getId());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(bookingCarriage));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        EventsRequest request = (EventsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Event update");
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Event update");
        }
        long id = request.getId();
        Optional<Events> oldEntity = eventDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Event is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Events events = convertRequestToEntity(request);
        events.setId(oldEntity.get().getId());
        try {
            events = eventDao.save(events);
            log.info("Updated the event details for Id {} ", id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(events));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Event list");
            }
            // construct specifications for filter request
            Pair<Specification<Events>, Pageable> tuple = fetchData(request, Events.class);
            Page<Events> bookingCarriagePage = eventDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Event list retrieved successfully");
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(bookingCarriagePage.getContent()),
                    bookingCarriagePage.getTotalPages(),
                    bookingCarriagePage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Event async list");
            }
            // construct specifications for filter request
            Pair<Specification<Events>, Pageable> tuple = fetchData(request, Events.class);
            Page<Events> eventsPage = eventDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Event async list retrieved successfully");
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

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Event delete");
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Event delete");
            }
            long id = request.getId();

            Optional<Events> events = eventDao.findById(id);
            if (!events.isPresent()) {
                log.debug("Event is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            eventDao.delete(events.get());
            log.info("Deleted Event for Id {}", id);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Event retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Event retrieve");
            }
            long id = request.getId();
            Optional<Events> events = eventDao.findById(id);
            if (events.isEmpty()) {
                log.debug("Event is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Event details fetched successfully for Id {}", id);
            EventsResponse response = (EventsResponse) convertEntityToDto(events.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Events convertRequestToEntity(EventsRequest request) {
        return modelMapper.map(request, Events.class);
    }

    private EventsResponse convertEntityToDto(Events event) {
        return modelMapper.map(event, EventsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Events> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(event -> {
            responseList.add(convertEntityToDto(event));
        });
        return responseList;
    }

}
