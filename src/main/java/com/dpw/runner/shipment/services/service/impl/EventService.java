package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import javax.json.JsonString;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class EventService implements IEventService {

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDao;

    @Autowired
    private RestTemplate restTemplate;
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    @Value("${v1service.url.base}${v1.service.url.trackEventDetails}")
    private String TRACK_EVENT_DETAILS_URL;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        EventsRequest request = null;
        request = (EventsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Event create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Events event = convertRequestToEntity(request);
        try {
            event = eventDao.save(event);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(event)
                            .prevData(null)
                            .parent(Events.class.getSimpleName())
                            .parentId(event.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Event Details created successfully for Id {} with Request Id {}", event.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(event));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        EventsRequest request = (EventsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Event update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Event update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Events> oldEntity = eventDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Event is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Events events = convertRequestToEntity(request);
        events.setId(oldEntity.get().getId());
        if(events.getGuid() != null && !oldEntity.get().getGuid().equals(events.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            events = eventDao.save(events);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(events)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Events.class))
                            .parent(Events.class.getSimpleName())
                            .parentId(events.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the event details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
            if (request == null) {
                log.error("Request is empty for Event list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Events>, Pageable> tuple = fetchData(request, Events.class);
            Page<Events> bookingCarriagePage = eventDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Event list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
            if (request == null) {
                log.error("Request is empty for Event async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Events>, Pageable> tuple = fetchData(request, Events.class);
            Page<Events> eventsPage = eventDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Event async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
            if (request == null) {
                log.debug("Request is empty for Event delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Event delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<Events> events = eventDao.findById(id);
            if (!events.isPresent()) {
                log.debug("Event is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(events.get());
            eventDao.delete(events.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Events.class))
                            .parent(Events.class.getSimpleName())
                            .parentId(events.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted Event for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
            if (request == null) {
                log.error("Request is empty for Event retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Event retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Events> events = eventDao.findById(id);
            if (events.isEmpty()) {
                log.debug("Event is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Event details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            EventsResponse response = (EventsResponse) convertEntityToDto(events.get());

            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
            return ResponseHelper.buildSuccessResponse(response);
            else{
                return  ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
            }

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);

            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Events convertRequestToEntity(EventsRequest request) {
        return jsonHelper.convertValue(request, Events.class);
    }

    private EventsResponse convertEntityToDto(Events event) {
        return jsonHelper.convertValue(event, EventsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Events> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(event -> {
            responseList.add(convertEntityToDto(event));
        });
        return responseList;
    }

    @Override
    public ResponseEntity<?> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
        EventsRequestV2 eventsRequestV2 = (EventsRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.EVENTS, StringUtility.convertToString(eventsRequestV2.getGuid()), eventsRequestV2);
            }
            Optional<Events> existingEvent = eventDao.findByGuid(eventsRequestV2.getGuid());
            Events events = modelMapper.map(eventsRequestV2, Events.class);
            if (existingEvent != null && existingEvent.isPresent()) {
                events.setId(existingEvent.get().getId());
                events.setEntityId(existingEvent.get().getEntityId());
                events.setEntityType(existingEvent.get().getEntityType());
            } else {
                if (eventsRequestV2.getEntityType() != null
                        && eventsRequestV2.getEntityType().equals("Shipment")
                        && eventsRequestV2.getShipmentGuid() != null) {
                    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(eventsRequestV2.getShipmentGuid());
                    if (shipmentDetails.isPresent()) {
                        events.setEntityId(shipmentDetails.get().getId());
                        events.setEntityType(eventsRequestV2.getEntityType());
                    }
                }
                if (eventsRequestV2.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findByGuid(eventsRequestV2.getConsolidationGuid());
                    if (consolidationDetails.isPresent()) {
                        events.setEntityId(consolidationDetails.get().getId());
                        events.setEntityType(eventsRequestV2.getEntityType());
                    }
                }
            }
            events = eventDao.save(events);
            EventsResponse response = objectMapper.convertValue(events, EventsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

  public ResponseEntity<?> trackEvents(Long id) {
    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
    if (shipmentDetails.isEmpty()) {
      log.debug(
          "No Shipment present for the current Event",
              id,
          LoggerHelper.getRequestIdFromMDC());
      throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }
    String shipmentId = shipmentDetails.get().getShipmentId();
    TrackingRequest trackingRequest = TrackingRequest.builder().referenceNumber(shipmentId).build();

    HttpEntity<V1DataResponse> entity = new HttpEntity(trackingRequest, V1AuthHelper.getHeaders());
    V1DataResponse response = new V1DataResponse();
    try {
      var v1response =
          this.restTemplate.postForEntity(TRACK_EVENT_DETAILS_URL, entity, V1DataResponse.class);
      response = v1response.getBody();
    } catch (HttpClientErrorException | HttpServerErrorException ex) {
      throw new V1ServiceException(
          jsonHelper
              .readFromJson(ex.getResponseBodyAsString(), V1ErrorResponse.class)
              .getError()
              .getMessage());
    }
    List<EventsResponse> res = new ArrayList<>();
    if (response != null && response.getEntities() != null) {
      List<EventsRequestV2> responseEvents =
          jsonHelper.convertValue(response.getEntities(), new TypeReference<>() {});
      for (var i : responseEvents) {
        EventsResponse eventsResponse = modelMapper.map(i, EventsResponse.class);
        eventsResponse.setShipmentId(id);
        res.add(eventsResponse);
      }
    }

    return ResponseHelper.buildSuccessResponse(res);
  }
}
