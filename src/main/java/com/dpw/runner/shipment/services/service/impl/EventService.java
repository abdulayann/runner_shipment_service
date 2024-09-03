package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDumpDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.EventsDump;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

@Slf4j
@Service
public class EventService implements IEventService {

    private IEventDao eventDao;
    private JsonHelper jsonHelper;
    private IAuditLogService auditLogService;
    private ObjectMapper objectMapper;
    private ModelMapper modelMapper;
    private IShipmentDao shipmentDao;
    private IShipmentSync shipmentSync;
    private IConsolidationDetailsDao consolidationDao;
    private SyncConfig syncConfig;
    private IDateTimeChangeLogService dateTimeChangeLogService;

    @Value("${v1service.url.base}${v1.service.url.trackEventDetails}")
    private String trackEventDetailsUrl;

    private PartialFetchUtils partialFetchUtils;
    private ITrackingServiceAdapter trackingServiceAdapter;
    private IEventDumpDao eventDumpDao;

    @Autowired
    public EventService(IEventDao eventDao, JsonHelper jsonHelper, IAuditLogService auditLogService, ObjectMapper objectMapper, ModelMapper modelMapper, IShipmentDao shipmentDao, IShipmentSync shipmentSync, IConsolidationDetailsDao consolidationDao, SyncConfig syncConfig, IDateTimeChangeLogService dateTimeChangeLogService, PartialFetchUtils partialFetchUtils, ITrackingServiceAdapter trackingServiceAdapter, IEventDumpDao eventDumpDao) {
        this.eventDao = eventDao;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
        this.objectMapper = objectMapper;
        this.modelMapper = modelMapper;
        this.shipmentDao = shipmentDao;
        this.shipmentSync = shipmentSync;
        this.consolidationDao = consolidationDao;
        this.syncConfig = syncConfig;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
        this.partialFetchUtils = partialFetchUtils;
        this.trackingServiceAdapter = trackingServiceAdapter;
        this.eventDumpDao = eventDumpDao;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        EventsRequest request = (EventsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Event update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(EventConstants.EMPTY_REQUEST_ERROR);
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Event update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(EventConstants.EMPTY_REQUEST_ID_ERROR);
        }
        long id = request.getId();
        Optional<Events> oldEntity = eventDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(EventConstants.EVENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
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

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Event list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException(EventConstants.EMPTY_REQUEST_ERROR);
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
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Event async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException(EventConstants.EMPTY_REQUEST_ERROR);
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

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
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
                log.debug(EventConstants.EVENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
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
                log.debug(EventConstants.EVENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Event details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            EventsResponse response = convertEntityToDto(events.get());

            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else{
                return  ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
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
        lst.forEach(event -> responseList.add(convertEntityToDto(event)));
        return responseList;
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        EventsRequestV2 eventsRequestV2 = (EventsRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return ResponseHelper.buildSuccessResponse();
            }
            Optional<Events> existingEvent = eventDao.findByGuid(eventsRequestV2.getGuid());
            Events events = modelMapper.map(eventsRequestV2, Events.class);
            if (existingEvent != null && existingEvent.isPresent()) {
                events.setId(existingEvent.get().getId());
            }
            if (eventsRequestV2.getShipmentGuid() != null) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(eventsRequestV2.getShipmentGuid());
                if (shipmentDetails.isPresent()) {
                    events.setEntityId(shipmentDetails.get().getId());
                    events.setEntityType(Constants.SHIPMENT);
                }
            }
            if (eventsRequestV2.getConsolidationGuid() != null) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findByGuid(eventsRequestV2.getConsolidationGuid());
                if (consolidationDetails.isPresent()) {
                    events.setEntityId(consolidationDetails.get().getId());
                    events.setEntityType(Constants.CONSOLIDATION);
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

    public ResponseEntity<IRunnerResponse> trackEvents(Optional<Long> shipmentId, Optional<Long> consolidationId) throws RunnerException {
        Optional<ShipmentDetails> optionalShipmentDetails = Optional.empty();
        Optional<ConsolidationDetails> optionalConsolidationDetails = Optional.empty();
        String referenceNumber = null;
        Long entityId = null;
        String entityType = null;
        if (shipmentId.isPresent()) {
            optionalShipmentDetails = shipmentDao.findById(shipmentId.get());
            if (optionalShipmentDetails.isEmpty()) {
                log.debug(
                    "No Shipment present for the current Event ",
                    LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            referenceNumber = optionalShipmentDetails.get().getShipmentId();
            entityId = shipmentId.get();
            entityType = Constants.SHIPMENT;
        } else if (consolidationId.isPresent()) {
            optionalConsolidationDetails = consolidationDao.findById(consolidationId.get());
            if (optionalConsolidationDetails.isEmpty()) {
                log.debug(
                    "No Consolidation present for the current Event",
                    LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            referenceNumber = optionalConsolidationDetails.get().getConsolidationNumber();
            entityId = consolidationId.get();
            entityType = Constants.CONSOLIDATION;
        } else {
            throw new RunnerException("Both shipmentId and consolidationId are empty !");
        }

        TrackingEventsResponse trackingEventsResponse = null;

        try {
            trackingEventsResponse = trackingServiceAdapter.getTrackingEventsResponse(referenceNumber);
        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }
        List<EventsResponse> res = new ArrayList<>();
        boolean isEmptyContainerReturnedEvent = false;

        if (trackingEventsResponse != null) {
            if (trackingEventsResponse.getEventsList() != null) {
                ShipmentDetails shipmentDetails = optionalShipmentDetails.orElse(null);
                for (var trackingEvent : trackingEventsResponse.getEventsList()) {
                    EventsResponse eventsResponse = modelMapper.map(trackingEvent, EventsResponse.class);
                    shipmentId.ifPresent(eventsResponse::setShipmentId);
                    res.add(eventsResponse);
                    if (Objects.equals(trackingEvent.getEventCode(), EventConstants.EMCR)) {
                        isEmptyContainerReturnedEvent = true;
                    }
                }
                saveTrackingEventsToEventsDump(jsonHelper.convertValueToList(res, Events.class), entityId, entityType);
                saveTrackingEventsToEvents(jsonHelper.convertValueToList(res, Events.class), entityId, entityType, shipmentDetails);
            }

            if (optionalShipmentDetails.isPresent()) {
                ShipmentDetails shipment = optionalShipmentDetails.get();
                boolean isShipmentUpdateRequired = updateShipmentDetails(shipment, trackingEventsResponse, isEmptyContainerReturnedEvent);

                if (isShipmentUpdateRequired) {
                    shipmentDao.save(shipment, false);
                    try {
                        shipmentSync.sync(shipment, null, null, UUID.randomUUID().toString(), false);
                    } catch (Exception e) {
                        log.error("Error performing sync on shipment entity, {}", e);
                    }
                }
            }
        }

        return ResponseHelper.buildSuccessResponse(res);
    }

    private void saveTrackingEventsToEvents(List<Events> trackingEvents, Long entityId, String entityType, ShipmentDetails shipmentDetails) {
        if (ObjectUtils.isEmpty(trackingEvents) || shipmentDetails == null) {
            return;
        }

        // Construct list criteria and fetch existing events based on entity
        var listCriteria = CommonUtils.constructListRequestFromEntityId(entityId, entityType);
        Pair<Specification<Events>, Pageable> pair = fetchData(listCriteria, Events.class);
        List<Events> existingEvents = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();

        // Create a map of existing events by their event code
        Map<String, Events> existingEventsMap = existingEvents.stream()
                .collect(Collectors.toMap(Events::getEventCode, Function.identity()));

        // Filter, map, and collect relevant tracking events based on custom logic
        List<Events> updatedEvents = trackingEvents.stream()
                .filter(trackingEvent -> shouldProcessEvent(trackingEvent, shipmentDetails))
                .map(trackingEvent -> mapToUpdatedEvent(trackingEvent, existingEventsMap, entityId, entityType))
                .toList();

        // Save all the updated events
        eventDao.saveAll(updatedEvents);
    }

    private boolean shouldProcessEvent(Events event, ShipmentDetails shipmentDetails) {
        if (event == null || shipmentDetails == null) {
            return false;
        }

        String entityType = event.getEntityType();
        String locationRole = event.getLocationRole();
        String shipmentType = shipmentDetails.getShipmentType();
        String transportMode = shipmentDetails.getTransportMode();

        if (isGateInWithContainerEmpty(entityType, locationRole, shipmentType)) {
            return true;
        }

        if (isGateInWithContainerFull(entityType, locationRole, shipmentType)) {
            return true;
        }

        if (isVesselDepartureOrArrival(entityType, locationRole, shipmentType, transportMode)) {
            return true;
        }

        return isGateOutWithContainerFull(entityType, locationRole, shipmentType);
    }

    private boolean isGateInWithContainerEmpty(String entityType, String locationRole, String shipmentType) {
        return EventConstants.GateInWithContainerEmpty.equalsIgnoreCase(safeString(entityType))
                && (safeString(locationRole).startsWith("origin") || safeString(locationRole).startsWith("destination"))
                && isFclShipment(shipmentType);
    }

    private boolean isGateInWithContainerFull(String entityType, String locationRole, String shipmentType) {
        return EventConstants.GateInWithContainerFull.equalsIgnoreCase(safeString(entityType))
                && "originPort".equalsIgnoreCase(safeString(locationRole))
                && isFclShipment(shipmentType);
    }

    private boolean isVesselDepartureOrArrival(String entityType, String locationRole, String shipmentType, String transportMode) {
        boolean isDeparture = EventConstants.VesselDepartureWithContainer.equalsIgnoreCase(safeString(entityType))
                && "originPort".equalsIgnoreCase(safeString(locationRole));
        boolean isArrival = EventConstants.VesselArrivalWithContainer.equalsIgnoreCase(safeString(entityType))
                && "destinationPort".equalsIgnoreCase(safeString(locationRole));

        return (isDeparture || isArrival) &&
                (isFclShipment(shipmentType) || isLclShipment(shipmentType) || isAirShipment(transportMode));
    }

    private boolean isGateOutWithContainerFull(String entityType, String locationRole, String shipmentType) {
        return EventConstants.GateOutWithContainerFull.equalsIgnoreCase(safeString(entityType))
                && "destinationPort".equalsIgnoreCase(safeString(locationRole))
                && isFclShipment(shipmentType);
    }

    private boolean isFclShipment(String shipmentType) {
        return Constants.CARGO_TYPE_FCL.equalsIgnoreCase(safeString(shipmentType));
    }

    private boolean isLclShipment(String shipmentType) {
        return Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(safeString(shipmentType));
    }

    private boolean isAirShipment(String transportMode) {
        return Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(safeString(transportMode));
    }

    private String safeString(String value) {
        return value != null ? value : "";
    }

    private Events mapToUpdatedEvent(Events trackingEvent, Map<String, Events> existingEventsMap,
            Long entityId, String entityType) {
        Events event = modelMapper.map(trackingEvent, Events.class);
        Events existingEvent = existingEventsMap.get(trackingEvent.getEventCode());

        if (existingEvent != null) {
            // Update ID and GUID if the event already exists
            event.setId(existingEvent.getId());
            event.setGuid(existingEvent.getGuid());
        }

        // Set entity-related details and source
        event.setEntityId(entityId);
        event.setEntityType(entityType);
        event.setSource(Constants.CARGOES_TRACKING);

        return event;
    }

    private boolean updateShipmentDetails(ShipmentDetails shipment, TrackingEventsResponse trackingEventsResponse, boolean isEmptyContainerReturnedEvent) {
        boolean isShipmentUpdateRequired = false;

        if (trackingEventsResponse != null && (trackingEventsResponse.getShipmentAta() != null || trackingEventsResponse.getShipmentAtd() != null)) {
                CarrierDetails carrierDetails = shipment.getCarrierDetails() != null ? shipment.getCarrierDetails() : new CarrierDetails();

                if (trackingEventsResponse.getShipmentAta() != null) {
                    carrierDetails.setAta(trackingEventsResponse.getShipmentAta());
                }
                if (trackingEventsResponse.getShipmentAtd() != null) {
                    carrierDetails.setAtd(trackingEventsResponse.getShipmentAtd());
                }
                isShipmentUpdateRequired = true;
        }
        if (isEmptyContainerReturnedEvent && shipment.getAdditionalDetails() != null &&
                !Boolean.TRUE.equals(shipment.getAdditionalDetails().getEmptyContainerReturned()) &&
                Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                && TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())) {
                shipment.getAdditionalDetails().setEmptyContainerReturned(true);
                isShipmentUpdateRequired = true;
        }
        return isShipmentUpdateRequired;
    }

    @Override
    public void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings) {
        if (events != null && events.size() > 0) {
            Events lastEvent = events.get(events.size() - 1);
            if (tenantSettings.getIsAtdAtaAutoPopulateEnabled() != null && tenantSettings.getIsAtdAtaAutoPopulateEnabled().equals(true)) {
                if (lastEvent.getActual() != null) {
                    shipmentDetails.setCarrierDetails(shipmentDetails.getCarrierDetails() == null ? new CarrierDetails() : shipmentDetails.getCarrierDetails());
                    if (EventConstants.ATA_EVENT_CODES.contains(lastEvent.getEventCode())) {
                        shipmentDetails.getCarrierDetails().setAta(lastEvent.getActual());
                        createDateTimeChangeLog(DateType.ATA, lastEvent.getActual(), shipmentDetails.getId());
                    }
                    if (EventConstants.ATD_EVENT_CODES.contains(lastEvent.getEventCode())) {
                        shipmentDetails.getCarrierDetails().setAtd(lastEvent.getActual());
                        createDateTimeChangeLog(DateType.ATD, lastEvent.getActual(), shipmentDetails.getId());
                    }
                }
            }
        }
    }

    private void createDateTimeChangeLog(DateType dateType, LocalDateTime localDateTime, Long shipmentId) {
        dateTimeChangeLogService.saveDateTimeChangeLog(dateType, localDateTime, shipmentId, DateTimeChangeLogConstants.EVENT_SOURCE);
    }

    /**
     * @param trackingEvents
     * @param entityId
     * @param entityType
     * save tracking response events into separate table and update if any existing event that's already saved
     */
    private void saveTrackingEventsToEventsDump(List<Events> trackingEvents, Long entityId, String entityType) {
        if (trackingEvents == null || trackingEvents.isEmpty())
            return;

        var listCriteria = CommonUtils.constructListRequestFromEntityId(entityId, entityType);
        Pair<Specification<EventsDump>, Pageable> pair = fetchData(listCriteria, EventsDump.class);
        Page<EventsDump> eventsDumpPage = eventDumpDao.findAll(pair.getLeft(), pair.getRight());

        Map<String, EventsDump> existingEvents = eventsDumpPage.getContent().stream().collect(
            Collectors.toMap(EventsDump::getEventCode, Function.identity(), (oldVal, newVal) -> newVal));

        List<EventsDump> updatedEvents = new ArrayList<>();
        trackingEvents.forEach(e -> {
            EventsDump event = modelMapper.map(e, EventsDump.class);
            if (existingEvents.containsKey(e.getEventCode())) {
                event.setId(existingEvents.get(e.getEventCode()).getId());
                event.setGuid(existingEvents.get(e.getEventCode()).getGuid());
            }
            event.setEntityId(entityId);
            event.setEntityType(entityType);
            event.setSource(Constants.FLOW);
            updatedEvents.add(event);
        });

        eventDumpDao.saveAll(updatedEvents);
    }

}
