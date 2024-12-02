package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
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
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.ContainerBase;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Place;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.EventsDump;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import java.time.LocalDateTime;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.core.parameters.P;
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
    private PartialFetchUtils partialFetchUtils;
    private ITrackingServiceAdapter trackingServiceAdapter;
    private IEventDumpDao eventDumpDao;
    private IV1Service v1Service;
    private CommonUtils commonUtils;

    @Autowired
    public EventService(IEventDao eventDao, JsonHelper jsonHelper, IAuditLogService auditLogService, ObjectMapper objectMapper, ModelMapper modelMapper, IShipmentDao shipmentDao
            , IShipmentSync shipmentSync, IConsolidationDetailsDao consolidationDao, SyncConfig syncConfig, IDateTimeChangeLogService dateTimeChangeLogService,
            PartialFetchUtils partialFetchUtils, ITrackingServiceAdapter trackingServiceAdapter, IEventDumpDao eventDumpDao, IV1Service v1Service, CommonUtils commonUtils) {
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
        this.v1Service = v1Service;
        this.commonUtils = commonUtils;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        EventsRequest request = null;
        request = (EventsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Event create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Empty request received");
        }
        Events event = convertRequestToEntity(request);
        try {
            saveEvent(request);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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

            saveEvent(request);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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

    public ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException {
        var shipmentId = request.getShipmentId();
        var consolidationId = request.getConsolidationId();
        Optional<ShipmentDetails> optionalShipmentDetails = Optional.empty();
        Optional<ConsolidationDetails> optionalConsolidationDetails = Optional.empty();
        String referenceNumber = null;
        Long entityId = null;
        String entityType = null;
        if (shipmentId != null) {
            optionalShipmentDetails = shipmentDao.findById(shipmentId);
            if (optionalShipmentDetails.isEmpty()) {
                log.debug(
                    "No Shipment present for the current Event ",
                    LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            referenceNumber = optionalShipmentDetails.get().getShipmentId();
            entityId = shipmentId;
            entityType = Constants.SHIPMENT;

            Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap = getIdentifier2ToLocationRoleMap();

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
                        EventsResponse eventsResponse = getEventsResponse(Optional.ofNullable(shipmentId), trackingEvent);
                        res.add(eventsResponse);
                    }
                    saveTrackingEventsToEventsDump(jsonHelper.convertValueToList(res, Events.class), entityId, entityType);
                    List<Events> updatedEventsList = saveTrackingEventsToEvents(jsonHelper.convertValueToList(res, Events.class), entityId, entityType, shipmentDetails, identifier2ToLocationRoleMap);
                    res = jsonHelper.convertValueToList(updatedEventsList, EventsResponse.class);
                }

                for (EventsResponse eventsResponse : res) {
                    if (Objects.equals(eventsResponse.getEventCode(), EventConstants.EMCR)) {
                        isEmptyContainerReturnedEvent = true;
                        break;
                    }
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

        } else if (consolidationId != null) {
            optionalConsolidationDetails = consolidationDao.findById(consolidationId);
            if (optionalConsolidationDetails.isEmpty()) {
                log.debug(
                    "No Consolidation present for the current Event",
                    LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            entityId = consolidationId;
            entityType = Constants.CONSOLIDATION;
            commonUtils.setInterBranchContextForHub();
        } else {
            throw new RunnerException("Both shipmentId and consolidationId are empty !");
        }


        // Bring all Events saved from DB
        List<EventsResponse> allEventResponses;
        ListCommonRequest listRequest = jsonHelper.convertValue(request, ListCommonRequest.class);

        if (entityType.equalsIgnoreCase(Constants.SHIPMENT)) {
            listRequest = CommonUtils.andCriteria("entityId", entityId, "=", listRequest);
            listRequest = CommonUtils.andCriteria("entityType", entityType, "=", listRequest);
            Pair<Specification<Events>, Pageable> pair = fetchData(listRequest, Events.class);
            List<Events> allEvents = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();
            allEventResponses = jsonHelper.convertValueToList(allEvents, EventsResponse.class);
        }
        else {
            log.info("Creating criteria for fetching consolidation events");
            listRequest = CommonUtils.andCriteria("consolidationId", consolidationId, "=", listRequest);
            Pair<Specification<Events>, Pageable> pair = fetchData(listRequest, Events.class);
            Page<Events> consolEventsPage = eventDao.findAll(pair.getLeft(), pair.getRight());
            log.info("Received {} events for consolidation with id {}", consolEventsPage.getTotalElements(), consolidationId);
            allEventResponses = jsonHelper.convertValueToList(consolEventsPage.getContent(), EventsResponse.class);
        }

        // set MasterData
        setEventCodesMasterData(
                allEventResponses,
                EventsResponse::getEventCode,
                EventsResponse::setDescription
        );

        return ResponseHelper.buildSuccessResponse(allEventResponses);
    }

    @NotNull
    private EventsResponse getEventsResponse(Optional<Long> shipmentId,
            Events trackingEvent) {

        EventsResponse eventsResponse = modelMapper.map(trackingEvent, EventsResponse.class);

        // Sets the shipment ID in the EventsResponse if the shipmentId is present.
        shipmentId.ifPresent(eventsResponse::setShipmentId);

        // Returns the fully populated EventsResponse object.
        return eventsResponse;
    }

    private String convertLocationRoleWRTMasterData(Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap, String locationRoleIdentifier2) {
        // Return the updated location role if found, otherwise return the original location role
        return Optional.ofNullable(locationRoleIdentifier2)
                .map(identifier2ToLocationRoleMap::get)
                .map(EntityTransferMasterLists::getItemValue)
                .orElse(locationRoleIdentifier2);
    }

    @NotNull
    private Map<String, EntityTransferMasterLists> getIdentifier2ToLocationRoleMap() {
        try {
            // Define criteria for fetching location role master data
            List<Object> locationRoleMasterDataCriteria = Arrays.asList(
                    List.of(MasterDataConstants.ITEM_TYPE),
                    "=",
                    MasterDataType.LOCATION_ROLE.getId()
            );

            // Fetch location role data using the defined criteria
            V1DataResponse locationRoleV1DataResponse = getLocationRoleV1DataResponse(locationRoleMasterDataCriteria);

            // Convert the response entities to a list of EntityTransferMasterLists
            List<EntityTransferMasterLists> locationRoleMasterDataList = Optional.ofNullable(locationRoleV1DataResponse)
                    .map(response -> jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class))
                    .orElse(Collections.emptyList());

            // Convert the list to a map with identifier2 as the key
            return locationRoleMasterDataList.stream().collect(Collectors.toMap(
                    EntityTransferMasterLists::getIdentifier2,
                    Function.identity(),
                    (existing, replacement) -> existing // Handle duplicate keys by keeping the existing entry
            ));
        } catch (Exception e) {
            // Log the error message for debugging purposes
            log.error("Error fetching or processing location role master data: {}", e.getMessage(), e);

            // Return an empty map if an error occurs
            return Collections.emptyMap();
        }
    }

    @Nullable
    private V1DataResponse getLocationRoleV1DataResponse(List<Object> locationRoleMasterDataCriteria) {
        V1DataResponse locationRoleV1DataResponse = null;
        try {
            locationRoleV1DataResponse = v1Service.fetchMasterData(CommonV1ListRequest.builder()
                    .criteriaRequests(locationRoleMasterDataCriteria).build());
        } catch (Exception e) {
            log.error("Call for masterdata failed.{}", e.getMessage());
        }
        return locationRoleV1DataResponse;
    }

    private <T> void setEventCodesMasterData(List<T> eventsList, Function<T, String> getEventCode, BiConsumer<T, String> setDescription) {
        try {
            if(Objects.isNull(eventsList) || eventsList.isEmpty())
                return;
            // Define criteria for fetching event codes master data
            List<String> eventCodes = eventsList.stream().map(getEventCode).toList();
            List<Object> subCriteria1 = Arrays.asList(
                    List.of(MasterDataConstants.ITEM_TYPE),
                    "=",
                    MasterDataType.ORDER_EVENTS.getId()
            );
            List<Object> subCriteria2 = Arrays.asList(
                    List.of(MasterDataConstants.ITEM_VALUE),
                    "IN",
                    List.of(eventCodes)
            );
            var eventCodeMasterDataCriteria = List.of(subCriteria1, "and", subCriteria2);

            // Fetch master data using the defined criteria
            V1DataResponse masterDataV1Response = v1Service.fetchMasterData(CommonV1ListRequest.builder()
                    .criteriaRequests(eventCodeMasterDataCriteria).build());

            // Convert the response entities to a list of EntityTransferMasterLists
            List<EntityTransferMasterLists> entityTransferMasterLists =
                    Optional.ofNullable(masterDataV1Response)
                            .map(v1DataResponse -> jsonHelper.convertValueToList(masterDataV1Response.entities, EntityTransferMasterLists.class))
                            .orElse(Collections.emptyList());

            // Convert the list to a map with identifier2 as the key
            Map<String, EntityTransferMasterLists> eventCodeMap = entityTransferMasterLists.stream()
                    .collect(Collectors.toMap(
                            EntityTransferMasterLists::getItemValue,
                            Function.identity(),
                            (existing, replacement) -> existing // Handle duplicate keys by keeping the existing entry
                    ));

            // Set description for each event in the list
            eventsList.forEach(event ->
                    Optional.ofNullable(eventCodeMap.get(getEventCode.apply(event)))
                            .ifPresentOrElse(
                                    masterList -> setDescription.accept(event, masterList.getItemDescription()),
                                    () -> log.warn("No mapping found for event code: {}", getEventCode.apply(event))
                            )
            );
        } catch (Exception e) {
            // Log the error message for debugging purposes
            log.error("Error fetching or processing event codes master data: {}", e.getMessage(), e);
        }
    }

    /**
     * Processes and saves tracking events to the database.
     *
     * This method converts the provided list of tracking events, fetches existing events from the database,
     * and updates or creates new events based on custom logic. It logs relevant information at various stages
     * of processing for debugging and tracking purposes.
     *
     * @param trackingEvents the list of original tracking events to process
     * @param entityId the ID of the entity associated with the events
     * @param entityType the type of the entity associated with the events
     * @param shipmentDetails the shipment details used for filtering and processing events
     * @param identifier2ToLocationRoleMap a map of identifiers to location roles used for mapping
     * @return a list of saved events
     */
    private List<Events> saveTrackingEventsToEvents(List<Events> trackingEvents, Long entityId, String entityType,
            ShipmentDetails shipmentDetails, Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap) {

        if (ObjectUtils.isEmpty(trackingEvents) || shipmentDetails == null) {
            log.warn("Original tracking events or shipment details are null or empty. Returning the original tracking events.");
            return trackingEvents;
        }
        // Construct list criteria and fetch existing events based on entity
        var listCriteria = CommonUtils.constructListRequestFromEntityId(entityId, entityType);
        Pair<Specification<Events>, Pageable> pair = fetchData(listCriteria, Events.class);
        log.info("Fetching existing events from the database using criteria: {}", listCriteria);
        List<Events> eventsFromDb = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();

        // Create a map of existing events by their event code
        Map<String, Events> existingEventsMap = eventsFromDb.stream()
                .collect(Collectors.toMap(
                        event -> commonUtils.getTrackingEventsUniqueKey(
                                event.getEventCode(),
                                event.getContainerNumber(),
                                shipmentDetails.getShipmentId(),
                                event.getSource(),
                                event.getPlaceName()),
                        Function.identity(),
                        (existingEvent, newEvent) -> {
                            log.debug("Duplicate key detected. Replacing existing event with new event: {}", newEvent);
                            return newEvent;
                        }
                ));

        log.info("Mapping and filtering tracking events.");
        // Filter, map, and collect relevant tracking events based on custom logic
        List<Events> updatedEvents = trackingEvents.stream()
                .filter(trackingEvent -> shouldProcessEvent(trackingEvent, shipmentDetails))
                .map(trackingEvent -> mapToUpdatedEvent(trackingEvent, existingEventsMap, entityId, entityType, identifier2ToLocationRoleMap, shipmentDetails.getShipmentId()))
                .toList();

        setEventCodesMasterData(
                updatedEvents,
                Events::getEventCode,
                Events::setDescription
        );

        log.info("Saving updated events to the database.");
        return eventDao.saveAll(updatedEvents);
    }

    /**
     * Determines whether an event should be processed based on its code and shipment details.
     *
     * This method evaluates if an event qualifies for processing based on its code and the type or transport mode
     * of the shipment. It follows predefined criteria for various event codes.
     *
     * @param event the event to be evaluated
     * @param shipmentDetails the details of the shipment
     * @return true if the event should be processed, false otherwise
     */
    private boolean shouldProcessEvent(Events event, ShipmentDetails shipmentDetails) {
        if (event == null || shipmentDetails == null) {
            return false;
        }

        String eventCode = safeString(event.getEventCode());
        String shipmentType = shipmentDetails.getShipmentType();
        String transportMode = shipmentDetails.getTransportMode();

        // Log the input values for debugging
        log.info("Evaluating event with code: {}, shipmentType: {}, transportMode: {}", eventCode, shipmentType, transportMode);

        if (EventConstants.ECPK.equalsIgnoreCase(eventCode) && isFclShipment(shipmentType)) {
            log.debug("Event code {} matches FCL shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.FCGI.equalsIgnoreCase(eventCode) && isFclShipment(shipmentType)) {
            log.debug("Event code {} matches FCL shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.VSDP.equalsIgnoreCase(eventCode) &&
                (isFclShipment(shipmentType) || isLclShipment(shipmentType) || isAirShipment(transportMode))) {
            log.debug("Event code {} matches FCL/LCL/Air shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.ARDP.equalsIgnoreCase(eventCode) &&
                (isFclShipment(shipmentType) || isLclShipment(shipmentType) || isAirShipment(transportMode))) {
            log.debug("Event code {} matches FCL/LCL/Air shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.FUGO.equalsIgnoreCase(eventCode) && isFclShipment(shipmentType)) {
            log.debug("Event code {} matches FCL shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.EMCR.equalsIgnoreCase(eventCode) && isFclShipment(shipmentType)) {
            log.debug("Event code {} matches FCL shipment criteria", eventCode);
            return true;
        }

        if (EventConstants.AIR_TRACKING_CODE_LIST.contains(eventCode) && isAirShipment(transportMode)) {
            log.info("Event code {} matches air transport shipment criteria", eventCode);
            return true;
        }

        log.info("Event code {} does not match any processing criteria", eventCode);
        return false;
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

    /**
     * Maps a tracking event to an updated event with additional details.
     *
     * @param trackingEvent The tracking event to map.
     * @param existingEventsMap A map of existing events to check for duplicates.
     * @param entityId The ID of the entity associated with the event.
     * @param entityType The type of the entity associated with the event.
     * @param identifier2ToLocationRoleMap A map of identifiers to location roles for conversion.
     * @param shipmentId
     * @return The updated event with additional details.
     */
    private Events mapToUpdatedEvent(Events trackingEvent, Map<String, Events> existingEventsMap,
            Long entityId, String entityType, Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap, String shipmentId) {
        // Log the start of mapping
        log.info("Mapping tracking event with container number {} and event code {}.",
                trackingEvent.getContainerNumber(), trackingEvent.getEventCode());

        // Map the tracking event to a new Events object
        Events event = populateEventDetails(trackingEvent, entityId, entityType, identifier2ToLocationRoleMap);

        // Check if the event already exists
        Events existingEvent = existingEventsMap.get(
                commonUtils.getTrackingEventsUniqueKey(
                        event.getEventCode(),
                        event.getContainerNumber(),
                        shipmentId,
                        event.getSource(),
                        event.getPlaceName()));

        if (existingEvent != null) {
            // Update ID and GUID if the event already exists
            event.setId(existingEvent.getId());
            event.setGuid(existingEvent.getGuid());
            log.info("Event already exists. Updated ID and GUID from existing event.");
        } else {
            log.info("Event is new. No existing event found.");
        }

        // Return the updated event
        return event;
    }

    @NotNull
    private Events populateEventDetails(Events trackingEvent, Long entityId, String entityType, Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap) {
        Events event = modelMapper.map(trackingEvent, Events.class);

        // Convert and set the location role
        String convertedLocationRole = convertLocationRoleWRTMasterData(identifier2ToLocationRoleMap, event.getLocationRole());
        event.setLocationRole(convertedLocationRole);
        log.info("Converted location role: {}", convertedLocationRole);

        // Set entity details and source
        event.setEntityId(entityId);
        event.setEntityType(entityType);
        event.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING);
        eventDao.updateEventDetails(event); // updating the consolidation ID & shipment number
        log.info("Set entityId: {}, entityType: {}, source: {}", entityId, entityType, Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING);
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

    private boolean updateShipmentDetails(ShipmentDetails shipment, List<Events> events,
            LocalDateTime shipmentAta, LocalDateTime shipmentAtd, Container container) {
        boolean isShipmentUpdateRequired = false;
        // Try to update carrier details
        try {
            isShipmentUpdateRequired |= updateCarrierDetails(shipment, shipmentAta, shipmentAtd);
        } catch (Exception e) {
            log.error("Failed to update carrier details for shipment ID {}: {}", shipment.getShipmentId(), e.getMessage());
        }
        // Try to update empty container returned status
        try {
            isShipmentUpdateRequired |= updateEmptyContainerReturnedStatus(shipment, events);
        } catch (Exception e) {
            log.error("Failed to update empty container returned status for shipment ID {}: {}", shipment.getShipmentId(), e.getMessage());
        }
        // Try to update actual event times
        try {
            isShipmentUpdateRequired |= updateActual(shipment, container);
        } catch (Exception e) {
            log.error("Failed to update actual event times for shipment ID {}: {}", shipment.getShipmentId(), e.getMessage());
        }
        return isShipmentUpdateRequired;
    }
    private boolean updateActual(ShipmentDetails shipmentDetails, Container trackingContainer) {
        AtomicBoolean isShipmentUpdateRequired = new AtomicBoolean(false);
        if (trackingContainer == null || trackingContainer.getEvents() == null) {
            log.warn("No trackingContainer or events available for shipment ID {}", shipmentDetails.getShipmentId());
            return false;
        }
        List<Events> shipmentEvents = shipmentDetails.getEventsList();
        Map<String, Event> containerEventMapFromTracking = trackingContainer.getEvents().stream()
                .filter(Objects::nonNull)
                .map(event -> {
                    String eventCode = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                            event.getLocationRole(), event.getEventType(), event.getDescription());
                    String placeName = Optional.ofNullable(trackingContainer.getPlaces()).orElse(Collections.emptyList()).stream()
                            .filter(place -> Objects.equals(place.getId(), event.getLocation()))
                            .map(Place::getCode).map(StringUtils::defaultString).findFirst()
                            .orElse(StringUtils.EMPTY);
                    String trackingEventsUniqueKey = commonUtils.getTrackingEventsUniqueKey(
                            eventCode,
                            trackingContainer.getContainerNumber(),
                            shipmentDetails.getShipmentId(),
                            Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING,
                            placeName);
                    return new SimpleEntry<>(trackingEventsUniqueKey, event);
                })
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (existing, newEvent) ->
                                Optional.ofNullable(existing.getId()).orElse(0) >
                                        Optional.ofNullable(newEvent.getId()).orElse(0) ? existing : newEvent
                ));
        shipmentEvents.forEach(shipmentEvent -> {
            if (Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING.equalsIgnoreCase(shipmentEvent.getSource())) {
                EventsResponse shipmentEventsResponse = Optional.ofNullable(jsonHelper.convertValue(shipmentEvent, EventsResponse.class)).orElse(new EventsResponse());
                String key = commonUtils.getTrackingEventsUniqueKey(shipmentEventsResponse.getEventCode(),
                        shipmentEventsResponse.getContainerNumber(), shipmentEventsResponse.getShipmentNumber(),
                        shipmentEventsResponse.getSource(), shipmentEventsResponse.getPlaceName());
                Event eventFromTracking = containerEventMapFromTracking.get(key);
                if (eventFromTracking != null && eventFromTracking.getActualEventTime() != null) {
                    shipmentEvent.setActual(eventFromTracking.getActualEventTime().getDateTime());
                    isShipmentUpdateRequired.set(true);
                    log.info("Updated actual event time for event code {} in trackingContainer {}",
                            shipmentEventsResponse.getEventCode(), shipmentEventsResponse.getContainerNumber());
                } else {
                    log.warn("No matching event found or missing actual event time for key: {}", key);
                }
            }
        });
        return isShipmentUpdateRequired.get();
    }
    private boolean updateEmptyContainerReturnedStatus(ShipmentDetails shipment, List<Events> events) {
        boolean isShipmentUpdateRequired = false;

        // Check for empty container returned event
        boolean isEmptyContainerReturnedEvent = events.stream()
                .anyMatch(event -> EventConstants.EMCR.equals(event.getEventCode()));

        // Update empty container returned status if conditions are met
        if (isEmptyContainerReturnedEvent
                && shipment.getAdditionalDetails() != null
                && !Boolean.TRUE.equals(shipment.getAdditionalDetails().getEmptyContainerReturned())
                && Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                && TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())) {

            shipment.getAdditionalDetails().setEmptyContainerReturned(true);
            isShipmentUpdateRequired = true;
        }

        return isShipmentUpdateRequired;
    }

    private boolean updateCarrierDetails(ShipmentDetails shipment, LocalDateTime shipmentAta, LocalDateTime shipmentAtd) {
        // Update carrier details with ATA and ATD if present
        CarrierDetails carrierDetails = Optional.ofNullable(shipment.getCarrierDetails()).orElse(new CarrierDetails());
        boolean isShipmentUpdateRequired = false;
        if (shipmentAta != null) {
            carrierDetails.setAta(shipmentAta);
            createDateTimeChangeLog(DateType.ATA, shipmentAta, shipment.getId());
            isShipmentUpdateRequired = true;
        }
        if (shipmentAtd != null) {
            carrierDetails.setAtd(shipmentAtd);
            createDateTimeChangeLog(DateType.ATA, shipmentAtd, shipment.getId());
            isShipmentUpdateRequired = true;
        }

        if (isShipmentUpdateRequired) {
            shipment.setCarrierDetails(carrierDetails);
        }

        return isShipmentUpdateRequired;
    }

    @Override
    public void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings) {
        if (ObjectUtils.isNotEmpty(events)) {
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
            event.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING);
            updatedEvents.add(event);
        });

        eventDumpDao.saveAll(updatedEvents);
    }

    /**
     * Processes an upstream tracking message, generates events, and persists them.
     *
     * This method is the main entry point for processing tracking messages. It takes a container object,
     * generates tracking events based on the container's data using the tracking service adapter,
     * and persists these events by calling the `persistTrackingEvents` method. If the container is null or empty,
     * it returns early, logging a warning.
     *
     * @param container The container object received in the tracking message that contains shipment and journey details.
     *
     * @return boolean  Returns true if the message was successfully processed and events persisted,
     *                  or if the container was empty. Returns false if any error occurs during the processing.
     */
    @Override
    @Transactional
    public boolean processUpstreamTrackingMessage(TrackingServiceApiResponse.Container container) {
        log.info("Starting processUpstreamTrackingMessage with container: {}", container);

        if (ObjectUtils.isEmpty(container)) {
            log.warn("Received empty or null container. Returning true.");
            return true;
        }

        TrackingServiceApiResponse trackingServiceApiResponse = new TrackingServiceApiResponse();
        trackingServiceApiResponse.setContainers(List.of(container));

        List<Events> trackEvents = trackingServiceAdapter.generateEventsFromTrackingResponse(trackingServiceApiResponse);
        log.info("Generated {} events from container: {}", trackEvents.size(), trackEvents);

        boolean result = persistTrackingEvents(trackingServiceApiResponse, trackEvents);
        log.info("Finished processing upstream tracking message for container. Result: {}", result);
        return result;
    }

    /**
     * Persists tracking events to the database and updates the relevant shipment details.
     *
     * This method takes a response from the tracking service API and a list of tracking events,
     * and attempts to persist them into the database. It performs necessary validations such as
     * checking for null or empty events and handles the retrieval of shipment details based on the container data.
     * It also sets and clears the authentication context required for the transaction.
     *
     * @param trackingServiceApiResponse The response from the tracking service API, which contains container information.
     * @param trackingEvents             The list of tracking events generated from the container data.
     *
     * @return boolean                   Returns true if all events were successfully persisted and processed,
     *                                   false if any failure occurred during the process.
     */
    private boolean persistTrackingEvents(TrackingServiceApiResponse trackingServiceApiResponse, List<Events> trackingEvents) {
        log.info("Starting persistTrackingEvents with trackingEvents: {}", trackingEvents);

        boolean isSuccess = true;
        //Moving this from here to tracking consumer-> v1Service.setAuthContext()

        if (ObjectUtils.isEmpty(trackingEvents)) {
            log.error("Tracking events are null or empty. Skipping event persistence.");
            return true;
        }

        Container container = trackingServiceApiResponse.getContainers().get(0);
        log.info("Processing container: {}", container);

        String shipmentNumber = Optional.ofNullable(container.getContainerBase())
                .map(ContainerBase::getShipmentReference)
                .orElse(null);

        log.info("Shipment number extracted: {}", shipmentNumber);

        if (ObjectUtils.isEmpty(shipmentNumber)) {
            log.warn("Shipment number is empty or null. Skipping further processing.");
            return true;
        }

        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findByShipmentId(shipmentNumber);
        log.info("Found {} shipment details for shipment number: {}", shipmentDetailsList.size(), shipmentNumber);

        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            log.info("Processing shipment details: {}", shipmentDetails);
            TenantContext.setCurrentTenant(shipmentDetails.getTenantId());
            boolean updateSuccess = updateShipmentWithTrackingEvents(trackingEvents, shipmentDetails, container);
            isSuccess &= updateSuccess;
            log.info("Updated shipment: {} with tracking events. Success: {}", shipmentDetails.getShipmentId(), updateSuccess);
        }

        //moving this from here to tracking consumer -> v1Service.clearAuthContext()

        return isSuccess;
    }

    /**
     * Updates the shipment entity with tracking events and additional data such as ATA and ATD.
     *
     * This method processes a list of tracking events, saves them to the appropriate database tables,
     * and updates shipment details based on the provided container and event data.
     * It also handles synchronization of the shipment entity if updates are made.
     *
     * @param trackingEvents    The list of tracking events generated from the container data.
     * @param shipmentDetails   The shipment details entity that is being updated with the events.
     * @param container         The container data from which the tracking events were generated.
     *
     * @return boolean          Returns true if the update and sync operations were successful, false if any operation failed.
     */
    private boolean updateShipmentWithTrackingEvents(List<Events> trackingEvents, ShipmentDetails shipmentDetails,
            Container container) {
        log.info("Starting updateShipmentWithTrackingEvents for shipment: {} and container: {}",
                shipmentDetails.getShipmentId(), container);

        boolean isSuccess = true;
        Map<String, EntityTransferMasterLists> identifier2ToLocationRoleMap = getIdentifier2ToLocationRoleMap();
        log.debug("Fetched identifier-to-location-role map: {}", identifier2ToLocationRoleMap);

        log.info("Saving tracking events to EventsDump for shipment ID: {}", shipmentDetails.getId());
        saveTrackingEventsToEventsDump(trackingEvents, shipmentDetails.getId(), Constants.SHIPMENT);

        List<Events> eventSaved = saveTrackingEventsToEvents(trackingEvents, shipmentDetails.getId(),
                Constants.SHIPMENT, shipmentDetails, identifier2ToLocationRoleMap);
        log.info("Saved {} events to Events table for shipment: {}", eventSaved.size(), shipmentDetails.getShipmentId());

        // Extract ATA and ATD from container's journey details
        LocalDateTime shipmentAta = Optional.ofNullable(container.getJourney())
                .map(TrackingServiceApiResponse.Journey::getPortOfArrivalAta)
                .map(TrackingServiceApiResponse.DateAndSources::getDateTime)
                .orElse(null);

        LocalDateTime shipmentAtd = Optional.ofNullable(container.getJourney())
                .map(TrackingServiceApiResponse.Journey::getPortOfDepartureAtd)
                .map(TrackingServiceApiResponse.DateAndSources::getDateTime)
                .orElse(null);

        log.info("Extracted ATA: {}, ATD: {} for shipment: {}", shipmentAta, shipmentAtd, shipmentDetails.getShipmentId());

        boolean isShipmentUpdateRequired = updateShipmentDetails(shipmentDetails, eventSaved, shipmentAta, shipmentAtd, container);
        log.info("Shipment update required: {}", isShipmentUpdateRequired);

        if (isShipmentUpdateRequired) {
            try {
                saveAndSyncShipment(shipmentDetails);
                log.info("Successfully saved and synced shipment: {}", shipmentDetails.getShipmentId());
            } catch (Exception e) {
                isSuccess = false;
                log.error("Error performing sync on shipment entity: {}", shipmentDetails.getShipmentId(), e);
            }
        }

        log.info("Finished updating shipment with tracking events. Success: {}", isSuccess);
        return isSuccess;
    }

    private void saveAndSyncShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        log.info("Saving shipment entity: {}", shipmentDetails.getShipmentId());
        shipmentDao.saveWithoutValidation(shipmentDetails);
//        log.info("Synchronizing shipment: {}", shipmentDetails.getShipmentId());
//        shipmentSync.sync(shipmentDetails, null, null, UUID.randomUUID().toString(), false);
    }

    @Override
    public ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel) {
        TrackingEventsRequest request = (TrackingEventsRequest) commonRequestModel.getData();

        Long shipmentId = request.getShipmentId();
        Long consolidationId = request.getConsolidationId();

        List<EventsResponse> allEventResponses = new ArrayList<>();
        ListCommonRequest listRequest = jsonHelper.convertValue(request, ListCommonRequest.class);

        if (shipmentId != null) {
            List<Events> shipmentEvents = getEventsListForCriteria(shipmentId, true, listRequest);
            allEventResponses = jsonHelper.convertValueToList(shipmentEvents, EventsResponse.class);
        }
        else if (consolidationId != null) {
            List<Events> consolEvents = getEventsListForCriteria(consolidationId, false, listRequest);
            allEventResponses = jsonHelper.convertValueToList(consolEvents, EventsResponse.class);
        }

        // set MasterData
        setEventCodesMasterData(
                allEventResponses,
                EventsResponse::getEventCode,
                EventsResponse::setDescription
        );

        List<EventsResponse> groupedEvents = allEventResponses;

        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            return ResponseHelper.buildSuccessResponse(groupedEvents);
        }

        // Events grouping logic if events revamp feature flag is enabled
        if (Objects.isNull(request.getSortRequest())) {
            groupedEvents = allEventResponses.stream()
                    // Group by eventCode and sort each group by `actual` in descending order
                    .collect(Collectors.groupingBy(EventsResponse::getEventCode))
                    .values().stream()
                    // Sort each group by `actual` in descending order
                    .map(group -> {
                        group.sort(
                                Comparator.comparing(EventsResponse::getShipmentNumber)
                                .thenComparing(EventsResponse::getActual, Comparator.nullsLast(Comparator.reverseOrder()))
                        );
                        return group;
                    })
//                     Sort groups by the latest actual date in descending order
                    .sorted(Comparator.comparing(
                            group -> group.get(0).getActual(), Comparator.nullsLast(Comparator.reverseOrder())
                    ))
                    .flatMap(List::stream)
                    .toList();
        }

        return ResponseHelper.buildSuccessResponse(groupedEvents);
    }

    private List<Events> getEventsListForCriteria(Long id, boolean isShipment, ListCommonRequest listRequest) {
        if(isShipment) {
            listRequest = CommonUtils.andCriteria("entityId", id, "=", listRequest);
            listRequest = CommonUtils.andCriteria("entityType", Constants.SHIPMENT, "=", listRequest);
        }
        else {
            listRequest = CommonUtils.andCriteria("consolidationId", id, "=", listRequest);
        }
        Pair<Specification<Events>, Pageable> pair = fetchData(listRequest, Events.class);
        List<Events> allEvents = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();
        log.info("EventsList - fetched {} events", allEvents.size());
        return allEvents;
    }

    /**
     * Trigger point for creating / updating event
     * @param eventsRequest
     */
    public void saveEvent(EventsRequest eventsRequest) {
        Events entity = convertRequestToEntity(eventsRequest);

        if (Boolean.TRUE.equals(eventsRequest.getSaveFromShipment())) {
            entity.setEntityType(Constants.SHIPMENT);
        }
        // event code and master-data description
        commonUtils.updateEventWithMasterDataDescription(List.of(entity));
        eventDao.updateEventDetails(entity);

        handleDuplicationForExistingEvents(entity);

        eventDao.save(entity);
        // auto generate runner events | will remain as it is inside shipment and consolidation
    }

    private void handleDuplicationForExistingEvents(Events event) {

        // find the event with duplication key criteria
        // shipment number, container number, event code, source, place name
        ListCommonRequest duplicateEventRequest = CommonUtils.constructListCommonRequest("eventCode", "=", event.getEventCode());
        duplicateEventRequest = CommonUtils.andCriteria("shipmentNumber",  event.getShipmentNumber(), "=", duplicateEventRequest);
        duplicateEventRequest = CommonUtils.andCriteria("containerNumber",  event.getContainerNumber(), "=", duplicateEventRequest);
        duplicateEventRequest = CommonUtils.andCriteria("source",  event.getSource(), "=", duplicateEventRequest);
        duplicateEventRequest = CommonUtils.andCriteria("placeName",  event.getPlaceName(), "=", duplicateEventRequest);
        duplicateEventRequest = CommonUtils.andCriteria("entityId",  event.getEntityId(), "=", duplicateEventRequest);
        duplicateEventRequest = CommonUtils.andCriteria("entityType",  event.getEntityType(), "=", duplicateEventRequest);
        Pair<Specification<Events>, Pageable> pair = fetchData(duplicateEventRequest, Events.class);
        Page<Events> duplicateEventPage = eventDao.findAll(pair.getLeft(), pair.getRight());

        if (duplicateEventPage != null && duplicateEventPage.hasContent()) {
            // List of events fetched based on the duplication criteria, (getting single event is fine we can update existing event) but can we make an invariant on this
            // these events are irrelevant as we found a replacement : current event | Delete all rest events excluding the current one
            duplicateEventPage.getContent().stream().filter(i -> !i.getId().equals(event.getId())).forEach(i -> eventDao.delete(i));
        }
    }

}
