package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class EventV3Service implements IEventsV3Service {

    private IEventDao eventDao;
    private JsonHelper jsonHelper;
    private final IDateTimeChangeLogService dateTimeChangeLogService;
    private IV1Service v1Service;
    private CommonUtils commonUtils;
    private final EventService eventV2Service;

    @Autowired
    public EventV3Service(IEventDao eventDao, JsonHelper jsonHelper, IDateTimeChangeLogService dateTimeChangeLogService,
            IV1Service v1Service, CommonUtils commonUtils, EventService eventV2Service) {
        this.eventDao = eventDao;
        this.jsonHelper = jsonHelper;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
        this.v1Service = v1Service;
        this.commonUtils = commonUtils;
        this.eventV2Service = eventV2Service;
    }

    @Override
    public List<EventsResponse> listV2(CommonRequestModel commonRequestModel, String source) {
        TrackingEventsRequest request = (TrackingEventsRequest) commonRequestModel.getData();

        Long shipmentId = request.getShipmentId();
        Long consolidationId = request.getConsolidationId();

        List<EventsResponse> allEventResponses = new ArrayList<>();
        ListCommonRequest listRequest = jsonHelper.convertValue(request, ListCommonRequest.class);

        if (shipmentId != null) {
            List<Events> shipmentEvents = getEventsListForCriteria(shipmentId, true, listRequest, source);
            allEventResponses = jsonHelper.convertValueToList(shipmentEvents, EventsResponse.class);
        } else if (consolidationId != null) {
            List<Events> consolEvents = getEventsListForCriteria(consolidationId, false, listRequest, source);
            allEventResponses = jsonHelper.convertValueToList(consolEvents, EventsResponse.class);
        }

        // set Branch name for every event response
        populateBranchNames(allEventResponses);

        // set MasterData
        setEventCodesMasterData(
                allEventResponses,
                EventsResponse::getEventCode,
                EventsResponse::setDescription
        );

        List<EventsResponse> groupedEvents = allEventResponses;

        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            return groupedEvents;
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
                                Comparator.comparing(EventsResponse::getShipmentNumber, Comparator.nullsLast(Comparator.naturalOrder()))
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

        return groupedEvents;
    }

    private List<Events> getEventsListForCriteria(Long id, boolean isShipment, ListCommonRequest listRequest, String source) {
        if (isShipment) {
            listRequest = CommonUtils.andCriteria(EventConstants.ENTITY_ID, id, "=", listRequest);
            listRequest = CommonUtils.andCriteria(EventConstants.ENTITY_TYPE, Constants.SHIPMENT, "=", listRequest);
        } else {
            listRequest = CommonUtils.andCriteria("consolidationId", id, "=", listRequest);
        }
        Pair<Specification<Events>, Pageable> pair = fetchData(listRequest, Events.class);
        List<Events> allEvents;
        if (Objects.equals(source, Constants.NETWORK_TRANSFER)) {
            allEvents = eventDao.findAllWithoutTenantFilter(pair.getLeft(), pair.getRight()).getContent();
        } else {
            allEvents = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();
        }
        log.info("EventsList - fetched {} events", allEvents.size());
        return allEvents;
    }

    /**
     * Populates branch display names in the provided list of event responses by fetching cousin branch data from the V1 service and mapping event codes to branch display names.
     *
     * @param eventResponses the list of events to update with branch names
     */
    public void populateBranchNames(List<EventsResponse> eventResponses) {
        if (eventResponses == null || eventResponses.isEmpty()) {
            log.debug("No eventResponses to process in populateBranchNames.");
            return;
        }

        List<V1TenantResponse> tenants = Collections.emptyList();
        try {
            // Step 1: Fetch cousin branches from V1 service
            V1DataResponse dataResponse = v1Service.listCousinBranches(Collections.emptyMap());

            if (dataResponse == null || dataResponse.getEntities() == null) {
                log.warn("Received null or empty response from V1 service while fetching cousin branches.");
                return;
            }

            // Step 2: Convert raw entity list to V1TenantResponse list
            tenants = jsonHelper.convertValueToList(dataResponse.getEntities(), V1TenantResponse.class);
        } catch (V1ServiceException e) {
            log.error("Failed to fetch cousin branches from V1 service: {}", e.getMessage(), e);
            return;
        } catch (Exception e) {
            log.error("Unexpected error while converting cousin branch data: {}", e.getMessage(), e);
            return;
        }

        // Step 3: Build a map of code -> display name
        Map<String, String> codeToNameMap = tenants.stream()
                .filter(t -> t.getCode() != null && t.getDisplayName() != null)
                .collect(Collectors.toMap(
                        V1TenantResponse::getCode,
                        V1TenantResponse::getDisplayName,
                        (existing, replacement) -> existing // keep the first
                ));

        // Step 4: Set branch name on each event
        for (EventsResponse event : eventResponses) {
            String branchCode = event.getBranch();
            if (branchCode != null) {
                String displayName = codeToNameMap.get(branchCode);
                if (displayName != null) {
                    event.setBranchName(displayName);
                }
            }
        }
    }

    private <T> void setEventCodesMasterData(List<T> eventsList, Function<T, String> getEventCode, BiConsumer<T, String> setDescription) {
        try {
            if (Objects.isNull(eventsList) || eventsList.isEmpty()) {
                return;
            }
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

    @Override
    public void processEventsAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getEventsList() != null) {
            List<Events> eventsList = shipmentDetails.getEventsList();
            for (Events event : eventsList) {
                // Update only if event qualifies for shipment-to-consolidation transfer
                if (eventDao.shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                    event.setConsolidationId(consolidationId);
                }
            }
            eventDao.saveAll(eventsList);
        }
    }

    @Transactional
    @Override
    public void saveAllEvent(List<EventsRequest> eventsRequests) {
        if (CommonUtils.listIsNullOrEmpty(eventsRequests)) {
            return;
        }
        List<Events> entities = jsonHelper.convertValueToList(eventsRequests, Events.class);

        commonUtils.updateEventWithMasterData(entities);
        eventDao.updateAllEventDetails(entities);

        for (Events event : entities) {
            handleDuplicationForExistingEvents(event);
        }

        eventDao.saveAll(entities);
    }

    private void handleDuplicationForExistingEvents(Events event) {
        Specification<Events> duplicateEventSpecification = buildDuplicateEventSpecification(event);
        Page<Events> duplicateEventPage = eventDao.findAll(duplicateEventSpecification, Pageable.unpaged());

        if (duplicateEventPage != null && duplicateEventPage.hasContent()) {
            // List of events fetched based on the duplication criteria, (getting single event is fine we can update existing event) but can we make an invariant on this
            // these events are irrelevant as we found a replacement : current event | Delete all rest events excluding the current one
            List<Events> eventsToDelete = new ArrayList<>();
            duplicateEventPage.getContent().stream()
                    .filter(dupEvent -> !dupEvent.getId().equals(event.getId()))
                    .forEach(dupEvent -> {
                                dupEvent.setIsDeleted(true);
                                eventsToDelete.add(dupEvent);
                            }
                    );
            if (ObjectUtils.isNotEmpty(eventsToDelete)) {
                eventDao.saveAll(eventsToDelete);
            }
        }
    }

    public Specification<Events> buildDuplicateEventSpecification(Events event) {
        return (root, query, cb) -> {
            Predicate predicate = cb.conjunction();

            if (event.getEventCode() != null) {
                predicate = cb.and(predicate, cb.equal(root.get("eventCode"), event.getEventCode()));
            } else {
                predicate = cb.and(predicate, cb.isNull(root.get("eventCode")));
            }

            if (event.getShipmentNumber() != null) {
                predicate = cb.and(predicate, cb.equal(root.get("shipmentNumber"), event.getShipmentNumber()));
            } else {
                predicate = cb.and(predicate, cb.isNull(root.get("shipmentNumber")));
            }

            if (event.getContainerNumber() != null) {
                predicate = cb.and(predicate, cb.equal(root.get("containerNumber"), event.getContainerNumber()));
            } else {
                predicate = cb.and(predicate, cb.isNull(root.get("containerNumber")));
            }

            if (event.getSource() != null) {
                predicate = cb.and(predicate, cb.equal(root.get("source"), event.getSource()));
            } else {
                predicate = cb.and(predicate, cb.isNull(root.get("source")));
            }

            predicate = getPredicateForPlaceName(event, root, cb, predicate);

            predicate = getPredicateForEntityId(event, root, cb, predicate);

            predicate = getPredicateForEventType(event, root, cb, predicate);

            predicate = cb.and(predicate, cb.equal(root.get("isDeleted"), false));

            return predicate;
        };
    }

    private Predicate getPredicateForPlaceName(Events event, Root<Events> root, CriteriaBuilder cb, Predicate predicate) {
        if (event.getPlaceName() != null) {
            predicate = cb.and(predicate, cb.equal(root.get("placeName"), event.getPlaceName()));
        } else {
            predicate = cb.and(predicate, cb.isNull(root.get("placeName")));
        }
        return predicate;
    }

    private Predicate getPredicateForEntityId(Events event, Root<Events> root, CriteriaBuilder cb, Predicate predicate) {
        if (event.getEntityId() != null) {
            predicate = cb.and(predicate, cb.equal(root.get(EventConstants.ENTITY_ID), event.getEntityId()));
        } else {
            predicate = cb.and(predicate, cb.isNull(root.get(EventConstants.ENTITY_ID)));
        }
        return predicate;
    }

    private Predicate getPredicateForEventType(Events event, Root<Events> root, CriteriaBuilder cb, Predicate predicate) {
        if (event.getEntityType() != null) {
            predicate = cb.and(predicate, cb.equal(root.get(EventConstants.ENTITY_TYPE), event.getEntityType()));
        } else {
            predicate = cb.and(predicate, cb.isNull(root.get(EventConstants.ENTITY_TYPE)));
        }
        return predicate;
    }


    @Override
    public void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings) {
        if (ObjectUtils.isNotEmpty(events)) {
            Events lastEvent = events.get(events.size() - 1);
            if (tenantSettings.getIsAtdAtaAutoPopulateEnabled() != null && tenantSettings.getIsAtdAtaAutoPopulateEnabled().equals(true) && lastEvent.getActual() != null) {
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

    private void createDateTimeChangeLog(DateType dateType, LocalDateTime localDateTime, Long shipmentId) {
        dateTimeChangeLogService.saveDateTimeChangeLog(dateType, localDateTime, shipmentId, DateTimeChangeLogConstants.EVENT_SOURCE);
    }

    /**
     * Trigger point for creating / updating event
     *
     * @param eventsRequest
     */
    @Transactional
    @Override
    public void saveEvent(EventsRequest eventsRequest) {
        eventV2Service.saveEvent(eventsRequest);
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return eventV2Service.create(commonRequestModel);
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return eventV2Service.update(commonRequestModel);
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return eventV2Service.list(commonRequestModel);
    }

    @Async
    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return eventV2Service.listAsync(commonRequestModel);
    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return eventV2Service.delete(commonRequestModel);
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return eventV2Service.retrieveById(commonRequestModel);
    }

    public Events convertRequestToEntity(EventsRequest request) {
        return jsonHelper.convertValue(request, Events.class);
    }

    @Override
    public ResponseEntity<IRunnerResponse> v1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        return eventV2Service.v1EventsCreateAndUpdate(commonRequestModel, checkForSync);
    }

    @Override
    public ResponseEntity<IRunnerResponse> trackEvents(TrackingEventsRequest request) throws RunnerException {
        return eventV2Service.trackEvents(request);
    }

}
