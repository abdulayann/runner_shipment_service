package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class EventV3Service implements IEventsV3Service {

    private IEventDao eventDao;
    private JsonHelper jsonHelper;
    private IV1Service v1Service;
    private CommonUtils commonUtils;
    private IDateTimeChangeLogService dateTimeChangeLogService;

    @Autowired
    public EventV3Service(IEventDao eventDao, JsonHelper jsonHelper, IV1Service v1Service, IDateTimeChangeLogService dateTimeChangeLogService, CommonUtils commonUtils) {
        this.eventDao = eventDao;
        this.jsonHelper = jsonHelper;
        this.v1Service = v1Service;
        this.commonUtils = commonUtils;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
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
        }
        else if (consolidationId != null) {
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
        if(isShipment) {
            listRequest = CommonUtils.andCriteria(EventConstants.ENTITY_ID, id, "=", listRequest);
            listRequest = CommonUtils.andCriteria(EventConstants.ENTITY_TYPE, Constants.SHIPMENT, "=", listRequest);
        }
        else {
            listRequest = CommonUtils.andCriteria("consolidationId", id, "=", listRequest);
        }
        Pair<Specification<Events>, Pageable> pair = fetchData(listRequest, Events.class);
        List<Events> allEvents;
        if(Objects.equals(source, Constants.NETWORK_TRANSFER))
            allEvents = eventDao.findAllWithoutTenantFilter(pair.getLeft(), pair.getRight()).getContent();
        else
            allEvents = eventDao.findAll(pair.getLeft(), pair.getRight()).getContent();
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
}
