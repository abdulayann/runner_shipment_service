package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Component
@Slf4j
public class ConsolidationV3Util {

    private CommonUtils commonUtils;
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private IShipmentDao shipmentDao;
    private IPartiesDao partiesDao;
    private IEventDao eventDao;
    private IPackingDao packingDao;
    private IContainerDao containerDao;
    private IAwbDao awbDao;
    private IReferenceNumbersDao referenceNumbersDao;
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    private IRoutingsDao routingsDao;

    @Autowired
    public ConsolidationV3Util(CommonUtils commonUtils, IConsoleShipmentMappingDao consoleShipmentMappingDao, IShipmentDao shipmentDao,
                               IPartiesDao partiesDao, IEventDao eventDao, IPackingDao packingDao, IContainerDao containerDao,
                               IAwbDao awbDao, IReferenceNumbersDao referenceNumbersDao, ITruckDriverDetailsDao truckDriverDetailsDao,
                               IRoutingsDao routingsDao) {
        this.commonUtils = commonUtils;
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.shipmentDao = shipmentDao;
        this.partiesDao = partiesDao;
        this.eventDao = eventDao;
        this.packingDao=packingDao;
        this.containerDao=containerDao;
        this.awbDao=awbDao;
        this.referenceNumbersDao=referenceNumbersDao;
        this.truckDriverDetailsDao=truckDriverDetailsDao;
        this.routingsDao=routingsDao;
    }

    /**
     * Checks if a consolidation is eligible for CFS (Container Freight Station) validation. Criteria: - Transport mode must be SEA - Shipment type must be EXPORT - LCL
     * consolidation setting must be enabled in context
     *
     * @param consolidationDetails the consolidation to validate
     * @return true if eligible for CFS validation, false otherwise
     */
    public boolean checkConsolidationEligibleForCFSValidation(ConsolidationDetails consolidationDetails) {
        // Must be SEA transport mode
        if (!Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())) {
            return false;
        }

        // Must be an Export shipment
        if (!Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType())) {
            return false;
        }

        // LCL consolidation setting must be enabled
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    /**
     * Retrieves a list of shipments associated with a given consolidation ID.
     * <p>
     * This method fetches mappings from the `ConsoleShipmentMapping` table, extracts shipment IDs, constructs a dynamic list query, and returns the corresponding
     * `ShipmentDetails`.
     *
     * @param consoleId The ID of the consolidation whose shipments need to be fetched
     * @return List of linked `ShipmentDetails`
     */
    public List<ShipmentDetails> getShipmentsList(Long consoleId) {
        // Fetch mapping records between the consolidation and its linked shipments
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consoleId);

        // Extract shipment IDs from the mapping
        List<Long> shipmentIdList = consoleShipmentMappings.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Build a dynamic list request using "IN" operation on shipment IDs
        ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");

        // Create JPA Specification and pagination info from the request
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        // Execute the paginated query using the specification
        Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

        // Return the list of shipment details
        return new ArrayList<>(page.getContent());
    }

    public void afterSaveForET(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity,
                               ConsolidationEtV3Request consolidationDetailsRequest, boolean isCreate,
                               ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean includeGuid) throws RunnerException {
        List<PackingV3Request> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerV3Request> containerRequestList = consolidationDetailsRequest.getContainersList();

        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

        Long id = consolidationDetails.getId();

        if (Boolean.FALSE.equals(isCreate) && checkForAwbUpdate(consolidationDetails, oldEntity)) {
            awbDao.updatedAwbInformationEvent(consolidationDetails, oldEntity);
        }

        setContainerAndPackingList(consolidationDetails, isCreate, shipmentSettingsDetails, isFromBooking, includeGuid, containerRequestList, id, packingRequestList);

        // Events section
        setEventsForConsole(consolidationDetails, isCreate, shipmentSettingsDetails, isFromBooking, eventsRequestList, id);

        processRequestLists(consolidationDetails, isCreate, isFromBooking, referenceNumbersRequestList, id, truckDriverDetailsRequestList, routingsRequestList);
        if (consolidationAddressRequest != null) {
            List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, !Boolean.TRUE.equals(isFromBooking) && isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedFileRepos);
        }
    }

    private boolean checkForAwbUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(consolidationDetails.getSci(), oldEntity.getSci())) return true;
        return !Objects.equals(consolidationDetails.getEfreightStatus(), oldEntity.getEfreightStatus());
    }

    private void setContainerAndPackingList(ConsolidationDetails consolidationDetails, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean includeGuid, List<ContainerV3Request> containerRequestList, Long id, List<PackingV3Request> packingRequestList) throws RunnerException {
        if(containerRequestList != null && (shipmentSettingsDetails.getMergeContainers() == null || !shipmentSettingsDetails.getMergeContainers())
                && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
            List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class, (!isFromBooking && !includeGuid) && isCreate), id, (Long) null, true);
            consolidationDetails.setContainersList(updatedContainers);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class, (!isFromBooking && !includeGuid) && isCreate), id);
            consolidationDetails.setPackingList(updatedPackings);
        }
    }

    private void setEventsForConsole(ConsolidationDetails consolidationDetails, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, List<EventsRequest> eventsRequestList, Long id) throws RunnerException {
        if (eventsRequestList != null && !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            setEventDetails(eventsRequestList, consolidationDetails);
            List<Events> eventsList = new ArrayList<>(commonUtils.convertToEntityList(eventsRequestList, Events.class, !Boolean.TRUE.equals(isFromBooking) && isCreate));
            commonUtils.removeDuplicateTrackingEvents(eventsList);
            commonUtils.updateEventWithMasterData(eventsList);
            eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.CONSOLIDATION);
            consolidationDetails.setEventsList(eventsList);
        }
        if (Boolean.TRUE.equals(isCreate) && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate())) {
            generateEvents(consolidationDetails);
        }
    }

    private void setEventDetails(List<EventsRequest> eventsRequestList, ConsolidationDetails consolidationDetails) {
        if(eventsRequestList != null && !eventsRequestList.isEmpty()) {
            for (EventsRequest req : eventsRequestList) {
                req.setConsolidationId(consolidationDetails.getId());
            }
        }
    }
    public void generateEvents(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getEventsList() == null) {
            consolidationDetails.setEventsList(new ArrayList<>());
        }
        consolidationDetails.getEventsList().add(createEvent(consolidationDetails, EventConstants.COCR));
    }

    private Events createEvent(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.CONSOLIDATION);
        events.setEntityId(consolidationDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setConsolidationId(consolidationDetails.getId());
        events.setDirection(consolidationDetails.getShipmentType());
        // Persist the event
        eventDao.save(events);
        return events;
    }

    private void processRequestLists(ConsolidationDetails consolidationDetails, Boolean isCreate, Boolean isFromBooking, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id, List<TruckDriverDetailsRequest> truckDriverDetailsRequestList, List<RoutingsRequest> routingsRequestList) throws RunnerException {
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, !Boolean.TRUE.equals(isFromBooking) && isCreate), id);
            consolidationDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (truckDriverDetailsRequestList != null) {
            truckDriverDetailsDao.updateEntityFromConsole(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, !Boolean.TRUE.equals(isFromBooking) && isCreate), id);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class, !Boolean.TRUE.equals(isFromBooking) && isCreate), id);
            consolidationDetails.setRoutingsList(updatedRoutings);
        }
    }

}
