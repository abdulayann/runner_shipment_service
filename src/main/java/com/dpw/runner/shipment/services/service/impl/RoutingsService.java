package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsUpdateRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.DateAndSources;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Place;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RoutingException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class RoutingsService implements IRoutingsService {

    @Autowired
    private IRoutingsDao routingsDao;
    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;
    @Autowired
    private ShipmentDao shipmentDao;
    @Autowired
    public MasterDataUtils masterDataUtils;
    @Autowired
    private CommonUtils commonUtils;
    @Qualifier("executorServiceRouting")
    @Autowired
    ExecutorService executorServiceRouting;

    @Override
    public void updateRoutingsBasedOnTracking(Long shipmentId, List<Routings> routings)
        throws RunnerException, ExecutionException, InterruptedException {
        if (shipmentId == null) {
            log.warn("Received null shipment ID. Aborting routing update.");
            return;
        }

        if (routings == null || routings.isEmpty()) {
            log.warn("Received empty or null routings for shipment ID: {}", shipmentId);
            return;
        }

        // Fetch shipment details
        log.info("Fetching shipment details for shipment ID: {}", shipmentId);
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).orElse(null);
        if (shipmentDetails == null) {
            log.warn("No shipment details found for shipment ID: {}", shipmentId);
            return;
        }

        TrackingServiceApiResponse trackingServiceApiResponse;
        try {
            trackingServiceApiResponse = getTrackingServiceApiResponse(shipmentId, shipmentDetails);
        } catch (RunnerException e) {
            throw new RoutingException(e.getMessage(), e);
        }
        if (trackingServiceApiResponse == null) {
            return;
        }
        log.info("Tracking data successfully fetched for shipment ID: {}. Processing container events.", shipmentId);
        Map<String, List<Routings>> polToRoutingMap = new HashMap<>();
        Map<String, List<Routings>> podToRoutingMap = new HashMap<>();
        CompletableFuture<Void> polToRoutingMapFuture = CompletableFuture.runAsync(
            masterDataUtils.withMdc(() -> createRoutingMap(routings, true, polToRoutingMap)),
            executorServiceRouting);
        CompletableFuture<Void> podToRoutingMapFuture = CompletableFuture.runAsync(
            masterDataUtils.withMdc(() -> createRoutingMap(routings, false, podToRoutingMap)),
            executorServiceRouting);
        CompletableFuture.allOf(polToRoutingMapFuture, podToRoutingMapFuture).join();

        // Create routing maps for POL and POD

        log.info("Routing maps created for shipment ID: {}. POL size: {}, POD size: {}",
                shipmentId, polToRoutingMap.size(), podToRoutingMap.size());

        // Process each container's events
        for (Container tsContainer : trackingServiceApiResponse.getContainers()) {
            log.debug("Processing events for container ID: {}", tsContainer.getContainerNumber());
            processContainerEvents(tsContainer, polToRoutingMap, podToRoutingMap);
        }

        log.info("Completed processing container events for shipment ID: {}", shipmentId);
    }

    @Nullable
    private TrackingServiceApiResponse getTrackingServiceApiResponse(Long shipmentId, ShipmentDetails shipmentDetails) throws RunnerException {
        // Build tracking request
        TrackingRequest trackingRequest = TrackingRequest.builder()
                .referenceNumber(shipmentDetails.getShipmentId())
                .build();

        // Fetch tracking data
        log.info("Fetching tracking data for shipment ID: {}", shipmentId);
        TrackingServiceApiResponse trackingResponse = trackingServiceAdapter.fetchTrackingData(trackingRequest);

        if (trackingResponse == null || ObjectUtils.isEmpty(trackingResponse.getContainers())) {
            log.warn("No tracking data or containers found for shipment ID: {}", shipmentId);
            return null;
        }
        return trackingResponse;
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateRoutings(RoutingsUpdateRequest routingsUpdateRequest) {
        List<Routings> routings;

        try {
            // Convert the incoming request to a list of Routings entities
            routings = commonUtils.convertToEntityList(routingsUpdateRequest.getRoutingsRequests(), Routings.class);

            // Extract the shipment ID from the first routing entity, if present
            Long shipmentId = routings.stream().findFirst()
                    .map(Routings::getShipmentId).orElse(null);

            // clear Routing Timestamps
            routings.forEach(this::clearRoutingTimestamps);

            // Update routings based on tracking data
            updateRoutingsBasedOnTracking(shipmentId, routings);

        } catch (RuntimeException | RunnerException | ExecutionException | InterruptedException e) { //NOSONAR - Ignoring interrupt intentionally
            log.error("Error updating routings: {}", e.getMessage(), e);
            throw new RoutingException("Failed to update routings", e);
        }

        // Convert Routings to RoutingsResponse
        List<RoutingsResponse> routingsResponses = new ArrayList<>(
                Optional.ofNullable(routingsListToRoutingsResponseList(routings))
                        .orElse(Collections.emptyList()));

        // Sort the responses by leg
        routingsResponses.sort(Comparator.comparingLong(RoutingsResponse::getLeg));

        // Build and return the success response
        return ResponseHelper.buildListSuccessResponse(
                new ArrayList<>(routingsResponses), 0, routingsResponses.size());
    }

    private void createRoutingMap(List<Routings> routings, boolean isPol,
        Map<String, List<Routings>> routingMap) {
            log.info("Starting to create routing map. Total routings: {}", routings.size());
            log.info("Grouping by {} location code.", isPol ? "Pol" : "Pod");

            // Collect all unique Pol and Pod locations
            Set<String> referenceGuids = routings.stream()
                .flatMap(routing -> Stream.of(routing.getPol(), routing.getPod()))
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

            log.debug("Collected unique reference GUIDs from Pol and Pod: {}", referenceGuids);

            // Fetch location data for the collected Pol and Pod
            Map<String, UnlocationsResponse> locationData = masterDataUtils.getLocationData(
                referenceGuids);
            log.debug("Fetched location data: {}", locationData);

            // Iterate over routings and group them by the appropriate location key
            for (Routings routing : routings) {
                String pol = routing.getPol();
                String pod = routing.getPod();

                log.debug("Processing routing: {}", routing);

                // Skip processing if both Pol and Pod are null
                if (pol == null && pod == null) {
                    log.warn(
                        "Skipping routing due to both Pol and Pod being null. Routing details: {}",
                        routing);
                    continue;
                }

                // Determine the location key
                String locationKey = getLocationKey(pol, pod, locationData, isPol);

                if (locationKey != null) {
                    log.debug("Location key for this routing: {}", locationKey);
                    routingMap.computeIfAbsent(locationKey, key -> {
                        log.info("Creating new routing list for location key: {}", key);
                        return new ArrayList<>();
                    }).add(routing);
                    log.debug("Added routing to location key '{}': {}", locationKey, routing);
                } else {
                    log.warn("Missing location key for routing with Pol: {} and Pod: {}", pol, pod);
                }
            }

            log.info("Finished creating routing map. Total groups created: {}. Routing map: {}",
                routingMap.size(), routingMap);
    }

    private String getLocationKey(String pol, String pod, Map<String, UnlocationsResponse> locationData, boolean isPol) {
        log.debug("Fetching location key. isPol = {}", isPol);

        // Fetch location responses based on POL or POD
        UnlocationsResponse polResponse = (pol != null) ? locationData.get(pol) : null;
        UnlocationsResponse podResponse = (pod != null) ? locationData.get(pod) : null;

        log.debug("Pol response for '{}': {}", pol, polResponse);
        log.debug("Pod response for '{}': {}", pod, podResponse);

        // Determine location codes based on availability of airports or sea ports
        String locationCode = isPol ? getLocationCode(polResponse) : getLocationCode(podResponse);

        log.debug("Resolved location code: {}", locationCode);
        return locationCode;
    }

    /**
     * Retrieves the location code if the UnlocationsResponse has valid airport or sea port information.
     *
     * @param response The UnlocationsResponse object containing location details.
     * @return The location code or null if not applicable.
     */
    private String getLocationCode(UnlocationsResponse response) {
        if (response != null && (response.getHasAirport() || response.getHasSeaPort())) {
            return response.getLocCode();
        }
        return null;
    }

    /**
     * Processes container events, mapping places to their respective IDs and updating routing maps based on the events associated with the container.
     *
     * @param tsContainer     The container object containing events and places.
     * @param polToRoutingMap A map of POL codes to their respective list of routings.
     * @param podToRoutingMap A map of POD codes to their respective list of routings.
     */
    private void processContainerEvents(Container tsContainer,
            Map<String, List<Routings>> polToRoutingMap,
            Map<String, List<Routings>> podToRoutingMap) {

        if (ObjectUtils.isEmpty(tsContainer.getEvents())) {
            log.warn("Container has no events, skipping processing.");
            return;
        }

        if (ObjectUtils.isEmpty(tsContainer.getPlaces())) {
            log.warn("Container has no places, skipping processing.");
            return;
        }

        // Log container details for debugging
        log.debug("Processing container with {} events and {} places.",
                tsContainer.getEvents().size(), tsContainer.getPlaces().size());

        // Map places by their ID for quick lookup
        Map<Integer, Place> placeIdToPlaceMap = tsContainer.getPlaces().stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(Place::getId, Function.identity()));

        // Log the number of places mapped
        log.info("Mapped {} places to their respective IDs.", placeIdToPlaceMap.size());

        // Process each event in the container
        for (Event tsEvent : tsContainer.getEvents()) {
            if (tsEvent == null) {
                log.warn("Encountered null event in container, skipping.");
                continue;
            }
            log.debug("Processing event with ID: {}, location: {}", tsEvent.getId(), tsEvent.getLocation());
            updateRoutingsForEvent(tsEvent, placeIdToPlaceMap, polToRoutingMap, podToRoutingMap);
        }
    }

    /**
     * Updates the routings based on the provided event. Determines whether the event is a vessel
     * departure or arrival and updates the POL or POD routing maps accordingly.
     *
     * @param event              The event containing information about the routing updates.
     * @param placeIdToPlaceMap   A map of place IDs to Place objects.
     * @param polToRoutingMap     A map of POL codes to their respective list of routings.
     * @param podToRoutingMap     A map of POD codes to their respective list of routings.
     */
    private void updateRoutingsForEvent(Event event, Map<Integer, Place> placeIdToPlaceMap,
            Map<String, List<Routings>> polToRoutingMap,
            Map<String, List<Routings>> podToRoutingMap) {

        // Log event details for debugging
        log.debug("Processing event: location={}, eventType={}, description={}",
                event.getLocation(), event.getEventType(), event.getDescription());

        Place place = placeIdToPlaceMap.get(event.getLocation());

        if (place == null) {
            log.warn("No place found for location ID: {}, skipping update.", event.getLocation());
            return;
        }

        if (StringUtils.isEmpty(place.getCode())) {
            log.warn("Place code is null or empty for place ID: {}, skipping update.", event.getLocation());
            return;
        }

        String tsPlaceCode = StringUtils.defaultString(place.getCode());
        String tsEventType = event.getEventType();
        String tsEventDescription = event.getDescription();
        DateAndSources actualDateAndSources = event.getActualEventTime();
        DateAndSources projectedDateAndSources = event.getProjectedEventTime();

        // Log the event and place details
        log.info("Updating routing for place code: {}, event type: {}, description: {}",
                tsPlaceCode, tsEventType, tsEventDescription);

        // Update based on event type (departure or arrival)
        if (isVesselDepartureEvent(tsEventType, tsEventDescription)) {
            log.debug("Vessel departure event detected. Updating POL routing map.");
            updateRouting(tsPlaceCode, actualDateAndSources, projectedDateAndSources, polToRoutingMap, true);
        } else if (isVesselArrivalEvent(tsEventType, tsEventDescription)) {
            log.debug("Vessel arrival event detected. Updating POD routing map.");
            updateRouting(tsPlaceCode, actualDateAndSources, projectedDateAndSources, podToRoutingMap, false);
        } else {
            log.info("Event is neither a vessel departure nor arrival. No routing update required.");
        }
    }

    /**
     * Updates routing information based on provided projected and actual dates.
     * The routing will be updated for either POL (Place of Loading) or POD (Place of Discharge)
     * based on the value of the 'isPol' flag.
     *
     * @param tsPlaceCode               The code of the place (POL or POD).
     * @param actualDateAndSources      The actual event date and source information.
     * @param projectedDateAndSources   The projected event date and source information.
     * @param routingMap                A map of place codes to routing lists.
     * @param isPol                     Flag to indicate if the update is for POL (true) or POD (false).
     */
    private void updateRouting(String tsPlaceCode, DateAndSources actualDateAndSources,
            DateAndSources projectedDateAndSources,
            Map<String, List<Routings>> routingMap, boolean isPol) {

        log.debug("Updating routing for place code: {}, isPol: {}", tsPlaceCode, isPol);

        if (tsPlaceCode == null || routingMap == null || routingMap.isEmpty()) {
            log.warn("Invalid input: tsPlaceCode is null or routingMap is empty.");
            return;
        }

        // Retrieve the list of routings for the specified place code
        List<Routings> routings = routingMap.get(tsPlaceCode);

        // Check if any routings exist for the provided place code
        if (routings != null && !routings.isEmpty()) {
            log.info("Found {} routings for place code: {}", routings.size(), tsPlaceCode);

            // Iterate through each routing and update times
            for (Routings routing : routings) {
                updateProjectedTime(routing, projectedDateAndSources, isPol);
                updateActualTime(routing, actualDateAndSources, isPol);
            }

        } else {
            log.warn("No routings found for place code: {}", tsPlaceCode);
        }
    }

    private void clearRoutingTimestamps(Routings routing) {
        routing.setAta(null);
        routing.setEta(null);
        routing.setAtd(null);
        routing.setEtd(null);
    }

    /**
     * Updates the projected date and time (ETD or ETA) for the routing.
     *
     * @param routing                 The routing to be updated.
     * @param projectedDateAndSources The projected date and source information.
     * @param isPol                   Flag to indicate if the update is for POL (true) or POD (false).
     */
    private void updateProjectedTime(Routings routing, DateAndSources projectedDateAndSources, boolean isPol) {
        if (projectedDateAndSources != null && projectedDateAndSources.getDateTime() != null) {
            LocalDateTime projectedDate = projectedDateAndSources.getDateTime();

            if (isPol) {
                routing.setEtd(projectedDate);
                log.debug("Updated ETD for routing {}: {}", routing.getId(), projectedDate);
            } else {
                routing.setEta(projectedDate);
                log.debug("Updated ETA for routing {}: {}", routing.getId(), projectedDate);
            }
        }
    }

    /**
     * Updates the actual date and time (ATD or ATA) for the routing.
     *
     * @param routing              The routing to be updated.
     * @param actualDateAndSources The actual date and source information.
     * @param isPol                Flag to indicate if the update is for POL (true) or POD (false).
     */
    private void updateActualTime(Routings routing, DateAndSources actualDateAndSources, boolean isPol) {
        if (actualDateAndSources != null && actualDateAndSources.getDateTime() != null) {
            LocalDateTime actualDate = actualDateAndSources.getDateTime();

            if (isPol) {
                routing.setAtd(actualDate);
                log.debug("Updated ATD for routing {}: {}", routing.getId(), actualDate);
            } else {
                routing.setAta(actualDate);
                log.debug("Updated ATA for routing {}: {}", routing.getId(), actualDate);
            }
        }
    }

    private boolean isVesselDepartureEvent(String eventType, String description) {
        return EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER.equalsIgnoreCase(eventType)
                && EventConstants.VESSEL_DEPARTURE.equalsIgnoreCase(description);
    }

    private boolean isVesselArrivalEvent(String eventType, String description) {
        return EventConstants.VESSEL_ARRIVAL_WITH_CONTAINER.equalsIgnoreCase(eventType)
                && EventConstants.VESSEL_ARRIVAL.equalsIgnoreCase(description);
    }

    @Override
    public RoutingsResponse routingsToRoutingsResponse(Routings routings) {
        if (routings == null) {
            return null;
        }

        RoutingsResponse.RoutingsResponseBuilder routingsResponse = RoutingsResponse.builder();

        routingsResponse.id(routings.getId());
        routingsResponse.guid(routings.getGuid());
        routingsResponse.shipmentId(routings.getShipmentId());
        routingsResponse.bookingId(routings.getBookingId());
        routingsResponse.leg(routings.getLeg());
        routingsResponse.carriage(routings.getCarriage());
        routingsResponse.mode(routings.getMode());
        routingsResponse.routingStatus(routings.getRoutingStatus());
        routingsResponse.vesselName(routings.getVesselName());
        routingsResponse.pol(routings.getPol());
        routingsResponse.pod(routings.getPod());
        routingsResponse.isDomestic(routings.getIsDomestic());
        routingsResponse.eta(routings.getEta());
        routingsResponse.etd(routings.getEtd());
        routingsResponse.ata(routings.getAta());
        routingsResponse.atd(routings.getAtd());
        routingsResponse.consolidationId(routings.getConsolidationId());
        routingsResponse.isLinked(routings.getIsLinked());
        routingsResponse.voyage(routings.getVoyage());
        routingsResponse.aircraftRegistration(routings.getAircraftRegistration());
        routingsResponse.flightNumber(routings.getFlightNumber());
        routingsResponse.aircraftType(routings.getAircraftType());
        routingsResponse.vehicleNumber(routings.getVehicleNumber());
        routingsResponse.routeLegId(routings.getRouteLegId());
        routingsResponse.transitDays(routings.getTransitDays());
        routingsResponse.carrier(routings.getCarrier());
        routingsResponse.truckReferenceNumber(routings.getTruckReferenceNumber());
        routingsResponse.carrierCountry(routings.getCarrierCountry());
        routingsResponse.isSelectedForDocument(routings.getIsSelectedForDocument());

        return routingsResponse.build();
    }

    @Override
    public List<RoutingsResponse> routingsListToRoutingsResponseList(List<Routings> routings) {
        return (routings == null) ? null : routings.stream()
                .map(this::routingsToRoutingsResponse).toList();
    }

}
