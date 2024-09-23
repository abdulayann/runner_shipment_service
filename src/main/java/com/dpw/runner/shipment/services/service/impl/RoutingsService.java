package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
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
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
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
    private CommonUtils commonUtils;

    @Override
    public void updateRoutingsBasedOnTracking(Long shipmentId, List<Routings> routings) throws RunnerException {
        if (shipmentId == null || routings == null || routings.isEmpty()) {
            return;
        }

        // Fetch shipment details
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).orElse(null);
        if (shipmentDetails == null) {
            return;
        }

        // Fetch tracking data
        TrackingServiceApiResponse trackingServiceApiResponse = trackingServiceAdapter.fetchTrackingData(
                TrackingRequest.builder().referenceNumber(shipmentDetails.getShipmentId()).build());

        if (trackingServiceApiResponse == null || ObjectUtils.isEmpty(trackingServiceApiResponse.getContainers())) return;

        // Create lookup maps for quicker routing updates
        Map<String, List<Routings>> polToRoutingMap = createRoutingMap(routings, true);
        Map<String, List<Routings>> podToRoutingMap = createRoutingMap(routings, false);

        // Process each container's events
        for (Container tsContainer : trackingServiceApiResponse.getContainers()) {
            processContainerEvents(tsContainer, polToRoutingMap, podToRoutingMap);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateRoutings(RoutingsUpdateRequest routingsUpdateRequest) {

        List<Routings> routings;
        try {
            routings = commonUtils.convertToEntityList(routingsUpdateRequest.getRoutingsRequests(), Routings.class);
            Long shipmentId = routings.stream().findFirst().map(Routings::getShipmentId).orElse(null);

            updateRoutingsBasedOnTracking(shipmentId, routings);
        } catch (RuntimeException | RunnerException e) {
            throw new RoutingException(e);
        }
        List<RoutingsResponse> routingsResponses = new ArrayList<>(
                Optional.ofNullable(routingsListToRoutingsResponseList(routings))
                        .orElse(Collections.emptyList())
        );
        routingsResponses.sort(Comparator.comparingLong(RoutingsResponse::getLeg));

        return ResponseHelper.buildListSuccessResponse(
                new ArrayList<>(routingsResponses), 0, routingsResponses.size());
    }

    private Map<String, List<Routings>> createRoutingMap(List<Routings> routings, boolean isPol) {
        return routings.stream()
                .filter(routing -> isPol ? routing.getPol() != null : routing.getPod() != null)
                .collect(Collectors.groupingBy(
                        routing -> isPol ? routing.getPol() : routing.getPod()
                ));
    }

    private void processContainerEvents(Container tsContainer, Map<String, List<Routings>> polToRoutingMap, Map<String, List<Routings>> podToRoutingMap) {
        if (tsContainer == null || ObjectUtils.isEmpty(tsContainer.getEvents()) || ObjectUtils.isEmpty(tsContainer.getPlaces())) return;

        Map<Integer, Place> placeIdToPlaceMap = tsContainer.getPlaces().stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(Place::getId, Function.identity()));

        for (Event tsEvent : tsContainer.getEvents()) {
            updateRoutingsForEvent(tsEvent, placeIdToPlaceMap, polToRoutingMap, podToRoutingMap);
        }
    }

    private void updateRoutingsForEvent(Event event, Map<Integer, Place> placeIdToPlaceMap,
            Map<String, List<Routings>> polToRoutingMap, Map<String, List<Routings>> podToRoutingMap) {
        if (event == null) return;

        Place place = placeIdToPlaceMap.get(event.getLocation());
        if (place == null || place.getCode() == null) return;

        String tsPlaceCode = StringUtils.defaultString(place.getCode())+EventConstants._POR;
        String tsEventType = event.getEventType();
        String tsEventDescription = event.getDescription();
        DateAndSources actualDateAndSources = event.getActualEventTime();
        DateAndSources projectedDateAndSources = event.getProjectedEventTime();

        if (isVesselDepartureEvent(tsEventType, tsEventDescription)) {
            updateRouting(tsPlaceCode, actualDateAndSources, projectedDateAndSources, polToRoutingMap,
                    Routings::setEtd, Routings::setAtd);
        } else if (isVesselArrivalEvent(tsEventType, tsEventDescription)) {
            updateRouting(tsPlaceCode, actualDateAndSources, projectedDateAndSources, podToRoutingMap,
                    Routings::setEta, Routings::setAta);
        }
    }

    private void updateRouting(String tsPlaceCode, DateAndSources actualDateAndSources, DateAndSources projectedDateAndSources,
            Map<String, List<Routings>> routingMap,
            BiConsumer<Routings, LocalDateTime> setProjectedTime,
            BiConsumer<Routings, LocalDateTime> setActualTime) {

        List<Routings> routings = routingMap.get(tsPlaceCode);

        if (routings != null) {
            for (Routings routing : routings) {
                if (projectedDateAndSources != null && projectedDateAndSources.getDateTime() != null) {
                    setProjectedTime.accept(routing, projectedDateAndSources.getDateTime());
                }
                if (actualDateAndSources != null && actualDateAndSources.getDateTime() != null) {
                    setActualTime.accept(routing, actualDateAndSources.getDateTime());
                }
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

        return routingsResponse.build();
    }

    @Override
    public List<RoutingsResponse> routingsListToRoutingsResponseList(List<Routings> routings) {
        return (routings == null) ? null : routings.stream()
                .map(this::routingsToRoutingsResponse).toList();
    }

}
