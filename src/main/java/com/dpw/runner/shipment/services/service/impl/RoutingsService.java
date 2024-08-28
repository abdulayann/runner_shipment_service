package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.DateAndSources;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Place;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class RoutingsService implements IRoutingsService {

    private final IRoutingsDao routingsDao;
    private final ITrackingServiceAdapter trackingServiceAdapter;
    private final ShipmentDao shipmentDao;

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws RunnerException {
        List<Routings> routings = routingsDao.updateEntityFromShipment(routingsList, shipmentId, oldEntityList);
        updateRoutingsBasedOnTracking(shipmentId, routings);
        return routings;
    }

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws RunnerException {
        List<Routings> routings = routingsDao.updateEntityFromShipment(routingsList, shipmentId);
        updateRoutingsBasedOnTracking(shipmentId, routings);
        return routings;
    }

    @Override
    public void updateRoutingsBasedOnTracking(Long shipmentId, List<Routings> routings) throws RunnerException {
        if (shipmentId == null || routings == null || routings.isEmpty()) return;

        // Fetch shipment details
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).orElse(null);
        if (shipmentDetails == null) return;

        // Fetch tracking data
        TrackingServiceApiResponse trackingServiceApiResponse = trackingServiceAdapter.fetchTrackingData(
                TrackingRequest.builder().referenceNumber(shipmentDetails.getShipmentId()).build());

        if (trackingServiceApiResponse == null || ObjectUtils.isEmpty(trackingServiceApiResponse.getContainers())) return;

        // Create lookup maps for quicker routing updates
        Map<String, Routings> polToRoutingMap = createRoutingMap(routings, true);
        Map<String, Routings> podToRoutingMap = createRoutingMap(routings, false);

        // Process each container's events
        for (Container container : trackingServiceApiResponse.getContainers()) {
            processContainerEvents(container, polToRoutingMap, podToRoutingMap);
        }
    }

    private Map<String, Routings> createRoutingMap(List<Routings> routings, boolean isPol) {
        return routings.stream()
                .filter(routing -> isPol ? routing.getPol() != null : routing.getPod() != null)
                .collect(Collectors.toMap(
                        routing -> isPol ? routing.getPol() : routing.getPod(),
                        Function.identity(),
                        (a, b) -> a));
    }

    private void processContainerEvents(Container container, Map<String, Routings> polToRoutingMap, Map<String, Routings> podToRoutingMap) {
        if (container == null || ObjectUtils.isEmpty(container.getEvents()) || ObjectUtils.isEmpty(container.getPlaces())) return;

        Map<Integer, Place> placeIdToPlaceMap = container.getPlaces().stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(Place::getId, Function.identity()));

        for (Event event : container.getEvents()) {
            updateRoutingsForEvent(event, placeIdToPlaceMap, polToRoutingMap, podToRoutingMap);
        }
    }

    private void updateRoutingsForEvent(Event event, Map<Integer, Place> placeIdToPlaceMap,
            Map<String, Routings> polToRoutingMap, Map<String, Routings> podToRoutingMap) {
        if (event == null) return;

        Place place = placeIdToPlaceMap.get(event.getLocation());
        if (place == null || place.getCode() == null) return;

        String code = place.getCode();
        String eventType = event.getEventType();
        String description = event.getDescription();
        String descriptionFromSource = event.getDescriptionFromSource();

        if (isVesselDepartureEvent(eventType, description, descriptionFromSource)) {
            updatePolRouting(code, event.getProjectedEventTime(), polToRoutingMap);
        } else if (isVesselArrivalEvent(eventType, description, descriptionFromSource)) {
            updatePodRouting(code, event.getActualEventTime(), podToRoutingMap);
        }
    }

    private void updatePolRouting(String code, DateAndSources projectedEventTime, Map<String, Routings> polToRoutingMap) {
        if (projectedEventTime == null) return;

        LocalDateTime eventTime = projectedEventTime.getDateTime();
        Routings routing = polToRoutingMap.get(code);
        if (routing != null) {
            routing.setEtd(routing.getEtd() == null ? eventTime : routing.getEtd());
            routing.setAtd(routing.getAtd() == null ? eventTime : routing.getAtd());
        }
    }

    private void updatePodRouting(String code, DateAndSources actualEventTime, Map<String, Routings> podToRoutingMap) {
        if (actualEventTime == null) return;

        LocalDateTime eventTime = actualEventTime.getDateTime();
        Routings routing = podToRoutingMap.get(code);
        if (routing != null) {
            routing.setEta(routing.getEta() == null ? eventTime : routing.getEta());
            routing.setAta(routing.getAta() == null ? eventTime : routing.getAta());
        }
    }

    private boolean isVesselDepartureEvent(String eventType, String description, String descriptionFromSource) {
        return Constants.VESSELDEPARTUREWITHCONTAINER.equalsIgnoreCase(eventType)
                && Constants.VESSEL_DEPARTURE.equalsIgnoreCase(description)
                && (Constants.VESSEL_DEPARTURE_FROM_POL.equalsIgnoreCase(descriptionFromSource)
                || Constants.VESSEL_DEPARTURE_FROM_TS_PORT.equalsIgnoreCase(descriptionFromSource));
    }

    private boolean isVesselArrivalEvent(String eventType, String description, String descriptionFromSource) {
        return Constants.VESSELARRIVALWITHCONTAINER.equalsIgnoreCase(eventType)
                && Constants.VESSEL_ARRIVAL.equalsIgnoreCase(description)
                && (Constants.VESSEL_ARRIVAL_AT_TS_PORT.equalsIgnoreCase(descriptionFromSource)
                || Constants.VESSEL_ARRIVAL_AT_POD.equalsIgnoreCase(descriptionFromSource));
    }
}
