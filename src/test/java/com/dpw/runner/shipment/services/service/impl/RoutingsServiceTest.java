package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.RoutingsDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.DateAndSources;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Place;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingsServiceTest extends CommonMocks {

    @InjectMocks
    private RoutingsService routingsService;

    @Mock
    private RoutingsDao routingsDao;

    @Mock
    private TrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private ShipmentDao shipmentDao;

    private Routings routings;
    private List<Routings> routingsList;
    private ShipmentDetails shipmentDetails;
    private Container container;
    private Event event;
    private Event event2;
    private Place place;
    private Place place2;
    private TrackingServiceApiResponse trackingServiceApiResponse;

    @BeforeEach
    void setUp() {
        routings = new Routings();
        routings.setPol("POL1");
        routings.setPod("POD1");
        routingsList = List.of(routings);

        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("SH123");

        event = new Event();
        event.setLocation(1);
        event.setEventType(Constants.VESSELDEPARTUREWITHCONTAINER);
        event.setDescription(Constants.VESSEL_DEPARTURE);
        event.setDescriptionFromSource(Constants.VESSEL_DEPARTURE_FROM_POL);
        event.setProjectedEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());
        event.setActualEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());

        event2 = new Event();
        event2.setLocation(2);
        event2.setEventType(Constants.VESSELARRIVALWITHCONTAINER);
        event2.setDescription(Constants.VESSEL_ARRIVAL);
        event2.setDescriptionFromSource(Constants.VESSEL_ARRIVAL_AT_POD);
        event2.setProjectedEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());
        event2.setActualEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());

        container = new Container();
        container.setEvents(List.of(event, event2));

        place = new Place();
        place.setId(1);
        place.setCode("POL1");

        place2 = new Place();
        place2.setId(2);
        place2.setCode("POD1");

        container.setPlaces(List.of(place, place2));

        trackingServiceApiResponse = new TrackingServiceApiResponse();
        trackingServiceApiResponse.setContainers(List.of(container));
    }

    @Test
    void testUpdateEntityFromShipment_withOldEntityList() throws RunnerException {
        Long shipmentId = 123L;
        List<Routings> oldEntityList = List.of(new Routings());
        List<Routings> updatedRoutings = List.of(new Routings());

        Mockito.when(routingsDao.updateEntityFromShipment(routingsList, shipmentId, oldEntityList)).thenReturn(routingsList);
        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(trackingServiceApiResponse);

        List<Routings> result = routingsService.updateEntityFromShipment(routingsList, shipmentId, oldEntityList);

        assertEquals(updatedRoutings, result);
        assertNotNull(result.get(0).getEtd());
        assertNotNull(result.get(0).getAtd());
    }

    @Test
    void testUpdateEntityFromShipment_withoutOldEntityList() throws RunnerException {
        Long shipmentId = 123L;
        List<Routings> updatedRoutings = List.of(new Routings());

        Mockito.when(routingsDao.updateEntityFromShipment(routingsList, shipmentId)).thenReturn(routingsList);
        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(trackingServiceApiResponse);

        List<Routings> result = routingsService.updateEntityFromShipment(routingsList, shipmentId);

        assertEquals(updatedRoutings, result);
        assertNotNull(result.get(0).getEtd());
        assertNotNull(result.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_validData() throws RunnerException {
        Long shipmentId = 123L;

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(trackingServiceApiResponse);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNotNull(routingsList.get(0).getEtd());
        assertNotNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_nullShipmentDetails() throws RunnerException {
        Long shipmentId = 123L;

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_emptyTrackingServiceResponse() throws RunnerException {
        Long shipmentId = 123L;

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(null);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_noContainers() throws RunnerException {
        Long shipmentId = 123L;
        TrackingServiceApiResponse emptyResponse = new TrackingServiceApiResponse();
        emptyResponse.setContainers(null);

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(emptyResponse);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_noEventsOrPlacesInContainer() throws RunnerException {
        Long shipmentId = 123L;
        Container containerWithoutEventsAndPlaces = new Container();
        containerWithoutEventsAndPlaces.setEvents(null);
        containerWithoutEventsAndPlaces.setPlaces(null);
        TrackingServiceApiResponse responseWithoutEventsAndPlaces = new TrackingServiceApiResponse();
        responseWithoutEventsAndPlaces.setContainers(List.of(containerWithoutEventsAndPlaces));

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(Mockito.any())).thenReturn(responseWithoutEventsAndPlaces);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

}
