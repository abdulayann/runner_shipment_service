package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.impl.RoutingsDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsUpdateRequest;
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
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingsServiceTest extends CommonMocks {

    @InjectMocks
    private RoutingsService routingsService;

    @Mock
    private RoutingsDao routingsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private TrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private ShipmentDao shipmentDao;

    private Routings routings1;
    private Routings routings2;
    private List<Routings> routingsList;
    private ShipmentDetails shipmentDetails;
    private Container container;
    private Event event;
    private Event event2;
    private Place place;
    private Place place2;
    private TrackingServiceApiResponse trackingServiceApiResponse;
    private RoutingsUpdateRequest routingsUpdateRequest;
    private RoutingsRequest routingsRequest;
    private List<RoutingsRequest> routingsRequests;
    private List<RoutingsResponse> routingsResponseList;
    private ExecutorService executorService = Executors.newFixedThreadPool(10);


    @BeforeEach
    void setUp() {
        routings1 = new Routings();
        routings1.setPol("POL1_POR");
        routings1.setPod("POD1_POR");
        routings1.setShipmentId(123L);
        routings1.setLeg(1L);

        routings2 = new Routings();
        routings2.setPol("POL2_POR");
        routings2.setPod("POD2_POR");
        routings2.setShipmentId(234L);
        routings2.setLeg(2L);

        routingsList = List.of(routings1, routings2);

        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("SH123");

        event = new Event();
        event.setLocation(1);
        event.setEventType(EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER);
        event.setDescription(EventConstants.VESSEL_DEPARTURE);
        event.setDescriptionFromSource(EventConstants.VESSEL_DEPARTURE_FROM_POL);
        event.setProjectedEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());
        event.setActualEventTime(DateAndSources.builder().dateTime(LocalDateTime.MIN).build());

        event2 = new Event();
        event2.setLocation(2);
        event2.setEventType(EventConstants.VESSEL_ARRIVAL_WITH_CONTAINER);
        event2.setDescription(EventConstants.VESSEL_ARRIVAL);
        event2.setDescriptionFromSource(EventConstants.VESSEL_ARRIVAL_AT_POD);
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

        routingsRequest = new RoutingsRequest();
        routingsRequest.setShipmentId(123L);

        routingsRequests = new ArrayList<>();
        routingsRequests.add(routingsRequest);

        routingsUpdateRequest = new RoutingsUpdateRequest();
        routingsUpdateRequest.setRoutingsRequests(routingsRequests);

        routingsResponseList = List.of(new RoutingsResponse());
        routingsService.executorServiceRouting = executorService;
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_validData()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;
        Set<String> referenceGuids = routingsList.stream()
                .flatMap(routing -> Stream.of(routing.getPol(), routing.getPod()))
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        UnlocationsResponse unlocationsResponse = UnlocationsResponse.builder()
                .locationsReferenceGUID("POL1_POR")
                .locCode("POL1")
                .hasAirport(true).build();
        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("POL1_POR",unlocationsResponse);

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(trackingServiceApiResponse);
        Runnable mockedRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any())).thenReturn(mockedRunnable);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        verify(masterDataUtils, times(2)).withMdc(any());
        verify(mockedRunnable, times(2)).run();
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_nullShipmentDetails()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_nullRoutings()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;
         List<Routings> vRoutings = new ArrayList<>();

        routingsService.updateRoutingsBasedOnTracking(shipmentId, vRoutings);

        assertEquals(0, vRoutings.size());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_nullPolAndPod()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;
        Routings routings = new Routings();
        List<Routings> routingsList = new ArrayList<>();
        routingsList.add(routings);
        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
        assertNull(routingsList.get(0).getAta());
        assertNull(routingsList.get(0).getEta());

    }

    @Test
    void testUpdateRoutingsBasedOnTracking_emptyTrackingServiceResponse()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(null);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_noContainers()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;
        TrackingServiceApiResponse emptyResponse = new TrackingServiceApiResponse();
        emptyResponse.setContainers(null);

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(emptyResponse);

        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }

    @Test
    void testUpdateRoutingsBasedOnTracking_noEventsOrPlacesInContainer()
        throws RunnerException, ExecutionException, InterruptedException {
        Long shipmentId = 123L;
        Container containerWithoutEventsAndPlaces = new Container();
        containerWithoutEventsAndPlaces.setEvents(null);
        containerWithoutEventsAndPlaces.setPlaces(null);
        TrackingServiceApiResponse responseWithoutEventsAndPlaces = new TrackingServiceApiResponse();
        responseWithoutEventsAndPlaces.setContainers(List.of(containerWithoutEventsAndPlaces));
        Set<String> referenceGuids = routingsList.stream()
                .flatMap(routing -> Stream.of(routing.getPol(), routing.getPod()))
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(responseWithoutEventsAndPlaces);
        Runnable mockedRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any())).thenReturn(mockedRunnable);
        routingsService.updateRoutingsBasedOnTracking(shipmentId, routingsList);

        assertNull(routingsList.get(0).getEtd());
        assertNull(routingsList.get(0).getAtd());
    }
    @Test
    void testUpdateRoutings_success() throws RunnerException {
        Set<String> referenceGuids = routingsList.stream()
                .flatMap(routing -> Stream.of(routing.getPol(), routing.getPod()))
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();

        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(trackingServiceApiResponse);
        Mockito.when(commonUtils.convertToEntityList(Mockito.anyList(), Mockito.eq(Routings.class)))
                .thenReturn(routingsList);
        Mockito.when(shipmentDao.findById(Mockito.anyLong())).thenReturn(Optional.of(shipmentDetails));
        Runnable mockedRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any())).thenReturn(mockedRunnable);

        ResponseEntity<IRunnerResponse> response = routingsService.updateRoutings(routingsUpdateRequest);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testUpdateRoutings_trackingException() throws RunnerException {
        Mockito.when(commonUtils.convertToEntityList(Mockito.anyList(), Mockito.eq(Routings.class)))
                .thenReturn(routingsList);
        Mockito.when(shipmentDao.findById(Mockito.anyLong())).thenReturn(Optional.of(shipmentDetails));
        Mockito.doThrow(new RunnerException("Tracking update failed"))
                .when(trackingServiceAdapter).fetchTrackingData(any());

        assertThrows(RoutingException.class, () -> routingsService.updateRoutings(routingsUpdateRequest));
    }

    @Test
    void testUpdateRoutings_emptyRoutingsList() {
        routingsUpdateRequest.setRoutingsRequests(new ArrayList<>());

        Mockito.when(commonUtils.convertToEntityList(Mockito.anyList(), Mockito.eq(Routings.class)))
                .thenReturn(new ArrayList<>());

        ResponseEntity<IRunnerResponse> response = routingsService.updateRoutings(routingsUpdateRequest);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testUpdateRoutings_runtimeException() {
        Mockito.when(commonUtils.convertToEntityList(Mockito.anyList(), Mockito.eq(Routings.class)))
                .thenThrow(new RuntimeException("Conversion failed"));

        assertThrows(RoutingException.class, () -> routingsService.updateRoutings(routingsUpdateRequest));
    }

    @Test
    void testUpdateRoutings_sortingOrder() throws RunnerException {
        // Arrange
        Routings secondRoutings = new Routings();
        secondRoutings.setShipmentId(1L);
        secondRoutings.setLeg(2L);

        var x = new ArrayList<>(routingsList);
        x.add(secondRoutings);
        Set<String> referenceGuids = routingsList.stream()
                .flatMap(routing -> Stream.of(routing.getPol(), routing.getPod()))
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(trackingServiceApiResponse);
        Mockito.when(commonUtils.convertToEntityList(routingsUpdateRequest.getRoutingsRequests(), Routings.class))
                .thenReturn(routingsList);
        Runnable mockedRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any())).thenReturn(mockedRunnable);

        // Act
        ResponseEntity<IRunnerResponse> response = routingsService.updateRoutings(routingsUpdateRequest);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        // Assuming the IRunnerResponse body is a specific implementation that can be cast or adapted
        List<RoutingsResponse> responseData = ((RunnerListResponse) response.getBody()).getData();

        assertNotNull(responseData);
        assertFalse(responseData.isEmpty());
        assertEquals(1L, responseData.get(0).getLeg());
        assertEquals(2L, responseData.get(1).getLeg());
    }


}
