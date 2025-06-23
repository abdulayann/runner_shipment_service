package com.dpw.runner.shipment.services.adapters;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.config.TrackingServiceConfig;
import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Journey;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TrackingServiceAdapterTest {

    @Mock
    private ISBUtils sbUtils;

    @Mock
    private ISBProperties isbProperties;

    @Mock
    private TrackingServiceConfig trackingServiceConfig;

    @Mock
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private ConsolidationDao consolidationDao;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private CommonUtils commonUtils;

    @InjectMocks
    private TrackingServiceAdapter trackingServiceAdapter;

    private static JsonTestUtility jsonTestUtility;

    private static ObjectMapper objectMapperTest;

    private static TrackingServiceApiResponse trackingServiceApiResponse;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void mapEventDetailsForTracking() {
        assertDoesNotThrow(() -> trackingServiceAdapter.mapEventDetailsForTracking("str1", "str2", "str3", List.of(jsonTestUtility.getTestEventData())));
    }

    @Test
    void mapEvents() {
        assertDoesNotThrow(() -> trackingServiceAdapter.mapEvents(List.of(jsonTestUtility.getTestEventData())));
    }

    @Test
    void publishUpdatesToTrackingServiceQueue() {
        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue("strJson", true);
        verify(trackingServiceConfig, times(1)).getEventsMessageTopic();
    }

    @Test
    void publishUpdatesToTrackingServiceQueue_() {
        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue("strJson", false);
        verify(trackingServiceConfig, times(1)).getRunnerFlowMessageTopic();
    }

    @Test
    void checkIfConsolAttached() {
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getTestConsolidation()));
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainersList(Set.of(jsonTestUtility.getTestContainer()));
        boolean res = trackingServiceAdapter.checkIfConsolAttached(shipmentDetails);
        assertTrue(res);
    }

    @Test
    void checkIfConsolAttached_AIR() {
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainersList(Set.of(jsonTestUtility.getTestContainer()));
        boolean res = trackingServiceAdapter.checkIfConsolAttached(shipmentDetails);
        assertFalse(res);
    }

    @Test
    void checkIfConsolAttached_SEA() {
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Collections.emptyList());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setMasterBill("MasterBill");
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setContainersList(Set.of(jsonTestUtility.getTestContainer()));
        boolean res = trackingServiceAdapter.checkIfConsolAttached(shipmentDetails);
        assertTrue(res);
    }

    @Test
    void checkIfConsolAttached_Failure() {
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(consolidationDao.findById(any())).thenThrow(new RuntimeException());
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setContainersList(Set.of(jsonTestUtility.getTestContainer()));
        boolean res = trackingServiceAdapter.checkIfConsolAttached(shipmentDetails);
        assertFalse(res);
    }

    @Test
    void checkIfAwbExists_NoMappingFound() {
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(new ArrayList<>());
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        boolean res = trackingServiceAdapter.checkIfAwbExists(consolidationDetails);
        assertFalse(res);
    }

    @Test
    void checkIfAwbExists() {
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getTestShipment()));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        boolean res = trackingServiceAdapter.checkIfAwbExists(consolidationDetails);
        assertFalse(res);
    }

    @Test
    void checkIfAwbExists2() {
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        boolean res = trackingServiceAdapter.checkIfAwbExists(consolidationDetails);
        assertFalse(res);
    }

    @Test
    void checkIfConsolContainersExist() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        boolean res = trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails);
        assertTrue(res);
    }

    @Test
    void mapConsoleDataToTrackingServiceData() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(new ArrayList<>());
        UniversalTrackingPayload res = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails, new ShipmentDetails());
        assertNotNull(res);
    }

    @Test
    void mapConsoleDataToTrackingServiceData_MappingFound() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        lenient().when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        lenient().when(shipmentDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getTestShipment()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        lenient().when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        lenient().when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(new ArrayList<>());
        UniversalTrackingPayload res = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails, new ShipmentDetails());
        assertNotNull(res);
    }

    @Test
    void mapShipmentDataToTrackingServiceData() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(new UnlocationsResponse()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().identifier1("APL").build()));
        UniversalTrackingPayload response = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
        assertNotNull(response);
        assertEquals("APL", response.getCarrier());
    }

    @Test
    void mapShipmentDataToTrackingServiceData_MappingFound() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDao.findById(any())).thenReturn(Optional.empty());
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(new UnlocationsResponse()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().identifier1("APL").build()));
        UniversalTrackingPayload response = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
        assertNotNull(response);
        assertEquals("APL", response.getCarrier());
    }

    @Test
    void getAllEvents() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        when(consolidationDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        List<Events> events = trackingServiceAdapter.getAllEvents(shipmentDetails, consolidationDetails, "refNum");
        assertNotNull(events);
        assertEquals(new ArrayList<>(), events);
    }

    @Test
    void getTrackingEvents() {
        String refNumber = "refNum";
        TrackingServiceApiResponse response = new TrackingServiceApiResponse();
        TrackingServiceApiResponse mockResponse = jsonTestUtility.getJson("TRACKING_SERVICE_SHIPMENT_RESPONSE", TrackingServiceApiResponse.class);

        try {
            when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(TrackingServiceApiResponse.class))).thenReturn(ResponseEntity.ok(mockResponse));
            var trackingEventsResponse = trackingServiceAdapter.getTrackingEventsResponse(refNumber);

            assertNotNull(trackingEventsResponse.getEventsList());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void getTrackingEventsThrowsException() {
        String refNumber = "refNum";
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(TrackingServiceApiResponse.class))).
                thenThrow(new RuntimeException());

        assertThrows(RunnerException.class, () -> trackingServiceAdapter.getTrackingEventsResponse(refNumber));
    }

    @Test
    void getTrackingEventsShouldNotFailForEmptyTrackingResponse() {
        String refNumber = "refNum";
        TrackingServiceApiResponse mockResponse = jsonTestUtility.getJson("TRACKING_SERVICE_SHIPMENT_RESPONSE", TrackingServiceApiResponse.class);
        mockResponse.setContainers(Collections.emptyList());

        try {
            when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(TrackingServiceApiResponse.class))).thenReturn(ResponseEntity.ok(mockResponse));
            var trackingEventsResponse = trackingServiceAdapter.getTrackingEventsResponse(refNumber);

            assertNotNull(trackingEventsResponse.getEventsList());
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void convertTrackingEventCodeToShortCode_FlightArrival() {
        Event event = new Event();
        event.setLocationRole("someRole");
        event.setEventType(EventConstants.FLIGHT_ARRIVAL);
        event.setDescription("Flight Arrival");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.FLAR, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_FlightDeparture() {
        Event event = new Event();
        event.setLocationRole("someRole");
        event.setEventType(EventConstants.FLIGHT_DEPARTURE);
        event.setDescription("Flight Departure");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.FLDR, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_Literal_ReceivedFromFlight() {
        Event event = new Event();
        event.setLocationRole("someRole");
        event.setEventType(EventConstants.LITERAL);
        event.setDescription("Received from Flight");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.TRCF, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_Literal_ConsigneeNotified() {
        Event event = new Event();
        event.setLocationRole("someRole");
        event.setEventType(EventConstants.LITERAL);
        event.setDescription("Consignee notified");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.TNFD, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_GateInWithContainerEmpty_Origin() {
        Event event = new Event();
        event.setLocationRole(EventConstants.ORIGIN);
        event.setEventType(EventConstants.GATE_IN_WITH_CONTAINER_EMPTY);
        event.setDescription("some description");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.ECPK, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_NoMatch() {
        Event event = new Event();
        event.setLocationRole("someRole");
        event.setEventType("UNKNOWN_EVENT");
        event.setDescription("some description");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals("UNKNOWN_EVENT", result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_TRCS() {
        // Scenario: EventCode is LITERAL and description is "Received from Shipper"
        Event event = new Event();
        event.setLocationRole("");
        event.setEventType(EventConstants.LITERAL);
        event.setDescription("Received from Shipper");

        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();

        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        assertEquals(EventConstants.TRCS, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_VSDP() {
        Event event = new Event();
        event.setEventType(EventConstants.LOAD_ON_VESSEL);
        event.setDescriptionFromSource(EventConstants.EXPORT_LOADED_ON_VESSEL);
        event.setLocationRole(EventConstants.ORIGIN_PORT);
        Container container = Container.builder()
                .journey(Journey.builder().scacCode(EventConstants.MSCU).build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        // Expect VSDP to be returned
        assertEquals(EventConstants.VSDP, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_FUGO() {
        // Scenario: EventCode is GATE_OUT_WITH_CONTAINER_FULL and locationRole is "destinationPort"
        Event event = new Event();
        event.setLocationRole("destinationPort");
        event.setEventType(EventConstants.GATE_OUT_WITH_CONTAINER_FULL);
        event.setDescription("");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        // Expect FUGO to be returned
        assertEquals(EventConstants.FUGO, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_EMCR() {
        // Scenario: EventCode is GATE_IN_WITH_CONTAINER_EMPTY and locationRole starts with "DESTINATION"
        Event event = new Event();
        event.setLocationRole(EventConstants.DESTINATION);
        event.setEventType(EventConstants.GATE_IN_WITH_CONTAINER_EMPTY);
        event.setDescription("");
        Container container = Container.builder()
                .journey(Journey.builder().scacCode("").build())
                .events(List.of(event)).build();
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(event, container);

        // Expect EMCR to be returned
        assertEquals(EventConstants.EMCR, result);
    }

    @Test
    void mapShipmentDataToTrackingServiceDataWhenUserIsNull() {
        UserContext.setUser(null);
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();

        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(new UnlocationsResponse()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().identifier1("APL").build()));
        UniversalTrackingPayload response = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
        assertNotNull(response);
        assertEquals("APL", response.getCarrier());
        assertNull(response.getShipmentDetails().get(0).getCountryCode());
    }

    @Test
    void mapShipmentDataToTrackingServiceDataPopulatesCountryCodeFromBranchCountry() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantCountryCode("AUS");
        UserContext.setUser(mockUser);
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();

        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(new UnlocationsResponse()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder()
                .data(new ArrayList<>())
                .build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().identifier1("APL").build()));
        UniversalTrackingPayload response = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
        assertNotNull(response);
        assertEquals("APL", response.getCarrier());
        assertEquals("AU", response.getShipmentDetails().get(0).getCountryCode());
    }

    @Test
    void testSetBookingReference_whenShipmentIsAPI_andRequestFromShipmentFalse_andConsolNotNull() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setSource("API");

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setReferenceNumber("CONSOL_REF_123");

        UniversalTrackingPayload trackingPayload = new UniversalTrackingPayload();

        trackingServiceAdapter.setBookingReferenceNumberInTrackingPayload(consol, shipment, false, trackingPayload);

        assertEquals("CONSOL_REF_123", trackingPayload.getBookingReferenceNumber());
    }

    @Test
    void testSetBookingReference_whenShipmentIsAPI_andRequestFromShipmentFalse_andConsolNull() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setSource("API");

        UniversalTrackingPayload trackingPayload = new UniversalTrackingPayload();

        trackingServiceAdapter.setBookingReferenceNumberInTrackingPayload(null, shipment, false, trackingPayload);

        assertNull(trackingPayload.getBookingReferenceNumber());
    }

    @Test
    void testSetBookingReference_whenShipmentIsAPI_andRequestFromShipmentTrue() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setSource("API");
        shipment.setBookingReference("BOOK_REF_456");

        UniversalTrackingPayload trackingPayload = new UniversalTrackingPayload();

        trackingServiceAdapter.setBookingReferenceNumberInTrackingPayload(new ConsolidationDetails(), shipment, true, trackingPayload);

        assertEquals("BOOK_REF_456", trackingPayload.getBookingReferenceNumber());
    }

    @Test
    void testSetBookingReference_whenShipmentSourceIsNotAPI() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setSource("MANUAL");

        UniversalTrackingPayload trackingPayload = new UniversalTrackingPayload();

        trackingServiceAdapter.setBookingReferenceNumberInTrackingPayload(new ConsolidationDetails(), shipment, true, trackingPayload);

        assertNull(trackingPayload.getBookingReferenceNumber());
    }

    @Test
    void testSetBookingReference_whenShipmentIsNull() {
        UniversalTrackingPayload trackingPayload = new UniversalTrackingPayload();

        trackingServiceAdapter.setBookingReferenceNumberInTrackingPayload(new ConsolidationDetails(), null, false, trackingPayload);

        assertNull(trackingPayload.getBookingReferenceNumber());
    }

}
