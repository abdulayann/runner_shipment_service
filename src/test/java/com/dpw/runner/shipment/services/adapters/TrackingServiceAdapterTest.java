package com.dpw.runner.shipment.services.adapters;

import static org.junit.jupiter.api.Assertions.*;
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
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
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
        shipmentDetails.setContainersList(List.of(jsonTestUtility.getTestContainer()));
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
        shipmentDetails.setContainersList(List.of(jsonTestUtility.getTestContainer()));
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
        shipmentDetails.setContainersList(List.of(jsonTestUtility.getTestContainer()));
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
        shipmentDetails.setContainersList(List.of(jsonTestUtility.getTestContainer()));
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
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                "someRole", EventConstants.FLIGHT_ARRIVAL, "Flight Arrival");

        assertEquals(EventConstants.FLAR, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_FlightDeparture() {
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                "someRole", EventConstants.FLIGHT_DEPARTURE, "Flight Departure");

        assertEquals(EventConstants.FLDR, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_Literal_ReceivedFromFlight() {
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                "someRole", EventConstants.LITERAL, "Received from Flight");

        assertEquals(EventConstants.TRCF, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_Literal_ConsigneeNotified() {
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                "someRole", EventConstants.LITERAL, "Consignee notified");

        assertEquals(EventConstants.TNFD, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_GateInWithContainerEmpty_Origin() {
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                EventConstants.ORIGIN, EventConstants.GATE_IN_WITH_CONTAINER_EMPTY, "some description");

        assertEquals(EventConstants.ECPK, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_NoMatch() {
        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(
                "someRole", "UNKNOWN_EVENT", "some description");

        assertEquals("UNKNOWN_EVENT", result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_TRCS() {
        // Scenario: EventCode is LITERAL and description is "Received from Shipper"
        String eventCode = EventConstants.LITERAL;
        String locationRole = "";
        String description = "Received from Shipper";

        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(locationRole, eventCode, description);

        assertEquals(EventConstants.TRCS, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_FUGO() {
        // Scenario: EventCode is GATE_OUT_WITH_CONTAINER_FULL and locationRole is "destinationPort"
        String eventCode = EventConstants.GATE_OUT_WITH_CONTAINER_FULL;
        String locationRole = "destinationPort";
        String description = "";

        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(locationRole, eventCode, description);

        // Expect FUGO to be returned
        assertEquals(EventConstants.FUGO, result);
    }

    @Test
    void convertTrackingEventCodeToShortCode_EMCR() {
        // Scenario: EventCode is GATE_IN_WITH_CONTAINER_EMPTY and locationRole starts with "DESTINATION"
        String eventCode = EventConstants.GATE_IN_WITH_CONTAINER_EMPTY;
        String locationRole = EventConstants.DESTINATION;
        String description = "";

        String result = trackingServiceAdapter.convertTrackingEventCodeToShortCode(locationRole, eventCode, description);

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

}
