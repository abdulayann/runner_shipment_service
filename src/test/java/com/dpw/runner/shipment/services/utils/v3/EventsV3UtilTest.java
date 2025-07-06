package com.dpw.runner.shipment.services.utils.v3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class EventsV3UtilTest extends CommonMocks {

    @InjectMocks
    private EventsV3Util eventsV3Util;

    @Mock
    private IEventDao eventDao;

    private ShipmentDetails shipmentDetails;
    private ShipmentDetails oldEntity;
    private AdditionalDetails additionalDetails;
    private AdditionalDetails oldAdditionalDetails;
    private LocalDateTime now;

    @BeforeEach
    void setUp() {
        TenantContext.setCurrentTenant(1);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setDisplayName("User");
        mockUser.setEmail("email@gmail.com");
        mockUser.setCode("userCode");
        UserContext.setUser(mockUser);

        now = LocalDateTime.now();
        when(commonUtils.getUserZoneTime(any(LocalDateTime.class))).thenAnswer(i -> i.getArgument(0));

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().eventsRevampEnabled(true).build());

        setupTestData();
    }

    private void setupTestData() {
        // Initialize shipment details
        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("TEST-SHIP-001");
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setBookingNumber("BOOK-123");
        shipmentDetails.setMasterBill("MAWB-123");
        shipmentDetails.setShipmentGateInDate(now);
        shipmentDetails.setDateType(DateBehaviorType.ACTUAL);
        shipmentDetails.setBrokerageAtOriginDate(now);

        // Initialize additional details
        additionalDetails = new AdditionalDetails();
        additionalDetails.setCargoDeliveredDate(now);
        additionalDetails.setPickupDate(now);
        additionalDetails.setCustomReleaseDate(now);
        additionalDetails.setDocTurnedOverToCustomer(true);
        additionalDetails.setProofOfDeliveryDate(now);
        additionalDetails.setPickupByConsigneeCompleted(true);
        additionalDetails.setWarehouseCargoArrivalDate(now);
        additionalDetails.setEmptyContainerReturned(true);
        additionalDetails.setBlInstructionReceived(now);
        additionalDetails.setCargoOutForDelivery(now);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        // Initialize old entity
        oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setShipmentId("TEST-SHIP-001");
        oldEntity.setDirection(Constants.DIRECTION_EXP);

        // Initialize old additional details (empty/null values)
        oldAdditionalDetails = new AdditionalDetails();
        oldEntity.setAdditionalDetails(oldAdditionalDetails);
    }

    @Test
    void testCreateOrUpdateEvents_NewShipment() {
        List<Events> existingEvents = new ArrayList<>();

        mockShipmentSettings();

        List<Events> result = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, existingEvents, true);

        assertNotNull(result);
    }

    @Test
    void testCreateOrUpdateEvents_WithEventsRevampEnabled() {
        List<Events> existingEvents = new ArrayList<>();
        List<Events> oldEvents = new ArrayList<>();
        oldEvents.add(createEvent("SHCR"));
        oldEntity.setEventsList(oldEvents);

        mockShipmentSettings();

        List<Events> result = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, existingEvents, false);

        assertNotNull(result);
        assertEquals(oldEvents.size(), result.size());
    }

    @Test
    void testCreateOrUpdateEvents_WithOldEntityNull() {
        List<Events> existingEvents = new ArrayList<>();

        mockShipmentSettings();

        List<Events> result = eventsV3Util.createOrUpdateEvents(shipmentDetails, null, existingEvents, true);

        assertNotNull(result);
    }

    @Test
    void testCreateOrUpdateEvents_WithNullEvents() {
        mockShipmentSettings();

        List<Events> result = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, null, true);

        assertNotNull(result);
    }

    @Test
    void testCreateOrUpdateEvents_UpdatesDirection() {
        List<Events> existingEvents = new ArrayList<>();
        Events event = createEvent("SHCR");
        event.setDirection(null);
        existingEvents.add(event);

        mockShipmentSettings();

        List<Events> result = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, existingEvents, true);

        assertNotNull(result);
        assertEquals(Constants.DIRECTION_EXP, result.get(0).getDirection());
    }

    @Test
    void testCreateUpdateEvent() throws Exception {
        Method createUpdateEventMethod = EventsV3Util.class.getDeclaredMethod(
                "createUpdateEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class);
        createUpdateEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();

        createUpdateEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true);

        verify(commonUtils).removeDuplicateTrackingEvents(events);
    }

    @Test
    void testGroupCargoesRunnerEventsByCode() throws Exception {
        Method groupCargoesRunnerEventsByCodeMethod = EventsV3Util.class.getDeclaredMethod(
                "groupCargoesRunnerEventsByCode",
                List.class);
        groupCargoesRunnerEventsByCodeMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();

        Events event1 = createEvent("BOCO");
        event1.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);

        Events event2 = createEvent("BOCO");
        event2.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);

        Events event3 = createEvent("CADE");
        event3.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);

        Events event4 = createEvent("BOCO");
        event4.setSource("OTHER_SOURCE");

        events.add(event1);
        events.add(event2);
        events.add(event3);
        events.add(event4);

        @SuppressWarnings("unchecked")
        Map<String, List<Events>> result = (Map<String, List<Events>>)
                groupCargoesRunnerEventsByCodeMethod.invoke(eventsV3Util, events);

        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals(2, result.get("BOCO").size());
        assertEquals(1, result.get("CADE").size());
        assertNull(result.get("OTHER_SOURCE"));
    }

    @Test
    void testProcessLclOrFclOrAirEvents() throws Exception {
        Method processLclOrFclOrAirEventsMethod = EventsV3Util.class.getDeclaredMethod(
                "processLclOrFclOrAirEvents",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processLclOrFclOrAirEventsMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        when(eventDao.updateUserFieldsInEvent(any(Events.class), anyBoolean())).thenReturn(new Events());

        // Execute with FCL type
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        processLclOrFclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        // Execute with LCL type
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        processLclOrFclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        shipmentDetails.setShipmentType("BULK");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        processLclOrFclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertFalse(events.isEmpty());
    }

    @Test
    void testProcessBOCOEvent() throws Exception {
        Method processBOCOEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processBOCOEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processBOCOEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        shipmentDetails.setBookingNumber("BOOK-123");
        oldEntity.setBookingNumber(null);
        processBOCOEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.BOCO, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.BOCO);
        dbEvent.setContainerNumber("OLD-BOOKING");
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.BOCO, dbEvents);

        processBOCOEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);
    }

    @Test
    void testCreateBOCOEvent() throws Exception {
        Method createBOCOEventMethod = EventsV3Util.class.getDeclaredMethod(
                "createBOCOEvent",
                ShipmentDetails.class,
                List.class,
                boolean.class);
        createBOCOEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();

        createBOCOEventMethod.invoke(eventsV3Util, shipmentDetails, events, true);

        assertEquals(1, events.size());
        assertEquals(EventConstants.BOCO, events.get(0).getEventCode());
        assertEquals(shipmentDetails.getBookingNumber(), events.get(0).getContainerNumber());

        events.clear();
        createBOCOEventMethod.invoke(eventsV3Util, shipmentDetails, events, false);

        assertEquals(0, events.size());
    }

    @Test
    void testProcessCADEEvent() throws Exception {
        Method processCADEEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processCADEEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processCADEEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processCADEEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.CADE, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CADE);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.CADE, dbEvents);

        processCADEEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessCACOEvent() throws Exception {
        Method processCACOEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processCACOEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processCACOEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processCACOEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.CACO, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CACO);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.CACO, dbEvents);

        processCACOEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessCUREEvent() throws Exception {
        Method processCUREEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processCUREEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processCUREEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processCUREEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(0, events.size());
        assertEquals(EventConstants.CURE, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CURE);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.CURE, dbEvents);

        processCUREEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessDOTPEvent() throws Exception {
        Method processDOTPEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processDOTPEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processDOTPEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processDOTPEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.DOTP, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.DOTP);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.DOTP, dbEvents);

        processDOTPEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessPRDEEvent() throws Exception {
        Method processPRDEEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processPRDEEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processPRDEEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processPRDEEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.PRDE, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.PRDE);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.PRDE, dbEvents);

        processPRDEEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessSEPUEvent() throws Exception {
        Method processSEPUEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processSEPUEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processSEPUEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processSEPUEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.SEPU, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.SEPU);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.SEPU, dbEvents);

        processSEPUEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessLclOrAirEvents() throws Exception {
        Method processLclOrAirEventsMethod = EventsV3Util.class.getDeclaredMethod(
                "processLclOrAirEvents",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processLclOrAirEventsMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        shipmentDetails.setShipmentType("OTHER");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        processLclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(2, events.size());
        assertEquals(EventConstants.CAFS, events.get(0).getEventCode());

        events.clear();
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setTransportMode("OTHER");
        processLclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(2, events.size());
        assertEquals(EventConstants.CAFS, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CAFS);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.CAFS, dbEvents);

        processLclOrAirEventsMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(1, events.size());
    }

    @Test
    void testProcessCAAWEvent() throws Exception {
        Method processCAAWEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processCAAWEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processCAAWEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        shipmentDetails.setDateType(DateBehaviorType.ACTUAL);
        processCAAWEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.CAAW, events.get(0).getEventCode());
        assertNotNull(events.get(0).getActual());
        assertNull(events.get(0).getEstimated());

        events.clear();
        shipmentDetails.setDateType(DateBehaviorType.ESTIMATED);
        processCAAWEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.CAAW, events.get(0).getEventCode());
        assertNull(events.get(0).getActual());
        assertNotNull(events.get(0).getEstimated());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CAAW);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.CAAW, dbEvents);

        shipmentDetails.setDateType(DateBehaviorType.ACTUAL);
        processCAAWEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessDbCaawEvent() throws Exception {
        Method processDbCaawEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processDbCaawEvent",
                ShipmentDetails.class,
                List.class);
        processDbCaawEventMethod.setAccessible(true);

        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.CAAW);
        dbEvents.add(dbEvent);

        shipmentDetails.setDateType(DateBehaviorType.ACTUAL);
        processDbCaawEventMethod.invoke(eventsV3Util, shipmentDetails, dbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertNotNull(dbEvent.getActual());

        dbEvent.setActual(null);
        shipmentDetails.setDateType(DateBehaviorType.ESTIMATED);
        processDbCaawEventMethod.invoke(eventsV3Util, shipmentDetails, dbEvents);

        verify(eventDao, times(2)).updateUserFieldsInEvent(dbEvent, true);
        assertNotNull(dbEvent.getEstimated());
    }

    @Test
    void testProcessEMCREvent() throws Exception {
        Method processEMCREventMethod = EventsV3Util.class.getDeclaredMethod(
                "processEMCREvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processEMCREventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        processEMCREventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.EMCR, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.EMCR);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.EMCR, dbEvents);

        processEMCREventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessECCCEvent() throws Exception {
        Method processECCCEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processECCCEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processECCCEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        processECCCEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.ECCC, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.ECCC);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.ECCC, dbEvents);

        processECCCEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessBLRSEvent() throws Exception {
        Method processBLRSEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processBLRSEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processBLRSEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processBLRSEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.BLRS, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.BLRS);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.BLRS, dbEvents);

        processBLRSEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testProcessFNMUEvent() throws Exception {
        Method processFNMUEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processFNMUEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                Boolean.class,
                Map.class);
        processFNMUEventMethod.setAccessible(true);

        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.FNMU);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.FNMU, dbEvents);

        shipmentDetails.setMasterBill("MAWB-123");
        oldEntity.setMasterBill(null);
        processFNMUEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals("MAWB-123", dbEvent.getContainerNumber());
    }

    @Test
    void testProcessCOODEvent() throws Exception {
        Method processCOODEventMethod = EventsV3Util.class.getDeclaredMethod(
                "processCOODEvent",
                ShipmentDetails.class,
                ShipmentDetails.class,
                List.class,
                Boolean.class,
                Map.class);
        processCOODEventMethod.setAccessible(true);

        List<Events> events = new ArrayList<>();
        Map<String, List<Events>> cargoesRunnerDbEvents = new HashMap<>();

        processCOODEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        assertEquals(1, events.size());
        assertEquals(EventConstants.COOD, events.get(0).getEventCode());

        events.clear();
        List<Events> dbEvents = new ArrayList<>();
        Events dbEvent = createEvent(EventConstants.COOD);
        dbEvents.add(dbEvent);
        cargoesRunnerDbEvents.put(EventConstants.COOD, dbEvents);

        processCOODEventMethod.invoke(eventsV3Util, shipmentDetails, oldEntity, events, true, cargoesRunnerDbEvents);

        verify(eventDao).updateUserFieldsInEvent(dbEvent, true);
        assertEquals(0, events.size());
    }

    @Test
    void testAutoGenerateCreateEvent() {
        Events event = new Events();
        doNothing().when(commonUtils).updateEventWithMasterData(any());
        when(eventDao.save(any(Events.class))).thenReturn(event);

        eventsV3Util.autoGenerateCreateEvent(shipmentDetails);

        assertNotNull(shipmentDetails.getEventsList());
        assertEquals(1, shipmentDetails.getEventsList().size());
        assertEquals(EventConstants.SHCR, shipmentDetails.getEventsList().get(0).getEventCode());
        verify(eventDao).save(any(Events.class));
    }

    @Test
    void testCreateAutomatedEvents() throws Exception {
        Method createAutomatedEventsMethod = EventsV3Util.class.getDeclaredMethod(
                "createAutomatedEvents",
                ShipmentDetails.class,
                String.class,
                LocalDateTime.class,
                LocalDateTime.class);
        createAutomatedEventsMethod.setAccessible(true);

        Events event = new Events();
        doNothing().when(commonUtils).updateEventWithMasterData(any());
        when(eventDao.save(any(Events.class))).thenReturn(event);

        Events result = (Events) createAutomatedEventsMethod.invoke(
                eventsV3Util,
                shipmentDetails,
                EventConstants.BOCO,
                now,
                null);

        assertNotNull(result);
        verify(eventDao).save(any(Events.class));
        verify(commonUtils).updateEventWithMasterData(anyList());
    }

    @Test
    void testInitializeAutomatedEvents() throws Exception {
        Method initializeAutomatedEventsMethod = EventsV3Util.class.getDeclaredMethod(
                "initializeAutomatedEvents",
                ShipmentDetails.class,
                String.class,
                LocalDateTime.class,
                LocalDateTime.class);
        initializeAutomatedEventsMethod.setAccessible(true);

        Events result = (Events) initializeAutomatedEventsMethod.invoke(
                eventsV3Util,
                shipmentDetails,
                EventConstants.BOCO,
                now,
                null);

        assertNotNull(result);
        assertEquals(EventConstants.BOCO, result.getEventCode());
        assertEquals(now, result.getActual());
        assertNull(result.getEstimated());
        assertEquals(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER, result.getSource());
        assertEquals(true, result.getIsPublicTrackingEvent());
        assertEquals(Constants.SHIPMENT, result.getEntityType());
        assertEquals(shipmentDetails.getId(), result.getEntityId());
        assertEquals(1, result.getTenantId());
        assertEquals(shipmentDetails.getShipmentId(), result.getShipmentNumber());
        assertEquals(shipmentDetails.getDirection(), result.getDirection());
    }

    @Test
    void testIsEventChanged() throws Exception {
        Method isEventChangedMethod = EventsV3Util.class.getDeclaredMethod(
                "isEventChanged",
                Object.class,
                Object.class,
                Boolean.class);
        isEventChangedMethod.setAccessible(true);

        Boolean result1 = (Boolean) isEventChangedMethod.invoke(eventsV3Util, "value", null, true);
        assertTrue(result1);

        Boolean result2 = (Boolean) isEventChangedMethod.invoke(eventsV3Util, null, null, true);
        assertFalse(result2);

        Boolean result3 = (Boolean) isEventChangedMethod.invoke(eventsV3Util, "new", "old", false);
        assertTrue(result3);

        Boolean result4 = (Boolean) isEventChangedMethod.invoke(eventsV3Util, "same", "same", false);
        assertFalse(result4);
    }

    @Test
    void testIsEventBooleanChanged() throws Exception {
        Method isEventBooleanChangedMethod = EventsV3Util.class.getDeclaredMethod(
                "isEventBooleanChanged",
                Boolean.class,
                Boolean.class,
                Boolean.class);
        isEventBooleanChangedMethod.setAccessible(true);

        Boolean result1 = (Boolean) isEventBooleanChangedMethod.invoke(eventsV3Util, true, null, true);
        assertTrue(result1);

        Boolean result2 = (Boolean) isEventBooleanChangedMethod.invoke(eventsV3Util, false, null, true);
        assertFalse(result2);

        Boolean result3 = (Boolean) isEventBooleanChangedMethod.invoke(eventsV3Util, true, false, false);
        assertTrue(result3);

        Boolean result4 = (Boolean) isEventBooleanChangedMethod.invoke(eventsV3Util, true, true, false);
        assertFalse(result4);
    }

    @Test
    void testIsLclOrFclOrAir() throws Exception {
        Method isLclOrFclOrAirMethod = EventsV3Util.class.getDeclaredMethod(
                "isLclOrFclOrAir",
                ShipmentDetails.class);
        isLclOrFclOrAirMethod.setAccessible(true);

        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setTransportMode("OTHER");
        Boolean result1 = (Boolean) isLclOrFclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result1);

        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setTransportMode("OTHER");
        Boolean result2 = (Boolean) isLclOrFclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result2);

        shipmentDetails.setShipmentType("OTHER");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        Boolean result3 = (Boolean) isLclOrFclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result3);

        shipmentDetails.setShipmentType("OTHER");
        shipmentDetails.setTransportMode("OTHER");
        Boolean result4 = (Boolean) isLclOrFclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertFalse(result4);
    }

    @Test
    void testIsLclOrAir() throws Exception {
        Method isLclOrAirMethod = EventsV3Util.class.getDeclaredMethod(
                "isLclOrAir",
                ShipmentDetails.class);
        isLclOrAirMethod.setAccessible(true);

        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setTransportMode("OTHER");
        Boolean result1 = (Boolean) isLclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result1);

        shipmentDetails.setShipmentType("OTHER");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        Boolean result2 = (Boolean) isLclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result2);

        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setTransportMode("OTHER");
        Boolean result3 = (Boolean) isLclOrAirMethod.invoke(eventsV3Util, shipmentDetails);
        assertFalse(result3);
    }

    @Test
    void testIsFcl() throws Exception {
        Method isFclMethod = EventsV3Util.class.getDeclaredMethod(
                "isFcl",
                ShipmentDetails.class);
        isFclMethod.setAccessible(true);

        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        Boolean result1 = (Boolean) isFclMethod.invoke(eventsV3Util, shipmentDetails);
        assertTrue(result1);

        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        Boolean result2 = (Boolean) isFclMethod.invoke(eventsV3Util, shipmentDetails);
        assertFalse(result2);
    }

    private Events createEvent(String eventCode) {
        Events event = new Events();
        event.setEventCode(eventCode);
        event.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        event.setEntityId(1L);
        event.setEntityType(Constants.SHIPMENT);
        event.setTenantId(1);
        event.setDirection(Constants.DIRECTION_EXP);
        return event;
    }
}