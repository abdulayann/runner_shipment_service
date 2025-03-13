package com.dpw.runner.shipment.services.dao.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventDaoTest {

    private static JsonTestUtility jsonTestUtility;
    private static Events testData;

    @InjectMocks
    private EventDao eventDao;

    @Mock
    private IEventRepository eventRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IEventsSync eventsSync;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private EntityManager entityManager;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private CommonUtils commonUtils;

    private static ObjectMapper objectMapper;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testData = jsonTestUtility.getTestEventData();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        testData.setId(1L);
        when(eventRepository.save(any())).thenReturn(testData);

        var r = eventDao.save(testData);

        assertNotNull(r);
        assertNotNull(r.getId());
    }

    @Test
    void saveThrowsException() {
        String error = "error";
        Set<String> errors = Set.of(error);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        testData.setId(1L);

        var e = assertThrows(ValidationException.class, () -> eventDao.save(testData));

        assertNotNull(e);
        assertEquals(String.join(",", errors), e.getMessage());
    }

    @Test
    void saveAll() {
        Set<String> errors = new HashSet<>();
        List<Events> input = List.of(testData);
        testData.setId(1L);

        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        when(eventRepository.saveAll(input)).thenReturn(List.of(testData));

        var r = eventDao.saveAll(input);

        assertNotNull(r);
        assertNotNull(r.get(0).getId());
    }

    @Test
    void saveAllThrowsException() {
        String error = "error";
        Set<String> errors = Set.of(error);
        List<Events> input = List.of(testData);
        testData.setId(1L);

        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);

        var e = assertThrows(ValidationException.class, () -> eventDao.saveAll(input));

        assertNotNull(e);
        assertEquals(String.join(",", errors), e.getMessage());
    }

    @Test
    void findByGuid() {
        testData.setGuid(UUID.randomUUID());

        when(eventRepository.findByGuid(testData.getGuid())).thenReturn(Optional.of(testData));

        Optional<Events> result = eventDao.findByGuid(testData.getGuid());

        assertTrue(result.isPresent());
        assertEquals(testData, result.get());
    }

    @Test
    void updateEntityFromOtherEntity() {
        testData.setId(1L);

        Events savedEvent = testData;

        Page<Events> page = new PageImpl<>(List.of(savedEvent));
        when(eventRepository.findAll(ArgumentMatchers.<Specification<Events>>any(), any(Pageable.class))).thenReturn(page);

        try {
            var result = eventDao.updateEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityConsolidation() {
        testData.setId(1L);

        Events savedEvent = testData;

        Page<Events> page = new PageImpl<>(List.of(savedEvent));
        when(eventRepository.findAll(ArgumentMatchers.<Specification<Events>>any(), any(Pageable.class))).thenReturn(page);

        try {
            var result = eventDao.updateEntityFromOtherEntity(List.of(testData) , 1L , "CONSOLIDATION");
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityList() {
        testData.setId(1L);
        when(eventRepository.findById(1L)).thenReturn(Optional.of(testData));

        try {
            var result = eventDao.updateEntityFromOtherEntity(
              List.of(testData), 1L, "Shipment", List.of(testData));
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityListDeletesOldEvents() throws JsonProcessingException {
        testData.setId(2L);
        testData.setGuid(UUID.randomUUID());
        Events oldEvent = objectMapper.convertValue(testData, Events.class);
        oldEvent.setId(1L);
        oldEvent.setGuid(UUID.randomUUID());

        when(eventRepository.findById(2L)).thenReturn(Optional.of(oldEvent));
        when(eventRepository.save(testData)).thenReturn(testData);
        when(eventRepository.save(oldEvent)).thenReturn(oldEvent);
        when(jsonHelper.convertToJson(oldEvent)).thenReturn(objectMapper.writeValueAsString(oldEvent));

        try {
            var result = eventDao.updateEntityFromOtherEntity(
                    List.of(testData), 1L, "Shipment", List.of(testData, oldEvent));
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityListThrowsException() {
        testData.setId(1L);

        var e = assertThrows(RunnerException.class, () ->
                eventDao.updateEntityFromOtherEntity(
                        List.of(testData), 1L, "Shipment", List.of(testData)));


        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }


    @Test
    void saveEntityFromOtherEntity() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long eventId = 1L;
        testData.setId(eventId);

        when(eventRepository.findById(eventId)).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(eventRepository.save(testData)).thenReturn(testData);

        eventDao.saveEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");

        verify(auditLogService, atLeast(1)).addAuditLog(any());
    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMap() throws JsonProcessingException {
        Long eventId = 1L;
        testData.setId(eventId);

        Map<Long, Events> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testData.getId(), testData);

        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(eventRepository.saveAll(anyList())).thenReturn(List.of(testData));

        assertDoesNotThrow(() ->eventDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", oldEntityMap));

    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMapThrowsException(){
        Long eventId = 1L;
        testData.setId(eventId);
        Map<Long, Events> oldEntityMap = new HashMap<>();
        List<Events> events = List.of(testData);
        assertThrows(DataRetrievalFailureException.class, () ->
                eventDao.saveEntityFromOtherEntity(events, 1L, "Shipment", oldEntityMap));

    }

    @Test
    void saveEntityFromOtherEntityIgnoresEntityTypeIfAlreadyPresentInEvents() throws JsonProcessingException{
        Long eventId = 1L;
        testData.setId(eventId);
        testData.setEntityId(5L);
        testData.setEntityType(Constants.CONSOLIDATION);

        Map<Long, Events> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testData.getId(), testData);

        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(eventRepository.saveAll(anyList())).thenReturn(List.of(testData));

        var result = eventDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", oldEntityMap);

        assertEquals(Constants.CONSOLIDATION, result.get(0).getEntityType());

    }

    @Test
    void saveEntityFromOtherEntityUpdateEntityTypeIfNotPresentInEvents() throws JsonProcessingException{
        Long eventId = 1L;
        testData.setId(eventId);
        testData.setEntityId(null);
        testData.setEntityType(null);

        Map<Long, Events> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testData.getId(), testData);

        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(eventRepository.saveAll(anyList())).thenReturn(List.of(testData));

        var result = eventDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", oldEntityMap);

        assertEquals("Shipment", result.get(0).getEntityType());

    }


    @Test
    void autoGenerateEvents() {
        CustomAutoEventRequest request = new CustomAutoEventRequest();
        request.createDuplicate = true;
        request.entityType = "SHIPMENT";
        request.entityId = 1L;
        request.eventCode = "eventCode";
        request.isEstimatedRequired = true;
        request.isActualRequired = true;
        request.placeName = "test";
        request.placeDesc = "test";

        var spyBean = spy(eventDao);

        Events savedEvent = new Events();
        savedEvent.setEventCode(request.eventCode);

        when(eventRepository.save(testData)).thenReturn(testData);
        doNothing().when(spyBean).updateEventDetails(any(Events.class));

        spyBean.autoGenerateEvents(request);

        verify(eventsSync, times(0)).sync(anyList());
    }
    @Test
    void autoGenerateEventsWhenEventsRowExistForEntityTypeAndEntityId() {
        CustomAutoEventRequest request = new CustomAutoEventRequest();
        request.createDuplicate = false;
        request.entityType = "SHIPMENT";
        request.entityId = 1L;
        request.eventCode = "eventCode";
        request.isEstimatedRequired = true;
        request.isActualRequired = true;
        request.placeName = "test";
        request.placeDesc = "test";


        Events savedEvent = new Events();
        savedEvent.setEventCode(request.eventCode);

        Page<Events> page = new PageImpl<>(List.of(savedEvent));
        when(eventRepository.findAll(ArgumentMatchers.<Specification<Events>>any(), any(Pageable.class))).thenReturn(page);

        eventDao.autoGenerateEvents(request);

        verify(eventRepository, times(0)).save(any());
    }

    @Test
    void createEventForAirMessagingEvent() {
        Events events = new Events();
        events.setEntityType(Constants.CONSOLIDATION);

        Query queryMock = mock(Query.class);

        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        eventDao.createEventForAirMessagingEvent(events);
        verify(queryMock, times(1)).executeUpdate();
    }

    @Test
    void createEventForAirMessagingStatus() {
        Query queryMock = mock(Query.class);
        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), 1L, Constants.CONSOLIDATION, "code", "desc", LocalDateTime.now(), LocalDateTime.now(),  "source", 1, "status", LocalDateTime.now(), LocalDateTime.now());

        verify(queryMock).executeUpdate();
    }

    @Test
    void createEventForAirMessagingEventDatesNotNull() {
        Events events = new Events();
        events.setReceivedDate(LocalDateTime.now());
        events.setScheduledDate(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setActual(LocalDateTime.now());
        events.setEntityType(Constants.CONSOLIDATION);
        Query queryMock = mock(Query.class);

        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        eventDao.createEventForAirMessagingEvent(events);
        verify(queryMock, times(1)).executeUpdate();
    }

    @Test
    void checkIfEventsRowExistsForEntityTypeAndEntityId() {
        CustomAutoEventRequest customAutoEventRequest = new CustomAutoEventRequest();
        Events savedEvent = new Events();
        savedEvent.setEventCode("EventCode");

        Page<Events> page = new PageImpl<>(List.of());
        when(eventRepository.findAll(ArgumentMatchers.<Specification<Events>>any(), any(Pageable.class))).thenReturn(page);

        assertFalse(eventDao.checkIfEventsRowExistsForEntityTypeAndEntityId(customAutoEventRequest));
    }

    @Test
    void getTheDataFromEntity() {
        Events savedEvent = new Events();
        savedEvent.setEventCode("EventCode");

        Page<Events> page = new PageImpl<>(List.of(savedEvent));
        when(eventRepository.findAll(ArgumentMatchers.<Specification<Events>>any(), any(Pageable.class))).thenReturn(page);

        assertEquals(List.of(savedEvent), eventDao.getTheDataFromEntity("SHIPMENTS", 1, false));
    }

    @Test
    void updateEventDetailsForShipmentEvent() {
        Events events = new Events();
        events.setEntityId(1L);
        events.setEntityType(Constants.SHIPMENT);
        events.setEventCode(EventConstants.BOCO);

        ShipmentDetails shipment = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR).shipmentId("SHP01").build();

        List<ConsoleShipmentMapping> consoleShipmentMappings = List.of(ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build());
        List<ShipmentDetails> shipmentDetailsList = List.of(shipment);

        when(consoleShipmentMappingDao.findByShipmentId(anyLong())).thenReturn(consoleShipmentMappings);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(shipmentDetailsList);

        eventDao.updateEventDetails(events);

        assertEquals(1L, events.getConsolidationId());
        assertEquals(shipment.getShipmentId(), events.getShipmentNumber());
    }

    @Test
    void updateEventDetailsForConsolidationEvent() {
        Events events = new Events();
        events.setEntityId(1L);
        events.setEntityType(Constants.CONSOLIDATION);

        eventDao.updateEventDetails(events);

        assertEquals(1L, events.getConsolidationId());
    }


}