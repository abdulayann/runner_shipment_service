package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;


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
    void createEventForAirMessagingEvent() {
        Events events = new Events();

        Query queryMock = mock(Query.class);

        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        eventDao.createEventForAirMessagingEvent(events);
    }

    @Test
    void createEventForAirMessagingStatus() {
        eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), 1L, "type", "code", "desc", LocalDateTime.now(), LocalDateTime.now(),  "source", 1, "status", LocalDateTime.now(), LocalDateTime.now());
        verify(eventRepository, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void createEventForAirMessagingEventDatesNotNull() {
        Events events = new Events();
        events.setReceivedDate(LocalDateTime.now());
        events.setScheduledDate(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setActual(LocalDateTime.now());
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

        Page<Events> page = new PageImpl(List.of());
        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);

        assertEquals(false, eventDao.checkIfEventsRowExistsForEntityTypeAndEntityId(customAutoEventRequest));
    }

    @Test
    void getTheDataFromEntity() {
        Events savedEvent = new Events();
        savedEvent.setEventCode("EventCode");

        Page<Events> page = new PageImpl(List.of(savedEvent));
        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);

        assertEquals(List.of(savedEvent), eventDao.getTheDataFromEntity("SHIPMENTS", 1, false));
    }
}