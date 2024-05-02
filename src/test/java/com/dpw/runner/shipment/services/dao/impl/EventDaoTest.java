package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.checkerframework.checker.nullness.Opt;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.verification.VerificationMode;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import javax.persistence.EntityManager;
import javax.persistence.Parameter;
import javax.persistence.Query;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
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


//    @Container
//    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
//
//    static {
//        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
//                .withDatabaseName("integration-tests-db")
//                .withUsername("sa")
//                .withPassword("sa");
//        postgresContainer.start();
//    }
//
    @BeforeAll
    static void beforeAll() throws IOException {
//        postgresContainer.start();
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }
//
//    @AfterAll
//    static void afterAll() {
//        postgresContainer.stop();
//    }
//
//    @DynamicPropertySource
//    static void dynamicConfiguration(DynamicPropertyRegistry registry){
//        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
//        registry.add("spring.datasource.username", postgresContainer::getUsername);
//        registry.add("spring.datasource.password", postgresContainer::getPassword);
//    }

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
//        when(eventRepository.saveAll(input)).thenReturn(List.of(testData));

        var e = assertThrows(ValidationException.class, () -> eventDao.saveAll(input));

        assertNotNull(e);
        assertEquals(String.join(",", errors), e.getMessage());
    }

//    @Test
//    void findAll() {
//        var r = eventDao.save(testData);
//        assertNotNull(r.getId());
//        Specification<Events> spec = (root, query, criteriaBuilder) -> {
//            return criteriaBuilder.equal(root.get("id"), r.getId());
//        };
//        var result = eventDao.findAll(spec, PageRequest.of(0 , 10));
//        assertNotNull(result);
//        assertEquals(result.stream().toList().get(0).getId(), r.getId());
//    }
//
//    @Test
//    void findById() {
//        var r = eventDao.save(testData);
//        assertNotNull(r.getId());
//        var result = eventDao.findById(r.getId());
//        assertNotNull(result);
//        assertEquals(result.get(), r);
//    }
//
//    @Test
//    void findByGuid() {
//        var r = eventDao.save(testData);
//        assertNotNull(r.getId());
//        var result = eventDao.findByGuid(r.getGuid());
//        assertNotNull(result);
//        assertEquals(result.get(), r);
//    }
//
//    @Test
//    void delete() {
//        var r = eventDao.save(testData);
//        assertNotNull(r.getId());
//        eventDao.delete(r);
//        var result = eventDao.findById(r.getId());
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void updateEntityFromOtherEntity() throws RunnerException {
//        var r = eventDao.save(testData);
//        testData.setId(null);
//        var result = eventDao.updateEntityFromOtherEntity(List.of(testData), 2L, "Shipment");
//        assertNotNull(result);
//        assertNotNull(r);
//    }

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

        Page<Events> page = new PageImpl(List.of(savedEvent));
        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);

        try {
            var result = eventDao.updateEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityList() {
        testData.setId(1L);

//        Events savedEvent = testData;

//        Page<Events> page = new PageImpl(List.of(savedEvent));
//        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);
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

//        Events savedEvent = testData;

//        Page<Events> page = new PageImpl(List.of(savedEvent));
//        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);
//        when(eventRepository.findById(1L)).thenReturn(Optional.of(testData));
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

        var result = eventDao.saveEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");

        verify(auditLogService, atLeast(1)).addAuditLog(any());
    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMap() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long eventId = 1L;
        testData.setId(eventId);

        Map<Long, Events> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testData.getId(), testData);

//        when(eventRepository.findById(eventId)).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
//        when(eventRepository.save(testData)).thenReturn(testData);
        when(eventRepository.saveAll(anyList())).thenReturn(List.of(testData));

        var result = eventDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", oldEntityMap);

    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMapThrowsException() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long eventId = 1L;
        testData.setId(eventId);

//        when(eventRepository.findById(eventId)).thenReturn(Optional.of(testData));
//        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
//        when(eventRepository.save(testData)).thenReturn(testData);

        var e = assertThrows(DataRetrievalFailureException.class, () ->
                eventDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", new HashMap<>()));

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


        Events savedEvent = new Events();
        savedEvent.setEventCode(request.eventCode);

        Page<Events> page = new PageImpl(List.of(savedEvent));
        when(eventRepository.save(testData)).thenReturn(testData);
        when(eventsSync.sync(anyList())).thenReturn(ResponseEntity.ok(null));

        eventDao.autoGenerateEvents(request);

        verify(eventsSync, times(1)).sync(anyList());
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

        Page<Events> page = new PageImpl(List.of(savedEvent));
        when(eventRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(page);

        eventDao.autoGenerateEvents(request);

        verify(eventRepository, times(0)).save(any());
    }

    @Test
    void createEventForAirMessagingEvent() {
        UUID guid = null;
        Long entityId = null;
        String entityType = null;
        String eventCode = null;
        String description = null;
        String source = null;
        Integer tenantId = null;
        Integer pieces = null;
        Integer totalPieces = null;
        BigDecimal weight = null;
        BigDecimal totalWeight = null;
        String partial = null;
        LocalDateTime receivedDate = null;
        LocalDateTime scheduledDate = null;
        LocalDateTime createdAt = null;
        LocalDateTime updatedAt = null;

        Query queryMock = mock(Query.class);

        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        eventDao.createEventForAirMessagingEvent(guid,entityId,entityType,eventCode,description,source,tenantId,pieces,totalPieces,weight,totalWeight,partial,receivedDate,scheduledDate,createdAt,updatedAt);
    }



}