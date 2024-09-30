package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.booking.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.booking.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.booking.services.dao.interfaces.IPackingDao;
import com.dpw.runner.booking.services.dto.request.UsersDto;
import com.dpw.runner.booking.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.booking.services.entity.Containers;
import com.dpw.runner.booking.services.entity.Events;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import com.dpw.runner.booking.services.exception.exceptions.ValidationException;
import com.dpw.runner.booking.services.helper.JsonTestUtility;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.booking.services.service.impl.AuditLogService;
import com.dpw.runner.booking.services.validator.ValidatorUtility;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ContainerDaoTest {

    @Mock
    private IPackingDao packingDao;

    @Mock
    private IContainerRepository containerRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private AuditLogService auditLogService;

    @InjectMocks
    private ContainerDao containerDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static Containers testContainer;

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
        testContainer = jsonTestUtility.getTestContainer();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSave_DataRetException() {
        testContainer.setId(4L);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        assertThrows(DataRetrievalFailureException.class, () -> containerDao.save(testContainer));
    }

    @Test
    void testSave_Failure() {
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        testContainer.setHazardous(true);
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(testContainer));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> containerDao.save(testContainer));
    }

    @Test
    void testSave() {
        testContainer.setId(4L);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(containerRepository.save(any())).thenReturn(testContainer);
        Containers containers = containerDao.save(testContainer);
        assertEquals(testContainer, containers);
    }

    @Test
    void testSave_NewCont() {
        testContainer.setId(null);
        testContainer.setHazardous(true);
        testContainer.setDgClass("dgClass");
        List<Events> eventsList = new ArrayList<>();
        eventsList.add(new Events());
        testContainer.setEventsList(eventsList);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(containerRepository.save(any())).thenReturn(testContainer);
        Containers containers = containerDao.save(testContainer);
        assertEquals(testContainer, containers);
    }

    @Test
    void testSave_Branches() {
        testContainer.setId(7L);
        testContainer.setHazardous(true);
        testContainer.setDgClass("dgClass");
        List<Events> eventsList = new ArrayList<>();
        eventsList.add(new Events());
        testContainer.setEventsList(eventsList);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(containerRepository.save(any())).thenReturn(testContainer);
        Containers containers = containerDao.save(testContainer);
        assertEquals(testContainer, containers);
    }

    @Test
    void testUpdateEntityFromBooking() throws RunnerException {
        testContainer.setId(4L);
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doReturn(containersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Containers> containersList1 = spyService.updateEntityFromBooking(containersList, 1L);
        assertNotNull(containersList1);
        assertEquals(containersList, containersList1);
    }

    @Test
    void CestUpdateEntityFromBooking_Nullcontainers() throws RunnerException {
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        List<Containers> containersList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(containersList1);
        assertEquals(new ArrayList<>(), containersList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId() throws RunnerException {
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doReturn(containersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Containers> containersList1 = spyService.updateEntityFromBooking(containersList, 1L);
        assertNotNull(containersList1);
        assertEquals(containersList, containersList1);
    }

    @Test
    void testUpdateEntityFromBooking_Failure() throws RunnerException {
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doThrow(new RuntimeException()).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromBooking(containersList, 1L));
    }

    @Test
    void testSaveEntityFromBooking() throws Exception {
        testContainer.setId(4L);
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testContainer).when(spyService).save(any());
        List<Containers> containers = spyService.saveEntityFromBooking(containersList, 1L);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void testSaveEntityFromBooking_NullId() throws Exception {
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testContainer).when(spyService).save(any());
        List<Containers> containers = spyService.saveEntityFromBooking(containersList, 1L);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void testSaveEntityFromBooking_RetrievalFailure() throws Exception {
        testContainer.setId(4L);
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(RuntimeException.class, () -> spyService.saveEntityFromBooking(containersList, 1L));
    }

    @Test
    void testSaveEntityFromBooking_AuditLogFailure() throws Exception {
        testContainer.setId(4L);
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testContainer).when(spyService).save(any());
        List<Containers> containers = spyService.saveEntityFromBooking(containersList, 1L);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void findByShipmentId() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        when(containerRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl<>(containersList));
        List<Containers> containers = containerDao.findByShipmentId(4L);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void findByGuid() {
        when(containerRepository.findByGuid(any())).thenReturn(List.of(testContainer));
        List<Containers> containersList = containerDao.findByGuid(UUID.randomUUID());
        assertNotNull(containersList);
        assertEquals(List.of(testContainer), containersList);
    }

    @Test
    void deleteById() {
        assertDoesNotThrow(() -> containerDao.deleteById(6L));
    }

    @Test
    void CestUpdateEntityFromBooking_FailedAuditLog() throws Exception {
        List<Containers> containersList = Collections.singletonList(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(new PageImpl<>(containersList)).when(spyService).findAll(any(), any());
        doThrow(new RunnerException()).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        List<Containers> containersList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(containersList1);
        assertEquals(new ArrayList<>(), containersList1);
    }

    @Test
    void saveAll() {
        List<Containers> containersList = List.of(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(testContainer).when(spyService).save(any());
        List<Containers> containers = spyService.saveAll(containersList);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void findByConsolidationId() {
        assertDoesNotThrow(() -> containerDao.findByConsolidationId(1L));
    }

    @Test
    void getAllContainers() {
        assertDoesNotThrow(() -> containerDao.getAllContainers());
    }
}
