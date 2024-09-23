package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
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
class RoutingsDaoTest {

    @Mock
    private IRoutingsRepository routingsRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private AuditLogService auditLogService;

    @InjectMocks
    private RoutingsDao routingsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static Routings testRoutings;
    private static Routings testNewRoutings;

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
        testRoutings = jsonTestUtility.getRoutingsList().get(0);
        testNewRoutings = jsonTestUtility.getTestRouting();
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
    void testSave() {
        Routings routings = jsonTestUtility.getRoutingsList().get(0);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(routingsRepository.save(any(Routings.class))).thenReturn(routings);

        Routings savedRoutings = routingsDao.save(routings);

        assertEquals(routings, savedRoutings);
    }

    @Test
    void testSave_Failure() {
        Routings routings = jsonTestUtility.getRoutingsList().get(0);
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(routings));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> routingsDao.save(routings));
    }

    @Test
    void testSaveAll() {
        List<Routings> routingsList = Arrays.asList(jsonTestUtility.getRoutingsList().get(0), jsonTestUtility.getRoutingsList().get(0));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(routingsRepository.saveAll(anyList())).thenReturn(routingsList);

        List<Routings> savedRoutingsList = routingsDao.saveAll(routingsList);

        assertEquals(routingsList.size(), savedRoutingsList.size());
        assertEquals(routingsList.get(0), savedRoutingsList.get(0));
        assertEquals(routingsList.get(1), savedRoutingsList.get(1));
    }

    @Test
    void testSaveAll_Failure() {
        List<Routings> routingsList = Arrays.asList(jsonTestUtility.getRoutingsList().get(0), jsonTestUtility.getRoutingsList().get(0));
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> routingsDao.saveAll(routingsList));
    }

    @Test
    void testFindAll() {
        Page<Routings> routingsPage = mock(Page.class);
        Specification<Routings> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(routingsRepository.findAll(spec, pageable)).thenReturn(routingsPage);

        Page<Routings> foundRoutingsPage = routingsDao.findAll(spec, pageable);

        assertEquals(routingsPage, foundRoutingsPage);
    }

    @Test
    void testFindById() {
        Routings routings = jsonTestUtility.getRoutingsList().get(0);
        Optional<Routings> optionalRoutings = Optional.of(routings);
        when(routingsRepository.findById(anyLong())).thenReturn(optionalRoutings);

        Optional<Routings> foundRoutings = routingsDao.findById(1L);

        assertTrue(foundRoutings.isPresent());
        assertEquals(routings, foundRoutings.get());
    }

    @Test
    void testFindByGuid() {
        Routings routings = jsonTestUtility.getRoutingsList().get(0);
        Optional<Routings> optionalRoutings = Optional.of(routings);
        when(routingsRepository.findByGuid(any(UUID.class))).thenReturn(optionalRoutings);

        Optional<Routings> foundRoutings = routingsDao.findByGuid(UUID.randomUUID());

        assertTrue(foundRoutings.isPresent());
        assertEquals(routings, foundRoutings.get());
    }

    @Test
    void testDelete() {
        Routings routings = jsonTestUtility.getRoutingsList().get(0);

        assertDoesNotThrow(() -> routingsDao.delete(routings));
        verify(routingsRepository, Mockito.times(1)).delete(routings);
    }

    @Test
    void testUpdateEntityFromBooking() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Routings> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullRoutings() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Routings> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_Failure() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromBooking(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromBooking() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromBooking(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromBooking_NullId() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromBooking(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromBooking_RetrievalFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(RuntimeException.class, () -> spyService.saveEntityFromBooking(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromBooking_AuditLogFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromBooking(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }
}
