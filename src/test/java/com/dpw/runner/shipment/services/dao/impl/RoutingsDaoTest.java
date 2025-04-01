package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
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
import org.springframework.dao.DataRetrievalFailureException;
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
class RoutingsDaoTest extends CommonMocks {

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
    static void init() {
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testRoutings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        testNewRoutings = jsonTestUtility.getTestRouting();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG")
                .enableRouteMaster(true).build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSave() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(routingsRepository.save(any(Routings.class))).thenReturn(routings);

        Routings savedRoutings = routingsDao.save(routings);

        assertEquals(routings, savedRoutings);
    }

    @Test
    void testSave_CarriageValidationFailure() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        routings.setCarriage(RoutingCarriage.ON_CARRIAGE);
        routings.setIsSelectedForDocument(Boolean.TRUE);

        assertThrows(ValidationException.class, () -> routingsDao.save(routings));
    }

    @Test
    void testSave_CarriageValidationFailurePreCarriage() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        routings.setCarriage(RoutingCarriage.PRE_CARRIAGE);
        routings.setIsSelectedForDocument(Boolean.TRUE);

        assertThrows(ValidationException.class, () -> routingsDao.save(routings));
    }

    @Test
    void testSave_Failure() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(routings));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> routingsDao.save(routings));
    }

    @Test
    void testSaveAll() {
        List<Routings> routingsList = Arrays.asList(jsonTestUtility.getCompleteShipment().getRoutingsList().get(0), jsonTestUtility.getCompleteShipment().getRoutingsList().get(0));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(routingsRepository.saveAll(anyList())).thenReturn(routingsList);

        List<Routings> savedRoutingsList = routingsDao.saveAll(routingsList);

        assertEquals(routingsList.size(), savedRoutingsList.size());
        assertEquals(routingsList.get(0), savedRoutingsList.get(0));
        assertEquals(routingsList.get(1), savedRoutingsList.get(1));
    }

    @Test
    void testSaveAll_Failure() {
        List<Routings> routingsList = Arrays.asList(jsonTestUtility.getCompleteShipment().getRoutingsList().get(0), jsonTestUtility.getCompleteShipment().getRoutingsList().get(0));
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
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        Optional<Routings> optionalRoutings = Optional.of(routings);
        when(routingsRepository.findById(anyLong())).thenReturn(optionalRoutings);

        Optional<Routings> foundRoutings = routingsDao.findById(1L);

        assertTrue(foundRoutings.isPresent());
        assertEquals(routings, foundRoutings.get());
    }

    @Test
    void testFindByGuid() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);
        Optional<Routings> optionalRoutings = Optional.of(routings);
        when(routingsRepository.findByGuid(any(UUID.class))).thenReturn(optionalRoutings);

        Optional<Routings> foundRoutings = routingsDao.findByGuid(UUID.randomUUID());

        assertTrue(foundRoutings.isPresent());
        assertEquals(routings, foundRoutings.get());
    }

    @Test
    void testDelete() {
        Routings routings = jsonTestUtility.getCompleteShipment().getRoutingsList().get(0);

        assertDoesNotThrow(() -> routingsDao.delete(routings));
        verify(routingsRepository, Mockito.times(1)).delete(routings);
    }

    @Test
    void testUpdateEntityFromShipment() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_NullId() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_NullRoutings() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(null, 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_DeleteRoutingsCases() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        List<Routings> oldRoutingsList = Arrays.asList(testRoutings, objectMapperTest.convertValue(testRoutings, Routings.class));
        oldRoutingsList.get(1).setId(5L);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_DeleteRoutingsCases_Failures() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        List<Routings> oldRoutingsList = Arrays.asList(testRoutings, objectMapperTest.convertValue(testRoutings, Routings.class));
        oldRoutingsList.get(1).setId(5L);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_DeleteRoutingsCases_AuditLogFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        List<Routings> oldRoutingsList = Arrays.asList(testRoutings, objectMapperTest.convertValue(testRoutings, Routings.class));
        oldRoutingsList.get(1).setId(5L);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Routings> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_Failure() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromShipment() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(Optional.of(testRoutings)).when(spyService).findById(anyLong());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_NullId() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_RetrievalFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(RuntimeException.class, () -> spyService.saveEntityFromShipment(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromShipment_AuditLogFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(Optional.of(testRoutings)).when(spyService).findById(anyLong());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Routings> map = new HashMap<>();
        map.put(testRoutings.getId(), testRoutings);
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity_NullId() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Routings> map = new HashMap<>();
        map.put(testRoutings.getId(), testRoutings);
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity_AuditLogFailure() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Routings> map = new HashMap<>();
        map.put(testRoutings.getId(), testRoutings);
        List<Routings> routings = spyService.saveEntityFromShipment(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
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

    @Test
    void testUpdateEntityFromConsole() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(anyList(), anyLong(), anyMap());
        List<Routings> routingsList1 = spyService.updateEntityFromConsole(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromConsole_NullRoutings() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        List<Routings> routingsList1 = spyService.updateEntityFromConsole(null, 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromConsole_Failure() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromConsole(routingsList, 1L));
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Routings> routings = spyService.updateEntityFromConsole(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_NullRoutings() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        List<Routings> routings = spyService.updateEntityFromConsole(null, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(new ArrayList<>(), routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntityNull() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Routings> routings = spyService.updateEntityFromConsole(routingsList, 1L, null);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_AuditLogFailure() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Routings> routings = spyService.updateEntityFromConsole(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_Failure() throws RunnerException {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doThrow(RuntimeException.class).when(spyService).saveEntityFromConsole(anyList(), anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromConsole(routingsList, 1L, routingsList));
    }

    @Test
    void testSaveEntityFromConsole() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(Optional.of(testRoutings)).when(spyService).findById(anyLong());
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromConsole(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromConsole_NullId() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(testRoutings).when(spyService).save(any());
        List<Routings> routings = spyService.saveEntityFromConsole(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromConsole_RetrievalFailure() throws Exception {
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromConsole(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromConsole_WithOldEntity() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Routings> map = new HashMap<>();
        map.put(testRoutings.getId(), testRoutings);
        List<Routings> routings = spyService.saveEntityFromConsole(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromConsole_WithOldEntity_NullId() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Routings> map = new HashMap<>();
        map.put(testRoutings.getId(), testRoutings);
        List<Routings> routings = spyService.saveEntityFromConsole(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(any(), anyLong());
        List<Routings> routings = spyService.updateEntityFromShipment(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_NullRoutings() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        List<Routings> routings = spyService.updateEntityFromShipment(null, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(new ArrayList<>(), routings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_NullId() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testNewRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(any(), anyLong());
        List<Routings> routings = spyService.updateEntityFromShipment(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_Failure() throws Exception{
        List<Routings> routingsList = Collections.singletonList(testRoutings);
        RoutingsDao spyService = spy(routingsDao);
        doThrow(RuntimeException.class).when(spyService).saveEntityFromShipment(any(), anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(routingsList, 1L, routingsList));
    }


    @Test
    void testGetCustomerBookingRequestRoutingList_NoExistingRoutingList() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");

        mockShipmentSettings();
        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, null);

        assertNotNull(result);
        assertEquals(3, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_AIR() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");
        carrierDetails.setFlightNumber("554");
        carrierDetails.setShippingLine("air");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_AIR);

        assertNotNull(result);
        assertEquals(3, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_NullCarrierDetails() {

        List<Routings> result = routingsDao.generateDefaultRouting(null, Constants.TRANSPORT_MODE_AIR);

        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_NullOrigin() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin(null);
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_NullPortOfLoading() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort(null);
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_NullPortOfDischarge() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort(null);
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_NullDestination() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination(null);
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_SameOriginAndPortOfLoading() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Origin");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_SamePortOfLoadingAndPortOfDischarge() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Port of Loading");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_SamePortOfDischargeAndDestination() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Port of Loading");
        carrierDetails.setDestinationPort("Destination");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_SameOriginPortOfLoadingAndPortOfDischarge() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Origin");
        carrierDetails.setDestinationPort("Origin");
        carrierDetails.setDestination("Destination");
        mockShipmentSettings();

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(1, result.size());
    }

    @Test
    void testGetCustomerBookingRequestRoutingList_SameOriginPortOfLoadingPortOfDischargeAndDestination() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("Origin");
        carrierDetails.setDestinationPort("Origin");
        carrierDetails.setDestination("Origin");

        List<Routings> result = routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA);

        assertNotNull(result);
        assertEquals(0, result.size());
    }

}
