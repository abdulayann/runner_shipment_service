package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
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
class PackingDaoTest {

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private IPackingRepository packingRepository;

    @Mock
    private AuditLogService auditLogService;

    @InjectMocks
    private PackingDao packingDao;

    private static JsonTestUtility jsonTestUtility;
    private static Packing testPacking;

    private static ObjectMapper objectMapperTest;
    private static PackingRequest testPackingRequest;
    private static PackingResponse testPackingResponse;

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
        testPacking = jsonTestUtility.getTestPacking();
        testPackingRequest = objectMapperTest.convertValue(testPacking , PackingRequest.class);
        testPackingResponse = objectMapperTest.convertValue(testPacking , PackingResponse.class);
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
    void save() {
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(testPacking));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(packingRepository.save(any(Packing.class))).thenReturn(testPacking);

        Packing packing = packingDao.save(testPacking);

        assertEquals(testPacking, packing);
    }

    @Test
    void testSave_Failure() {
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(testPacking));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> packingDao.save(testPacking));
    }

    @Test
    void findAll() {
        Page<Packing> packingPage = mock(Page.class);
        Specification<Packing> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(packingRepository.findAll(spec, pageable)).thenReturn(packingPage);

        Page<Packing> packings = packingDao.findAll(spec, pageable);

        assertEquals(packingPage, packings);
    }

    @Test
    void findAllWithoutTenantFilter() {
        Page<Packing> packingPage = mock(Page.class);
        Specification<Packing> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(packingRepository.findAllWithoutTenantFilter(spec, pageable)).thenReturn(packingPage);

        Page<Packing> packings = packingDao.findAllWithoutTenantFilter(spec, pageable);

        assertEquals(packingPage, packings);
    }

    @Test
    void findById() {
        Optional<Packing> optionalPacking = Optional.of(testPacking);
        when(packingRepository.findById(anyLong())).thenReturn(optionalPacking);

        Optional<Packing> packing = packingDao.findById(1L);

        assertTrue(packing.isPresent());
        assertEquals(testPacking, packing.get());
    }

    @Test
    void findByIdWithQuery() {
        Optional<Packing> optionalPacking = Optional.of(testPacking);
        when(packingRepository.findByIdWithQuery(anyLong())).thenReturn(optionalPacking);

        Optional<Packing> packing = packingDao.findByIdWithQuery(1L);

        assertTrue(packing.isPresent());
        assertEquals(testPacking, packing.get());
    }

    @Test
    void findByGuid() {
        Packing routings = jsonTestUtility.getCompleteShipment().getPackingList().get(0);
        Optional<Packing> optionalPacking = Optional.of(routings);
        when(packingRepository.findByGuid(any(UUID.class))).thenReturn(optionalPacking);

        Optional<Packing> foundPacking = packingDao.findByGuid(UUID.randomUUID());

        assertTrue(foundPacking.isPresent());
        assertEquals(routings, foundPacking.get());
    }

    @Test
    void findByGuidWithQuery() {
        Packing routings = jsonTestUtility.getCompleteShipment().getPackingList().get(0);
        Optional<Packing> optionalPacking = Optional.of(routings);
        when(packingRepository.findByGuidWithQuery(any(UUID.class))).thenReturn(optionalPacking);

        Optional<Packing> foundPacking = packingDao.findByGuidWithQuery(UUID.randomUUID());

        assertTrue(foundPacking.isPresent());
        assertEquals(routings, foundPacking.get());
    }

    @Test
    void delete() {
        Packing routings = jsonTestUtility.getCompleteShipment().getPackingList().get(0);

        assertDoesNotThrow(() -> packingDao.delete(routings));
        verify(packingRepository, Mockito.times(1)).delete(routings);
    }

    @Test
    void findByContainerIdInWithoutTenantFilter() {
        List<Packing> packingList = Collections.singletonList(testPacking);
        when(packingRepository.findByContainerIdInWithoutTenantFilter(List.of(1L))).thenReturn(packingList);

        List<Packing> packingRes = packingDao.findByContainerIdInWithoutTenantFilter(List.of(1L));

        assertEquals(packingList, packingRes);
    }

    @Test
    void testUpdateEntityFromShipment() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(any(), anyLong(), anyMap());
        List<Long> deletedConts = new ArrayList<>();
        deletedConts.add(5L);
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, deletedConts);
        assertNotNull(packings);
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_ContBranch() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(any(), anyLong(), anyMap());
        List<Long> deletedConts = new ArrayList<>();
        deletedConts.add(6L);
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, deletedConts);
        assertNotNull(packings);
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_NullPacking() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        List<Long> deletedConts = new ArrayList<>();
        List<Packing> packings = spyService.updateEntityFromShipment(null, 1L, deletedConts);
        assertEquals(new ArrayList<>(), packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_EmptyPacking() throws Exception{
        List<Packing> packings = new ArrayList<>();
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        List<Long> deletedConts = new ArrayList<>();
        List<Packing> packingsList = spyService.updateEntityFromShipment(packings, 1L, deletedConts);
        assertEquals(new ArrayList<>(), packingsList);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_NotNullId() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(any(), anyLong(), anyMap());
        List<Packing> routings = spyService.updateEntityFromShipment(packingList, 1L, null);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testUpdateEntityFromShipment_failure() throws Exception {
        PackingDao spyService = spy(packingDao);
        doThrow(new RuntimeException()).when(spyService).findByShipmentId(any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(null, 1L, null));
    }

    @Test
    void testUpdateEntityFromBooking() throws RunnerException {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(packingList)).when(spyService).findAll(any(), any());
        doReturn(packingList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Packing> routingsList1 = spyService.updateEntityFromBooking(packingList, 1L);
        assertNotNull(routingsList1);
        assertEquals(packingList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullPackings() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        List<Packing> routingsList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_EmptyPackings() throws RunnerException {
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(packingList)).when(spyService).findAll(any(), any());
        List<Packing> routingsList1 = spyService.updateEntityFromBooking(new ArrayList<>(), 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<Packing> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_Failure() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(new RuntimeException()).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromBooking(routingsList, 1L));
    }

    @Test
    void testUpdateEntityFromConsole() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        routingsList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromConsole(anyList(), anyLong(), anyMap());
        List<Packing> routingsList1 = spyService.updateEntityFromConsole(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromConsole_NullPacking() throws RunnerException {
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        List<Packing> packings = spyService.updateEntityFromConsole(null, 1L);
        assertEquals(new ArrayList<>(), packings);
    }

    @Test
    void testUpdateEntityFromConsole_EmptyPacking() throws RunnerException {
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        List<Packing> packings = spyService.updateEntityFromConsole(new ArrayList<>(), 1L);
        assertEquals(new ArrayList<>(), packings);
    }

    @Test
    void testUpdateEntityFromConsole_NullId() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        doReturn(packingList).when(spyService).saveEntityFromConsole(any(), anyLong(), anyMap());
        List<Packing> routings = spyService.updateEntityFromConsole(packingList, 1L);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_Failure() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(new RuntimeException()).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromConsole(routingsList, 1L));
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Packing> routings = spyService.updateEntityFromConsole(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_NullPacking() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        List<Packing> routings = spyService.updateEntityFromConsole(null, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(new ArrayList<>(), routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_EmptyPacking() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        List<Packing> routings = spyService.updateEntityFromConsole(new ArrayList<>(), 1L, routingsList);
        assertNotNull(routings);
        assertEquals(new ArrayList<>(), routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntityNull() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Packing> routings = spyService.updateEntityFromConsole(routingsList, 1L, null);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_AuditLogFailure() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(routingsList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Packing> routings = spyService.updateEntityFromConsole(routingsList, 1L, routingsList);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_oldEntityBranch() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromConsole(any(), anyLong());
        List<Packing> packings = spyService.updateEntityFromConsole(packingList, 1L, new ArrayList<>());
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromConsole_WithOldEntity_Failure() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(RuntimeException.class).when(spyService).saveEntityFromConsole(anyList(), anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromConsole(routingsList, 1L, routingsList));
    }

    @Test
    void getAllPackings() {
        testPacking.setConsolidationId(19L);
        packingDao.save(testPacking);
        var packingList = packingDao.getAllPackings();
        assertTrue(packingList.isEmpty());
    }

    @Test
    void testSaveAll() {
        List<Packing> routingsList = Arrays.asList(jsonTestUtility.getCompleteShipment().getPackingList().get(0), jsonTestUtility.getCompleteShipment().getPackingList().get(0));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(packingRepository.saveAll(anyList())).thenReturn(routingsList);

        List<Packing> savedPackingList = packingDao.saveAll(routingsList);

        assertEquals(routingsList.size(), savedPackingList.size());
        assertEquals(routingsList.get(0), savedPackingList.get(0));
        assertEquals(routingsList.get(1), savedPackingList.get(1));
    }

    @Test
    void testSaveAll_Failure() {
        List<Packing> routingsList = Arrays.asList(jsonTestUtility.getCompleteShipment().getPackingList().get(0), jsonTestUtility.getCompleteShipment().getPackingList().get(0));
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> packingDao.saveAll(routingsList));
    }

    @Test
    void testSaveEntityFromShipment() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_NullId() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_RetrievalFailure() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        routingsList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromShipment(routingsList, 1L));
    }

    @Test
    void testSaveEntityFromShipment_AuditLogFailure() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromShipment(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_Update() throws RunnerException {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.of(testPacking)).when(spyService).findById(anyLong());
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> packings = spyService.saveEntityFromShipment(packingList, 1L);
        Assertions.assertEquals(packingList, packings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(packingList).when(spyService).saveAll(any());
        Map<Long, Packing> map = new HashMap<>();
        map.put(testPacking.getId(), testPacking);
        List<Packing> routings = spyService.saveEntityFromShipment(packingList, 1L, map);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity_NewMap() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        Map<Long, Packing> map = new HashMap<>();
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromShipment(packingList, 1L, map));
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity_NullId() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Packing> map = new HashMap<>();
        map.put(testPacking.getId(), testPacking);
        List<Packing> routings = spyService.saveEntityFromShipment(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromShipment_WithOldEntity_AuditLogFailure() throws Exception{
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveAll(any());
        Map<Long, Packing> map = new HashMap<>();
        map.put(testPacking.getId(), testPacking);
        List<Packing> routings = spyService.saveEntityFromShipment(routingsList, 1L, map);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromBooking() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(packingList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromBooking(packingList, 1L);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testSaveEntityFromBooking_hashMap() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromBooking(packingList, 1L));
    }

    @Test
    void testSaveEntityFromBooking_NullId() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromBooking(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromBooking_RetrievalFailure() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> packings = spyService.saveEntityFromBooking(packingList, 1L);
        assertEquals(packingList, packings);
    }

    @Test
    void testSaveEntityFromBooking_AuditLogFailure() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromBooking(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromConsole() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.of(testPacking)).when(spyService).findById(anyLong());
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromConsole(packingList, 1L);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testSaveEntityFromConsole_NullId() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> routings = spyService.saveEntityFromConsole(routingsList, 1L);
        assertNotNull(routings);
        assertEquals(routingsList, routings);
    }

    @Test
    void testSaveEntityFromConsole_RetrievalFailure() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromConsole(packingList, 1L));
    }

    @Test
    void testSaveEntityFromConsole_WithOldEntity() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveAll(any());
        Map<Long, Packing> map = new HashMap<>();
        map.put(testPacking.getId(), testPacking);
        List<Packing> routings = spyService.saveEntityFromConsole(packingList, 1L, map);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testSaveEntityFromConsole_WithOldEntity_Failure() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        Map<Long, Packing> map = new HashMap<>();
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromConsole(packingList, 1L, map));
    }

    @Test
    void testSaveEntityFromConsole_WithOldEntity_NullId() throws Exception{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveAll(any());
        Map<Long, Packing> map = new HashMap<>();
        map.put(testPacking.getId(), testPacking);
        List<Packing> routings = spyService.saveEntityFromConsole(packingList, 1L, map);
        assertNotNull(routings);
        assertEquals(packingList, routings);
    }

    @Test
    void testUpdateEntityFromShipment_DeletePackingCases() throws RunnerException {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        List<Packing> oldPackingList = Arrays.asList(testPacking, objectMapperTest.convertValue(testPacking, Packing.class));
        oldPackingList.get(1).setId(5L);
        PackingDao spyService = spy(packingDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Packing> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L, null);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_DeletePackingCases_AuditLogFailure() throws Exception {
        List<Packing> routingsList = Collections.singletonList(testPacking);
        List<Packing> oldPackingList = Arrays.asList(testPacking, objectMapperTest.convertValue(testPacking, Packing.class));
        oldPackingList.get(1).setId(5L);
        PackingDao spyService = spy(packingDao);
        doReturn(routingsList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Packing> routingsList1 = spyService.updateEntityFromShipment(routingsList, 1L, null);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromShipment_DeletePackingCases_Failures() throws Exception {
        List<Packing> packingList = Collections.singletonList(testPacking);
        List<Packing> oldPackingList = Arrays.asList(testPacking, objectMapperTest.convertValue(testPacking, Packing.class));
        oldPackingList.get(1).setId(5L);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(anyList(), anyLong(), any());
        List<Packing> routingsList1 = spyService.updateEntityFromShipment(packingList, 1L, null);
        assertNotNull(routingsList1);
        assertEquals(packingList, routingsList1);
    }

    @Test
    void testSaveEntityFromContainer() {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.of(testPacking)).when(spyService).findById(anyLong());
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> packings = spyService.saveEntityFromContainer(packingList, 1L);
        assertEquals(packingList, packings);
    }

    @Test
    void testSaveEntityFromContainer_Failure() {
        List<Packing> packingList = Collections.singletonList(testPacking);
        packingList.get(0).setId(1L);
        PackingDao spyService = spy(packingDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromContainer(packingList, 1L));
    }

    @Test
    void testSaveEntityFromContainer_NullId() {
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(testPacking).when(spyService).save(any());
        List<Packing> packings = spyService.saveEntityFromContainer(packingList, 1L);
        assertEquals(packingList, packings);
    }

    @Test
    void testDeleteEntityFromContainer() {
        PackingDao spyService = spy(packingDao);
        List<Packing> packingList = Collections.singletonList(testPacking);
        doReturn(new PageImpl<>(packingList)).when(spyService).findAll(any(), any());
        doReturn(packingList).when(spyService).saveEntityFromContainer(any(), any());
        spyService.deleteEntityFromContainer(1L);
        verify(spyService, times(1)).saveEntityFromContainer(any(), any());
    }

    @Test
    void testFindByConsolidationId() {
        List<Packing> packingList = Collections.singletonList(testPacking);
        when(packingRepository.findByConsolidationId(anyLong())).thenReturn(packingList);
        List<Packing> packings = packingDao.findByConsolidationId(1L);
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(anyList(), anyLong());
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, new ArrayList<>(), new ArrayList<>(), new HashSet<>(), new HashMap<>());
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntityNull() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(anyList(), anyLong());
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, null, null, null, new HashMap<>());
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithSameOldEntity() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(anyList(), anyLong());
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, packingList, new ArrayList<>(), new HashSet<>(), new HashMap<>());
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_ConsolePacks() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doReturn(packingList).when(spyService).saveEntityFromShipment(anyList(), anyLong());
        List<Packing> packings = spyService.updateEntityFromShipment(packingList, 1L, new ArrayList<>(), List.of(testPacking), new HashSet<>(), new HashMap<>());
        assertEquals(packingList, packings);
    }

    @Test
    void testUpdateEntityFromShipment_WithOldEntity_Failure() throws RunnerException{
        List<Packing> packingList = Collections.singletonList(testPacking);
        PackingDao spyService = spy(packingDao);
        doThrow(new RuntimeException()).when(spyService).saveEntityFromShipment(anyList(), anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(packingList, 1L, new ArrayList<>(), new ArrayList<>(), new HashSet<>(), new HashMap<>()));
    }

}
