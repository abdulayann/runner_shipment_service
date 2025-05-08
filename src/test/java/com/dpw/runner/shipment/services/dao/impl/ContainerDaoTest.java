package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.doReturn;

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
    private static Containers testContainer;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
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
        Set<ShipmentDetails> shipmentDetails = new HashSet<>();
        shipmentDetails.add(new ShipmentDetails());
        testContainer.setShipmentsList(shipmentDetails);
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
        Set<ShipmentDetails> shipmentDetails = new HashSet<>();
        testContainer.setShipmentsList(shipmentDetails);
        List<TruckDriverDetails> truckDriverDetails = new ArrayList<>();
        testContainer.setTruckingDetails(truckDriverDetails);
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
    void testUpdateEntityFromBooking_Failure() {
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
    void testSaveEntityFromBooking_RetrievalFailure() {
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
    void testUpdateEntityFromShipmentConsole() throws RunnerException {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(containersList).when(spyService).saveAll(anyList());
        List<Containers> containers = spyService.updateEntityFromShipmentConsole(containersList, 1L, 2L, true);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void testUpdateEntityFromConsolidationV1() throws RunnerException {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(containersList).when(spyService).saveAll(anyList());
        List<Containers> containers = spyService.updateEntityFromConsolidationV1(containersList, 1L, containersList);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void testUpdateEntityFromConsolidationV1_Failure() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        ContainerDao spyService = spy(containerDao);
        doThrow(new RuntimeException()).when(spyService).saveAll(anyList());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromConsolidationV1(containersList, 1L, containersList));
    }

    @Test
    void testUpdateEntityFromShipmentV1() throws RunnerException {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        ContainerDao spyService = spy(containerDao);
        doReturn(containersList).when(spyService).saveAll(anyList());
        List<Containers> containers = spyService.updateEntityFromShipmentV1(containersList, containersList);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void testUpdateEntityFromShipmentV1_Failure() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        ContainerDao spyService = spy(containerDao);
        doThrow(new RuntimeException()).when(spyService).saveAll(anyList());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipmentV1(containersList, containersList));
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
    void findByShipmentIdWithoutTenantFilter() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        when(containerRepository.findAllWithoutTenantFilter(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl<>(containersList));
        List<Containers> containers = containerDao.findByShipmentIdWithoutTenantFilter(4L);
        assertNotNull(containers);
        assertEquals(containersList, containers);
    }

    @Test
    void findAllWithoutTenantFilter() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        when(containerRepository.findAllWithoutTenantFilter(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl<>(containersList));
        Specification<Containers> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        Page<Containers> containers = containerDao.findAllWithoutTenantFilter(spec, pageable);
        assertNotNull(containers);
        assertEquals(containersList, containers.stream().toList());
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
    void findByConsolidationIdWithoutTenantFilter() {
        assertDoesNotThrow(() -> containerDao.findByConsolidationIdWithoutTenantFilter(1L));
    }

    @Test
    void getAllContainers() {
        assertDoesNotThrow(() -> containerDao.getAllContainers());
    }

    @Test
    void updateEntityFromShipmentConsole() {
        when(containerRepository.findByConsolidationId(anyLong())).thenThrow(new RuntimeException());
        assertThrows(RunnerException.class, () -> containerDao.updateEntityFromShipmentConsole(List.of(testContainer), 3L, null, true));
    }

    @Test
    void updateEntityFromConsolidationV1() throws RunnerException {
        UUID uuid1 = UUID.randomUUID();
        UUID uuid2 = UUID.randomUUID();

        Containers container1 = Containers.builder().build();
        container1.setGuid(uuid1);

        Containers container2 = Containers.builder().build();
        container2.setGuid(uuid2);

        List<Containers> containersList = Arrays.asList(container1);
        List<Containers> oldList = Arrays.asList(container1, container2);

        doNothing().when(containerRepository).deleteById(any());
        when(containerRepository.save(any())).thenReturn(container1);

        assertEquals(containersList, containerDao.updateEntityFromConsolidationV1(containersList, 1L, oldList));
    }
}
