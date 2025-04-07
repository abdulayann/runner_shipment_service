package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
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
class TruckDriverDetailsDaoTest {

    @Mock
    private ITruckDriverDetailsRepository truckDriverDetailsRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @InjectMocks
    private TruckDriverDetailsDao truckDriverDetailsDao;

    private TruckDriverDetails testData;
    private JsonTestUtility jsonTestUtility;


    @BeforeEach
    void setUp() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        testData = jsonTestUtility.getTestTruckDriverDetails();
        UserContext.setUser(
            UsersDto.builder().Username("user").TenantId(1).build());
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidTruckDriverDetails_ReturnsSavedTruckDriverDetails() {
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        when(truckDriverDetailsRepository.save(any(TruckDriverDetails.class))).thenReturn(truckDriverDetails);

        TruckDriverDetails savedTruckDriverDetails = truckDriverDetailsDao.save(truckDriverDetails);

        assertEquals(truckDriverDetails, savedTruckDriverDetails);
        verify(truckDriverDetailsRepository).save(any(TruckDriverDetails.class));
    }

    @Test
    void saveAll_ValidTruckDriverDetailss_ReturnsSavedTruckDriverDetailss() {
        List<TruckDriverDetails> truckDriverDetailsList = Arrays.asList(new TruckDriverDetails(), new TruckDriverDetails());
        when(truckDriverDetailsRepository.saveAll(anyList())).thenReturn(truckDriverDetailsList);
        List<TruckDriverDetails> savedTruckDriverDetailss = truckDriverDetailsDao.saveAll(truckDriverDetailsList);
        assertEquals(truckDriverDetailsList, savedTruckDriverDetailss);
        verify(truckDriverDetailsRepository).saveAll(truckDriverDetailsList);
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfTruckDriverDetailss() {
        Page<TruckDriverDetails> expectedPage = mock(Page.class);
        Specification<TruckDriverDetails> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(truckDriverDetailsRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<TruckDriverDetails> resultPage = truckDriverDetailsDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(truckDriverDetailsRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfTruckDriverDetails() {
        Long id = 1L;
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        when(truckDriverDetailsRepository.findById(id)).thenReturn(Optional.of(truckDriverDetails));

        Optional<TruckDriverDetails> result = truckDriverDetailsDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(truckDriverDetails, result.get());
        verify(truckDriverDetailsRepository).findById(id);
    }

    @Test
    void delete_ValidTruckDriverDetails_CallsRepositoryDelete() {
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        truckDriverDetailsDao.delete(truckDriverDetails);
        verify(truckDriverDetailsRepository).delete(truckDriverDetails);
    }


    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList() {
        Long shipmentId = 1L;
        var truckDriverDetailsDaoSpy = Mockito.spy(truckDriverDetailsDao);
        when(truckDriverDetailsDao.findByShipmentId(anyLong())).thenThrow(new RuntimeException("Test"));
        assertThrows(RunnerException.class, () -> truckDriverDetailsDaoSpy.updateEntityFromShipment(Collections.emptyList(), shipmentId));
    }

    @Test
    void updateEntityFromShipment_TruckDriverDetailsListHasElements_ReturnsResponseTruckDriverDetails() throws RunnerException {
        Long shipmentId = 1L;
        List<TruckDriverDetails> truckDriverDetailsList = Arrays.asList(new TruckDriverDetails(), new TruckDriverDetails());
        var truckDriverDetailsDaoSpy = Mockito.spy(truckDriverDetailsDao);
        doReturn(truckDriverDetailsList).when(truckDriverDetailsDaoSpy).saveEntityFromShipment(anyList(), eq(shipmentId), anyMap());
        List<TruckDriverDetails> result = truckDriverDetailsDaoSpy.updateEntityFromShipment(truckDriverDetailsList, shipmentId);
        assertEquals(truckDriverDetailsList, result);
    }

    @Test
    void updateEntityFromShipment_TruckDriverDetailsListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        testData.setId(1L);
        when(truckDriverDetailsRepository.findByShipmentId(any())).thenReturn(List.of(testData));
        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromShipment(Collections.singletonList(testData), shipmentId);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void saveEntityFromShipment_ValidDataWithExistingIds_ReturnsList() {
        List<TruckDriverDetails> truckDriverDetailss = Arrays.asList(new TruckDriverDetails(), new TruckDriverDetails());
        Long shipmentId = 1L;
        Map<Long, TruckDriverDetails> oldEntityMap = new HashMap<>();
        oldEntityMap.put(1L, new TruckDriverDetails());
        oldEntityMap.put(2L, new TruckDriverDetails());
        when(truckDriverDetailsDao.saveAll(anyList())).thenReturn(truckDriverDetailss);

        List<TruckDriverDetails> result = truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailss, shipmentId, oldEntityMap);

        assertNotNull(result);
        assertEquals(truckDriverDetailss.size(), result.size());
    }

    @Test
    void deleteTruckDriverDetails_ValidData_CallsDeleteAndAuditLogService() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        Map<Long, TruckDriverDetails> hashMap = new HashMap<>();
        TruckDriverDetails truckDriverDetails1 = new TruckDriverDetails();
        TruckDriverDetails truckDriverDetails2 = new TruckDriverDetails();
        hashMap.put(1L, truckDriverDetails1);
        hashMap.put(2L, truckDriverDetails2);

        truckDriverDetailsDao.deleteTruckDriverDetails(hashMap, "entityType", 1L);

        verify(truckDriverDetailsRepository, times(2)).delete(any(TruckDriverDetails.class));
        verify(auditLogService, times(2)).addAuditLog(any());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;

        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, null);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, Collections.emptyList());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_TruckDriverDetailsListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<TruckDriverDetails> oldEntityList = Arrays.asList(new TruckDriverDetails(), new TruckDriverDetails());
        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromShipment(null, shipmentId, oldEntityList);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList_() {
        Long shipmentId = 1L;
        List<TruckDriverDetails> oldEntityList = Arrays.asList(new TruckDriverDetails(), new TruckDriverDetails());
        var truckDriverDetailsDaospy = Mockito.spy(truckDriverDetailsDao);
        doThrow(new RuntimeException()).when(truckDriverDetailsDaospy).saveEntityFromShipment(any() , any());
        assertThrows(RunnerException.class, () -> truckDriverDetailsDaospy.updateEntityFromShipment(List.of(testData), shipmentId, oldEntityList));
    }

    @Test
    void saveEntityFromShipment_ExceptionThrown_ReturnsEmptyList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long shipmentId = 1L;
        testData.setId(1L);
        List<TruckDriverDetails> truckDriverDetailss = List.of(testData);
        when(truckDriverDetailsDao.findById(anyLong())).thenReturn(Optional.of(testData));
        doThrow(IllegalArgumentException.class).when(auditLogService).addAuditLog(any());
        assertThrows(Exception.class, () -> truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailss, shipmentId));
    }

    @Test
    void saveEntityFromShipment_TruckDriverDetailssListHasElements_ReturnsPopulatedList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long shipmentId = 1L;
        List<TruckDriverDetails> truckDriverDetailss = Collections.singletonList(testData);
        when(truckDriverDetailsDao.findById(anyLong())).thenReturn(Optional.of(new TruckDriverDetails()));
        when(truckDriverDetailsDao.save(any())).thenReturn(new TruckDriverDetails());
        List<TruckDriverDetails> result = truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailss, shipmentId);
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertEquals(truckDriverDetailss.size(), result.size());
        verify(auditLogService,times(1)).addAuditLog(any());
    }

    @Test
    void delete_ValidTruckDriverDetails_ThrowsException() {
        assertDoesNotThrow(() ->truckDriverDetailsDao.deleteTruckDriverDetails(Map.of(1L , testData),"truckDriverDetails", 1L));
    }

    @Test
    void saveEntityFromConsole_ValidInput_ReturnsSavedEntities() {
        // Arrange
        TruckDriverDetails truckDriverDetail1 = new TruckDriverDetails();
        truckDriverDetail1.setId(1L);
        truckDriverDetail1.setCreatedBy("user1");

        TruckDriverDetails truckDriverDetail2 = new TruckDriverDetails();
        truckDriverDetail2.setId(2L);

        when(truckDriverDetailsRepository.saveAll(anyList())).thenReturn(List.of(truckDriverDetail1, truckDriverDetail2));

        // Act
        List<TruckDriverDetails> savedEntities = truckDriverDetailsDao.saveEntityFromConsole(
                List.of(truckDriverDetail1, testData),
                123L,
                Map.of(1L , truckDriverDetail1, testData.getId(), testData)
        );

        // Assert
        assertEquals(2, savedEntities.size());
        assertEquals("user1", savedEntities.get(0).getCreatedBy());
        assertNull(savedEntities.get(1).getCreatedAt());
        assertNull(savedEntities.get(1).getCreatedBy());
        verify(truckDriverDetailsRepository, times(1)).saveAll(anyList());
    }

    @Test
    void saveEntityFromConsole_InvalidInput_ThrowsException() {
        // Arrange
        TruckDriverDetails truckDriverDetail1 = new TruckDriverDetails();
        truckDriverDetail1.setId(1L);
        truckDriverDetail1.setCreatedBy("user1");

        TruckDriverDetails truckDriverDetail2 = new TruckDriverDetails();
        truckDriverDetail2.setId(2L);

        assertThrows(Exception.class, () -> truckDriverDetailsDao.saveEntityFromConsole(
                List.of(truckDriverDetail1, testData),
                123L,
                Collections.emptyMap()
        ));
    }

    @Test
    void updateEntityFromConsole_ValidInput_ReturnsResponseTruckDriverDetails() throws RunnerException {
        // Arrange
        long consolidationId = 123L;
        List<ConsoleShipmentMapping> consoleShipmentMappings = List.of(
                new ConsoleShipmentMapping(1L, 1L, true, null),
                new ConsoleShipmentMapping(2L, 2L, true, null)
        );
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(consoleShipmentMappings);

        List<TruckDriverDetails> truckDriverDetails = List.of(testData);
        when(truckDriverDetailsRepository.findAll(ArgumentMatchers.<Specification<TruckDriverDetails>>any(), (Pageable) any())).thenReturn(new PageImpl<>(truckDriverDetails));

        // Act
        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromConsole(truckDriverDetails, consolidationId);

        // Assert
        assertNotNull(result);
        verify(consoleShipmentMappingDao, times(1)).findByConsolidationId(consolidationId);
        verify(truckDriverDetailsRepository, times(1)).findAll(ArgumentMatchers.<Specification<TruckDriverDetails>>any(), (Pageable) any());
    }

    @Test
    void updateEntityFromConsole_EmptyShipmentIds_ReturnsEmptyList() throws RunnerException {
        // Arrange
        long consolidationId = 123L;
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(new ArrayList<>());
        when(truckDriverDetailsRepository.findAll(ArgumentMatchers.<Specification<TruckDriverDetails>>any(), (Pageable) any())).thenReturn(new PageImpl<>(List.of(testData)));

        // Act
        List<TruckDriverDetails> result = truckDriverDetailsDao.updateEntityFromConsole(new ArrayList<>(), consolidationId);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(consoleShipmentMappingDao, times(1)).findByConsolidationId(consolidationId);
    }

    @Test
    void saveEntityFromShipmentEntityNotPresent() {
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        truckDriverDetails.setId(1L);
        List<TruckDriverDetails> truckDriverDetailsList = List.of(truckDriverDetails);

        when(truckDriverDetailsRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,() -> truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailsList, 1L));
    }

    @Test
    void saveEntityFromShipmentMapNotContainsId() {
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        truckDriverDetails.setId(1L);
        List<TruckDriverDetails> truckDriverDetailsList = List.of(truckDriverDetails);
        Map<Long, TruckDriverDetails> oldEntityMap = new HashMap<>();
        assertThrows(DataRetrievalFailureException.class,() -> truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailsList, 1L, oldEntityMap));
    }

    @Test
    void saveEntityFromShipment() {
        TruckDriverDetails truckDriverDetails = new TruckDriverDetails();
        truckDriverDetails.setId(1L);

        HashMap<Long, TruckDriverDetails> map = new HashMap<>();
        map.put(1L, truckDriverDetails);

        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(truckDriverDetailsRepository.saveAll(any())).thenReturn(List.of(truckDriverDetails));

        List<TruckDriverDetails> truckDriverDetailsList = List.of(truckDriverDetails);
        assertEquals(truckDriverDetailsList, truckDriverDetailsDao.saveEntityFromShipment(truckDriverDetailsList, 1L, map));
    }

}