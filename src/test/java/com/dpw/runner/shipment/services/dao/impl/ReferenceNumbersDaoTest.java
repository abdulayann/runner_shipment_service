//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
//import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
//import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import com.dpw.runner.shipment.services.helpers.JsonHelper;
//import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
//import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
//import com.dpw.runner.shipment.services.validator.ValidatorUtility;
//import com.fasterxml.jackson.core.JsonProcessingException;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.jupiter.api.parallel.Execution;
//import org.junit.jupiter.api.parallel.ExecutionMode;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.Mockito;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.data.domain.Page;
//import org.springframework.data.domain.PageImpl;
//import org.springframework.data.domain.Pageable;
//import org.springframework.data.jpa.domain.Specification;
//
//import java.io.IOException;
//import java.lang.reflect.InvocationTargetException;
//import java.util.*;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.mockito.ArgumentMatchers.*;
//import static org.mockito.Mockito.*;
//
//@ExtendWith(MockitoExtension.class)
//@Execution(ExecutionMode.CONCURRENT)
//class ReferenceNumbersDaoTest {
//
//    @Mock
//    private IReferenceNumbersRepository referenceNumbersRepository;
//
//    @Mock
//    private ValidatorUtility validatorUtility;
//
//    @Mock
//    private JsonHelper jsonHelper;
//
//    @Mock
//    private IAuditLogService auditLogService;
//
//    @Mock
//    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
//
//    @InjectMocks
//    private ReferenceNumbersDao referenceNumbersDao;
//
//    private ReferenceNumbers testData;
//    private JsonTestUtility jsonTestUtility;
//
//
//    @BeforeEach
//    void setUp() throws IOException {
//        jsonTestUtility = new JsonTestUtility();
//        testData = jsonTestUtility.getTestReferenceNumbers();
//        MockitoAnnotations.openMocks(this);
//    }
//
//    @Test
//    void save_ValidReferenceNumbers_ReturnsSavedReferenceNumbers() {
//        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
//        when(referenceNumbersRepository.save(any(ReferenceNumbers.class))).thenReturn(referenceNumbers);
//
//        ReferenceNumbers savedReferenceNumbers = referenceNumbersDao.save(referenceNumbers);
//
//        assertEquals(referenceNumbers, savedReferenceNumbers);
//        verify(referenceNumbersRepository).save(any(ReferenceNumbers.class));
//    }
//
//    @Test
//    void saveAll_ValidReferenceNumberss_ReturnsSavedReferenceNumberss() {
//        List<ReferenceNumbers> referenceNumbersList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
//        when(referenceNumbersRepository.saveAll(anyList())).thenReturn(referenceNumbersList);
//        List<ReferenceNumbers> savedReferenceNumberss = referenceNumbersDao.saveAll(referenceNumbersList);
//        assertEquals(referenceNumbersList, savedReferenceNumberss);
//        verify(referenceNumbersRepository).saveAll(referenceNumbersList);
//    }
//
//    @Test
//    void findAll_ValidSpecificationAndPageable_ReturnsPageOfReferenceNumberss() {
//        Page<ReferenceNumbers> expectedPage = mock(Page.class);
//        Specification<ReferenceNumbers> spec = mock(Specification.class);
//        Pageable pageable = mock(Pageable.class);
//        when(referenceNumbersRepository.findAll(spec, pageable)).thenReturn(expectedPage);
//
//        Page<ReferenceNumbers> resultPage = referenceNumbersDao.findAll(spec, pageable);
//
//        assertEquals(expectedPage, resultPage);
//        verify(referenceNumbersRepository).findAll(spec, pageable);
//    }
//
//    @Test
//    void findById_ValidId_ReturnsOptionalOfReferenceNumbers() {
//        Long id = 1L;
//        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
//        when(referenceNumbersRepository.findById(id)).thenReturn(Optional.of(referenceNumbers));
//
//        Optional<ReferenceNumbers> result = referenceNumbersDao.findById(id);
//
//        assertTrue(result.isPresent());
//        assertEquals(referenceNumbers, result.get());
//        verify(referenceNumbersRepository).findById(id);
//    }
//
//    @Test
//    void delete_ValidReferenceNumbers_CallsRepositoryDelete() {
//        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
//        referenceNumbersDao.delete(referenceNumbers);
//        verify(referenceNumbersRepository).delete(referenceNumbers);
//    }
//
//
//    @Test
//    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList() {
//        Long shipmentId = 1L;
//        var referenceNumbersDaoSpy = Mockito.spy(referenceNumbersDao);
//        doThrow(new RuntimeException("Test")).when(referenceNumbersDaoSpy).findAll(any(),any());
//        assertThrows(RunnerException.class, () -> referenceNumbersDaoSpy.updateEntityFromShipment(Collections.emptyList(), shipmentId));
//    }
//
//    @Test
//    void updateEntityFromShipment_ReferenceNumbersListHasElements_ReturnsResponseReferenceNumbers() throws RunnerException {
//        Long shipmentId = 1L;
//        List<ReferenceNumbers> referenceNumbersList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
//        var referenceNumbersDaoSpy = Mockito.spy(referenceNumbersDao);
//        doReturn(mock(Page.class)).when(referenceNumbersDaoSpy).findAll(any(), any());
//        doReturn(referenceNumbersList).when(referenceNumbersDaoSpy).saveEntityFromShipment(anyList(), eq(shipmentId), anyMap());
//        List<ReferenceNumbers> result = referenceNumbersDaoSpy.updateEntityFromShipment(referenceNumbersList, shipmentId);
//        assertEquals(referenceNumbersList, result);
//    }
//
//    @Test
//    void updateEntityFromShipment_ReferenceNumbersListIsEmpty_ReturnsEmptyList() throws RunnerException {
//        Long shipmentId = 1L;
//        testData.setId(1L);
//        when(referenceNumbersRepository.findAll((Specification<ReferenceNumbers>) any(), (Pageable) any())).thenReturn(new PageImpl<ReferenceNumbers>(List.of(testData)));
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.singletonList(testData), shipmentId);
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void saveEntityFromShipment_ValidDataWithExistingIds_ReturnsList() {
//        List<ReferenceNumbers> referenceNumberss = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
//        Long shipmentId = 1L;
//        Map<Long, ReferenceNumbers> oldEntityMap = new HashMap<>();
//        oldEntityMap.put(1L, new ReferenceNumbers());
//        oldEntityMap.put(2L, new ReferenceNumbers());
//        when(referenceNumbersDao.saveAll(anyList())).thenReturn(referenceNumberss);
//
//        List<ReferenceNumbers> result = referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId, oldEntityMap);
//
//        assertNotNull(result);
//        assertEquals(referenceNumberss.size(), result.size());
//    }
//
//    @Test
//    void deleteReferenceNumbers_ValidData_CallsDeleteAndAuditLogService() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        // Arrange
//        Map<Long, ReferenceNumbers> hashMap = new HashMap<>();
//        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
//        ReferenceNumbers referenceNumbers2 = new ReferenceNumbers();
//        hashMap.put(1L, referenceNumbers1);
//        hashMap.put(2L, referenceNumbers2);
//        when(jsonHelper.convertToJson(any(ReferenceNumbers.class))).thenReturn("{}");
//
//        referenceNumbersDao.deleteReferenceNumbers(hashMap, "entityType", 1L);
//
//        verify(referenceNumbersRepository, times(2)).delete(any(ReferenceNumbers.class));
//        verify(auditLogService, times(2)).addAuditLog(any());
//    }
//
//    @Test
//    void updateEntityFromShipment_OldEntityListIsNull_ReturnsEmptyList() throws RunnerException {
//        Long shipmentId = 1L;
//
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, null);
//
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void updateEntityFromShipment_OldEntityListIsEmpty_ReturnsEmptyList() throws RunnerException {
//        Long shipmentId = 1L;
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, Collections.emptyList());
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void updateEntityFromShipment_ReferenceNumbersListIsNull_ReturnsEmptyList() throws RunnerException {
//        Long shipmentId = 1L;
//        List<ReferenceNumbers> oldEntityList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(null, shipmentId, oldEntityList);
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList_() {
//        Long shipmentId = 1L;
//        List<ReferenceNumbers> oldEntityList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
//        var referenceNumbersDaospy = Mockito.spy(referenceNumbersDao);
//        doThrow(new RuntimeException()).when(referenceNumbersDaospy).saveEntityFromShipment(any() , any());
//        assertThrows(RunnerException.class, () -> referenceNumbersDaospy.updateEntityFromShipment(List.of(testData), shipmentId, oldEntityList));
//    }
//
//    @Test
//    void saveEntityFromShipment_ExceptionThrown_ReturnsEmptyList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        Long shipmentId = 1L;
//        testData.setId(1L);
//        List<ReferenceNumbers> referenceNumberss = Arrays.asList(testData);
//        when(referenceNumbersDao.findById(anyLong())).thenReturn(Optional.of(testData));
//        doThrow(IllegalArgumentException.class).when(auditLogService).addAuditLog(any());
//        assertThrows(Exception.class, () -> referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId));
//    }
//
//    @Test
//    void saveEntityFromShipment_ReferenceNumberssListHasElements_ReturnsPopulatedList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        Long shipmentId = 1L;
//        List<ReferenceNumbers> referenceNumberss = Arrays.asList(testData);
//        when(referenceNumbersDao.findById(anyLong())).thenReturn(Optional.of(new ReferenceNumbers()));
//        when(referenceNumbersDao.save(any())).thenReturn(new ReferenceNumbers());
//        List<ReferenceNumbers> result = referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId);
//        assertNotNull(result);
//        assertFalse(result.isEmpty());
//        assertEquals(referenceNumberss.size(), result.size());
//        verify(auditLogService,times(1)).addAuditLog(any());
//    }
//
//    @Test
//    void delete_ValidReferenceNumbers_ThrowsException() {
//        doThrow(new RuntimeException("test")).when(jsonHelper).convertToJson(any(ReferenceNumbers.class));
//        referenceNumbersDao.deleteReferenceNumbers(Map.of(1L , testData),"referenceNumbers", 1L);
//    }
//
//    @Test
//    void saveEntityFromConsole_ValidInput_ReturnsSavedEntities() {
//        // Arrange
//        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
//        referenceNumbers1.setId(1L);
//        referenceNumbers1.setCreatedBy("user1");
//
//        ReferenceNumbers referenceNumbers2 = testData;
//        referenceNumbers2.setId(12L);
//
//        when(referenceNumbersRepository.saveAll(anyList())).thenReturn(List.of(referenceNumbers1, referenceNumbers2));
//        testData.setId(12L);
//        // Act
//        List<ReferenceNumbers> savedEntities = referenceNumbersDao.saveEntityFromConsole(
//                List.of(referenceNumbers1, testData),
//                123L,
//                Map.of(1L , referenceNumbers1, testData.getId(), testData)
//        );
//
//        // Assert
//        assertEquals(2, savedEntities.size());
//        assertEquals("user1", savedEntities.get(0).getCreatedBy());
//        assertNull(savedEntities.get(1).getCreatedAt());
//        assertNull(savedEntities.get(1).getCreatedBy());
//        verify(referenceNumbersRepository, times(1)).saveAll(anyList());
//    }
//
//    @Test
//    void saveEntityFromConsole_InvalidInput_ThrowsException() {
//        // Arrange
//        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
//        referenceNumbers1.setId(1L);
//        referenceNumbers1.setCreatedBy("user1");
//
//        ReferenceNumbers referenceNumbers2 = new ReferenceNumbers();
//        referenceNumbers2.setId(2L);
//
//        assertThrows(Exception.class, () -> referenceNumbersDao.saveEntityFromConsole(
//                List.of(referenceNumbers1, testData),
//                123L,
//                Collections.emptyMap()
//        ));
//    }
//
//    @Test
//    void updateEntityFromConsole_ValidInput_ReturnsResponseReferenceNumbers() throws RunnerException {
//        // Arrange
//        long consolidationId = 123L;
//
//        List<ReferenceNumbers> referenceNumbers = List.of(testData);
//        List<Long> shipmentIds = List.of(1L, 2L);
//        when(referenceNumbersRepository.findAll((Specification<ReferenceNumbers>) any(), (Pageable) any())).thenReturn(new PageImpl<>(referenceNumbers));
//
//        // Act
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromConsole(referenceNumbers, consolidationId);
//
//        // Assert
//        assertNotNull(result);
////        verify(consoleShipmentMappingDao, times(1)).findByConsolidationId(consolidationId);
//        verify(referenceNumbersRepository, times(1)).findAll((Specification<ReferenceNumbers>) any(), (Pageable) any());
//    }
//
//    @Test
//    void updateEntityFromConsole_EmptyShipmentIds_ReturnsEmptyList() throws RunnerException {
//        // Arrange
//        long consolidationId = 123L;
////        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(new ArrayList<>());
//        when(referenceNumbersRepository.findAll((Specification<ReferenceNumbers>) any(), (Pageable) any())).thenReturn(new PageImpl<>(List.of(testData)));
//
//        // Act
//        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromConsole(new ArrayList<>(), consolidationId);
//
//        // Assert
//        assertNotNull(result);
//        assertTrue(result.isEmpty());
//    }
//
//
//    @Test
//    void saveEntityFromConsole_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        // Arrange
//        long consolidationId = 123L;
//        List<ReferenceNumbers> referenceNumbersRequests = List.of(
//                testData
//        );
//        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.of(testData));
//
//        referenceNumbersDao.saveEntityFromConsole(referenceNumbersRequests, consolidationId);//        assertThrows(DataRetrievalFailureException.class, () -> referenceNumbersDao.saveEntityFromConsole(referenceNumbersRequests, consolidationId));
//    }
//
//    @Test
//    void updateEntityFromConsole_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        // Arrange
//        long consolidationId = 123L;
//        List<ReferenceNumbers> referenceNumbersRequests = List.of(
//                testData
//        );
//        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.of(testData));
//
//        referenceNumbersDao.updateEntityFromConsole(referenceNumbersRequests, consolidationId, Collections.emptyList());//        assertThrows(DataRetrievalFailureException.class, () -> referenceNumbersDao.saveEntityFromConsole(referenceNumbersRequests, consolidationId));
//    }
//
//    @Test
//    void updateEntityFromConsole_ReturnsEmptyList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
//        // Arrange
//        long consolidationId = 123L;
//        List<ReferenceNumbers> referenceNumbersRequests = List.of(
//                testData
//        );
//        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.empty());
//
//        assertThrows(RunnerException.class,() -> referenceNumbersDao.updateEntityFromConsole(referenceNumbersRequests, consolidationId, Collections.emptyList()));//        assertThrows(DataRetrievalFailureException.class, () -> referenceNumbersDao.saveEntityFromConsole(referenceNumbersRequests, consolidationId));
//    }
//
//
//}