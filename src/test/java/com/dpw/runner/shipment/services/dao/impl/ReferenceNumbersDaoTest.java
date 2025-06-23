package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
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
class ReferenceNumbersDaoTest {

    @Mock
    private IReferenceNumbersRepository referenceNumbersRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @InjectMocks
    private ReferenceNumbersDao referenceNumbersDao;

    private ReferenceNumbers testData;
    private JsonTestUtility jsonTestUtility;


    @BeforeEach
    void setUp() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        testData = jsonTestUtility.getTestReferenceNumbers();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidReferenceNumbers_ReturnsSavedReferenceNumbers() {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        when(referenceNumbersRepository.save(any(ReferenceNumbers.class))).thenReturn(referenceNumbers);

        ReferenceNumbers savedReferenceNumbers = referenceNumbersDao.save(referenceNumbers);

        assertEquals(referenceNumbers, savedReferenceNumbers);
        verify(referenceNumbersRepository).save(any(ReferenceNumbers.class));
    }

    @Test
    void saveAll_ValidReferenceNumberss_ReturnsSavedReferenceNumberss() {
        List<ReferenceNumbers> referenceNumbersList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
        when(referenceNumbersRepository.saveAll(anyList())).thenReturn(referenceNumbersList);
        List<ReferenceNumbers> savedReferenceNumberss = referenceNumbersDao.saveAll(referenceNumbersList);
        assertEquals(referenceNumbersList, savedReferenceNumberss);
        verify(referenceNumbersRepository).saveAll(referenceNumbersList);
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfReferenceNumberss() {
        Page<ReferenceNumbers> expectedPage = mock(Page.class);
        Specification<ReferenceNumbers> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(referenceNumbersRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<ReferenceNumbers> resultPage = referenceNumbersDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(referenceNumbersRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfReferenceNumbers() {
        Long id = 1L;
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        when(referenceNumbersRepository.findById(id)).thenReturn(Optional.of(referenceNumbers));

        Optional<ReferenceNumbers> result = referenceNumbersDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(referenceNumbers, result.get());
        verify(referenceNumbersRepository).findById(id);
    }

    @Test
    void delete_ValidReferenceNumbers_CallsRepositoryDelete() {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbersDao.delete(referenceNumbers);
        verify(referenceNumbersRepository).delete(referenceNumbers);
    }


    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList() {
        Long shipmentId = 1L;
        var referenceNumbersDaoSpy = Mockito.spy(referenceNumbersDao);
        doThrow(new RuntimeException("Test")).when(referenceNumbersDaoSpy).findByShipmentId(any());
        assertThrows(RunnerException.class, () -> referenceNumbersDaoSpy.updateEntityFromShipment(Collections.emptyList(), shipmentId));
    }

    @Test
    void updateEntityFromShipment_ReferenceNumbersListHasElements_ReturnsResponseReferenceNumbers() throws RunnerException {
        Long shipmentId = 1L;
        List<ReferenceNumbers> referenceNumbersList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
        var referenceNumbersDaoSpy = Mockito.spy(referenceNumbersDao);
        doReturn(referenceNumbersList).when(referenceNumbersDaoSpy).saveEntityFromShipment(anyList(), eq(shipmentId), anyMap());
        List<ReferenceNumbers> result = referenceNumbersDaoSpy.updateEntityFromShipment(referenceNumbersList, shipmentId);
        assertEquals(referenceNumbersList, result);
    }

    @Test
    void updateEntityFromShipment_ReferenceNumbersListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        testData.setId(1L);
        when(referenceNumbersRepository.findByShipmentId(any())).thenReturn(List.of(testData));
        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.singletonList(testData), shipmentId);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void saveEntityFromShipment_ValidDataWithExistingIds_ReturnsList() {
        List<ReferenceNumbers> referenceNumberss = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
        Long shipmentId = 1L;
        Map<Long, ReferenceNumbers> oldEntityMap = new HashMap<>();
        oldEntityMap.put(1L, new ReferenceNumbers());
        oldEntityMap.put(2L, new ReferenceNumbers());
        when(referenceNumbersDao.saveAll(anyList())).thenReturn(referenceNumberss);

        List<ReferenceNumbers> result = referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId, oldEntityMap);

        assertNotNull(result);
        assertEquals(referenceNumberss.size(), result.size());
    }

    @Test
    void deleteReferenceNumbers_ValidData_CallsDeleteAndAuditLogService() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        Map<Long, ReferenceNumbers> hashMap = new HashMap<>();
        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
        ReferenceNumbers referenceNumbers2 = new ReferenceNumbers();
        hashMap.put(1L, referenceNumbers1);
        hashMap.put(2L, referenceNumbers2);
        when(jsonHelper.convertToJson(any(ReferenceNumbers.class))).thenReturn("{}");

        referenceNumbersDao.deleteReferenceNumbers(hashMap, "entityType", 1L);

        verify(referenceNumbersRepository, times(2)).delete(any(ReferenceNumbers.class));
        verify(auditLogService, times(2)).addAuditLog(any());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;

        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, null);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, Collections.emptyList());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_ReferenceNumbersListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<ReferenceNumbers> oldEntityList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromShipment(null, shipmentId, oldEntityList);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList_() {
        Long shipmentId = 1L;
        List<ReferenceNumbers> oldEntityList = Arrays.asList(new ReferenceNumbers(), new ReferenceNumbers());
        var referenceNumbersDaospy = Mockito.spy(referenceNumbersDao);
        doThrow(new RuntimeException()).when(referenceNumbersDaospy).saveEntityFromShipment(any() , any());
        assertThrows(RunnerException.class, () -> referenceNumbersDaospy.updateEntityFromShipment(List.of(testData), shipmentId, oldEntityList));
    }

    @Test
    void saveEntityFromShipment_ExceptionThrown_ReturnsEmptyList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long shipmentId = 1L;
        testData.setId(1L);
        List<ReferenceNumbers> referenceNumberss = List.of(testData);
        when(referenceNumbersDao.findById(anyLong())).thenReturn(Optional.of(testData));
        doThrow(IllegalArgumentException.class).when(auditLogService).addAuditLog(any());
        assertThrows(Exception.class, () -> referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId));
    }

    @Test
    void saveEntityFromShipment_ReferenceNumberssListHasElements_ReturnsPopulatedList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long shipmentId = 1L;
        List<ReferenceNumbers> referenceNumberss = Collections.singletonList(testData);
        when(referenceNumbersDao.findById(anyLong())).thenReturn(Optional.of(new ReferenceNumbers()));
        when(referenceNumbersDao.save(any())).thenReturn(new ReferenceNumbers());
        List<ReferenceNumbers> result = referenceNumbersDao.saveEntityFromShipment(referenceNumberss, shipmentId);
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertEquals(referenceNumberss.size(), result.size());
        verify(auditLogService,times(1)).addAuditLog(any());
    }

    @Test
    void delete_ValidReferenceNumbers_ThrowsException() {
        doThrow(new RuntimeException("test")).when(jsonHelper).convertToJson(any(ReferenceNumbers.class));
        assertDoesNotThrow(() -> referenceNumbersDao.deleteReferenceNumbers(Map.of(1L , testData),"referenceNumbers", 1L));
    }

    @Test
    void saveEntityFromConsole_ValidInput_ReturnsSavedEntities() {
        // Arrange
        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
        referenceNumbers1.setId(1L);
        referenceNumbers1.setCreatedBy("user1");

        ReferenceNumbers referenceNumbers2 = testData;
        referenceNumbers2.setId(12L);

        when(referenceNumbersRepository.saveAll(anyList())).thenReturn(List.of(referenceNumbers1, referenceNumbers2));
        testData.setId(12L);
        // Act
        List<ReferenceNumbers> savedEntities = referenceNumbersDao.saveEntityFromConsole(
                List.of(referenceNumbers1, testData),
                123L,
                Map.of(1L , referenceNumbers1, testData.getId(), testData)
        );

        // Assert
        assertEquals(2, savedEntities.size());
        assertEquals("user1", savedEntities.get(0).getCreatedBy());
        assertNull(savedEntities.get(1).getCreatedAt());
        assertNull(savedEntities.get(1).getCreatedBy());
        verify(referenceNumbersRepository, times(1)).saveAll(anyList());
    }

    @Test
    void saveEntityFromConsole_InvalidInput_ThrowsException() {
        // Arrange
        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
        referenceNumbers1.setId(1L);
        referenceNumbers1.setCreatedBy("user1");

        ReferenceNumbers referenceNumbers2 = new ReferenceNumbers();
        referenceNumbers2.setId(2L);

        assertThrows(Exception.class, () -> referenceNumbersDao.saveEntityFromConsole(
                List.of(referenceNumbers1, testData),
                123L,
                Collections.emptyMap()
        ));
    }

    @Test
    void updateEntityFromConsole_ValidInput_ReturnsResponseReferenceNumbers() throws RunnerException {
        // Arrange
        long consolidationId = 123L;

        List<ReferenceNumbers> referenceNumbers = List.of(testData);
        when(referenceNumbersRepository.findAll(ArgumentMatchers.<Specification<ReferenceNumbers>>any(), any(Pageable.class))).thenReturn(new PageImpl<>(referenceNumbers));

        // Act
        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromConsole(referenceNumbers, consolidationId);

        // Assert
        assertNotNull(result);
        verify(referenceNumbersRepository, times(1)).findAll(ArgumentMatchers.<Specification<ReferenceNumbers>>any(), (Pageable) any());
    }

    @Test
    void updateEntityFromConsole_EmptyShipmentIds_ReturnsEmptyList() throws RunnerException {
        // Arrange
        long consolidationId = 123L;
        when(referenceNumbersRepository.findAll(ArgumentMatchers.<Specification<ReferenceNumbers>>any(), (Pageable) any())).thenReturn(new PageImpl<>(List.of(testData)));

        // Act
        List<ReferenceNumbers> result = referenceNumbersDao.updateEntityFromConsole(new ArrayList<>(), consolidationId);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }


    @Test
    void saveEntityFromConsole_Success(){
        // Arrange
        long consolidationId = 123L;
        List<ReferenceNumbers> referenceNumbersRequests = List.of(
                testData
        );
        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.of(testData));

        assertDoesNotThrow(() -> referenceNumbersDao.saveEntityFromConsole(referenceNumbersRequests, consolidationId));
    }

    @Test
    void updateEntityFromConsole_Success(){
        // Arrange
        long consolidationId = 123L;
        List<ReferenceNumbers> referenceNumbersRequests = List.of(
                testData
        );
        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.of(testData));

        assertDoesNotThrow(() -> referenceNumbersDao.updateEntityFromConsole(referenceNumbersRequests, consolidationId, Collections.emptyList()));
    }

    @Test
    void updateEntityFromConsole_ReturnsEmptyList() {
        // Arrange
        long consolidationId = 123L;
        List<ReferenceNumbers> referenceNumbersRequests = List.of(
                testData
        );
        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(RunnerException.class, () ->
            referenceNumbersDao.updateEntityFromConsole(referenceNumbersRequests, consolidationId, Collections.emptyList()));
    }

    @Test
    void saveEntityFromShipmentEntityNotPresent() {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setId(1L);
        List<ReferenceNumbers> referenceNumbersList = List.of(referenceNumbers);

        when(referenceNumbersRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () ->
            referenceNumbersDao.saveEntityFromShipment(referenceNumbersList, 1L));
    }

    @Test
    void saveEntityFromShipmentMapNotContainsId() {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setId(1L);
        List<ReferenceNumbers> referenceNumbersList = List.of(referenceNumbers);
        Map<Long, ReferenceNumbers> hashMap = new HashMap<>();
        assertThrows(DataRetrievalFailureException.class, () ->
            referenceNumbersDao.saveEntityFromShipment(referenceNumbersList, 1L, hashMap));
    }

    @Test
    void saveEntityFromShipment() {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setId(1L);

        HashMap<Long, ReferenceNumbers> map = new HashMap<>();
        map.put(1L, referenceNumbers);

        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(referenceNumbersRepository.saveAll(any())).thenReturn(List.of(referenceNumbers));

        List<ReferenceNumbers> referenceNumbersList = List.of(referenceNumbers);
        assertEquals(referenceNumbersList, referenceNumbersDao.saveEntityFromShipment(referenceNumbersList, 1L, map));
    }

    @Test
    void updateEntityFromConsole() throws RunnerException {
        UUID uuid1 = UUID.randomUUID();
        UUID uuid2 = UUID.randomUUID();

        ReferenceNumbers referenceNumbers1 = new ReferenceNumbers();
        referenceNumbers1.setGuid(uuid1);

        ReferenceNumbers referenceNumbers2 = new ReferenceNumbers();
        referenceNumbers2.setGuid(uuid2);

        List<ReferenceNumbers> referenceNumbersList = Arrays.asList(referenceNumbers1, referenceNumbers2);
        List<ReferenceNumbers> oldList = List.of(referenceNumbers1);

        when(referenceNumbersRepository.save(any())).thenReturn(referenceNumbers1);

        assertEquals(referenceNumbersList, referenceNumbersDao.updateEntityFromConsole(referenceNumbersList, 1L, oldList));
    }

    @Test
    void testUpdateEntityFromBooking() throws RunnerException {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        referenceNumbersList.get(0).setId(1L);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        doReturn(referenceNumbersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullPackings() throws RunnerException {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(new ArrayList<>(), referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_EmptyPackings() throws RunnerException {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(new ArrayList<>(), 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(new ArrayList<>(), referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId() throws RunnerException {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        doReturn(referenceNumbersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_Failure() {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doThrow(new RuntimeException()).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromBooking(referenceNumbersList, 1L));
    }

    @Test
    void testSaveEntityFromBooking() throws Exception {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        referenceNumbersList.get(0).setId(1L);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testData).when(spyService).save(any());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.saveEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testSaveEntityFromBooking_hashMap() throws Exception {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        referenceNumbersList.get(0).setId(1L);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromBooking(referenceNumbersList, 1L));
    }

    @Test
    void testSaveEntityFromBooking_NullId() throws Exception {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testData).when(spyService).save(any());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.saveEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testSaveEntityFromBooking_AuditLogFailure() throws Exception {
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(testData);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(referenceNumbersList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testData).when(spyService).save(any());
        List<ReferenceNumbers> referenceNumbersList1 = spyService.saveEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_JsonParseFailure() throws Exception {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setReferenceNumber("qwerty");
        referenceNumbers.setId(1234L);
        referenceNumbers.setBookingId(2321L);
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(referenceNumbers);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(List.of(testData))).when(spyService).findAll(any(), any());
        doReturn(referenceNumbersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        when(jsonHelper.convertToJson(any())).thenThrow(RuntimeException.class);
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }

    @Test
    void testUpdateEntityFromBooking_AuditLogFailure() throws Exception {
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setReferenceNumber("qwerty");
        referenceNumbers.setId(1234L);
        referenceNumbers.setBookingId(2321L);
        List<ReferenceNumbers> referenceNumbersList = Collections.singletonList(referenceNumbers);
        ReferenceNumbersDao spyService = spy(referenceNumbersDao);
        doReturn(new PageImpl<>(List.of(testData))).when(spyService).findAll(any(), any());
        doReturn(referenceNumbersList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        List<ReferenceNumbers> referenceNumbersList1 = spyService.updateEntityFromBooking(referenceNumbersList, 1L);
        assertNotNull(referenceNumbersList1);
        assertEquals(referenceNumbersList, referenceNumbersList1);
    }
}