package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
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
class BookingCarriageDaoTest {

    @Mock
    private IBookingCarriageRepository bookingCarriageRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private BookingCarriageDao bookingCarriageDao;

    private BookingCarriage testData;
    private JsonTestUtility jsonTestUtility;


    @BeforeEach
    void setUp() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        testData = jsonTestUtility.getTestBookingCarriage();
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidBookingCarriage_ReturnsSavedBookingCarriage() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(bookingCarriageRepository.save(any(BookingCarriage.class))).thenReturn(bookingCarriage);

        BookingCarriage savedBookingCarriage = bookingCarriageDao.save(bookingCarriage);

        assertEquals(bookingCarriage, savedBookingCarriage);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(bookingCarriageRepository).save(any(BookingCarriage.class));
    }

    @Test
    void save_ValidBookingCarriage_ThrowsValidationException() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(set);
        assertThrows(ValidationException.class, () -> bookingCarriageDao.save(bookingCarriage));
    }


    @Test
    void saveAll_ValidBookingCarriages_ReturnsSavedBookingCarriages() {
        List<BookingCarriage> bookingCarriageList = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(bookingCarriageRepository.saveAll(anyList())).thenReturn(bookingCarriageList);

        List<BookingCarriage> savedBookingCarriages = bookingCarriageDao.saveAll(bookingCarriageList);

        assertEquals(bookingCarriageList, savedBookingCarriages);
        verify(validatorUtility, times(2)).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(bookingCarriageRepository).saveAll(bookingCarriageList);
    }

    @Test
    void saveAll_ValidBookingCarriages_ThrowsValidationException() {
        List<BookingCarriage> bookingCarriageList = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Set.of("test"));
        assertThrows(ValidationException.class, () -> bookingCarriageDao.saveAll(bookingCarriageList));
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfBookingCarriages() {
        Page<BookingCarriage> expectedPage = mock(Page.class);
        Specification<BookingCarriage> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(bookingCarriageRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<BookingCarriage> resultPage = bookingCarriageDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(bookingCarriageRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfBookingCarriage() {
        Long id = 1L;
        BookingCarriage bookingCarriage = new BookingCarriage();
        when(bookingCarriageRepository.findById(id)).thenReturn(Optional.of(bookingCarriage));

        Optional<BookingCarriage> result = bookingCarriageDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(bookingCarriage, result.get());
        verify(bookingCarriageRepository).findById(id);
    }

    @Test
    void delete_ValidBookingCarriage_CallsRepositoryDelete() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriageDao.delete(bookingCarriage);
        verify(bookingCarriageRepository).delete(bookingCarriage);
    }


    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList() {
        Long shipmentId = 1L;
        var bookingCarriageDaoSpy = Mockito.spy(bookingCarriageDao);
        doThrow(new RuntimeException("Test")).when(bookingCarriageDaoSpy).findByShipmentId(any());
        assertThrows(RunnerException.class, () -> bookingCarriageDaoSpy.updateEntityFromShipment(Collections.emptyList(), shipmentId));
    }

    @Test
    void updateEntityFromShipment_BookingCarriageListHasElements_ReturnsResponseBookingCarriage() throws RunnerException {
        Long shipmentId = 1L;
        List<BookingCarriage> bookingCarriageList = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        var bookingCarriageDaoSpy = Mockito.spy(bookingCarriageDao);
        doReturn(bookingCarriageList).when(bookingCarriageDaoSpy).saveEntityFromShipment(anyList(), eq(shipmentId), anyMap());
        List<BookingCarriage> result = bookingCarriageDaoSpy.updateEntityFromShipment(bookingCarriageList, shipmentId);
        assertEquals(bookingCarriageList, result);
    }

    @Test
    void updateEntityFromShipment_BookingCarriageListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        testData.setId(1L);
        when(bookingCarriageRepository.findByShipmentId(any())).thenReturn(List.of(testData));
        List<BookingCarriage> result = bookingCarriageDao.updateEntityFromShipment(Collections.singletonList(testData), shipmentId);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void saveEntityFromShipment_ValidDataWithExistingIds_ReturnsList() {
        List<BookingCarriage> bookingCarriages = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        Long shipmentId = 1L;
        Map<Long, BookingCarriage> oldEntityMap = new HashMap<>();
        oldEntityMap.put(1L, new BookingCarriage());
        oldEntityMap.put(2L, new BookingCarriage());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(bookingCarriageDao.saveAll(anyList())).thenReturn(bookingCarriages);

        List<BookingCarriage> result = bookingCarriageDao.saveEntityFromShipment(bookingCarriages, shipmentId, oldEntityMap);

        assertNotNull(result);
        assertEquals(bookingCarriages.size(), result.size());
    }

    @Test
    void deleteBookingCarriage_ValidData_CallsDeleteAndAuditLogService() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        Map<Long, BookingCarriage> hashMap = new HashMap<>();
        BookingCarriage bookingCarriage1 = new BookingCarriage();
        BookingCarriage bookingCarriage2 = new BookingCarriage();
        hashMap.put(1L, bookingCarriage1);
        hashMap.put(2L, bookingCarriage2);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertToJson(any(BookingCarriage.class))).thenReturn("{}");

        bookingCarriageDao.deleteBookingCarriage(hashMap, "entityType", 1L);

        verify(bookingCarriageRepository, times(2)).delete(any(BookingCarriage.class));
        verify(auditLogService, times(2)).addAuditLog(any());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;

        List<BookingCarriage> result = bookingCarriageDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, null);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_OldEntityListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<BookingCarriage> result = bookingCarriageDao.updateEntityFromShipment(Collections.emptyList(), shipmentId, Collections.emptyList());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_BookingCarriageListIsNull_ReturnsEmptyList() throws RunnerException {
        Long shipmentId = 1L;
        List<BookingCarriage> oldEntityList = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        List<BookingCarriage> result = bookingCarriageDao.updateEntityFromShipment(null, shipmentId, oldEntityList);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment_ExceptionThrown_ReturnsEmptyList_() {
        Long shipmentId = 1L;
        List<BookingCarriage> oldEntityList = Arrays.asList(new BookingCarriage(), new BookingCarriage());
        var bookingCarriageDaospy = Mockito.spy(bookingCarriageDao);
        doThrow(new RuntimeException()).when(bookingCarriageDaospy).saveEntityFromShipment(any() , any());
        assertThrows(RunnerException.class, () -> bookingCarriageDaospy.updateEntityFromShipment(List.of(testData), shipmentId, oldEntityList));
    }

    @Test
    void saveEntityFromShipment_ExceptionThrown_ReturnsEmptyList() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long shipmentId = 1L;
        testData.setId(1L);
        List<BookingCarriage> bookingCarriages = Arrays.asList(testData);
        when(bookingCarriageDao.findById(anyLong())).thenReturn(Optional.of(testData));
        doThrow(IllegalArgumentException.class).when(jsonHelper).convertToJson(any());
        assertThrows(Exception.class, () -> bookingCarriageDao.saveEntityFromShipment(bookingCarriages, shipmentId));
    }

//    @Test
//    void saveEntityFromShipment_BookingCarriagesListHasElements_ReturnsPopulatedList() {
//        Long shipmentId = 1L;
//        List<BookingCarriage> bookingCarriages = Arrays.asList(new BookingCarriage(), new BookingCarriage());
//        when(bookingCarriageDao.findById(anyLong())).thenReturn(Optional.of(new BookingCarriage()));
//        when(bookingCarriageDao.save(any())).thenReturn(new BookingCarriage());
//        List<BookingCarriage> result = bookingCarriageDao.saveEntityFromShipment(bookingCarriages, shipmentId);
//        assertNotNull(result);
//        assertFalse(result.isEmpty());
//        assertEquals(bookingCarriages.size(), result.size());
//    }

    @Test
    void delete_ValidBookingCarriage_ThrowsException() {
        doThrow(new RuntimeException("test")).when(jsonHelper).convertToJson(any(BookingCarriage.class));
        bookingCarriageDao.deleteBookingCarriage(Map.of(1L , testData),"bookingCarriage", 1L);
    }

    @Test
    void saveEntityFromShipmentEntityNotPresent() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriage.setId(1L);
        List<BookingCarriage> bookingCarriageList = Arrays.asList(bookingCarriage);

        when(bookingCarriageRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,() -> bookingCarriageDao.saveEntityFromShipment(bookingCarriageList, 1L));
    }

    @Test
    void saveEntityFromShipmentMapNotContainsId() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriage.setId(1L);
        List<BookingCarriage> bookingCarriageList = Arrays.asList(bookingCarriage);
        assertThrows(DataRetrievalFailureException.class,() -> bookingCarriageDao.saveEntityFromShipment(bookingCarriageList, 1L, new HashMap<>()));
    }

    @Test
    void saveEntityFromShipment() {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriage.setId(1L);

        HashMap<Long, BookingCarriage> map = new HashMap<>();
        map.put(1L, bookingCarriage);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(bookingCarriageRepository.saveAll(any())).thenReturn(Arrays.asList(bookingCarriage));

        List<BookingCarriage> bookingCarriageList = Arrays.asList(bookingCarriage);
        assertEquals(bookingCarriageList, bookingCarriageDao.saveEntityFromShipment(bookingCarriageList, 1L, map));
    }
}