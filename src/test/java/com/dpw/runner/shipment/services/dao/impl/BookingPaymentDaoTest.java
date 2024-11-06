package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.BookingPayment;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingPaymentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.StringUtility;
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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class BookingPaymentDaoTest {
    @Mock
    private IBookingPaymentRepository bookingPaymentRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private BookingPaymentDao bookingPaymentDao;

    private BookingPayment testData;

    @BeforeEach
    void setUp() throws IOException {
        JsonTestUtility jsonTestUtility = new JsonTestUtility();
        testData = jsonTestUtility.getTestBookingPayment();
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidBookingPayment_ReturnsNewBookingPayment() {
        BookingPayment bookingPayment = new BookingPayment();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(bookingPaymentRepository.save(any(BookingPayment.class))).thenReturn(bookingPayment);

        BookingPayment savedBookingPayment = bookingPaymentDao.save(bookingPayment);

        assertEquals(bookingPayment, savedBookingPayment);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(bookingPaymentRepository).save(any(BookingPayment.class));
    }

    @Test
    void save_ValidBookingPayment_ReturnsSavedBookingPayment() {
        BookingPayment bookingPayment = new BookingPayment();
        bookingPayment.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(bookingPaymentRepository.save(any(BookingPayment.class))).thenReturn(bookingPayment);
        when(bookingPaymentRepository.findById(1L)).thenReturn(Optional.of(bookingPayment));

        BookingPayment savedBookingPayment = bookingPaymentDao.save(bookingPayment);

        assertEquals(bookingPayment, savedBookingPayment);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(bookingPaymentRepository).save(any(BookingPayment.class));
    }

    @Test
    void save_ValidBookingPayment_ThrowsValidationException() {
        BookingPayment bookingPayment = new BookingPayment();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(set);
        assertThrows(ValidationException.class, () -> bookingPaymentDao.save(bookingPayment));
    }


    @Test
    void saveAll_ValidBookingPayments_ReturnsSavedBookingPayments() {
        List<BookingPayment> bookingPaymentList = Arrays.asList(new BookingPayment(), new BookingPayment());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(bookingPaymentRepository.saveAll(anyList())).thenReturn(bookingPaymentList);

        List<BookingPayment> savedBookingPayments = bookingPaymentDao.saveAll(bookingPaymentList);

        assertEquals(bookingPaymentList, savedBookingPayments);
        verify(validatorUtility, times(2)).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(bookingPaymentRepository).saveAll(bookingPaymentList);
    }

    @Test
    void saveAll_ValidBookingPayments_ThrowsValidationException() {
        List<BookingPayment> bookingPaymentList = Arrays.asList(new BookingPayment(), new BookingPayment());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Set.of("test"));
        assertThrows(ValidationException.class, () -> bookingPaymentDao.saveAll(bookingPaymentList));
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfBookingPayments() {
        Page<BookingPayment> expectedPage = mock(Page.class);
        Specification<BookingPayment> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(bookingPaymentRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<BookingPayment> resultPage = bookingPaymentDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(bookingPaymentRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfBookingPayment() {
        Long id = 1L;
        BookingPayment bookingPayment = new BookingPayment();
        when(bookingPaymentRepository.findById(id)).thenReturn(Optional.of(bookingPayment));

        Optional<BookingPayment> result = bookingPaymentDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(bookingPayment, result.get());
        verify(bookingPaymentRepository).findById(id);
    }

    @Test
    void delete_ValidBookingPayment_CallsRepositoryDelete() {
        BookingPayment bookingPayment = new BookingPayment();
        bookingPaymentDao.delete(bookingPayment);
        verify(bookingPaymentRepository).delete(bookingPayment);
    }


    @Test
    void deleteBookingPayment_ValidData_CallsDeleteAndAuditLogService() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        Map<Long, BookingPayment> hashMap = new HashMap<>();
        BookingPayment bookingPayment1 = new BookingPayment();
        BookingPayment bookingPayment2 = new BookingPayment();
        hashMap.put(1L, bookingPayment1);
        hashMap.put(2L, bookingPayment2);
        when(jsonHelper.convertToJson(any(BookingPayment.class))).thenReturn("{}");

        bookingPaymentDao.deleteBookingPayments(hashMap, "entityType", 1L);

        verify(bookingPaymentRepository, times(2)).delete(any(BookingPayment.class));
        verify(auditLogService, times(2)).addAuditLog(any());
    }

    @Test
    void delete_ValidBookingPayment_ThrowsException() {
        doThrow(new RuntimeException("test")).when(jsonHelper).convertToJson(any(BookingPayment.class));
        bookingPaymentDao.deleteBookingPayments(Map.of(1L , testData),"bookingPayment", 1L);
    }

    @Test
    void updateEntityFromCarrierBooking_ExceptionThrown_ReturnsEmptyList() {
        Long carrierBookingId = 1L;
        var bookingPaymentDaoSpy = Mockito.spy(bookingPaymentDao);
        doThrow(new RuntimeException("Test")).when(bookingPaymentDaoSpy).findAll(any(),any());
        assertThrows(RunnerException.class, () -> bookingPaymentDaoSpy.updateEntityFromCarrierBooking(Collections.emptyList(), carrierBookingId));
    }

    @Test
    void updateEntityFromCarrierBooking_BookingPaymentListHasElements_ReturnsResponseBookingPayment() throws RunnerException {
        Long carrierBookingId = 1L;
        List<BookingPayment> bookingPaymentList = Arrays.asList(new BookingPayment(), new BookingPayment());
        var bookingPaymentDaoSpy = Mockito.spy(bookingPaymentDao);
        doReturn(mock(Page.class)).when(bookingPaymentDaoSpy).findAll(any(), any());
        doReturn(bookingPaymentList).when(bookingPaymentDaoSpy).saveEntityFromCarrierBooking(anyList(), eq(carrierBookingId), anyMap());
        List<BookingPayment> result = bookingPaymentDaoSpy.updateEntityFromCarrierBooking(bookingPaymentList, carrierBookingId);
        assertEquals(bookingPaymentList, result);
    }

    @Test
    void updateEntityFromCarrierBooking_BookingPaymentListIsEmpty_ReturnsEmptyList() throws RunnerException {
        Long carrierBookingId = 1L;
        testData.setId(1L);
        when(bookingPaymentRepository.findAll((Specification<BookingPayment>) any(), (Pageable) any())).thenReturn(new PageImpl<BookingPayment>(List.of(testData)));
        List<BookingPayment> result = bookingPaymentDao.updateEntityFromCarrierBooking(Collections.singletonList(testData), carrierBookingId);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void saveEntityFromCarrierBooking_ExceptionThrown() {
        Long carrierBookingId = 1L;
        var bookingPaymentDaoSpy = Mockito.spy(bookingPaymentDao);
        BookingPayment bookingPayment = new BookingPayment();
        bookingPayment.setId(1L);
        Map<Long, BookingPayment> hashMap = new HashMap<>();
        hashMap.put(2L, bookingPayment);
        assertThrows(DataRetrievalFailureException.class, () -> bookingPaymentDaoSpy.saveEntityFromCarrierBooking(Collections.singletonList(bookingPayment), carrierBookingId, hashMap));
    }

    @Test
    void saveEntityFromCarrierBooking_BookingPaymentNewRequest() {
        Long carrierBookingId = 1L;
        var bookingPaymentDaoSpy = Mockito.spy(bookingPaymentDao);
        BookingPayment reqBookingPayment = new BookingPayment();
        BookingPayment bookingPayment = new BookingPayment();
        bookingPayment.setId(1L);
        Map<Long, BookingPayment> hashMap = new HashMap<>();
        doReturn(Collections.singletonList(bookingPayment)).when(bookingPaymentDaoSpy).saveAll(any());
        List<BookingPayment> result = bookingPaymentDaoSpy.saveEntityFromCarrierBooking(Collections.singletonList(reqBookingPayment), carrierBookingId, hashMap);
        assertNotNull(result);
    }

    @Test
    void saveEntityFromCarrierBooking_BookingPaymentUpdateRequest() {
        Long carrierBookingId = 1L;
        var bookingPaymentDaoSpy = Mockito.spy(bookingPaymentDao);
        BookingPayment bookingPayment = new BookingPayment();
        bookingPayment.setId(1L);
        Map<Long, BookingPayment> hashMap = new HashMap<>();
        hashMap.put(1L, bookingPayment);
        doReturn(Collections.singletonList(bookingPayment)).when(bookingPaymentDaoSpy).saveAll(any());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        List<BookingPayment> result = bookingPaymentDaoSpy.saveEntityFromCarrierBooking(Collections.singletonList(bookingPayment), carrierBookingId, hashMap);
        assertNotNull(result);
    }
}
