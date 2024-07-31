package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.dto.request.UsersDto;
import com.dpw.runner.shipment.services.commons.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.commons.entity.BookingCharges;
import com.dpw.runner.shipment.services.commons.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingChargesRepository;
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
class BookingChargesDaoTest {

    @Mock
    private IBookingChargesRepository bookingChargesRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private AuditLogService auditLogService;

    @InjectMocks
    private BookingChargesDao bookingChargesDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static BookingCharges testBookingCharges;

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
        testBookingCharges = jsonTestUtility.getCustomerBooking().getBookingCharges().get(0);
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
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(bookingChargesRepository.save(any(BookingCharges.class))).thenReturn(testBookingCharges);

        BookingCharges savedBookingCharges = bookingChargesDao.save(testBookingCharges);

        assertEquals(testBookingCharges, savedBookingCharges);
    }

    @Test
    void testSave_Failure() {
        Set<String> errors = new HashSet<>();
        errors.add("Required field missing");
        when(jsonHelper.convertToJson(any())).thenReturn(jsonTestUtility.convertToJson(testBookingCharges));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(errors);
        assertThrows(ValidationException.class, () -> bookingChargesDao.save(testBookingCharges));
    }

    @Test
    void testFindAll() {
        Page<BookingCharges> bookingChargesPage = mock(Page.class);
        Specification<BookingCharges> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(bookingChargesRepository.findAll(spec, pageable)).thenReturn(bookingChargesPage);
        Page<BookingCharges> foundBookingChargesPage = bookingChargesDao.findAll(spec, pageable);
        assertEquals(bookingChargesPage, foundBookingChargesPage);
    }

    @Test
    void testFindById() {
        when(bookingChargesRepository.findById(anyLong())).thenReturn(Optional.of(testBookingCharges));
        Optional<BookingCharges> foundBookingCharges = bookingChargesDao.findById(1L);
        assertTrue(foundBookingCharges.isPresent());
        assertEquals(testBookingCharges, foundBookingCharges.get());
    }

    @Test
    void testDelete() {
        assertDoesNotThrow(() -> bookingChargesDao.delete(testBookingCharges));
        verify(bookingChargesRepository, Mockito.times(1)).delete(testBookingCharges);
    }

    @Test
    void testSaveEntityFromBooking() throws Exception {
        List<BookingCharges> bookingChargesList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(bookingChargesList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testBookingCharges).when(spyService).save(any());
        List<BookingCharges> routings = spyService.saveEntityFromBooking(bookingChargesList, 1L);
        assertNotNull(routings);
        assertEquals(bookingChargesList, routings);
    }

    @Test
    void testSaveEntityFromBooking_NullId() throws Exception {
        testBookingCharges.setId(null);
        List<BookingCharges> bookingChargesList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(bookingChargesList)).when(spyService).findAll(any(), any());
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testBookingCharges).when(spyService).save(any());
        List<BookingCharges> routings = spyService.saveEntityFromBooking(bookingChargesList, 1L);
        assertNotNull(routings);
        assertEquals(bookingChargesList, routings);
    }

    @Test
    void testSaveEntityFromBooking_RetrievalFailure() throws Exception {
        List<BookingCharges> bookingChargesList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(RuntimeException.class, () -> spyService.saveEntityFromBooking(bookingChargesList, 1L));
    }

    @Test
    void testSaveEntityFromBooking_AuditLogFailure() throws Exception {
        List<BookingCharges> bookingChargesList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(bookingChargesList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(testBookingCharges).when(spyService).save(any());
        List<BookingCharges> routings = spyService.saveEntityFromBooking(bookingChargesList, 1L);
        assertNotNull(routings);
        assertEquals(bookingChargesList, routings);
    }

    @Test
    void testUpdateEntityFromBooking() throws RunnerException {
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<BookingCharges> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullRoutings() throws RunnerException {
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        List<BookingCharges> routingsList1 = spyService.updateEntityFromBooking(null, 1L);
        assertNotNull(routingsList1);
        assertEquals(new ArrayList<>(), routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId() throws RunnerException {
        testBookingCharges.setId(null);
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<BookingCharges> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId_Failure() throws RunnerException {
        testBookingCharges.setId(null);
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doThrow(new RuntimeException()).when(bookingChargesRepository).delete(any());
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<BookingCharges> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_NullId_AuditLogFailure() throws Exception {
        testBookingCharges.setId(null);
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(routingsList)).when(spyService).findAll(any(), any());
        doThrow(InvocationTargetException.class).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doReturn(routingsList).when(spyService).saveEntityFromBooking(anyList(), anyLong());
        List<BookingCharges> routingsList1 = spyService.updateEntityFromBooking(routingsList, 1L);
        assertNotNull(routingsList1);
        assertEquals(routingsList, routingsList1);
    }

    @Test
    void testUpdateEntityFromBooking_Failure() throws RunnerException {
        List<BookingCharges> routingsList = Collections.singletonList(testBookingCharges);
        BookingChargesDao spyService = spy(bookingChargesDao);
        doReturn(new PageImpl<>(new ArrayList<>())).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromBooking(routingsList, 1L));
    }

    @Test
    void updateEntityFromShipmentConsole_Failure() throws RunnerException {
        assertThrows(RunnerException.class, () -> bookingChargesDao.updateEntityFromShipmentConsole(testBookingCharges));
    }

    @Test
    void updateEntityFromShipmentConsole() throws RunnerException {
        when(bookingChargesRepository.findById(any())).thenReturn(Optional.of(testBookingCharges));
        when(bookingChargesRepository.save(any())).thenReturn(testBookingCharges);
        BookingCharges bookingCharges = bookingChargesDao.updateEntityFromShipmentConsole(testBookingCharges);
        assertEquals(testBookingCharges, bookingCharges);
    }

}
