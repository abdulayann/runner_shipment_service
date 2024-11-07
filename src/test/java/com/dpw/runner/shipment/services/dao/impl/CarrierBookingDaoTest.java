package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingDaoTest {
    @Mock
    private ICarrierBookingRepository carrierBookingRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private CarrierBookingDao carrierBookingDao;


    @BeforeEach
    void setUp() {
        TenantContext.setCurrentTenant(1);
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidCarrierBooking_ReturnsNewCarrierBooking() {
        CarrierBooking carrierBooking = new CarrierBooking();
//        carrierBooking.setForwarderRefNumber("abcd");
//        carrierBooking.setBol("abcd");
//        when(carrierBookingRepository.findByForwarderRefNumber("abcd", 1)).thenReturn(Collections.emptyList());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(carrierBookingRepository.save(any(CarrierBooking.class))).thenReturn(carrierBooking);

        CarrierBooking savedCarrierBooking = carrierBookingDao.save(carrierBooking);

        assertEquals(carrierBooking, savedCarrierBooking);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(carrierBookingRepository).save(any(CarrierBooking.class));
    }

    @Test
    void save_ValidCarrierBooking_ReturnsSavedCarrierBooking() {
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setId(1L);
        carrierBooking.setIsLocked(Boolean.FALSE);
        carrierBooking.setForwarderRefNumber("abcd");
        carrierBooking.setBol("abcd");
        Parties consolidationAddress = Parties.builder().build().setType("abcd");
        carrierBooking.setConsolidationAddresses(Collections.singletonList(consolidationAddress));
        List<Long> carrierBookingList = Collections.singletonList(1L);
        when(carrierBookingRepository.findByForwarderRefNumber("abcd", 1)).thenReturn(carrierBookingList);
        when(carrierBookingRepository.findByBol("abcd", 1)).thenReturn(carrierBookingList);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(carrierBookingRepository.save(any(CarrierBooking.class))).thenReturn(carrierBooking);
        when(carrierBookingRepository.findById(1L)).thenReturn(Optional.of(carrierBooking));

        CarrierBooking savedCarrierBooking = carrierBookingDao.save(carrierBooking);
        assertEquals(carrierBooking, savedCarrierBooking);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(carrierBookingRepository).save(any(CarrierBooking.class));
    }

    @Test
    void save_ValidCarrierBooking_SavedCarrierBooking_ThrowsException() {
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setId(1L);
        carrierBooking.setIsLocked(Boolean.FALSE);
        carrierBooking.setForwarderRefNumber("abcd");
        carrierBooking.setBol("abcd");
        Parties consolidationAddress = Parties.builder().build().setType("abcd");
        Parties consolidationAddress2 = Parties.builder().build().setType("abcd");
        Parties consolidationAddress3 = Parties.builder().build().setType("abcde");
        carrierBooking.setConsolidationAddresses(Arrays.asList(consolidationAddress, consolidationAddress2, consolidationAddress3));

        List<Long> carrierBookingList = Collections.singletonList(2L);
        Set<String> errors = new HashSet<>();
        when(carrierBookingRepository.findByForwarderRefNumber("abcd", 1)).thenReturn(carrierBookingList);
        when(carrierBookingRepository.findByBol("abcd", 1)).thenReturn(carrierBookingList);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(errors);
        when(carrierBookingRepository.findById(1L)).thenReturn(Optional.of(carrierBooking));
        assertThrows(ValidationException.class, () -> carrierBookingDao.save(carrierBooking));
    }

    @Test
    void save_ValidCarrierBooking_SavedCarrierBooking_ThrowsValidationException() {
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setId(1L);
        carrierBooking.setIsLocked(Boolean.TRUE);
        carrierBooking.setForwarderRefNumber("abcd");
        carrierBooking.setBol("abcd");
        Parties consolidationAddress = Parties.builder().build().setType("abcd");
        Parties consolidationAddress2 = Parties.builder().build().setType("abcd");
        Parties consolidationAddress3 = Parties.builder().build().setType("abcde");
        carrierBooking.setConsolidationAddresses(Arrays.asList(consolidationAddress, consolidationAddress2, consolidationAddress3));

        Set<String> errors = new HashSet<>();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(errors);
        when(carrierBookingRepository.findById(1L)).thenReturn(Optional.of(carrierBooking));
        assertThrows(ValidationException.class, () -> carrierBookingDao.save(carrierBooking));
    }

    @Test
    void save_ValidCarrierBooking_ThrowsValidationException() {
        CarrierBooking carrierBooking = new CarrierBooking();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(set);
        assertThrows(ValidationException.class, () -> carrierBookingDao.save(carrierBooking));
    }

    @Test
    void save_ExistingCarrierBooking_ThrowsDataRetrievalException() {
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(carrierBookingRepository.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> carrierBookingDao.save(carrierBooking));
    }


    @Test
    void saveAll_ValidCarrierBookings_ReturnsSavedCarrierBookings() {
        List<CarrierBooking> carrierBookingList = Arrays.asList(new CarrierBooking(), new CarrierBooking());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(carrierBookingRepository.saveAll(anyList())).thenReturn(carrierBookingList);

        List<CarrierBooking> savedCarrierBookings = carrierBookingDao.saveAll(carrierBookingList);

        assertEquals(carrierBookingList, savedCarrierBookings);
        verify(validatorUtility, times(2)).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(carrierBookingRepository).saveAll(carrierBookingList);
    }

    @Test
    void saveAll_ValidCarrierBookings_ThrowsValidationException() {
        List<CarrierBooking> carrierBookingList = Arrays.asList(new CarrierBooking(), new CarrierBooking());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Set.of("test"));
        assertThrows(ValidationException.class, () -> carrierBookingDao.saveAll(carrierBookingList));
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfCarrierBookings() {
        Page<CarrierBooking> expectedPage = mock(Page.class);
        Specification<CarrierBooking> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(carrierBookingRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<CarrierBooking> resultPage = carrierBookingDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(carrierBookingRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfCarrierBooking() {
        Long id = 1L;
        CarrierBooking carrierBooking = new CarrierBooking();
        when(carrierBookingRepository.findById(id)).thenReturn(Optional.of(carrierBooking));

        Optional<CarrierBooking> result = carrierBookingDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(carrierBooking, result.get());
        verify(carrierBookingRepository).findById(id);
    }

    @Test
    void findByGuid_ValidId_ReturnsOptionalOfCarrierBooking() {
        CarrierBooking carrierBooking = new CarrierBooking();
        when(carrierBookingRepository.findByGuid(any())).thenReturn(Optional.of(carrierBooking));

        Optional<CarrierBooking> result = carrierBookingDao.findByGuid(any());

        assertTrue(result.isPresent());
        assertEquals(carrierBooking, result.get());
        verify(carrierBookingRepository).findByGuid(any());
    }

    @Test
    void findByBol_ValidBol_ReturnsOptionalOfCarrierBookingIdList() {
        Long carrierBookingId = 1L;
        when(carrierBookingRepository.findByBol("abcd", 1)).thenReturn(Collections.singletonList(carrierBookingId));
        List<Long> result = carrierBookingDao.getCarrierBookingIdsListFromBol("abcd");

        assertFalse(result.isEmpty());
        assertEquals(Collections.singletonList(carrierBookingId), result);
        verify(carrierBookingRepository).findByBol(any(), any());
    }

    @Test
    void findByForwarderRefNumber_ValidRef_ReturnsOptionalOfCarrierBookingIdList() {
        Long carrierBookingId = 1L;
        when(carrierBookingRepository.findByForwarderRefNumber("abcd", 1)).thenReturn(Collections.singletonList(carrierBookingId));

        List<Long> result = carrierBookingDao.getCarrierBookingIdsListFromForwarderRefNumber("abcd");

        assertFalse(result.isEmpty());
        assertEquals(Collections.singletonList(carrierBookingId), result);
        verify(carrierBookingRepository).findByForwarderRefNumber(any(), any());
    }

    @Test
    void getMaxId_ReturnsOptionalOfMaxDbId() {
        Long carrierBookingId = 1L;
        when(carrierBookingRepository.findMaxId()).thenReturn(Optional.of(carrierBookingId));

        Optional<Long> result = carrierBookingDao.getMaxId();

        assertTrue(result.isPresent());
        assertEquals(carrierBookingId, result.get());
        verify(carrierBookingRepository).findMaxId();
    }

    @Test
    void existsByIntraBookingId_ReturnsBoolean() {
        when(carrierBookingRepository.existsByIntraBookingId(any())).thenReturn(Boolean.TRUE);
        boolean result = carrierBookingDao.existsByIntraBookingId(any());
        assertTrue(result);
        verify(carrierBookingRepository).existsByIntraBookingId(any());
    }
}
