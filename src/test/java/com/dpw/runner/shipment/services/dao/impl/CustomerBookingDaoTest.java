package com.dpw.runner.shipment.services.dao.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.VersionContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.validator.custom.validations.CustomerBookingValidations;
import com.dpw.runner.shipment.services.validator.custom.validations.CustomerBookingValidationsV3;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
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
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingDaoTest {
    private static JsonTestUtility jsonTestUtility;

    @InjectMocks
    private CustomerBookingDao customerBookingDao;

    @Mock
    private ICustomerBookingRepository customerBookingRepository;
    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private CustomerBookingValidations customValidations;

    @Mock
    private CustomerBookingValidationsV3 customValidationsV3;

    @Mock
    private CustomerBooking customerBookingMock;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private CustomKeyGenerator keyGenerator;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        VersionContext.setVersionFromPath("/api/v2/");
    }

    @Test
    void save() {
        Parties mockParty = Parties.builder().orgCode("ORG123").addressCode("ADDR123").build();
        Parties mockParty2 = Parties.builder().orgCode("ORG323").addressCode("ADER123").build();
        Parties mockParty3 = Parties.builder().orgCode("ORG325").addressCode("ADEE123").build();

        CarrierDetails carrierDetails = CarrierDetails.builder().origin("origin").destination("destination")
                .originPort("origninPort").destinationPort("destinationPort").build();

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingDate(LocalDateTime.now());
        customerBooking.setBookingNumber("BKN33123");
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);
        customerBooking.setServiceMode("service mode");
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setCustomer(mockParty);
        customerBooking.setConsignee(mockParty2);
        customerBooking.setConsignor(mockParty3);
        customerBooking.setIncoTerms("Inco Terms");
        customerBooking.setCargoType(Constants.CARGO_TYPE_FCL);
        customerBooking.setCarrierDetails(carrierDetails);

        when(customerBookingRepository.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        // Test
        CustomerBooking savedCustomerBooking = customerBookingDao.save(customerBooking);

        // Assert
        assertEquals(customerBooking, savedCustomerBooking);
    }

    @Test
    void save_shouldEvictCacheWhenCacheIsPresentAndKeysAreValid() {
        // Given
        Parties mockParty = Parties.builder().orgCode("ORG123").addressCode("ADDR123").build();

        CarrierDetails carrierDetails = CarrierDetails.builder()
                .origin("origin")
                .destination("destination")
                .originPort("originPort")
                .destinationPort("destinationPort")
                .build();

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingDate(LocalDateTime.now());
        customerBooking.setBookingNumber("BKN33123");
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);
        customerBooking.setServiceMode("service mode");
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setCustomer(mockParty);
        customerBooking.setCargoType(Constants.CARGO_TYPE_FCL);
        customerBooking.setCarrierDetails(carrierDetails);
        UUID randomUUID = UUID.randomUUID();
        customerBooking.setGuid(randomUUID);
        customerBooking.setId(123L);

        // Mocks
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(customerBooking.getId());
        existingBooking.setBookingNumber(customerBooking.getBookingNumber());

        when(customerBookingDao.findById(customerBooking.getId()))
                .thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(customerBookingRepository.save(any())).thenReturn(customerBooking);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, 123L)).thenReturn("cache::booking:id:123");
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, randomUUID)).thenReturn("cache::booking:guid:"+randomUUID);

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);

        // When
        CustomerBooking saved = customerBookingDao.save(customerBooking);

        // Then
        assertEquals(customerBooking, saved);
    }

    @Test
    void save_shouldSkipCacheEvictionWhenExceptionOccurs() {
        // Given
        Parties mockParty = Parties.builder().orgCode("ORG123").addressCode("ADDR123").build();

        CarrierDetails carrierDetails = CarrierDetails.builder()
                .origin("origin")
                .destination("destination")
                .originPort("originPort")
                .destinationPort("destinationPort")
                .build();

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingDate(LocalDateTime.now());
        customerBooking.setBookingNumber("BKN33123");
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);
        customerBooking.setServiceMode("service mode");
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setCustomer(mockParty);
        customerBooking.setCargoType(Constants.CARGO_TYPE_FCL);
        customerBooking.setCarrierDetails(carrierDetails);
        UUID randomUUID = UUID.randomUUID();
        customerBooking.setGuid(randomUUID);
        customerBooking.setId(123L);

        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(customerBooking.getId());
        existingBooking.setBookingNumber(customerBooking.getBookingNumber());

        // Mocks
        when(customerBookingDao.findById(customerBooking.getId())).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(customerBookingRepository.save(any())).thenReturn(customerBooking);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, 123L)).thenReturn("cache::booking:id:123");
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, randomUUID)).thenReturn("cache::booking:guid:" + randomUUID);

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);

        // Simulate exception on eviction
        doThrow(new RuntimeException("Eviction failed")).when(mockCache).evictIfPresent(any());

        // When
        CustomerBooking saved = customerBookingDao.save(customerBooking);

        // Then
        assertEquals(customerBooking, saved);
    }

    @Test
    void findByGuid_shouldReturnFromCache_whenCacheHitOccurs() throws JsonProcessingException {
        UUID guid = UUID.randomUUID();
        CustomerBooking expectedBooking = new CustomerBooking();
        expectedBooking.setGuid(guid);
        expectedBooking.setId(101L);

        String serialized = new ObjectMapper().writeValueAsString(expectedBooking);

        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenReturn("cache::booking:guid:" + guid);
        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);
        when(mockCache.get("cache::booking:guid:" + guid, String.class)).thenReturn(serialized);
        when(objectMapper.readValue(serialized, CustomerBooking.class)).thenReturn(expectedBooking);

        Optional<CustomerBooking> result = customerBookingDao.findByGuid(guid);

        assertTrue(result.isPresent());
        assertEquals(expectedBooking, result.get());
    }

    @Test
    void findByGuid_shouldReturnFromDBAndStoreInCache_whenCacheMissOccurs() throws JsonProcessingException {
        UUID guid = UUID.randomUUID();
        CustomerBooking expectedBooking = new CustomerBooking();
        expectedBooking.setGuid(guid);
        expectedBooking.setId(202L);

        String serialized = new ObjectMapper().writeValueAsString(expectedBooking);

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenReturn("guidKey");
        when(mockCache.get("guidKey", String.class)).thenReturn(null); // Cache miss
        when(customerBookingRepository.findByGuid(guid)).thenReturn(Optional.of(expectedBooking));
        when(objectMapper.writeValueAsString(expectedBooking)).thenReturn(serialized);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, expectedBooking.getId())).thenReturn("idKey");

        Optional<CustomerBooking> result = customerBookingDao.findByGuid(guid);

        assertTrue(result.isPresent());
        verify(mockCache).put("idKey", serialized);
        verify(mockCache).put("guidKey", serialized);
    }


    @Test
    void findByGuid_shouldReturnFromDB_whenCacheIsNull() throws JsonProcessingException {
        UUID guid = UUID.randomUUID();
        CustomerBooking expected = new CustomerBooking();
        expected.setGuid(guid);
        expected.setId(303L);

        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(null); // cache is null
        when(customerBookingRepository.findByGuid(guid)).thenReturn(Optional.of(expected));
//        when(objectMapper.writeValueAsString(expected)).thenReturn("dummy-json");
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenReturn("guidKey");
//        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, expected.getId())).thenReturn("idKey");

        Optional<CustomerBooking> result = customerBookingDao.findByGuid(guid);

        assertTrue(result.isPresent());
        assertEquals(expected, result.get());
    }

    @Test
    void findByGuid_shouldReturnEmpty_whenNotFoundInCacheOrDB() throws JsonProcessingException {
        UUID guid = UUID.randomUUID();

        Cache mockCache = mock(Cache.class);
        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenReturn("guidKey");
        when(mockCache.get("guidKey", String.class)).thenReturn(null); // cache miss
        when(customerBookingRepository.findByGuid(guid)).thenReturn(Optional.empty());

        Optional<CustomerBooking> result = customerBookingDao.findByGuid(guid);

        assertFalse(result.isPresent());
        verify(mockCache, never()).put(any(), any()); // Should not cache anything
    }

    @Test
    void findByGuid_shouldFallbackToRepository_whenCacheThrowsException() throws JsonProcessingException {
        UUID guid = UUID.randomUUID();
        CustomerBooking fallbackBooking = new CustomerBooking();
        fallbackBooking.setGuid(guid);
        fallbackBooking.setId(404L);

        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenThrow(new RuntimeException("boom!"));
        when(customerBookingRepository.findByGuid(guid)).thenReturn(Optional.of(fallbackBooking));

        Optional<CustomerBooking> result = customerBookingDao.findByGuid(guid);

        assertTrue(result.isPresent());
        assertEquals(fallbackBooking, result.get());
    }

    @Test
    void delete_shouldEvictCacheAndDelete_whenCacheIsPresent() {
        // Arrange
        Long id = 123L;
        UUID guid = UUID.randomUUID();
        CustomerBooking booking = new CustomerBooking();
        booking.setId(id);
        booking.setGuid(guid);

        Cache mockCache = mock(Cache.class);

        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(mockCache);
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, id)).thenReturn("idKey");
        when(keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, guid)).thenReturn("guidKey");

        // Act
        customerBookingDao.delete(booking);

        // Assert
        verify(customerBookingRepository).delete(booking);
        verify(mockCache).evictIfPresent("idKey");
        verify(mockCache).evictIfPresent("guidKey");
    }

    @Test
    void delete_shouldOnlyDeleteFromRepository_whenCacheIsNull() {
        // Arrange
        Long id = 456L;
        UUID guid = UUID.randomUUID();
        CustomerBooking booking = new CustomerBooking();
        booking.setId(id);
        booking.setGuid(guid);

        when(cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING)).thenReturn(null);

        // Act
        customerBookingDao.delete(booking);

        // Assert
        verify(customerBookingRepository).delete(booking);
        verifyNoInteractions(keyGenerator);
    }


    // create tc for save by using input that fails all validations one by one to inc coverage
    @Test
    void findAll() {
        CustomerBooking testData = CustomerBooking.builder().build();

        List<CustomerBooking> customerBookingList = new ArrayList<>();
        customerBookingList.add(testData);

        PageImpl<CustomerBooking> customerBookingPage = new PageImpl<>(customerBookingList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<CustomerBooking>, Pageable> pair = fetchData(listReq, CustomerBooking.class);

        when(customerBookingRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(customerBookingPage);
        assertEquals(customerBookingPage, customerBookingDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findById() {
        CustomerBooking testData = CustomerBooking.builder().build();
        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(testData));
        assertEquals(testData, customerBookingDao.findById(1L).get());
    }

    @Test
    void delete() {
        CustomerBooking testData = CustomerBooking.builder().build();
        customerBookingDao.delete(testData);
        verify(customerBookingRepository, times(1)).delete(testData);
    }

    @Test
    void updateIsPlatformBookingCreated() {
        when(customerBookingRepository.updateIsPlatformBookingCreated(any(), any())).thenReturn(1);
        assertEquals(1, customerBookingDao.updateIsPlatformBookingCreated(1L, true));
    }

    @Test
    void updateBillStatus() {
        when(customerBookingRepository.updateBillingStatus(any(), any())).thenReturn(1);
        assertEquals(1, customerBookingDao.updateBillStatus(1L, true));
    }

    @Test
    void SaveError() {
        HashSet<String> error = new HashSet<>();
        error.add("An Error Occured");
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveV2Success() {
        VersionContext.setVersionFromPath("/api/v2/");
        CustomerBooking customerBooking = CustomerBooking.builder().bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT).build();
        customerBooking.setId(1L);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(customerBooking));
        doNothing().when(customValidations).onSave(any(),any());
        customerBookingDao.save(customerBooking);
        verify(customValidations, times(1)).onSave(any(), any());
    }

    @Test
    void SaveEntityNotPresent() {
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveEntityIdNull() {
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();

        when(customerBookingRepository.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void updateEntityFromShipmentConsoleEntityNotPresent() {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void updateEntityFromShipmentConsoleEntity() throws RunnerException {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(customerBooking));
        when(customerBookingRepository.save(any())).thenReturn(customerBooking);

        assertEquals(customerBooking, customerBookingDao.updateEntityFromShipmentConsole(customerBooking));
    }

    @Test
    void updateEntityFromShipmentConsoleCatch() {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(RunnerException.class, () -> {
            customerBookingDao.updateEntityFromShipmentConsole(customerBooking);
        });
    }

    @Test
    void SaveErrorV3() {
        VersionContext.setVersionFromPath("/api/v3/");
        HashSet<String> error = new HashSet<>();
        error.add("An Error Occured");
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveEntityNotPresentV3() {
        VersionContext.setVersionFromPath("/api/v3/");
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveEntityIdNullV3() {
        VersionContext.setVersionFromPath("/api/v3/");
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();

        when(customerBookingRepository.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void updateEntityFromShipmentConsoleEntityNotPresentV3() {
        VersionContext.setVersionFromPath("/api/v3/");
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveV3Success() {
        VersionContext.setVersionFromPath("/api/v3/");
        CustomerBooking customerBooking = CustomerBooking.builder().bookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT).build();
        customerBooking.setId(1L);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());
        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(customerBooking));
        doNothing().when(customValidationsV3).onSave(any(),any());
        customerBookingDao.save(customerBooking);
        verify(customValidationsV3, times(1)).onSave(any(), any());
    }
}