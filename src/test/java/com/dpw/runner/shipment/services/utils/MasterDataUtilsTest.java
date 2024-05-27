package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertNull;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import com.dpw.runner.shipment.services.entity.*;
import lombok.NonNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.TestPropertySource;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource("classpath:application-test.properties")
class MasterDataUtilsTest {

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails completeShipment;
    private static CustomerBooking customerBooking;

    @Mock
    private IV1Service v1Service;
    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private CacheManager cacheManager;
    @Mock
    CustomKeyGenerator keyGenerator;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private CommonUtils commonUtils;
    @InjectMocks
    private MasterDataUtils masterDataUtils;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        completeShipment = jsonTestUtility.getCompleteShipment();
        customerBooking = jsonTestUtility.getCustomerBooking();
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkContainerTypeRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void testCreateInBulkContainerTypeRequest() {
        // Arrange
        // Act and Assert
        var response = masterDataUtils.createInBulkMasterListRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void testCreateInBulkContainerTypeRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());

    }

    @Test
    void testCreateInBulkContainerTypeRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void testCreateInBulkContainerTypeRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

     @Test
    void testFetchInBulkMasterList() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferMasterLists.class))).thenReturn(List.of(EntityTransferMasterLists.builder().ItemValue("SEA").build()));
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(List.of(MasterListRequest.builder().build())).build());
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void testFetchInBulkMasterList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(List.of()).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    @Test
    void testFetchInBulkMasterList3() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkMasterList(MasterListRequestV2.builder().build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkUnLocationsRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkUnLocationsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkUnLocationsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkUnLocationsRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkUnLocationsRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkUnLocationsRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkUnlocations() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(EntityTransferUnLocations.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations2() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(EntityTransferUnLocations.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations3() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(null);
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations4() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkChargeTypeRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkChargeTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkChargeTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkChargeTypeRequest2() {
        // Arrange
        var mockCustomerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkChargeTypeRequest3() {
        // Arrange
        var mockCustomerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkChargeTypeRequest4() {
        // Arrange
        var mockCustomerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkChargeTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferChargeType.class))).thenReturn(List.of(EntityTransferChargeType.builder().ChargeCode("AMS").build()));
        when(v1Service.fetchChargeCodeData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkChargeTypes(List.of(UUID.randomUUID().toString()));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkChargeTypes2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkChargeTypes(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkContainerTypeRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkContainerTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkContainerTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkContainerTypeRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkContainerTypeRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkContainerTypeRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkContainerTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkContainerTypes(List.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkContainerTypes2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkContainerTypes(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCommodityTypeRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkCommodityTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkCommodityTypeRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkCommodityTypeRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkCommodityTypeRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCommodityTypeRequest(mockShipmentDetailsResponse.getContainersList().get(0), Containers.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkCommodityTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCommodityType.class))).thenReturn(List.of(EntityTransferCommodityType.builder().Code("20GP").build()));
        when(v1Service.fetchCommodityData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCommodityTypes(List.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkCommodityTypes2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCommodityTypes(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCarriersRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkCarriersRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCarriersRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkCarriersRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkCarriersRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var container = mockShipmentDetailsResponse.getContainersList().get(0);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkCarriersRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkCarriers() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().ItemValue("CODE").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriers(List.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkCarriers2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriers(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkVesselsRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkVesselsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkVesselsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNull(response);
    }

    @Test
    void createInBulkVesselsRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkVesselsRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var container = mockShipmentDetailsResponse.getContainersList().get(0);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkVesselsRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkVessels() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferVessels.class))).thenReturn(List.of(EntityTransferVessels.builder().Guid(UUID.randomUUID()).build()));
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkVessels(List.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkVessels2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkVessels(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCurrencyRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkCurrencyRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCurrencyRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkCurrencyRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkCurrencyRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkCurrencyRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInCurrencyList() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCurrency.class))).thenReturn(List.of(EntityTransferCurrency.builder().CurrenyCode("INR").build()));
        when(v1Service.fetchCurrenciesData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInCurrencyList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInCurrencyList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInCurrencyList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkTenantsRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkTenantsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkTenantsRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkTenantsRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkTenantsRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInTenantsList() {
        var tenantModel = new TenantModel();
        tenantModel.tenantId = 11L;
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(TenantModel.class))).thenReturn(List.of(tenantModel));
        when(v1Service.listCousinBranches(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInTenantsList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInTenantsList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInTenantsList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCurrencyRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkDGSubstanceRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkDGSubstanceRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkDGSubstanceRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var packing = mockShipmentDetailsResponse.getPackingList().get(0);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code");
        packing.setDGSubstanceId(11);
        var response2 = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code");

        assertNotNull(response);
        assertTrue(response.isEmpty());

        assertNotNull(response2);
        assertFalse(response2.isEmpty());
    }

    @Test
    void createInBulkDGSubstanceRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var packing = mockShipmentDetailsResponse.getPackingList().get(0);
        packing.setDGSubstanceId(11);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkDGSubstanceRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var packing = mockShipmentDetailsResponse.getPackingList().get(0);
        packing.setDGSubstanceId(11);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInDGSubstanceList() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferDGSubstance.class))).thenReturn(List.of(EntityTransferDGSubstance.builder().Id(123L).build()));
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInDGSubstanceList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInDGSubstanceList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInDGSubstanceList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkWareHouseRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkWareHouseRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkWareHouseRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkWareHouseRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setWarehouseId(111L);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());

    }

    @Test
    void createInBulkWareHouseRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setWarehouseId(111L);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkWareHouseRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setWarehouseId(111L);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInWareHousesList() {
        var mockResponse = new WareHouseResponse();
        mockResponse.setId(11L);
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(WareHouseResponse.class))).thenReturn(List.of(mockResponse));
        when(v1Service.fetchWarehouseData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInWareHousesList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInWareHousesList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInWareHousesList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkActivityTypeRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkActivityTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkActivityTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkActivityTypeRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setActivityType("111L");
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());

    }

    @Test
    void createInBulkActivityTypeRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setActivityType("111L");

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());

    }

    @Test
    void createInBulkActivityTypeRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.getAdditionalDetails().setActivityType("111L");

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInActivityMasterList() {
        var mockResponse = new ActivityMasterResponse();
        mockResponse.setActivityCode("11L");
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(ActivityMasterResponse.class))).thenReturn(List.of(mockResponse));
        when(v1Service.fetchActivityMaster(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInActivityMasterList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInActivityMasterList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInActivityMasterList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkSalesAgentRequest(IRunnerResponse, Class, Map, String)}
     */

    @Test
    void createInBulkSalesAgentRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkSalesAgentRequest(null, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkSalesAgentRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.setSalesAgent(123L);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertFalse(response.isEmpty());

    }

    @Test
    void createInBulkSalesAgentRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        mockShipmentDetailsResponse.setSalesAgent(123L);
        var response2 = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code");
        assertNotNull(response);
        assertTrue(response.isEmpty());
        assertNotNull(response2);
    }

    @Test
    void createInBulkSalesAgentRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.setSalesAgent(123L);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code"));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInSalesAgentList() {
        var mockResponse = new SalesAgentResponse();
        mockResponse.setId(111L);
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(SalesAgentResponse.class))).thenReturn(List.of(mockResponse));
        when(v1Service.fetchSalesAgentData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInSalesAgentList(List.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInSalesAgentList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInSalesAgentList(List.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    @Test
    void withMdc() {
        boolean isSuccess = true;
        var runnable = masterDataUtils.withMdc(() -> {});
        assertTrue(isSuccess);
    }

    @Test
    void getUNLocRow() {
        var response = masterDataUtils.getUNLocRow(null);
        assertNull(response);
    }

    @Test
    void getUNLocRow2() {
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(UnlocationsResponse.class))).thenReturn(List.of());
        var response = masterDataUtils.getUNLocRow(UUID.randomUUID().toString());
        assertNull(response);
    }

    @Test
    void getUNLocRow3() {
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(UnlocationsResponse.class))).thenReturn(List.of(new UnlocationsResponse()));
        var response = masterDataUtils.getUNLocRow(UUID.randomUUID().toString());
        assertNotNull(response);
    }

    @Test
    void getLocationData() {
        var response = masterDataUtils.getLocationData(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getLocationData2() {
        var response = masterDataUtils.getLocationData(Set.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getLocationData3() {
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(UnlocationsResponse.class))).thenReturn(null);
        var response = masterDataUtils.getLocationData(Set.of(locationGuid));
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getLocationData4() {
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        UnlocationsResponse mockV1Response = new UnlocationsResponse();
        mockV1Response.setLocationsReferenceGUID(locationGuid);
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(UnlocationsResponse.class))).thenReturn(List.of(mockV1Response));
        var response = masterDataUtils.getLocationData(Set.of(locationGuid));
        assertNotNull(response);
        assertFalse(response.isEmpty());
        assertTrue(response.containsKey(locationGuid));
    }


    @Test
    void fetchDgSubstanceRow() {
        var response = masterDataUtils.fetchDgSubstanceRow(null);
        assertNotNull(response);
        assertNull(response.getId());
    }


    @Test
    void fetchDgSubstanceRow2() {
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().build());
        var response = masterDataUtils.fetchDgSubstanceRow(11);
        assertNotNull(response);
        assertNull(response.getId());
    }

    @Test
    void fetchDgSubstanceRow3() {
        EntityTransferDGSubstance mockResponse = EntityTransferDGSubstance.builder().Id(11L).build();
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferDGSubstance.class))).thenReturn(List.of(mockResponse));
        var response = masterDataUtils.fetchDgSubstanceRow(11);
        assertNotNull(response);
        assertNotNull(response.getId());
        assertEquals(mockResponse.getId(), response.getId());
    }

    @Test
    void fetchOrganizations() {
        EntityTransferOrganizations mockResponse = EntityTransferOrganizations.builder().Id(11L).build();
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(List.of(mockResponse));
        var response = masterDataUtils.fetchOrganizations("field", "value");
        assertNotNull(response);
        assertNotNull(response.get(0));
        assertEquals(mockResponse.getId(), response.get(0).getId());
    }

    @Test
    void fetchWareHouseData() {
        var response = masterDataUtils.fetchWareHouseData(List.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }


}
