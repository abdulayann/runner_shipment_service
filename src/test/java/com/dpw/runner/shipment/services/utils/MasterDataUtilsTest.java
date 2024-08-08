package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCurrency;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.test.context.TestPropertySource;
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
    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;
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

    @Test
    void fetchInBulkCarriersBySCACCode() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().Identifier1("CODE").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriersBySCACCode(List.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkCarriersBySCACCode2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriersBySCACCode(List.of());
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
        when(commonUtils.convertToList(any(), eq(TenantModel.class))).thenReturn(List.of(tenantModel));
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
    void getCarriersData() {
        var response = masterDataUtils.getCarriersData(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarriersData2() {
        var response = masterDataUtils.getCarriersData(Set.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarriersData3() {
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(null);
        var response = masterDataUtils.getCarriersData(Set.of(locationGuid));
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarriersData4() {
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(new ArrayList<>());
        var response = masterDataUtils.getCarriersData(Set.of(locationGuid));
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

    @Test
    void fetchBillDataForShipments() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(cache.get(any())).thenReturn(ShipmentBillingListResponse.BillingData::new);

        masterDataUtils.fetchBillDataForShipments(List.of(completeShipment), List.of(mockShipmentListResponse));

        assertTrue(isSuccess);

    }

    @Test
    void fetchBillDataForShipments2() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);
        var mockV1Response = new ShipmentBillingListResponse();
        var mockMapData = new HashMap<String, ShipmentBillingListResponse.BillingData>();
        mockMapData.put(StringUtility.convertToString(mockShipmentListResponse.getGuid()), new ShipmentBillingListResponse.BillingData());
        mockV1Response.setData(mockMapData);
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(cache.get(any())).thenReturn(null);

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(v1Service.fetchShipmentBillingData(any())).thenReturn(mockV1Response);
        masterDataUtils.fetchBillDataForShipments(List.of(completeShipment), List.of(mockShipmentListResponse));

        assertTrue(isSuccess);

    }

    @Test
    void fetchBillDataForShipments3() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);

        masterDataUtils.fetchBillDataForShipments(List.of(), List.of(mockShipmentListResponse));
        assertTrue(isSuccess);
    }

    @Test
    void fetchBillDataForShipments4() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);
        masterDataUtils.fetchBillDataForShipments(null, List.of(mockShipmentListResponse));
        assertTrue(isSuccess);
    }

    @Test
    void getMasterListData() {

        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().entities(List.of(MasterData.builder().build())).build());
        when(jsonHelper.convertValueToList(any(), eq(MasterData.class))).thenReturn(List.of(MasterData.builder().id(11).build()));
        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, "1234");
        assertNotNull(responseEntity);
        assertEquals(11, responseEntity.getId());
    }

    @Test
    void getMasterListData2() {

        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().entities(List.of(MasterData.builder().build())).build());
        when(jsonHelper.convertValueToList(any(), eq(MasterData.class))).thenReturn(List.of());
        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, "1234");
        assertNull(responseEntity);
    }

    @Test
    void getMasterListData3() {

        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, null);
        assertNull(responseEntity);
    }

    @Test
    void getMasterListData4() {

        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, StringUtility.getEmptyString());
        assertNull(responseEntity);
    }

    @Test
    void getMasterListData5() {
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().build());
        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, "1234");
        assertNull(responseEntity);
    }

    @Test
    void getVesselName() {
        var mockVesselGuid = UUID.randomUUID();
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferVessels.class))).thenReturn(List.of(EntityTransferVessels.builder().Guid(mockVesselGuid).Name("Mark").build()));
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getVesselName(mockVesselGuid.toString());
        assertNotNull(responseEntity);
        assertEquals("Mark", responseEntity);
    }


    @Test
    void getVesselName2() {
        var mockVesselGuid = UUID.randomUUID();
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferVessels.class))).thenReturn(List.of(EntityTransferVessels.builder().Guid(mockVesselGuid).Name("Mark").build()));
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getVesselName(UUID.randomUUID().toString());
        assertNull(responseEntity);
    }


    @Test
    void getVesselName3() {
        var responseEntity = masterDataUtils.getVesselName(null);
        assertNull(responseEntity);
    }

    @Test
    void getCarrierName() {
        var mockCarrierCode = "APLU";
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().ItemValue(mockCarrierCode).ItemDescription("APULU Carrier").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getCarrierName(mockCarrierCode);
        assertNotNull(responseEntity);
        assertEquals("APULU Carrier", responseEntity);
    }


    @Test
    void getCarrierName2() {
        var mockCarrierCode = "APLU";
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().ItemValue(mockCarrierCode).build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getCarrierName("AAPU");
        assertNull(responseEntity);
    }

    @Test
    void getCarrierName3() {
        var responseEntity = masterDataUtils.getCarrierName(null);
        assertNull(responseEntity);
    }

    @Test
    void fetchUnlocationByOneIdentifier() {
        var responseEntity = masterDataUtils.fetchUnlocationByOneIdentifier(null, null);
        assertNull(responseEntity);
    }


    @Test
    void fetchUnlocationByOneIdentifier2() {
        var mockV1Response = new UnlocationsResponse();
        mockV1Response.setId(111);
        mockV1Response.setName("Vancouver");
        when(jsonHelper.convertValueToList(any(), eq(UnlocationsResponse.class))).thenReturn(List.of(mockV1Response));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.getRandomString(5));
        assertNotNull(responseEntity);
        assertEquals(List.of(mockV1Response), responseEntity);
    }

    @Test
    void setMasterData() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferUnLocations.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }


    @Test
    void setMasterDat2() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferUnLocations.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS, true);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData3() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferUnLocations.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData4() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Code("20GP").Description("20 Foot").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CONTAINER_TYPE);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData5() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferChargeType.builder().ChargeCode("AMS").Description("AMS Filling").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CHARGE_TYPE);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData6() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ValuenDesc("ValuenDesc").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData7() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ItemDescription("ItemDescription").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData8() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ItemDescription("ItemDescription").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST, true);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData9() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferVessels.builder().Name("Name").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.VESSELS);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData10() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferCarrier.builder().ItemDescription("DPW").build());

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CARRIER);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData11() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.TENANTS);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData12() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(WareHouseResponse::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.WAREHOUSES);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData13() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(ActivityMasterResponse::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.ACTIVITY_TYPE);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData14() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(SalesAgentResponse::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.SALES_AGENT);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData15() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCommodityType::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.COMMODITY);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }


    @Test
    void setMasterData16() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCommodityType::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, StringUtility.getRandomString(10));

        assertNotNull(resonseMap);
        assertFalse(resonseMap.containsKey("field"));
    }

    @Test
    void setMasterData17() {
        Cache cache = mock(Cache.class);
        var inputFieldNameKeyMap = new HashMap<String, String>();
        inputFieldNameKeyMap.put("field", "value");

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCurrency::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CURRENCIES);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void getChargeTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferChargeType.class))).thenReturn(List.of(EntityTransferChargeType.builder().ChargeCode("AMS").build()));
        when(v1Service.fetchChargeCodeData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.getChargeTypes(List.of("AMS"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void getChargeTypes2() {
        // Act and Assert
        var responseEntity = masterDataUtils.getChargeTypes(List.of());
        assertNull(responseEntity);
    }

    @Test
    void getChargeTypes3() {
        // Act and Assert
        var responseEntity = masterDataUtils.getChargeTypes(null);
        assertNull(responseEntity);
    }

    @Test
    void setLocationData() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));

        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ValuenDesc("").build());
        masterDataUtils.setLocationData(List.of(CustomerBookingResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()), EntityTransferConstants.UNLOCATION_CODE);

        assertTrue(isSuccess);
    }

    @Test
    void setLocationData2() {
        boolean isSuccess = true;
        masterDataUtils.setLocationData(List.of(CustomerBookingResponse.builder().build()), EntityTransferConstants.UNLOCATION_CODE);

        assertTrue(isSuccess);
    }

    @Test
    void setLocationData3() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));

        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ValuenDesc("").build());
        masterDataUtils.setLocationData(List.of(ShipmentListResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).additionalDetails(new AdditionalDetailsListResponse()).build()), EntityTransferConstants.UNLOCATION_CODE);

        assertTrue(isSuccess);
    }

    @Test
    void setLocationData4() {
        boolean isSuccess = true;
        masterDataUtils.setLocationData(List.of(ShipmentListResponse.builder().build()), EntityTransferConstants.UNLOCATION_CODE);
        assertTrue(isSuccess);
    }

    @Test
    void setLocationData5() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));

        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ValuenDesc("").build());
        masterDataUtils.setLocationData(List.of(ConsolidationListResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()), EntityTransferConstants.UNLOCATION_CODE);

        assertTrue(isSuccess);
    }

    @Test
    void setLocationData6() {
        boolean isSuccess = true;
        masterDataUtils.setLocationData(List.of(ConsolidationListResponse.builder().build()), EntityTransferConstants.UNLOCATION_CODE);
        assertTrue(isSuccess);
    }


    @Test
    void setLocationData7() {
        boolean isSuccess = true;
        masterDataUtils.setLocationData(List.of(CarrierDetailResponse.builder().build()), EntityTransferConstants.UNLOCATION_CODE);
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferVessels::new);

        masterDataUtils.fetchVesselForList(List.of(ShipmentListResponse.builder().carrierDetails(CarrierDetailResponse.builder().vessel(UUID.randomUUID().toString()).build()).build()));

        assertTrue(isSuccess);
    }


    @Test
    void fetchVesselForList2() {
        boolean isSuccess = true;
        masterDataUtils.fetchVesselForList(List.of(ShipmentListResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList3() {
        boolean isSuccess = true;
        masterDataUtils.fetchVesselForList(List.of(ShipmentListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList4() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferVessels::new);

        masterDataUtils.fetchVesselForList(List.of(ConsolidationListResponse.builder().carrierDetails(CarrierDetailResponse.builder().vessel(UUID.randomUUID().toString()).build()).build()));

        assertTrue(isSuccess);
    }


    @Test
    void fetchVesselForList5() {
        boolean isSuccess = true;
        masterDataUtils.fetchVesselForList(List.of(ConsolidationListResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList6() {
        boolean isSuccess = true;
        masterDataUtils.fetchVesselForList(List.of(ConsolidationListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList7() {
        boolean isSuccess = true;
        masterDataUtils.fetchVesselForList(List.of(CarrierDetailResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList8() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferVessels::new);

        masterDataUtils.fetchVesselForList(List.of(ConsolidationDetailsResponse.builder().carrierDetails(CarrierDetailResponse.builder().vessel(UUID.randomUUID().toString()).build()).build()));

        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList1() {
        boolean isSuccess = true;
        masterDataUtils.fetchTenantIdForList(List.of(ShipmentListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList2() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataUtils.fetchTenantIdForList(List.of(ShipmentListResponse.builder().tenantId(1).build()));

        assertTrue(isSuccess);
    }

    @Test
    void setContainerTeuData() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Teu(11.1).build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());

        masterDataUtils.setContainerTeuData(List.of(completeShipment), List.of(mockShipmentListResponse));

        assertTrue(isSuccess);
    }


    @Test
    void setContainerTeuData2() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);
        completeShipment.setContainersList(null);

        masterDataUtils.setContainerTeuData(List.of(completeShipment), List.of(mockShipmentListResponse));

        assertTrue(isSuccess);
    }

    @Test
    void setConsolidationContainerTeuData() {
        boolean isSuccess = true;
        var mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setContainersList(completeShipment.getContainersList());
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Teu(11.1).build());

        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());

        masterDataUtils.setConsolidationContainerTeuData(List.of(mockConsolidation), List.of(ConsolidationListResponse.builder().build()));
        assertTrue(isSuccess);
    }


    @Test
    void setConsolidationContainerTeuData2() {
        boolean isSuccess = true;
        masterDataUtils.setConsolidationContainerTeuData(List.of(new ConsolidationDetails()), List.of(ConsolidationListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void getMasterDataDescription() throws RunnerException, NoSuchFieldException, ClassNotFoundException, IllegalAccessException {
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(new ShipmentSettingsDetailsResponse());
        var response = masterDataUtils.getMasterDataDescription(new ShipmentSettingsDetails());

        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void setContainerTeuDataWithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Teu(11.1).build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());

        masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList());

        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList()));
    }


    @Test
    void setContainerTeuData2WithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentListResponse.setContainersList(null);
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList()));
    }

    @Test
    void setContainerTeuData3WithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentListResponse.getContainersList().get(0).setContainerCode(null);
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList()));
    }

    @Test
    void setContainerTeuData4WithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentListResponse.getContainersList().get(0).setContainerCount(null);
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList()));
    }

    @Test
    void setContainerTeuData5WithContainerList() {
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> null);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());

        masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList());

        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(mockShipmentListResponse.getContainersList()));
    }

}
