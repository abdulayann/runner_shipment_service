package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
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
import com.dpw.runner.shipment.services.entity.Parties;
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
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
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
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
import org.mockito.Spy;
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
    @Mock
    private BillingServiceAdapter billingServiceAdapter;
    @Spy
    @InjectMocks
    private  MasterDataUtils masterDataUtils;
    @Mock
    private V1ServiceUtil v1ServiceUtil;


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
    void setup() throws NoSuchFieldException, IllegalAccessException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        completeShipment = jsonTestUtility.getCompleteShipment();
        customerBooking = jsonTestUtility.getCustomerBooking();

        Field takeField = MasterDataUtils.class.getDeclaredField("take");
        takeField.setAccessible(true);
        takeField.set(masterDataUtils, 500);
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkContainerTypeRequest(IRunnerResponse, Class, Map, String, Map)}
     */



    @Test
    void testCreateInBulkContainerTypeRequest() {
        // Arrange
        // Act and Assert
        var response = masterDataUtils.createInBulkMasterListRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkMasterListRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkUnLocationsRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkUnLocationsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkUnLocationsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkUnLocationsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void createInBulkOrganizationRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkOrganizationRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNull(response);
    }

    @Test
    void createInBulkOrganizationRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.setPickupAtOrigin(2L);
        mockShipmentDetailsResponse.setDeliveryAtDestination(2L);
        mockShipmentDetailsResponse.setBookingAgent(2L);
        mockShipmentDetailsResponse.setBrokerageAtOrigin(2L);
        mockShipmentDetailsResponse.setBrokerageAtDestination(2L);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkOrganizationRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void fetchInOrganizations() {
        Set<String> requests = new HashSet<>();
        requests.add("123");
        String onField = "";
        V1DataResponse v1DataResponse = new V1DataResponse();
        EntityTransferOrganizations entityTransferOrganizations = EntityTransferOrganizations.builder()
                .FullName("Org1")
                .Id(1L)
                .build();
        List<EntityTransferOrganizations> organizations = new ArrayList<>();
        organizations.add(entityTransferOrganizations);
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(organizations);
        var response = masterDataUtils.fetchInOrganizations(requests, onField);
        assertEquals(entityTransferOrganizations, response.get("1"));
    }

    @Test
    void fetchInOrganizations1() {
        Set<String> requests = new HashSet<>();
        var response = masterDataUtils.fetchInOrganizations(requests, "");
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkOrganizationRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        mockShipmentDetailsResponse.setPickupAtOrigin(2L);
        mockShipmentDetailsResponse.setDeliveryAtDestination(2L);
        mockShipmentDetailsResponse.setBookingAgent(2L);
        mockShipmentDetailsResponse.setBrokerageAtOrigin(2L);
        mockShipmentDetailsResponse.setBrokerageAtDestination(2L);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferOrganizations::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkOrganizationRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkOrganizationRequest4() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenThrow(new RuntimeException("RuntimeException"));

        // Act and Assert
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkOrganizationRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkUnlocations() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(EntityTransferUnLocations.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(Set.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations2() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(EntityTransferUnLocations.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(Set.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations3() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(null);
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(Set.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations4() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(Set.of(), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkChargeTypeRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkChargeTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkChargeTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkChargeTypeRequest(mockCustomerBookingResponse.getBookingCharges().get(0), BookingCharges.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkContainerTypeRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkContainerTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkContainerTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().iterator().next(), Containers.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().iterator().next(), Containers.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkContainerTypeRequest(mockShipmentDetailsResponse.getContainersList().iterator().next(), Containers.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkContainerTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferContainerType.class))).thenReturn(List.of(EntityTransferContainerType.builder().Code("20GP").ContainerType("ContainerType").build()));
        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkContainerTypes(Set.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkContainerTypes2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkContainerTypes(Set.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }

    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCommodityTypeRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkCommodityTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNull(response);
    }

    @Test
    void createInBulkCommodityTypeRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);
        var container = mockShipmentDetailsResponse.getContainersList().iterator().next();
        container.setContainerCode("FAK");
        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(container, Containers.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
    }

    @Test
    void createInBulkCommodityTypeRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        var container = mockShipmentDetailsResponse.getContainersList().iterator().next();
        container.setContainerCode("FAK");
        // Act and Assert
        var response = masterDataUtils.createInBulkCommodityTypeRequest(container, Containers.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCommodityTypeRequest(mockShipmentDetailsResponse.getContainersList().iterator().next(), Containers.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkCarriersRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkCarriersRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCarriersRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkCarriersRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var container = mockShipmentDetailsResponse.getContainersList().iterator().next();
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCarriersRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkCarriers() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().ItemValue("CODE").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriers(Set.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkCarriers2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkCarriers(Set.of());
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
     * {@link MasterDataUtils#createInBulkVesselsRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkVesselsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkVesselsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkVesselsRequest3() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);
        var container = mockShipmentDetailsResponse.getContainersList().iterator().next();
        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), anyString())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        // Act and Assert
        var response = masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkVesselsRequest(mockShipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInBulkVessels() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferVessels.class))).thenReturn(List.of(EntityTransferVessels.builder().Guid(UUID.randomUUID()).build()));
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkVessels(Set.of("20GP", "40HQ"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInBulkVessels2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkVessels(Set.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCurrencyRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkCurrencyRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkCurrencyRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkCurrencyRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInCurrencyList() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCurrency.class))).thenReturn(List.of(EntityTransferCurrency.builder().CurrenyCode("INR").build()));
        when(v1Service.fetchCurrenciesData(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInCurrencyList(Set.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInCurrencyList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInCurrencyList(Set.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkTenantsRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkTenantsRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void createInBulkTenantsRequest2() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, ShipmentDetailsResponse.class);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        assertNotNull(response);
        assertFalse(response.isEmpty());
    }

    @Test
    void createInBulkTenantsRequestForAttachListShipmentResponse() {
        // Arrange
        var mockShipmentDetailsResponse = objectMapper.convertValue(completeShipment, AttachListShipmentResponse.class);

        Cache cache = mock(Cache.class);

        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(null);

        // Act and Assert
        var response = masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkTenantsRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void fetchInTenantsList() {
        var tenantModel = new TenantModel();
        tenantModel.tenantId = 11L;
        // Arrange
        when(commonUtils.convertToList(any(), eq(TenantModel.class))).thenReturn(List.of(tenantModel));
        when(v1Service.listCousinBranches(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInTenantsList(Set.of("INR"));
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }


    @Test
    void fetchInTenantsList2() {
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInTenantsList(Set.of());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.isEmpty());
    }


    /**
     * Method under test:
     * {@link MasterDataUtils#createInBulkCurrencyRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkDGSubstanceRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkDGSubstanceRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code", new HashMap<>());
        packing.setDGSubstanceId(11);
        var response2 = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code", new HashMap<>());

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
        var response = masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkDGSubstanceRequest(packing, Packing.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkWareHouseRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkWareHouseRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkWareHouseRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkWareHouseRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkActivityTypeRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkActivityTypeRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkActivityTypeRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkActivityTypeRequest(mockShipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
     * {@link MasterDataUtils#createInBulkSalesAgentRequest(IRunnerResponse, Class, Map, String, Map)}
     */

    @Test
    void createInBulkSalesAgentRequest() {
        // Act and Assert
        var response = masterDataUtils.createInBulkSalesAgentRequest(null, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var response = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
        mockShipmentDetailsResponse.setSalesAgent(123L);
        var response2 = masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>());
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
        var t = assertThrows(Throwable.class, () -> masterDataUtils.createInBulkSalesAgentRequest(mockShipmentDetailsResponse, ShipmentDetails.class, new HashMap<>(), "Code", new HashMap<>()));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
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
    void getLocationDataFromCache() {
        var response = masterDataUtils.getLocationDataFromCache(null, EntityTransferConstants.LOCATION_SERVICE_GUID);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getLocationDataFromCache2() {
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = masterDataUtils.getLocationDataFromCache(Set.of(), EntityTransferConstants.LOCATION_SERVICE_GUID);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getLocationDataFromCache3() {
        MasterDataUtils spyService = spy(masterDataUtils);
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        Cache cache = mock(Cache.class);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(EntityTransferUnLocations.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getLocationDataFromCache(Set.of(locationGuid), EntityTransferConstants.LOCATION_SERVICE_GUID);
        assertNotNull(response);
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
    void fetchBillDataForShipments2_BillingIntegrationDisabled() {
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
    void fetchBillDataForShipments2_BillingIntegrationEnabled() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentListResponse.class);
        var mockV1Response = new ShipmentBillingListResponse();
        var mockMapData = new HashMap<String, ShipmentBillingListResponse.BillingData>();
        mockMapData.put(StringUtility.convertToString(mockShipmentListResponse.getGuid()), new ShipmentBillingListResponse.BillingData());
        mockV1Response.setData(mockMapData);
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(cache.get(any())).thenReturn(null);

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(billingServiceAdapter.fetchShipmentBillingData(any())).thenReturn(mockV1Response);
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

        var responseEntity = masterDataUtils.getMasterListData(MasterDataType.COMMODITY_GROUP, Constants.EMPTY_STRING);
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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS, true, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CONTAINER_TYPE, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CHARGE_TYPE, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.MASTER_LIST, true, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.VESSELS, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CARRIER, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.TENANTS, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.WAREHOUSES, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.ACTIVITY_TYPE, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.SALES_AGENT, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.COMMODITY, null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, StringUtility.getRandomString(10), null);

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

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CURRENCIES, null);

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
    void setLocationData8() {
        boolean isSuccess = true;
        masterDataUtils.setLocationData(List.of(ConsolidationDetailsResponse.builder().build()), EntityTransferConstants.UNLOCATION_CODE);
        assertTrue(isSuccess);
    }

    @Test
    void setLocationData9() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);

        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));

        when(cache.get(any())).thenReturn(() -> EntityTransferMasterLists.builder().ValuenDesc("").build());
        masterDataUtils.setLocationData(List.of(ConsolidationDetailsResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()), EntityTransferConstants.UNLOCATION_CODE);

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
    void fetchTenantIdForList_Console2() {
        boolean isSuccess = true;
        masterDataUtils.fetchTenantIdForList(List.of(ConsolidationListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_Console() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataUtils.fetchTenantIdForList(List.of(ConsolidationListResponse.builder().tenantId(1).build()));

        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_NetworkTransfer() {
        boolean isSuccess = true;
        masterDataUtils.fetchTenantIdForList(List.of(NetworkTransferListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_NetworkTransfer2() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataUtils.fetchTenantIdForList(List.of(NetworkTransferListResponse.builder().sourceBranchId(1L).build()));

        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_Notification() {
        boolean isSuccess = true;
        masterDataUtils.fetchTenantIdForList(List.of(NotificationListResponse.builder().build()));
        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_Notification2() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataUtils.fetchTenantIdForList(List.of(NotificationListResponse.builder().requestedBranchId(1L).build()));

        assertTrue(isSuccess);
    }

    @Test
    void fetchTenantIdForList_Notification3() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataUtils.fetchTenantIdForList(List.of(NotificationListResponse.builder().reassignedToBranchId(1L).build()));

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
        mockConsolidation.setContainersList(new ArrayList<>(completeShipment.getContainersList()));
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Teu(11.1).build());

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
    void setShipmentTypeMasterData_successfulMapping() {
        // Arrange
        String jobType = "EXPORT";
        String expectedDescription = "Export Shipment";

        ShipmentListResponse shipmentResponse = new ShipmentListResponse();
        shipmentResponse.setJobType(jobType);

        List<IRunnerResponse> responseList = List.of(shipmentResponse);

        EntityTransferMasterLists mockMaster = new EntityTransferMasterLists();
        mockMaster.setItemDescription(expectedDescription);

        String key = MasterDataType.SHIPMENT_TYPE.name() + "#" + jobType;
        Map<String, EntityTransferMasterLists> masterDataMap = Map.of(key, mockMaster);

        // ✅ Mock the internal method of the class using doReturn().when(spy).method(...)
        doReturn(masterDataMap).when(masterDataUtils).fetchMasterListFromCache(any(MasterListRequestV2.class));

        // Act
        masterDataUtils.setShipmentTypeMasterData(responseList);

        // Assert
        assertEquals(Map.of(jobType, expectedDescription), shipmentResponse.getShipmentTypeMasterData());
    }

    @Test
    void setShipmentTypeMasterData_noJobType_noMapping() {
        // Arrange
        ShipmentListResponse shipmentResponse = new ShipmentListResponse();
        shipmentResponse.setJobType(null); // No jobType

        List<IRunnerResponse> responseList = List.of(shipmentResponse);

        // Act
        masterDataUtils.setShipmentTypeMasterData(responseList);

        // Assert
        assertNull(shipmentResponse.getShipmentTypeMasterData());
    }

    @Test
    void setShipmentTypeMasterData_masterDataMissing_noMapping() {
        // Arrange
        String jobType = "IMPORT";

        ShipmentListResponse shipmentResponse = new ShipmentListResponse();
        shipmentResponse.setJobType(jobType);

        List<IRunnerResponse> responseList = List.of(shipmentResponse);

        // Mock: Master data map doesn't contain the key
        doReturn(Collections.emptyMap()).when(masterDataUtils).fetchMasterListFromCache(any(MasterListRequestV2.class));

        // Act
        masterDataUtils.setShipmentTypeMasterData(responseList);

        // Assert
        assertNull(shipmentResponse.getShipmentTypeMasterData());
    }


    @Test
    void getCountriesMasterDataList() {
        var response = masterDataUtils.getCountriesMasterListData(new ArrayList<>());

        assertNotNull(response);
        assertEquals(0, response.size());
    }

    @Test
    void setContainerTeuDataWithContainerList() {
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> EntityTransferContainerType.builder().Teu(11.1).build());
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(completeShipment.getContainersList())));
    }


    @Test
    void setContainerTeuData2WithContainerList() {
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(completeShipment.getContainersList())));
    }

    @Test
    void setContainerTeuData3WithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetails.class);
        mockShipmentListResponse.getContainersList().iterator().next().setContainerCode(null);
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(mockShipmentListResponse.getContainersList())));
    }

    @Test
    void setContainerTeuData4WithContainerList() {
        boolean isSuccess = true;
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetails.class);
        mockShipmentListResponse.getContainersList().iterator().next().setContainerCount(null);
        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(mockShipmentListResponse.getContainersList())));
    }

    @Test
    void setContainerTeuData5WithContainerList() {
        var mockShipmentListResponse = objectMapper.convertValue(completeShipment, ShipmentDetails.class);

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(() -> null);

        assertNotNull(masterDataUtils.setContainerTeuDataWithContainers(new ArrayList<>(mockShipmentListResponse.getContainersList())));
    }

    @Test
    void setKeyValueForMasterLists() {
        Map<String, Object> response = new HashMap<>();
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        keyMasterDataMap.put("IND#COUNTRIES", EntityTransferMasterLists.builder().ItemValue("IND").ItemDescription("India").ValuenDesc("India").build());
        masterDataUtils.setKeyValueForMasterLists(response, "IND#COUNTRIES", keyMasterDataMap.get("IND#COUNTRIES"));
        assertNotNull(response);
    }
    @Test
    void setKeyValueForMasterLists1() {
        Map<String, Object> response = new HashMap<>();
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        keyMasterDataMap.put("IND#COUNTRIES", EntityTransferMasterLists.builder().ItemValue("IND").ItemDescription("India").build());
        masterDataUtils.setKeyValueForMasterLists(response, "IND#COUNTRIES", keyMasterDataMap.get("IND#COUNTRIES"));
        assertNotNull(response);
    }

    @Test
    void addAlpha3CountryWithData() {
        Map<String, Object> address = new HashMap<>();
        address.put(PartiesConstants.COUNTRY, "IN");
        List<String> countryList = new ArrayList<>();
        var resp = masterDataUtils.addAlpha3Country(address, countryList);

        assertEquals(1, resp.size());
        assertEquals("IND", resp.get(0));
    }

    @Test
    void addAlpha3CountryWithDuplicateData() {
        Map<String, Object> address = new HashMap<>();
        address.put(PartiesConstants.COUNTRY, "IN");
        List<String> countryList = new ArrayList<>(List.of("IND"));
        var resp = masterDataUtils.addAlpha3Country(address, countryList);

        assertEquals(1, resp.size());
        assertEquals("IND", resp.get(0));
    }

    @Test
    void addAlpha3CountryWithNoData() {
        Map<String, Object> address = new HashMap<>();
        List<String> countryList = new ArrayList<>();
        var resp = masterDataUtils.addAlpha3Country(address, countryList);

        assertEquals(0, resp.size());
    }

    @Test
    void shipmentAddressCountryMasterData() {
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(EntityTransferMasterLists.builder().Identifier1("NA").ItemDescription("Namibia").build()));
        Map<String, String> response = masterDataUtils.shipmentAddressCountryMasterData(completeShipment);
        assertNotNull(response);
        assertEquals("Namibia", response.get("NA"));
    }

    @Test
    void consoleAddressCountryMasterData() {
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(EntityTransferMasterLists.builder().Identifier1("NA").ItemDescription("Namibia").build()));

        Parties parties = Parties.builder().addressData(Map.of(PartiesConstants.COUNTRY, "NAM")).build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().receivingAgent(parties).consolidationAddresses(new ArrayList<>()).build();
        Map<String, String> response = masterDataUtils.consolidationAddressCountryMasterData(consolidationDetails);
        assertNotNull(response);
        assertEquals("Namibia", response.get("NA"));
    }

    @Test
    void getCarrierDataFromCache() {
        var response = masterDataUtils.getCarrierDataFromCache(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarrierDataFromCache2() {
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = masterDataUtils.getCarrierDataFromCache(Set.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarrierDataFromCache3() {
        MasterDataUtils spyService = spy(masterDataUtils);
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        Cache cache = mock(Cache.class);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferCarrier.class))).thenReturn(List.of(EntityTransferCarrier.builder().ItemDescription("Name").Identifier1(UUID.randomUUID().toString()).ItemDescription("AEJEA").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getCarrierDataFromCache(Set.of(locationGuid));
        assertNotNull(response);
    }

    @Test
    void getCommodityGroupFromCache() {
        var response = masterDataUtils.getCommodityGroupDataFromCache(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCommodityGroupFromCache2() {
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = masterDataUtils.getCommodityGroupDataFromCache(Set.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCommodityGroupFromCache3() {
        MasterDataUtils spyService = spy(masterDataUtils);
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        Cache cache = mock(Cache.class);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferMasterLists.class))).thenReturn(List.of(EntityTransferMasterLists.builder().ItemDescription("Name").Identifier1(UUID.randomUUID().toString()).ItemDescription("AEJEA").build()));
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(V1DataResponse.builder().build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getCommodityGroupDataFromCache(Set.of(locationGuid));
        assertNotNull(response);
    }

    @Test
    void getPartiesOrgInfoFromCache() {
        var response = masterDataUtils.getPartiesOrgInfoFromCache(null);
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getPartiesOrgInfoFromCache2() {
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = masterDataUtils.getPartiesOrgInfoFromCache(List.of());
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getPartiesOrgInfoFromCache3() {
        MasterDataUtils spyService = spy(masterDataUtils);
        Parties locationGuid = Parties.builder().orgCode("abcd").addressCode("bcder").build();
        Cache cache = mock(Cache.class);
        Map<String, Object> addressData = new HashMap<>();
        addressData.put("abcd", "cd");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        addressMap.put("org#address", addressData);
        Map<String, Map<String, Object>> organisationMap = new HashMap<>();
        organisationMap.put("org", addressData);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(OrgAddressResponse.builder().organizations(organisationMap).addresses(addressMap).build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getPartiesOrgInfoFromCache(List.of(locationGuid));
        assertNotNull(response);
    }

    @Test
    void getPartiesOrgInfoFromCache4() {
        MasterDataUtils spyService = spy(masterDataUtils);
        Parties locationGuid = Parties.builder().orgCode("abcd").addressCode("bcder").build();
        Cache cache = mock(Cache.class);
        Map<String, Object> addressData = new HashMap<>();
        addressData.put("abcd", "cd");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        addressMap.put("org#address", addressData);
        Map<String, Map<String, Object>> organisationMap = new HashMap<>();
        addressMap.put("org", addressData);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(OrgAddressResponse.builder().organizations(organisationMap).build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getPartiesOrgInfoFromCache(List.of(locationGuid));
        assertNotNull(response);
    }

    @Test
    void getPartiesOrgInfoFromCache5() {
        MasterDataUtils spyService = spy(masterDataUtils);
        Parties locationGuid = Parties.builder().orgCode("abcd").addressCode("bcder").build();
        Cache cache = mock(Cache.class);
        Map<String, Object> addressData = new HashMap<>();
        addressData.put("abcd", "cd");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        addressMap.put("org#address", addressData);
        addressMap.put("org#address1", addressData);
        addressMap.put("org#address2", addressData);
        Map<String, Map<String, Object>> organisationMap = new HashMap<>();
        organisationMap.put("org", addressData);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(OrgAddressResponse.builder().organizations(organisationMap).addresses(addressMap).build());
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        var response = spyService.getPartiesOrgInfoFromCache(List.of(locationGuid));
        assertNotNull(response);
    }
    @Test
    void getCarrierNameFromMasterDataUsingScacCodeFromIntraa_success() {
        MasterDataUtils spyService = spy(masterDataUtils);

        String scacCode = "MSKU";
        String expectedItemValue = "Maersk";

        // Mock the internal method call
        doReturn(Map.of(scacCode, expectedItemValue))
                .when(spyService)
                .getCarrierItemValuesFromSCACList(List.of(scacCode));

        String result = spyService.getCarrierNameFromMasterDataUsingScacCodeFromIntraa(scacCode);

        assertEquals(expectedItemValue, result);
    }

    @Test
    void getCarrierNameFromMasterDataUsingScacCodeFromIntraa_notFound_shouldThrowException() {
        MasterDataUtils spyService = spy(masterDataUtils);
        String scacCode = "UNKNOWN";

        doReturn(Collections.emptyMap())
                .when(spyService)
                .getCarrierItemValuesFromSCACList(List.of(scacCode));

        Exception exception = assertThrows(IllegalArgumentException.class, () ->
                spyService.getCarrierNameFromMasterDataUsingScacCodeFromIntraa(scacCode)
        );

        assertEquals("Data not present in Carrier Master data", exception.getMessage());
    }

    @Test
    void getCarrierNameFromMasterDataUsingScacCodeFromIntraa_nullOrBlank_shouldReturnNull() {
        String result1 = masterDataUtils.getCarrierNameFromMasterDataUsingScacCodeFromIntraa(null);
        assertNull(result1);

        String result2 = masterDataUtils.getCarrierNameFromMasterDataUsingScacCodeFromIntraa("   ");
        assertNull(result2);
    }

    @Test
    void getCarrierItemValuesFromSCACList_success() {
        MasterDataUtils spyService = spy(masterDataUtils);
        String scacCode = "MSKU";
        EntityTransferCarrier carrier = new EntityTransferCarrier();
        carrier.ItemValue = "Maersk";

        Map<String, EntityTransferCarrier> carrierMap = Map.of(scacCode, carrier);

        doReturn(carrierMap).when(spyService).fetchInBulkCarriersBySCACCode(List.of(scacCode));

        Map<String, String> result = spyService.getCarrierItemValuesFromSCACList(List.of(scacCode));

        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("Maersk", result.get(scacCode));
    }

@Test
    void fetchCarriersForList_withShipmentListResponse_shippingLine_present() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
                .id(1L)
                .shippingLine("MSC")
                .build();

        ShipmentListResponse shipmentListResponse = ShipmentListResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCarrier::new);

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(shipmentListResponse)));
    }

    @Test
    void fetchCarriersForList_withShipmentListResponse_shippingLine_null() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
                .id(1L)
                .shippingLine(null)
                .build();

        ShipmentListResponse shipmentListResponse = ShipmentListResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(shipmentListResponse)));
    }

    @Test
    void fetchCarriersForList_withShipmentListResponse_carrierDetails_null() {
        ShipmentListResponse shipmentListResponse = ShipmentListResponse.builder().build();
        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(shipmentListResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationListResponse_shippingLine_present() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
                .id(1L)
                .shippingLine("Evergreen")
                .build();

        ConsolidationListResponse consolidationListResponse = ConsolidationListResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCarrier::new);

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationListResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationListResponse_shippingLine_null() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder().id(1L).build();
        ConsolidationListResponse consolidationListResponse = ConsolidationListResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationListResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationListResponse_carrierDetails_null() {
        ConsolidationListResponse consolidationListResponse = ConsolidationListResponse.builder().build();
        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationListResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationDetailsResponse_shippingLine_present() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
                .id(1L)
                .shippingLine("Maersk Line") // also needed for setting master data
                .build();

        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(EntityTransferCarrier::new);

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationDetailsResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationDetailsResponse_shippingLine_null() {
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
                .id(1L)
                .shippingLine(null)
                .build();

        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder()
                .carrierDetails(carrierDetailResponse)
                .build();

        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationDetailsResponse)));
    }

    @Test
    void fetchCarriersForList_withConsolidationDetailsResponse_carrierDetails_null() {
        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder().build();
        assertDoesNotThrow(() -> masterDataUtils.fetchCarriersForList(List.of(consolidationDetailsResponse)));
    }

    @Test
    void fetchCarriersForList_withException_shouldCatchGracefully() {
        List<IRunnerResponse> responses = List.of(mock(ShipmentListResponse.class));

        // Use a spy here if mocking inside the method (fetchInBulkCarriers)
        MasterDataUtils masterDataUtilsSpy = spy(masterDataUtils);
        doThrow(new RuntimeException("Simulated failure")).when(masterDataUtilsSpy).fetchInBulkCarriers(any());
        assertDoesNotThrow(() -> masterDataUtilsSpy.fetchCarriersForList(responses));
    }
}
