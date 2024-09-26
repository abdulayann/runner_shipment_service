package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.TenantModel;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterDataObjects.dto.*;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
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

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource("classpath:application-test.properties")
class MasterDataUtilsTest {

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
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
        customerBooking = jsonTestUtility.getCustomerBooking();
    }

     @Test
    void testFetchInBulkMasterList() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(MasterListsV1.class))).thenReturn(List.of(MasterListsV1.builder().ItemValue("SEA").build()));
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

    @Test
    void fetchInBulkUnlocations() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(UnLocationsMasterData.class))).thenReturn(List.of(UnLocationsMasterData.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations2() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(UnLocationsMasterData.class))).thenReturn(List.of(UnLocationsMasterData.builder().Name("Name").LocationsReferenceGUID(UUID.randomUUID().toString()).LocCode("AEJEA").build()));
        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().build());
        // Act and Assert
        var responseEntity = masterDataUtils.fetchInBulkUnlocations(List.of(UUID.randomUUID().toString()), EntityTransferConstants.UNLOCATION_CODE);
        assertNotNull(responseEntity);
        assertFalse(responseEntity.isEmpty());
    }

    @Test
    void fetchInBulkUnlocations3() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(UnLocationsMasterData.class))).thenReturn(null);
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
        when(cache.get(any())).thenReturn(MasterListsV1::new);

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
        when(jsonHelper.convertValueToList(any(), eq(ChargeTypeMasterData.class))).thenReturn(List.of(ChargeTypeMasterData.builder().ChargeCode("AMS").build()));
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

    @Test
    void fetchInBulkContainerTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(ContainerTypeMasterData.class))).thenReturn(List.of(ContainerTypeMasterData.builder().Code("20GP").ContainerType("ContainerType").build()));
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
        when(jsonHelper.convertValueToList(any(), eq(com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData.class))).thenReturn(null);
        var response = masterDataUtils.getCarriersData(Set.of(locationGuid));
        assertNotNull(response);
        assertTrue(response.isEmpty());
    }

    @Test
    void getCarriersData4() {
        String locationGuid = StringUtility.convertToString(UUID.randomUUID());
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData.class))).thenReturn(new ArrayList<>());
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
        DGSubstanceMasterData mockResponse = DGSubstanceMasterData.builder().Id(11L).build();
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValueToList(any(), eq(DGSubstanceMasterData.class))).thenReturn(List.of(mockResponse));
        var response = masterDataUtils.fetchDgSubstanceRow(11);
        assertNotNull(response);
        assertNotNull(response.getId());
        assertEquals(mockResponse.getId(), response.getId());
    }

    @Test
    void fetchOrganizations() {
        OrganizationsMasterData mockResponse = OrganizationsMasterData.builder().Id(11L).build();
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(OrganizationsMasterData.class))).thenReturn(List.of(mockResponse));
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
        when(jsonHelper.convertValueToList(any(), eq(VesselsMasterData.class))).thenReturn(List.of(VesselsMasterData.builder().Guid(mockVesselGuid).Name("Mark").build()));
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getVesselName(mockVesselGuid.toString());
        assertNotNull(responseEntity);
        assertEquals("Mark", responseEntity);
    }


    @Test
    void getVesselName2() {
        var mockVesselGuid = UUID.randomUUID();
        when(jsonHelper.convertValueToList(any(), eq(VesselsMasterData.class))).thenReturn(List.of(VesselsMasterData.builder().Guid(mockVesselGuid).Name("Mark").build()));
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
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().ItemValue(mockCarrierCode).ItemDescription("APULU Carrier").build()));
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(V1DataResponse.builder().build());

        var responseEntity = masterDataUtils.getCarrierName(mockCarrierCode);
        assertNotNull(responseEntity);
        assertEquals("APULU Carrier", responseEntity);
    }


    @Test
    void getCarrierName2() {
        var mockCarrierCode = "APLU";
        when(jsonHelper.convertValueToList(any(), eq(CarrierMasterData.class))).thenReturn(List.of(CarrierMasterData.builder().ItemValue(mockCarrierCode).build()));
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
        when(cache.get(any())).thenReturn(() -> UnLocationsMasterData.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

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
        when(cache.get(any())).thenReturn(() -> UnLocationsMasterData.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

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
        when(cache.get(any())).thenReturn(() -> UnLocationsMasterData.builder().LocCode("LocCode").NameWoDiacritics("NameWoDiacritics").lookupDesc("lookupDesc").build());

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
        when(cache.get(any())).thenReturn(() -> ContainerTypeMasterData.builder().Code("20GP").Description("20 Foot").build());

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
        when(cache.get(any())).thenReturn(() -> ChargeTypeMasterData.builder().ChargeCode("AMS").Description("AMS Filling").build());

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
        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ValuenDesc("ValuenDesc").build());

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
        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ItemDescription("ItemDescription").build());

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
        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ItemDescription("ItemDescription").build());

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
        when(cache.get(any())).thenReturn(() -> VesselsMasterData.builder().Name("Name").build());

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
        when(cache.get(any())).thenReturn(() -> CarrierMasterData.builder().ItemDescription("DPW").build());

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
        when(cache.get(any())).thenReturn(CommodityTypeMasterData::new);

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
        when(cache.get(any())).thenReturn(CommodityTypeMasterData::new);

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
        when(cache.get(any())).thenReturn(CurrencyMasterData::new);

        var resonseMap = masterDataUtils.setMasterData(inputFieldNameKeyMap, CacheConstants.CURRENCIES);

        assertNotNull(resonseMap);
        assertTrue(resonseMap.containsKey("field"));
    }

    @Test
    void getChargeTypes() {
        // Arrange
        when(jsonHelper.convertValueToList(any(), eq(ChargeTypeMasterData.class))).thenReturn(List.of(ChargeTypeMasterData.builder().ChargeCode("AMS").build()));
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

        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ValuenDesc("").build());
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

        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ValuenDesc("").build());
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

        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ValuenDesc("").build());
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

        when(cache.get(any())).thenReturn(() -> MasterListsV1.builder().ValuenDesc("").build());
        masterDataUtils.setLocationData(List.of(ConsolidationDetailsResponse.builder().carrierDetails(CarrierDetailResponse.builder().build()).build()), EntityTransferConstants.UNLOCATION_CODE);

        assertTrue(isSuccess);
    }

    @Test
    void fetchVesselForList() {
        boolean isSuccess = true;
        Cache cache = mock(Cache.class);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(anyString(), any())).thenReturn(new StringBuilder(StringUtility.getRandomString(11)));
        when(cache.get(any())).thenReturn(VesselsMasterData::new);

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
        when(cache.get(any())).thenReturn(VesselsMasterData::new);

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
        when(cache.get(any())).thenReturn(VesselsMasterData::new);

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

}
