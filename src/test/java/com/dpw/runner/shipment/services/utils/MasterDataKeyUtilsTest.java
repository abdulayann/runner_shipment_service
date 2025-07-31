package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
@Execution(CONCURRENT)
class MasterDataKeyUtilsTest {

    @Mock
    CacheManager cacheManager;

    @Mock
    CustomKeyGenerator keyGenerator;

    @InjectMocks
    MasterDataKeyUtils masterDataKeyUtils;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testSetMasterDataValue_UnlocationsAWB() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferUnLocations::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.UNLOCATIONS));
    }

    @Test
    void testSetMasterDataValue_Unlocations() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferUnLocations::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.UNLOCATIONS));
    }

    @Test
    void testSetMasterDataValue_CONTAINER_TYPE() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferContainerType::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.CONTAINER_TYPE));
    }

    @Test
    void testSetMasterDataValue_CHARGE_TYPE() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferChargeType::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CHARGE_TYPE, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.CHARGE_TYPE));
    }

    @Test
    void testSetMasterDataValue_VESSELS() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferVessels::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.VESSELS));
    }

    @Test
    void testSetMasterDataValue_CARRIER() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferCarrier::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.CARRIER));
    }

    @Test
    void testSetMasterDataValue_CURRENCIES() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferCurrency::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.CURRENCIES));
    }

    @Test
    void testSetMasterDataValue_TENANTS() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(TenantModel::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.TENANTS));
    }

    @Test
    void testSetMasterDataValue_WAREHOUSES() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(WareHouseResponse::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.WAREHOUSES));
    }

    @Test
    void testSetMasterDataValue_ACTIVITY_TYPE() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(ActivityMasterResponse::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.ACTIVITY_TYPE, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.ACTIVITY_TYPE));
    }

    @Test
    void testSetMasterDataValue_SALES_AGENT() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(SalesAgentResponse::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.SALES_AGENT, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.SALES_AGENT));
    }

    @Test
    void testSetMasterDataValue_COMMODITY() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferCommodityType::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.COMMODITY));
    }

    @Test
    void testSetMasterDataValue_DG_SUBSTANCES() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferDGSubstance::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.DG_SUBSTANCES));
    }

    @Test
    void testSetMasterDataValue_MASTER_LISTS() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, String> valueMap = new HashMap<>();
        valueMap.put("key", "value#value2");
        fieldNameKeyMap.put("field", valueMap);

        Map<String, Object> masterDataResponse = new HashMap<>();
        masterDataResponse.put("value2" , new HashMap<String,String>());
        Cache cache = mock(Cache.class);

        when(keyGenerator.customCacheKeyForMasterData(anyString(),anyString())).thenReturn(new StringBuilder("xyz"));
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(EntityTransferMasterLists::new);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, null);

        assertTrue(masterDataResponse.containsKey(CacheConstants.MASTER_LIST));
    }

    @Test
    void testSetMasterDataValue_EmptyFieldNameKeyMap() {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, Object> masterDataResponse = new HashMap<>();

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, "ANY_TYPE", masterDataResponse, null);

        assertTrue(masterDataResponse.isEmpty());
    }

}
