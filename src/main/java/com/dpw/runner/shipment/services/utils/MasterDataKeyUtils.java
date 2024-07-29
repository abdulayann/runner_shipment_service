package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.commons.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.commons.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.commons.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Slf4j
@Component
public class MasterDataKeyUtils {

    @Autowired
    CacheManager cacheManager;

    @Autowired
    CustomKeyGenerator keyGenerator;

    public void setMasterDataValue(Map<String, Map<String, String>> fieldNameKeyMap, String masterDataType, Map<String, Object> masterDataResponse) {
        Map<String, Object> dataMap = new HashMap<>();
        setKeyValueInResponse(fieldNameKeyMap, masterDataType, dataMap);
        if(!dataMap.isEmpty()){
            if(Objects.equals(masterDataType, CacheConstants.UNLOCATIONS_AWB)) {
                masterDataType = CacheConstants.UNLOCATIONS;
            }
            masterDataResponse.put(masterDataType, dataMap);
        }
    }
    private void setKeyValueInResponse(Map<String, Map<String, String>> fieldNameKeyMap, String masterDataType, Map<String, Object> response) {
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return;
        fieldNameKeyMap.forEach((key1, value1) -> {
            if(value1 != null && !value1.isEmpty()) {
                value1.forEach((key, value) -> {
                    var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(masterDataType.equalsIgnoreCase(CacheConstants.UNLOCATIONS_AWB) ? CacheConstants.UNLOCATIONS : masterDataType, value));
                    if(!Objects.isNull(cache)) {
                        switch (masterDataType) {
                            case CacheConstants.UNLOCATIONS:
                                EntityTransferUnLocations object = (EntityTransferUnLocations) cache.get();
                                response.put(value, object.getLookupDesc());
                                break;
                            case CacheConstants.UNLOCATIONS_AWB:
                                EntityTransferUnLocations obj = (EntityTransferUnLocations) cache.get();
                                response.put(value, obj.NameWoDiacritics);
                                break;
                            case CacheConstants.CONTAINER_TYPE:
                                EntityTransferContainerType object1 = (EntityTransferContainerType) cache.get();
                                response.put(value, String.format("%s - %s", object1.getCode(), object1.getDescription()));
                                break;
                            case CacheConstants.CHARGE_TYPE:
                                EntityTransferChargeType object2 = (EntityTransferChargeType) cache.get();
                                response.put(value, object2.getDescription());
                                break;
                            case CacheConstants.MASTER_LIST:
                                setKeyValueForMasterLists(response, value);
                                break;
                            case CacheConstants.VESSELS:
                                EntityTransferVessels object4 = (EntityTransferVessels) cache.get();
                                response.put(value, object4.getName());
                                break;
                            case CacheConstants.CARRIER:
                                EntityTransferCarrier object5 = (EntityTransferCarrier) cache.get();
                                response.put(value, object5.getItemDescription());
                                break;
                            case CacheConstants.CURRENCIES:
                                EntityTransferCurrency object6 = (EntityTransferCurrency) cache.get();
                                response.put(value, object6.getCurrenyDescription());
                                break;
                            case CacheConstants.TENANTS:
                                TenantModel object7 = (TenantModel) cache.get();
                                response.put(value, object7.tenantName);
                                break;
                            case CacheConstants.WAREHOUSES:
                                WareHouseResponse object8 = (WareHouseResponse) cache.get();
                                response.put(value, object8.getWarehouseDepotCode() + " - " + object8.getWarehouseDepotName());
                                break;
                            case CacheConstants.ACTIVITY_TYPE:
                                ActivityMasterResponse object9 = (ActivityMasterResponse) cache.get();
                                response.put(value, object9.getActivityCode() + " - " + object9.getActivityName());
                                break;
                            case CacheConstants.SALES_AGENT:
                                SalesAgentResponse object10 = (SalesAgentResponse) cache.get();
                                response.put(value, object10.getSalesAgentName());
                                break;
                            case CacheConstants.COMMODITY:
                                EntityTransferCommodityType object11 = (EntityTransferCommodityType) cache.get();
                                response.put(value, object11.getCommodityDescriptionWithHSCode());
                                break;
                            case CacheConstants.DG_SUBSTANCES:
                                EntityTransferDGSubstance object12 = (EntityTransferDGSubstance) cache.get();
                                response.put(value, object12.getProperShippingName());
                                break;
                            default:
                        }
                    }
                });
            }
        });
    }

    private void setKeyValueForMasterLists(Map<String, Object> map, String key) { //key is SEA#TRANSPORT_MODE
        if(!IsStringNullOrEmpty(key)) {
            var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, key));
            EntityTransferMasterLists object3 = (EntityTransferMasterLists) cache.get();
            boolean isBooking = false;
            String value = null;

            if(!IsStringNullOrEmpty(object3.getValuenDesc()))
                value = object3.getValuenDesc();
            else
                value = object3.getItemDescription();

            String[] parts = key.split("#");
            if(parts.length == 2) {
                Map<String, String> finalValueMap = new HashMap<>();
                if(map.containsKey(parts[1]))
                    finalValueMap = (Map<String, String>) map.get(parts[1]);
                finalValueMap.put(parts[0], value);
                map.put(parts[1], finalValueMap);
            }
        }
    }

}
