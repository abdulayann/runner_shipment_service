package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;


@Slf4j
@Component
public class ServiceDetailsV3Util {

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    public void addAllMasterDataInSingleCallList(List<ServiceDetailsResponse> serviceDetailsResponseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            serviceDetailsResponseList.forEach(serviceDetails -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(serviceDetails, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + serviceDetails.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if (masterDataResponse == null) {
                serviceDetailsResponseList.forEach(serviceDetails -> serviceDetails.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ServiceDetails.class.getSimpleName() + serviceDetails.getId()), CacheConstants.MASTER_LIST, cacheMap)));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ServiceDetails.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllUnlocationInSingleCallList(List<ServiceDetailsResponse> serviceDetailsResponseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            serviceDetailsResponseList.forEach(serviceDetails -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(serviceDetails, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + serviceDetails.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if (masterDataResponse == null) {
                serviceDetailsResponseList.forEach(serviceDetails -> serviceDetails.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ServiceDetails.class.getSimpleName() + serviceDetails.getId()), CacheConstants.UNLOCATIONS, cacheMap)));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
            }
            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ServiceDetails.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Map<String, EntityTransferMasterLists>> addAllMasterDataInSingleCall(ServiceDetailsResponse serviceDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(serviceDetailsResponse, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName(), cacheMap));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(masterDataResponse == null) {
                serviceDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ServiceDetails.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ServiceDetailsV3Util.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Map<String, EntityTransferUnLocations>> addAllUnlocationDataInSingleCall(ServiceDetailsResponse serviceDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(serviceDetailsResponse, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if(masterDataResponse == null) {
                serviceDetailsResponse.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ServiceDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), PackingV3Util.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }
}
