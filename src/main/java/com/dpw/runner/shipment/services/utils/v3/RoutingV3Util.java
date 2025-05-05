package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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
public class RoutingV3Util {
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private CommonUtils commonUtils;

    public void addAllMasterDataInSingleCallList(List<RoutingsResponse> routingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            routingListResponse.forEach(route -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(route, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + route.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            routingListResponse.forEach(route -> route.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + route.getId()), CacheConstants.MASTER_LIST, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall RoutingList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Routings.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllUnlocationInSingleCallList(List<RoutingsResponse> routingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            routingListResponse.forEach(route -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(route, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + route.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            routingListResponse.forEach(route -> route.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + route.getId()), CacheConstants.UNLOCATIONS, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationInSingleCallList RoutingList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Routings.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllVesselInSingleCallList(List<RoutingsResponse> routingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Set<String> vessels = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            routingListResponse.forEach(route -> vessels.addAll(masterDataUtils.createInBulkVesselsRequest(route, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + route.getId(), cacheMap)));

            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vessels);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vessels, new EntityTransferVessels(), cacheMap);

            routingListResponse.forEach(route -> route.setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + route.getId()), CacheConstants.VESSELS, cacheMap)));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselInSingleCallList RoutingList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Routings.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }
}
