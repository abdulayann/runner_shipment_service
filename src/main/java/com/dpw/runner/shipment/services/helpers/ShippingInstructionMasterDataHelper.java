package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

@Slf4j
@Component
public class ShippingInstructionMasterDataHelper {

    private final MasterDataUtils masterDataUtils;
    private final ExecutorService executorServiceMasterData;
    private final CommonUtils commonUtils;
    private final MasterDataKeyUtils masterDataKeyUtils;
    private final MasterDataHelper masterDataHelper;

    public ShippingInstructionMasterDataHelper(MasterDataUtils masterDataUtils,
                                               @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData,
                                               CommonUtils commonUtils,
                                               MasterDataKeyUtils masterDataKeyUtils,
                                               MasterDataHelper masterDataHelper) {
        this.masterDataUtils = masterDataUtils;
        this.executorServiceMasterData = executorServiceMasterData;
        this.commonUtils = commonUtils;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.masterDataHelper = masterDataHelper;
    }

    public void getMasterDataForList(List<ShippingInstruction> list, List<IRunnerResponse> responseList,
                                     boolean getMasterData, boolean includeTenantData, Set<String> includeColumns) {
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() ->
                        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);

                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() ->
                        masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);

                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() ->
                        masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);

                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData)) {
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() ->
                            masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                }

                CompletableFuture.allOf(locationDataFuture, vesselDataFuture, tenantDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}",
                        LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(),
                        IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    public void addAllMasterDataInSingleCall(ShippingInstructionResponse response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            Set<MasterListRequest> listRequests = new HashSet<>(
                    masterDataUtils.createInBulkMasterListRequest(response, ShippingInstruction.class,
                            fieldNameKeyMap, ShippingInstruction.class.getSimpleName(), cacheMap));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);

            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
    }

    public void addAllUnlocationDataInSingleCall(ShippingInstructionResponse response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(response,
                    ShippingInstruction.class, fieldNameKeyMap, ShippingInstruction.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes,
                    EntityTransferConstants.LOCATION_SERVICE_GUID);

            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in ShippingInstruction: addAllUnlocationDataInSingleCall in class: {} with exception: {}",
                    LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllCarrierDataInSingleCall(ShippingInstructionResponse response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            Set<String> carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(response,
                    ShippingInstruction.class, fieldNameKeyMap, ShippingInstruction.class.getSimpleName(), cacheMap));

            if (CollectionUtils.isEmpty(carrierList)) {
                return;
            }

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in ShippingInstruction: addAllCarrierDataInSingleCall in class: {} with exception: {}",
                    LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllVesselDataInSingleCall(ShippingInstructionResponse response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            Set<String> vesselList = new HashSet<>(masterDataUtils.createInBulkVesselsRequest(response,
                    ShippingInstruction.class, fieldNameKeyMap, ShippingInstruction.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in ShippingInstruction: addAllVesselDataInSingleCall in class: {} with exception: {}",
                    LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }
}

