package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierRouting;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

@Component
@Slf4j
public class VerifiedGrossMassMasterDataHelper {

    private final MasterDataUtils masterDataUtils;
    ExecutorService executorServiceMasterData;
    private final CommonUtils commonUtils;
    private final MasterDataKeyUtils masterDataKeyUtils;

    public VerifiedGrossMassMasterDataHelper(MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, CommonUtils commonUtils, MasterDataKeyUtils masterDataKeyUtils) {
        this.masterDataUtils = masterDataUtils;
        this.commonUtils = commonUtils;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.executorServiceMasterData = executorServiceMasterData;
    }

    public void getMasterDataForList(List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData) {
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (includeTenantData)
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);

                CompletableFuture.allOf(locationDataFuture, vesselDataFuture, tenantDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    public void addAllMasterDataInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(verifiedGrossMassResponse, VerifiedGrossMass.class, fieldNameKeyMap, VerifiedGrossMass.class.getSimpleName(), cacheMap));
            if (Objects.nonNull(verifiedGrossMassResponse.getSailingInformation()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(verifiedGrossMassResponse.getSailingInformation(), SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap));

            if (Objects.nonNull(verifiedGrossMassResponse.getContainersList())) {
                verifiedGrossMassResponse.getContainersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, CommonContainers.class, fieldNameKeyMap, CommonContainers.class.getSimpleName() + r.getId(), cacheMap)));
            }

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

    public void addAllUnlocationDataInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(verifiedGrossMassResponse, VerifiedGrossMass.class, fieldNameKeyMap, VerifiedGrossMass.class.getSimpleName(), cacheMap));
            if (Objects.nonNull(verifiedGrossMassResponse.getSailingInformation())) {
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(verifiedGrossMassResponse.getSailingInformation(), SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap)));
            }

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in Carrier Booking: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllCarrierDataInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(verifiedGrossMassResponse.getSailingInformation())) {
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(verifiedGrossMassResponse.getSailingInformation(), SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap));
            }
            if (CollectionUtils.isEmpty(carrierList)) {
                return;
            }
            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse, cacheMap);

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllCommodityTypesInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            if (Objects.nonNull(verifiedGrossMassResponse.getContainersList())) {
                verifiedGrossMassResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, CommonContainers.class, fieldNameKeyMap, CommonContainers.class.getSimpleName(), cacheMap)));
            }
            if (CollectionUtils.isEmpty(commodityTypes)) {
                return;
            }
            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllContainerTypesInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(verifiedGrossMassResponse.getContainersList())) {
                verifiedGrossMassResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, CommonContainers.class, fieldNameKeyMap, CommonContainers.class.getSimpleName(), cacheMap)));
            }
            if (CollectionUtils.isEmpty(containerTypes)) {
                return;
            }
            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }

    public void addAllVesselDataInSingleCall(VerifiedGrossMassResponse verifiedGrossMassResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (Objects.nonNull(verifiedGrossMassResponse.getSailingInformation())) {
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(verifiedGrossMassResponse.getSailingInformation(), SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap)));
            }
            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse, cacheMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
    }
}
