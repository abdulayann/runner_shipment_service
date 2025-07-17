package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Component
@Slf4j
public class TransportInstructionValidationUtil {
    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentServiceV3;

    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;
    @Autowired
    private CommonUtils commonUtils;

    public ShipmentDetails validateShipmentId(PickupDeliveryDetailsRequest request) {
        if (request.getShipmentId() == null) {
            throw new ValidationException("ShipmentId is required to create transport instruction");
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentServiceV3.findById(request.getShipmentId());
        if (!shipmentDetails.isPresent()) {
            throw new ValidationException("Please provide valid ShipmentId");
        }
        return shipmentDetails.get();
    }

    public void addAllTiLegsMasterDataInSingleCallList(List<TransportInstructionLegsResponse> responseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            responseList.forEach(tiLeg -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(tiLeg, TiLegs.class, fieldNameKeyMap, TiLegs.class.getSimpleName() + tiLeg.getId(), cacheMap)));

            getMasterData(masterDataResponse, cacheMap, fieldNameKeyMap, listRequests);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTiLegsMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), TiLegs.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllTiPackagesMasterDataInSingleCallList(List<TransportInstructionLegsPackagesResponse> responseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            responseList.forEach(tiLegPackage -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(tiLegPackage, TiPackages.class, fieldNameKeyMap, TiPackages.class.getSimpleName() + tiLegPackage.getId(), cacheMap)));

            getMasterData(masterDataResponse, cacheMap, fieldNameKeyMap, listRequests);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTiPackagesMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), TiPackages.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllTiContainerMasterDataInSingleCallList(List<TransportInstructionLegsContainersResponse> responseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            responseList.forEach(tiLegContainer -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(tiLegContainer, TiContainers.class, fieldNameKeyMap, TiContainers.class.getSimpleName() + tiLegContainer.getId(), cacheMap)));

            getMasterData(masterDataResponse, cacheMap, fieldNameKeyMap, listRequests);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTiContainerMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), TiContainers.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllTiTruckDriverMasterDataInSingleCallList(List<TransportInstructionLegsTruckDriverResponse> responseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            responseList.forEach(tiLegTruckDriver -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(tiLegTruckDriver, TiTruckDriverDetails.class, fieldNameKeyMap, TiTruckDriverDetails.class.getSimpleName() + tiLegTruckDriver.getId(), cacheMap)));

            getMasterData(masterDataResponse, cacheMap, fieldNameKeyMap, listRequests);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTiTruckDriverMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), TiTruckDriverDetails.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    private void getMasterData(Map<String, Object> masterDataResponse, Map<String, Object> cacheMap, Map<String, Map<String, String>> fieldNameKeyMap, Set<MasterListRequest> listRequests) {
        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
        masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
        Set<String> keys = new HashSet<>();
        commonUtils.createMasterDataKeysList(listRequests, keys);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

        masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);

        CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    public void addAllTiReferenceNumberMasterDataInSingleCallList(List<TransportInstructionLegsReferenceResponse> responseList, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            responseList.forEach(tiLegReference -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(tiLegReference, TiReferences.class, fieldNameKeyMap, TiReferences.class.getSimpleName() + tiLegReference.getId(), cacheMap)));

            getMasterData(masterDataResponse, cacheMap, fieldNameKeyMap, listRequests);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTiReferenceNumberMasterDataInSingleCallList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), TiReferences.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }
}
