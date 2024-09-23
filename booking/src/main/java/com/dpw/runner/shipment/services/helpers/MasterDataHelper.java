package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Component
@Slf4j
public class MasterDataHelper {

    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private CommonUtils commonUtils;

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            List<String> carrierList = new ArrayList<>();
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));

            if(masterDataResponse != null && !Objects.isNull(shipmentDetailsResponse.getRoutingsList())) {
                List<String> finalCarrierList = carrierList;
                shipmentDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));
            }

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);

            if(masterDataResponse == null) {
                shipmentDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

            if(masterDataResponse != null && !Objects.isNull(shipmentDetailsResponse.getPackingList())) {
                shipmentDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));
            }

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                    shipmentDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllDGSubstanceDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> dgSubstanceIdList = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));

            Map<String, EntityTransferDGSubstance> v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES);

            if(!Objects.equals(null, masterDataResponse)) {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse);
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllDGSubstanceDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public void setContainersPacksAutoUpdateData (ShipmentDetailsResponse shipmentDetailsResponse, Map<Long, ContainerResponse> map) {
        List<PackingResponse> packings = shipmentDetailsResponse.getPackingList();
        List<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
        Map<Long, Map<String, String>> contMap = new HashMap<>();
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean flag = shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate() != null && shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate().booleanValue()
                && shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        if(packings != null && !packings.isEmpty()) {
            for (PackingResponse pack : packings) {
                if(pack.getContainerId() != null) {
                    if(map.containsKey(pack.getContainerId())) {
                        pack.setContainerNumber(map.get(pack.getContainerId()).getContainerNumber());
                        pack.setContainerDesc(String.format("%s-%s-%s", map.get(pack.getContainerId()).getContainerCount(), map.get(pack.getContainerId()).getContainerNumber(), map.get(pack.getContainerId()).getContainerCode()));
                    }
                    if(flag) {
                        if(!contMap.containsKey(pack.getContainerId())) {
                            Map<String, String> tempMap = new HashMap<>();
                            tempMap.put(Constants.HANDLING_INFO, "");
                            tempMap.put(Constants.DESCRIPTION_OF_GOODS, "");
                            contMap.put(pack.getContainerId(), tempMap);
                        }
                        String handlingInfo = contMap.get(pack.getContainerId()).get(Constants.HANDLING_INFO);
                        String descriptionOfGoods = contMap.get(pack.getContainerId()).get(Constants.DESCRIPTION_OF_GOODS);
                        if(!IsStringNullOrEmpty(pack.getHandlingInfo())) {
                            if (handlingInfo.length() == 0)
                                handlingInfo = pack.getHandlingInfo();
                            else
                                handlingInfo = handlingInfo + ", " + pack.getHandlingInfo();
                        }
                        if(!IsStringNullOrEmpty(pack.getGoodsDescription())) {
                            if(descriptionOfGoods.length() == 0)
                                descriptionOfGoods = pack.getGoodsDescription();
                            else
                                descriptionOfGoods = descriptionOfGoods + ", " + pack.getGoodsDescription();
                        }
                        contMap.get(pack.getContainerId()).put(Constants.HANDLING_INFO, handlingInfo);
                        contMap.get(pack.getContainerId()).put(Constants.DESCRIPTION_OF_GOODS, descriptionOfGoods);
                    }
                }
            }
        }
        if(containers != null && !containers.isEmpty()) {
            for(ContainerResponse container : containers) {
                if(flag) {
                    if(contMap.containsKey(container.getId())) {
                        container.setTextFieldData(contMap.get(container.getId()));
                    }
                    else {
                        Map<String, String> tempMap = new HashMap<>();
                        tempMap.put(Constants.HANDLING_INFO, "");
                        tempMap.put(Constants.DESCRIPTION_OF_GOODS, "");
                        container.setTextFieldData(tempMap);
                    }
                }
                else {
                    Map<String, String> tempMap = new HashMap<>();
                    tempMap.put(Constants.HANDLING_INFO, container.getHandlingInfo());
                    tempMap.put(Constants.DESCRIPTION_OF_GOODS, container.getDescriptionOfGoods());
                    container.setTextFieldData(tempMap);
                }
            }
        }
    }

    public void setTruckDriverDetailsData(ShipmentDetailsResponse shipmentDetailsResponse, Map<Long, ContainerResponse> map) {
        List<TruckDriverDetailsResponse> truckDriverDetailsResponses = shipmentDetailsResponse.getTruckDriverDetails();
        if(truckDriverDetailsResponses != null && !truckDriverDetailsResponses.isEmpty()) {
            for (TruckDriverDetailsResponse truckDriverDetailsResponse: truckDriverDetailsResponses) {
                if(truckDriverDetailsResponse.getContainerId() != null && map.containsKey(truckDriverDetailsResponse.getContainerId())) {
                    truckDriverDetailsResponse.setContainerNumber(map.get(truckDriverDetailsResponse.getContainerId()).getContainerNumber());
                }
            }
        }
    }
}
