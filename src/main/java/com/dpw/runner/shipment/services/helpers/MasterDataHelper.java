package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
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
    private IShipmentDao shipmentDao;
    @Autowired
    private CommonUtils commonUtils;

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() ));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() ));
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

            if(masterDataResponse != null) {
                if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                    shipmentDetailsResponse.getRoutingsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                    shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                    shipmentDetailsResponse.getPackingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getReferenceNumbersList()))
                    shipmentDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                    shipmentDetailsResponse.getServicesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                    shipmentDetailsResponse.getContainersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getEventsList()))
                    shipmentDetailsResponse.getEventsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Events.class, fieldNameKeyMap, Events.class.getSimpleName() + r.getId())));
            }

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists());

            if(masterDataResponse == null) {
                shipmentDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
                if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                    shipmentDetailsResponse.getAdditionalDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
                if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                    shipmentDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() )));

            if(masterDataResponse != null) {
                if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                    shipmentDetailsResponse.getRoutingsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                    shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                    shipmentDetailsResponse.getContainersList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                    shipmentDetailsResponse.getServicesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
                if(!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                    shipmentDetailsResponse.getPackingList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
            }

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations());

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                    shipmentDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
                if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                    shipmentDetailsResponse.getAdditionalDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
            if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                tenantIdList.addAll(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()));
            if(!Objects.isNull(shipmentDetailsResponse.getConsolidationList()) && !shipmentDetailsResponse.getConsolidationList().isEmpty()){
                tenantIdList.addAll(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse.getConsolidationList().get(0), MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName()));
            }

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel());

            if(masterDataResponse == null) {
                shipmentDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.TENANTS));
                if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                    shipmentDetailsResponse.getAdditionalDetails().setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.TENANTS));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTenantDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> currencyList = new HashSet<>(masterDataUtils.createInBulkCurrencyRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
            Map<String, EntityTransferCurrency> v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES, currencyList, new EntityTransferCurrency());

            if(masterDataResponse == null) {
                shipmentDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCurrencyDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));

            if(masterDataResponse != null && !Objects.isNull(shipmentDetailsResponse.getRoutingsList())) {
                Set<String> finalCarrierList = carrierList;
                shipmentDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));
            }

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier());

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
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType());

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

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> wareHouseTypes = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                wareHouseTypes.addAll(masterDataUtils.createInBulkWareHouseRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

            Map<String, WareHouseResponse> v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES, wareHouseTypes, new WareHouseResponse());

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                    shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllWarehouseDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllActivityDataInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> activityTypes = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                activityTypes.addAll(masterDataUtils.createInBulkActivityTypeRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

            Map<String, ActivityMasterResponse> v1Data = masterDataUtils.fetchInActivityMasterList(activityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.ACTIVITY_TYPE, activityTypes, new ActivityMasterResponse());

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                    shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.ACTIVITY_TYPE));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.ACTIVITY_TYPE, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllActivityDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllSalesAgentInSingleCall (ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> salesAgents = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse))
                salesAgents.addAll(masterDataUtils.createInBulkSalesAgentRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()) );

            Map<String, SalesAgentResponse> v1Data = masterDataUtils.fetchInSalesAgentList(salesAgents.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.SALES_AGENT, salesAgents, new SalesAgentResponse());

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse))
                    shipmentDetailsResponse.addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.SALES_AGENT));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.SALES_AGENT, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllSalesAgentInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType());

            if(masterDataResponse == null) {
                if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                    shipmentDetailsResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE)));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId() )));
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
            if (!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                shipmentDetailsResponse.getRoutingsList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));

            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels());

            if(masterDataResponse == null) {
                shipmentDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
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
            masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES, dgSubstanceIdList, new EntityTransferDGSubstance());

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
