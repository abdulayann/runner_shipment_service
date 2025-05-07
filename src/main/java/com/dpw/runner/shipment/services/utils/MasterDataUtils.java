package com.dpw.runner.shipment.services.utils;

import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCurrency;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.persistence.CollectionTable;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@SuppressWarnings("java:S2259")
public class MasterDataUtils{

    private static class ContainerCounts {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumbers = new HashSet<>();
    }

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @Autowired
    private BillingServiceAdapter billingServiceAdapter;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Value("${v1service.take}")
    private int take;

    private static Map<String, Map<String, List<String>>> entityFieldsMasterDataMap = new HashMap<>();

    public Map<String, EntityTransferChargeType> getChargeTypes(List<String> chargeCode) {
        if (Objects.isNull(chargeCode) || chargeCode.isEmpty())
            return null;
        List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CHARGE_CODE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(chargeCode)));
        V1DataResponse v1DataResponse = v1Service.fetchChargeCodeData(CommonV1ListRequest.builder().criteriaRequests(criteria).build());
        List<EntityTransferChargeType> list = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferChargeType.class);
        log.info("ChargeCodes list from V1: " + jsonHelper.convertToJson(list));
        return list.stream().collect(Collectors.toMap(EntityTransferChargeType::getChargeCode, Function.identity(), (oldValue, newValue) -> newValue));

    }

    /**
     * Master-data methods for list calls*
     */
    public void setLocationData(List<IRunnerResponse> responseList, String onField) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double startTime = System.currentTimeMillis();
            Set<String> locCodes = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                getLocCodesFromResponse(response, locCodes, fieldNameKeyMap, cacheMap);
            }

            Map<String, EntityTransferUnLocations> v1Data = fetchInBulkUnlocations(locCodes, onField);
            pushToCache(v1Data, CacheConstants.UNLOCATIONS, locCodes, new EntityTransferUnLocations(), cacheMap);

            for (IRunnerResponse response : responseList) {
                setUnlocationMasterData(response, fieldNameKeyMap, cacheMap);
            }
            log.info("Time taken to fetch location Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setLocationData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private void setUnlocationMasterData(IRunnerResponse response, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof CustomerBookingResponse bookingResponse) {
            setUnlocationMasterDataInCarrierDetails(bookingResponse.getCarrierDetails(), fieldNameKeyMap, cacheMap);
        } else if (response instanceof ShipmentListResponse shipmentListResponse) {
            setUnlocationMasterDataInCarrierDetails(shipmentListResponse.getCarrierDetails(), fieldNameKeyMap, cacheMap);

            if (shipmentListResponse.getAdditionalDetails() != null)
                shipmentListResponse.getAdditionalDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName() + shipmentListResponse.getAdditionalDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));
        } else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            setUnlocationMasterDataInCarrierDetails(attachListShipmentResponse.getCarrierDetails(), fieldNameKeyMap, cacheMap);

            if (attachListShipmentResponse.getAdditionalDetails() != null)
                attachListShipmentResponse.getAdditionalDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName() + attachListShipmentResponse.getAdditionalDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));
        } else if (response instanceof ConsolidationListResponse consolidationListResponse) {
            setUnlocationMasterDataInCarrierDetails(consolidationListResponse.getCarrierDetails(), fieldNameKeyMap, cacheMap);
        }
        else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse) {
            setUnlocationMasterDataInCarrierDetails(consolidationDetailsResponse.getCarrierDetails(), fieldNameKeyMap, cacheMap);
        }
    }

    private void getLocCodesFromResponse(IRunnerResponse response, Set<String> locCodes, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof CustomerBookingResponse bookingResponse) {
            addLocCodesFromCarrierDetailsResponse(bookingResponse.getCarrierDetails(), locCodes, fieldNameKeyMap, cacheMap);
        }
        else if (response instanceof ShipmentListResponse shipmentListResponse) {
            addLocCodesFromCarrierDetailsResponse(shipmentListResponse.getCarrierDetails(), locCodes, fieldNameKeyMap, cacheMap);
            if (shipmentListResponse.getAdditionalDetails() != null)
                locCodes.addAll(createInBulkUnLocationsRequest(shipmentListResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() + shipmentListResponse.getAdditionalDetails().getId(), cacheMap));
        }
        else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            addLocCodesFromCarrierDetailsResponse(attachListShipmentResponse.getCarrierDetails(), locCodes, fieldNameKeyMap, cacheMap);
            if (attachListShipmentResponse.getAdditionalDetails() != null)
                locCodes.addAll(createInBulkUnLocationsRequest(attachListShipmentResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() + attachListShipmentResponse.getAdditionalDetails().getId(), cacheMap));
        }
        else if (response instanceof ConsolidationListResponse consolidationListResponse) {
            addLocCodesFromCarrierDetailsResponse(consolidationListResponse.getCarrierDetails(), locCodes, fieldNameKeyMap, cacheMap);
        }
        else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse) {
            addLocCodesFromCarrierDetailsResponse(consolidationDetailsResponse.getCarrierDetails(), locCodes, fieldNameKeyMap, cacheMap);
        }
    }

    private void addLocCodesFromCarrierDetailsResponse(CarrierDetailResponse carrierDetails, Set<String> locCodes, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (carrierDetails != null) {
            locCodes.addAll(createInBulkUnLocationsRequest(carrierDetails, CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + carrierDetails.getId(), cacheMap));
        }
    }

    private void setUnlocationMasterDataInCarrierDetails(CarrierDetailResponse carrierDetails, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (carrierDetails != null)
            carrierDetails.setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + carrierDetails.getId()), CacheConstants.UNLOCATIONS, cacheMap));
    }

    public void fetchVesselForList(List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double startTime = System.currentTimeMillis();
            Set<String> vessels = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                getVesselsFromResponse(response, vessels, fieldNameKeyMap, cacheMap);
            }

            Map<String, EntityTransferVessels> v1Data = fetchInBulkVessels(vessels);
            pushToCache(v1Data, CacheConstants.VESSELS, vessels, new EntityTransferVessels(), cacheMap);

            for (IRunnerResponse response : responseList) {
                setVesselsMasterData(response, fieldNameKeyMap, cacheMap);
            }
            log.info("Time taken to fetch vessel Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchVesselForList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private void setVesselsMasterData(IRunnerResponse response, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof ShipmentListResponse shipmentListResponse) {
            if (shipmentListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentListResponse.getCarrierDetails().getVessel()))
                shipmentListResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
        }
        else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            if (attachListShipmentResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(attachListShipmentResponse.getCarrierDetails().getVessel()))
                attachListShipmentResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + attachListShipmentResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
        }
        else if (response instanceof ConsolidationListResponse consolidationListResponse) {
            if (consolidationListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationListResponse.getCarrierDetails().getVessel()))
                consolidationListResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
        }
        else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && consolidationDetailsResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationDetailsResponse.getCarrierDetails().getVessel())) {
            consolidationDetailsResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationDetailsResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
        }
    }

    private void getVesselsFromResponse(IRunnerResponse response, Set<String> vessels, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof ShipmentListResponse shipmentListResponse) {
            if (shipmentListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentListResponse.getCarrierDetails().getVessel())) {
                vessels.addAll(createInBulkVesselsRequest(shipmentListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId(), cacheMap));
            }
        }
        else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            if (attachListShipmentResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(attachListShipmentResponse.getCarrierDetails().getVessel())) {
                vessels.addAll(createInBulkVesselsRequest(attachListShipmentResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + attachListShipmentResponse.getCarrierDetails().getId(), cacheMap));
            }
        }
        else if (response instanceof ConsolidationListResponse consolidationListResponse) {
            if (consolidationListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationListResponse.getCarrierDetails().getVessel())) {
                vessels.addAll(createInBulkVesselsRequest(consolidationListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId(), cacheMap));
            }
        }
        else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && consolidationDetailsResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationDetailsResponse.getCarrierDetails().getVessel())) {
            vessels.addAll(createInBulkVesselsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + consolidationDetailsResponse.getCarrierDetails().getId(), cacheMap));
        }
    }

    public void fetchTenantIdForList(List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double startTime = System.currentTimeMillis();
            Set<String> tenantIdList = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                getTenantIdsFromResponseList(response, tenantIdList, fieldNameKeyMap, cacheMap);
            }

            Map<String, TenantModel> v1Data = fetchInTenantsList(tenantIdList);
            pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            for (IRunnerResponse response : responseList) {
                setTenantsMasterData(response, fieldNameKeyMap, cacheMap);
            }
            log.info("Time taken to fetch Tenant Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchTenantIdForList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private void setTenantsMasterData(IRunnerResponse response, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof ShipmentListResponse shipmentListResponse) {
            shipmentListResponse.setTenantMasterData(new HashMap<>());
            if (shipmentListResponse.getTenantId() != null)
                shipmentListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(MultiTenancy.class.getSimpleName() + shipmentListResponse.getId()), CacheConstants.TENANTS, cacheMap));
            shipmentListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName() + shipmentListResponse.getId()), CacheConstants.TENANTS, cacheMap));
        }
        if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            attachListShipmentResponse.setTenantMasterData(new HashMap<>());
            if (attachListShipmentResponse.getTenantId() != null)
                attachListShipmentResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(MultiTenancy.class.getSimpleName() + attachListShipmentResponse.getId()), CacheConstants.TENANTS, cacheMap));
            attachListShipmentResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName() + attachListShipmentResponse.getId()), CacheConstants.TENANTS, cacheMap));
        }
        if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && (consolidationDetailsResponse.getTenantId() != null)) {
            consolidationDetailsResponse.setTenantIdsData(setMasterData(fieldNameKeyMap.get(MultiTenancy.class.getSimpleName() + consolidationDetailsResponse.getId()), CacheConstants.TENANTS, cacheMap));
        }
        if (response instanceof ConsolidationListResponse consolidationListResponse) {
            consolidationListResponse.setTenantMasterData(new HashMap<>());
            if (consolidationListResponse.getTenantId() != null)
                consolidationListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(MultiTenancy.class.getSimpleName() + consolidationListResponse.getId()), CacheConstants.TENANTS, cacheMap));
        }

        if (response instanceof NetworkTransferListResponse networkTransferListResponse) {
            networkTransferListResponse.setTenantMasterData(new HashMap<>());
            if (networkTransferListResponse.getSourceBranchId() != null)
                networkTransferListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(NetworkTransfer.class.getSimpleName() + networkTransferListResponse.getId()), CacheConstants.TENANTS, cacheMap));
        }

        if (response instanceof NotificationListResponse notificationListResponse) {
            setTenantsMasterDataForNotificationListResponse(fieldNameKeyMap, cacheMap, notificationListResponse);
        }
    }

    private void setTenantsMasterDataForNotificationListResponse(Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap, NotificationListResponse notificationListResponse) {
        notificationListResponse.setTenantMasterData(new HashMap<>());
        if (notificationListResponse.getRequestedBranchId() != null || notificationListResponse.getReassignedToBranchId() != null)
            notificationListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(Notification.class.getSimpleName() + notificationListResponse.getId()), CacheConstants.TENANTS, cacheMap));
    }

    private void getTenantIdsFromResponseList(IRunnerResponse response, Set<String> tenantIdList, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if (response instanceof ShipmentListResponse shipmentListResponse) {
            if (shipmentListResponse.getTenantId() != null) {
                tenantIdList.addAll(createInBulkTenantsRequest(shipmentListResponse, MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName() + shipmentListResponse.getId(), cacheMap));
            }
            tenantIdList.addAll(createInBulkTenantsRequest(shipmentListResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() + shipmentListResponse.getId(), cacheMap));
        }
        if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            if (attachListShipmentResponse.getTenantId() != null) {
                tenantIdList.addAll(createInBulkTenantsRequest(attachListShipmentResponse, MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName() + attachListShipmentResponse.getId(), cacheMap));
            }
            tenantIdList.addAll(createInBulkTenantsRequest(attachListShipmentResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() + attachListShipmentResponse.getId(), cacheMap));
        }
        if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && (consolidationDetailsResponse.getTenantId() != null)) {
            tenantIdList.addAll(createInBulkTenantsRequest(consolidationDetailsResponse, MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName() + consolidationDetailsResponse.getId(), cacheMap));
        }
        if (response instanceof ConsolidationListResponse consolidationListResponse && (consolidationListResponse.getTenantId() != null)) {
                tenantIdList.addAll(createInBulkTenantsRequest(consolidationListResponse, MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName() + consolidationListResponse.getId(), cacheMap));
        }
        if (response instanceof NetworkTransferListResponse networkTransferListResponse && (networkTransferListResponse.getSourceBranchId() != null)) {
            tenantIdList.addAll(createInBulkTenantsRequest(networkTransferListResponse, NetworkTransfer.class, fieldNameKeyMap, NetworkTransfer.class.getSimpleName() + networkTransferListResponse.getId(), cacheMap));
        }
        if (response instanceof NotificationListResponse notificationListResponse && (notificationListResponse.getRequestedBranchId() != null || notificationListResponse.getReassignedToBranchId() != null || notificationListResponse.getReassignedFromBranchId() != null)) {
            tenantIdList.addAll(createInBulkTenantsRequest(notificationListResponse, Notification.class, fieldNameKeyMap, Notification.class.getSimpleName() + notificationListResponse.getId(), cacheMap));
        }
    }

    public <T extends IRunnerResponse> void setContainerTeuData(List<ShipmentDetails> shipmentDetailsList, List<T> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double startTime = System.currentTimeMillis();
            Map<Long, T> dataMap = new HashMap<>();
            for (T response : responseList) {
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    dataMap.put(shipmentListResponse.getId(), response);
                } else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
                    dataMap.put(attachListShipmentResponse.getId(), response);
                }
            }

            Set<String> containerTypes = new HashSet<>();
            Cache cacheQueue = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

            populateContainerTypesAndCacheMap(shipmentDetailsList, cacheQueue, containerTypes, cacheMap);

            Map<String, EntityTransferContainerType> v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
            pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            BigDecimal teu;
            for(ShipmentDetails shipment : shipmentDetailsList) {
                containerCountUpdate(shipment, dataMap.get(shipment.getId()));
                teu = BigDecimal.ZERO;
                if (shipment.getContainersList() != null) {
                    teu = calculateTeuForContainers(shipment.getContainersList(), cacheMap);
                }
                T response = dataMap.get(shipment.getId());
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    shipmentListResponse.setTeuCount(teu);
                } else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
                    attachListShipmentResponse.setTeuCount(teu);
                }
            }
            log.info("Time taken to fetch COntainer Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private void populateContainerTypesAndCacheMap(List<ShipmentDetails> shipmentDetailsList, Cache cacheQueue, Set<String> containerTypes, Map<String, Object> cacheMap) {
        for(ShipmentDetails shipment : shipmentDetailsList) {
            if(!Objects.isNull(shipment.getContainersList()))
                shipment.getContainersList().forEach(r -> {
                    Cache.ValueWrapper cacheValue = cacheQueue.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, r.getContainerCode()));
                    if (Objects.isNull(cacheValue))
                        containerTypes.add(r.getContainerCode());
                    else cacheMap.put(r.getContainerCode(), cacheValue.get());
                });
        }
    }

    private <T extends IRunnerResponse> void containerCountUpdate(ShipmentDetails shipmentDetail, T response) {
        ContainerCounts counts = new ContainerCounts();
        if (shipmentDetail.getContainersList() != null) {
            counts = countContainers(shipmentDetail.getContainersList());
        }
        if (response instanceof ShipmentListResponse shipmentListResponse) {
            shipmentListResponse.setContainer20Count(counts.container20Count);
            shipmentListResponse.setContainer40Count(counts.container40Count);
            shipmentListResponse.setContainer20GPCount(counts.container20GPCount);
            shipmentListResponse.setContainer20RECount(counts.container20RECount);
            shipmentListResponse.setContainer40GPCount(counts.container40GPCount);
            shipmentListResponse.setContainer40RECount(counts.container40RECount);
            shipmentListResponse.setContainerNumbers(counts.containerNumbers);
        } else if (response instanceof AttachListShipmentResponse attachListShipmentResponse) {
            attachListShipmentResponse.setContainer20Count(counts.container20Count);
            attachListShipmentResponse.setContainer40Count(counts.container40Count);
            attachListShipmentResponse.setContainer20GPCount(counts.container20GPCount);
            attachListShipmentResponse.setContainer20RECount(counts.container20RECount);
            attachListShipmentResponse.setContainer40GPCount(counts.container40GPCount);
            attachListShipmentResponse.setContainer40RECount(counts.container40RECount);
            attachListShipmentResponse.setContainerNumbers(counts.containerNumbers);
        }
    }

    private ContainerCounts countContainers(Set<Containers> containersList) {
        ContainerCounts counts = new ContainerCounts();
        for (Containers container : containersList) {
            if (container.getContainerCode() != null) {
                if (container.getContainerCode().contains(Constants.CONT_20)) {
                    counts.container20Count++;
                } else if (container.getContainerCode().contains(Constants.CONT_40)) {
                    counts.container40Count++;
                }
                switch (container.getContainerCode()) {
                    case Constants.CONT_20_GP -> counts.container20GPCount++;
                    case Constants.CONT_20_RE -> counts.container20RECount++;
                    case Constants.CONT_40_GP -> counts.container40GPCount++;
                    case Constants.CONT_40_RE -> counts.container40RECount++;
                    default -> { break; }
                }
            }
            if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                counts.containerNumbers.add(container.getContainerNumber());
            }
        }
        return counts;
    }

    public void setConsolidationContainerTeuData(List<ConsolidationDetails> consolidationDetailsList, List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<Long, ConsolidationListResponse> dataMap = new HashMap<>();
            for (IRunnerResponse response : responseList)
                dataMap.put(((ConsolidationListResponse) response).getId(), (ConsolidationListResponse) response);

            Set<String> containerTypes = new HashSet<>();
            Cache cacheQueue = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

            for(ConsolidationDetails consolidationDetails : consolidationDetailsList) {
                if(!Objects.isNull(consolidationDetails.getContainersList())) {
                    consolidationDetails.getContainersList().forEach(r -> {
                        Cache.ValueWrapper cacheValue = cacheQueue.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, r.getContainerCode()));
                        if (Objects.isNull(cacheValue))
                            containerTypes.add(r.getContainerCode());
                        else cacheMap.put(r.getContainerCode(), cacheValue.get());
                    });
                }
            }

            Map<String, EntityTransferContainerType> v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
            pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            BigDecimal teu;
            for(ConsolidationDetails consolidationDetails : consolidationDetailsList) {
                teu = BigDecimal.ZERO;
                if (consolidationDetails.getContainersList() != null) {
                    teu = calculateTeuForContainers(consolidationDetails.getContainersList(), cacheMap);
                }
                dataMap.get(consolidationDetails.getId()).setTeuCount(teu);
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setConsolidationContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    public <T> List<MasterListRequest> createInBulkMasterListRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<MasterListRequest> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.MASTER_DATA);
        for (String field: fields){
            try {
                Field field1 = Class.forName(entityPayload.getClass().getName()).getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                String itemType = mainClass.getDeclaredField(field).getDeclaredAnnotation(MasterData.class).type().getDescription();
                String itemTypeName = mainClass.getDeclaredField(field).getDeclaredAnnotation(MasterData.class).type().name();
                String cascadeField = mainClass.getDeclaredField(field).getDeclaredAnnotation(MasterData.class).cascade();
                String cascade = null;

                if(!cascadeField.equals("")){
                    Field field2 = entityPayload.getClass().getDeclaredField(cascadeField);
                    field2.setAccessible(true);
                    cascade = (String) field2.get(entityPayload);
                }
                if(itemValue != null) {
                    String key = itemValue + '#' + itemTypeName;
                    Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, key));
                    if (Objects.isNull(value))  requests.add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).Cascade(cascade).build());
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(key, value.get());
                    fieldNameKeyMap.put(field, key);
                }
            } catch (Exception e) {
                log.error("Error in createInBulkMasterListRequest : {}", e.getMessage());
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferMasterLists> fetchMasterListFromCache(MasterListRequestV2 requests) {
        Map<String, EntityTransferMasterLists> responseMap = new HashMap<>();
        if(Objects.isNull(requests) || Objects.isNull(requests.getMasterListRequests())|| requests.getMasterListRequests().isEmpty()){
            return new HashMap<>();
        }
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        List<MasterListRequest> fetchMasterListFromV1 = new ArrayList<>();
        for (MasterListRequest masterListRequest : requests.getMasterListRequests()) {
            String key = getEnumNameFromDescription(masterListRequest.getItemType()) + "#" + masterListRequest.getItemValue();
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, key));
            if(Objects.isNull(value)) {
                fetchMasterListFromV1.add(masterListRequest);
            } else {
                responseMap.put(key, (EntityTransferMasterLists) value.get());
            }
        }
        if (!fetchMasterListFromV1.isEmpty()) {
            MasterListRequestV2 missingRequestV2 = new MasterListRequestV2();
            missingRequestV2.setMasterListRequests(fetchMasterListFromV1);
            missingRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", MasterDataConstants.ITEM_DESCRIPTION));
            List<EntityTransferMasterLists> masterLists = fetchMultipleMasterData(missingRequestV2);
            Map<String, EntityTransferMasterLists> v1Datamap = new HashMap<>();
            masterLists.forEach(masterData -> {
                String key = (Objects.isNull(MasterDataType.masterData(masterData.ItemType)) ? Constants.EMPTY_STRING : MasterDataType.masterData(masterData.ItemType).name()) + '#' + masterData.ItemValue;
                v1Datamap.put(key, masterData);
                responseMap.put(key, masterData);
            });
            Set<String> masterDataKeys = new HashSet<>();
            populateMasterDataKeys(missingRequestV2, masterDataKeys);
            pushToCache(v1Datamap, CacheConstants.MASTER_LIST, masterDataKeys, new EntityTransferMasterLists(), null);
        }
        return responseMap;
    }

    private void populateMasterDataKeys(MasterListRequestV2 missingRequestV2, Set<String> masterDataKeys) {
        List<MasterListRequest> missingMasterListRequests = missingRequestV2.getMasterListRequests();
        missingMasterListRequests.forEach(
                masterListRequest -> masterDataKeys.add((Objects.isNull(masterListRequest.ItemType) ? Constants.EMPTY_STRING: masterListRequest.ItemType) + '#' + (Objects.isNull(masterListRequest.ItemValue) ? Constants.EMPTY_STRING: masterListRequest.ItemValue))
        );
    }

    public static String getEnumNameFromDescription(String description) {
        return Arrays.stream(MasterDataType.values())
                .filter(type -> type.getDescription().equalsIgnoreCase(description))
                .map(Enum::name)
                .findFirst()
                .orElse(null);
    }


    public Map<String, EntityTransferMasterLists> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();

        if (requests.getMasterListRequests() != null && !requests.getMasterListRequests().isEmpty()) {
            log.info("Request: {} || MasterListsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));

            List<MasterListRequest> masterListRequests = requests.getMasterListRequests();
            int batchSize = take;
            int totalBatches = (int) Math.ceil((double) masterListRequests.size() / batchSize);


            for (int i = 0; i < totalBatches; i++) {

                List<MasterListRequest> batch = masterListRequests.stream()
                    .skip((long) i * batchSize)
                    .limit(batchSize)
                    .toList();

                MasterListRequestV2 batchRequest = new MasterListRequestV2();
                batchRequest.setMasterListRequests(batch);
                batchRequest.setIncludeCols(requests.getIncludeCols());

                List<EntityTransferMasterLists> masterLists = fetchMultipleMasterData(batchRequest);

                masterLists.forEach(masterData -> {
                    String key = masterData.ItemValue + '#' + (Objects.isNull(MasterDataType.masterData(masterData.ItemType)) ? Constants.EMPTY_STRING : MasterDataType.masterData(masterData.ItemType).name());
                    keyMasterDataMap.put(key, masterData);
                });
            }
        }

        return keyMasterDataMap;
    }

    public Map<String, String> consolidationAddressCountryMasterData(ConsolidationDetails consolidationDetails) {
        List<String> alpha3CountriesList = new ArrayList<>();
        alpha3CountriesList = addAlpha3Country(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null,alpha3CountriesList);
        alpha3CountriesList = addAlpha3Country(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getAddressData() : null,alpha3CountriesList);
        for (var orgRow : consolidationDetails.getConsolidationAddresses())
            if (orgRow.getType().equals(Constants.FAG))
                alpha3CountriesList = addAlpha3Country(orgRow.getAddressData(), alpha3CountriesList);
        return getCountriesMasterListData(alpha3CountriesList);
    }

    public Map<String, String> shipmentAddressCountryMasterData(ShipmentDetails shipmentDetails) {
        List<String> alpha3CountriesList = new ArrayList<>();
        alpha3CountriesList = addAlpha3Country(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null,alpha3CountriesList);
        alpha3CountriesList = addAlpha3Country(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null,alpha3CountriesList);
        for (var orgRow : shipmentDetails.getShipmentAddresses())
            if (orgRow.getType().equals(Constants.FAG))
                alpha3CountriesList = addAlpha3Country(orgRow.getAddressData(), alpha3CountriesList);
        return getCountriesMasterListData(alpha3CountriesList);
    }

    public List<String> addAlpha3Country(Map<String, Object> address, List<String> alpha3CountriesList) {
        Set<String> alpha3CountriesSet = new HashSet<>(alpha3CountriesList);

        if (address != null && address.containsKey(PartiesConstants.COUNTRY)) {
            String country = StringUtility.convertToString(address.get(PartiesConstants.COUNTRY));
            if (country != null && country.length() == 2)
                country = CountryListHelper.ISO3166.getAlpha3FromAlpha2(country);
            if (country != null && !country.isEmpty())
                alpha3CountriesSet.add(country);
        }
        return new ArrayList<>(alpha3CountriesSet);
    }

    public Map<String, String> getCountriesMasterListData(List<String> alpha3Countries) {
        Set<MasterListRequest> listRequests = alpha3Countries.stream()
                .filter(country -> country != null && country.length() == 3)
                .map(country -> MasterListRequest.builder()
                        .ItemType(MasterDataType.COUNTRIES.getDescription())
                        .ItemValue(country)
                        .build())
                .collect(Collectors.toSet());

        if (listRequests.isEmpty()) {
            return new HashMap<>();
        }

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(new ArrayList<>(listRequests));
        masterListRequestV2.setIncludeCols(List.of(
                MasterDataConstants.ITEM_TYPE,
                MasterDataConstants.ITEM_VALUE,
                MasterDataConstants.ITEM_DESCRIPTION,
                "ValuenDesc",
                "Cascade"
        ));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = fetchInBulkMasterList(masterListRequestV2);

        return keyMasterDataMap.values().stream()
                .filter(entity -> entity != null && entity.getIdentifier1() != null)
                .collect(Collectors.toMap(
                        EntityTransferMasterLists::getIdentifier1,
                        EntityTransferMasterLists::getItemDescription
                ));
    }

    public void setKeyValueForMasterLists(Map<String, Object> map, String key, EntityTransferMasterLists masterLists) { //key is SEA#TRANSPORT_MODE
        if(!isStringNullOrEmpty(key)) {
            String value = null;

            if(!isStringNullOrEmpty(masterLists.getValuenDesc()))
                value = masterLists.getValuenDesc();
            else
                value = masterLists.getItemDescription();

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


    public List<EntityTransferMasterLists> fetchMultipleMasterData(MasterListRequestV2 requests) {
        V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
        return jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
    }

    // Fetch All Locations in single call from V1
    public <T> List<String> createInBulkUnLocationsRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> locCodesList = new ArrayList<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.UNLOCATIONS);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String locCode = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.UNLOCATIONS, locCode));
                if(locCode != null && !locCode.isEmpty()) {
                    if (Objects.isNull(cacheValue))  locCodesList.add(locCode);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(locCode, cacheValue.get());
                    fieldNameKeyMap.put(field, locCode);
                }
            } catch (Exception e) {
                log.error("Error in createInBulkUnLocationsRequest : {}", e.getMessage(), e);
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return locCodesList;
    }

    // Fetch All Organizations in single call from V1
    public <T> List<String> createInBulkOrganizationRequest(IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> orgIds = new ArrayList<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.ORGANIZATIONS);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String orgId = field1.get(entityPayload) != null ? String.valueOf(field1.get(entityPayload)): null;
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.ORGANIZATIONS, orgId));
                if(orgId != null && !orgId.isEmpty()) {
                    if (Objects.isNull(cacheValue))  orgIds.add(orgId);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(orgId, cacheValue.get());
                    fieldNameKeyMap.put(field, orgId);
                }
            } catch (Exception e) {
                log.error("Error in createInBulkOrganizationsRequest : {}", e.getMessage(), e);
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return orgIds;
    }

    public Map<String, EntityTransferUnLocations> fetchInBulkUnlocations(Set<String> requests, String onField) {
        Map<String, EntityTransferUnLocations> keyMasterDataMap = new HashMap<>();
        if (requests.isEmpty()) {
            return keyMasterDataMap;
        }

        log.info("Request: {} || UnLocationsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));

        int batchSize = take;
        int totalBatches = (int) Math.ceil((double) requests.size() / batchSize); // Calculate total number of batches

        for (int i = 0; i < totalBatches; i++) {

            List<String> batch = requests.stream()
                .skip((long) i * batchSize)
                .limit(batchSize)
                .toList();

            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(onField));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(batch)));
            request.setCriteriaRequests(criteria);

            V1DataResponse response = v1Service.fetchUnlocation(request);
            List<EntityTransferUnLocations> unLocationsList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);

            if (!Objects.isNull(unLocationsList)) {
                if(onField.equals(EntityTransferConstants.UNLOCATION_CODE))
                    unLocationsList.forEach(location -> keyMasterDataMap.put(location.getLocCode(), location));
                else if(onField.equals(EntityTransferConstants.NAME))
                    unLocationsList.forEach(location -> keyMasterDataMap.put(location.getName(), location));
                else
                    unLocationsList.forEach(location -> keyMasterDataMap.put(location.getLocationsReferenceGUID(), location));
            }
        }

        return keyMasterDataMap;
    }

    public Map<String, EntityTransferOrganizations> fetchInOrganizations(Set<String> requests, String onField) {
        Map<String, EntityTransferOrganizations> keyMasterDataMap = new HashMap<>();
        if (requests.isEmpty()) {
            return keyMasterDataMap;
        }

        log.info("Request: {} || OrganizationsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));

        int batchSize = take;
        int totalBatches = (int) Math.ceil((double) requests.size() / batchSize); // Calculate total number of batches

        for (int i = 0; i < totalBatches; i++) {

            List<String> batch = requests.stream()
                    .skip((long) i * batchSize)
                    .limit(batchSize)
                    .toList();

            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(onField));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(batch)));
            request.setCriteriaRequests(criteria);

            V1DataResponse response = v1Service.fetchOrganization(request);
            List<EntityTransferOrganizations> organizationsList = jsonHelper.convertValueToList(response.entities, EntityTransferOrganizations.class);

            if (!Objects.isNull(organizationsList)) {
                organizationsList.forEach(org -> keyMasterDataMap.put(String.valueOf(org.getId()), org));
            }
        }

        return keyMasterDataMap;
    }


    // Fetch All Charge Master in single call from V1
    public <T> List<String> createInBulkChargeTypeRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("chargeTypeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.CHARGE_TYPE_MASTER_DATA);
        for (String field: fields){
            try {
                log.info("ChargeField: "+field);
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CHARGE_TYPE, itemValue));
                if(itemValue != null && !itemValue.isEmpty()) {
                    if(Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }
    public Map<String, EntityTransferChargeType> fetchInBulkChargeTypes(List<String> requests) {
        Map<String, EntityTransferChargeType> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()){
            log.info("Request: {} || ChargesList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CHARGE_CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchChargeCodeData(request);
            List<EntityTransferChargeType> chargeCodeList = jsonHelper.convertValueToList(response.entities, EntityTransferChargeType.class);
            chargeCodeList.forEach(chargeCode -> keyMasterDataMap.put(chargeCode.getChargeCode(), chargeCode));
        }
        return keyMasterDataMap;
    }


    // Fetch All Charge Master in single call from V1
    public <T> List<String> createInBulkContainerTypeRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("containerCodeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.CONTAINER_TYPE_MASTER_DATA);
        for (String field: fields){
            try {
                log.info("ContainerField: "+field);
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, itemValue));
                if(itemValue != null && !itemValue.isEmpty()) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }
    public Map<String, EntityTransferContainerType> fetchInBulkContainerTypes(Set<String> requests) {
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || ContainersList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);

            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerTypesList.forEach(containerType -> keyMasterDataMap.put(containerType.getCode(), containerType));
        }
        return keyMasterDataMap;
    }

    // Fetch All Commodity Master in single call from V1
    public <T> List<String> createInBulkCommodityTypeRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("commodityCodeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.COMMODITY_TYPE_MASTER_DATA);
        for (String field: fields){
            try {
                log.info("CommodityField: "+field);
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.COMMODITY, itemValue));
                if(itemValue != null && !itemValue.equals("")) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferCommodityType> fetchInBulkCommodityTypes(List<String> requests) {
        Map<String, EntityTransferCommodityType> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || CommoditiesList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);

            List<EntityTransferCommodityType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            containerTypesList.forEach(containerType -> keyMasterDataMap.put(containerType.getCode(), containerType));
        }
        return keyMasterDataMap;
    }


    public <T> List<String> createInBulkVesselsRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("vesselsMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.VESSEL_MASTER_DATA);
        for (String field: fields){
            try {
                log.info("VesselField: "+field);
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.VESSELS, itemValue));
                if(itemValue != null && !itemValue.isEmpty()) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                log.error("Error in createInBulkVesselsRequest : {}", e.getMessage());
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public <T> List<String> fetchFieldsMap(Class<T> mainClass,String masterDataType) {
        if(entityFieldsMasterDataMap.containsKey(mainClass.getSimpleName()) && entityFieldsMasterDataMap.get(mainClass.getSimpleName()).containsKey(masterDataType)){
            return entityFieldsMasterDataMap.get(mainClass.getSimpleName()).get(masterDataType);
        } else {
            List<String> fields = getFieldsFromClassBasedOnMasterDataType(mainClass, masterDataType);
            if(!entityFieldsMasterDataMap.containsKey(mainClass.getSimpleName())){
                entityFieldsMasterDataMap.put(mainClass.getSimpleName(), new HashMap<>());
            }
            entityFieldsMasterDataMap.get(mainClass.getSimpleName()).put(masterDataType, fields);
            return fields;
        }
    }

    @NotNull
    private <T> List<String> getFieldsFromClassBasedOnMasterDataType(Class<T> mainClass, String masterDataType) {
        List<String> fields = new ArrayList<>();
        for (Field field : mainClass.getDeclaredFields()) {
            switch (masterDataType) {
                case Constants.COLLECTION_TABLE:
                    if (field.isAnnotationPresent(CollectionTable.class)) {
                        fields.add(field.getName());
                    }
                    break;
                case Constants.VESSEL_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.VESSEL_MASTER_DATA, fields);
                    break;
                case Constants.CHARGE_TYPE_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.CHARGE_TYPE_MASTER_DATA, fields);
                    break;
                case Constants.COMMODITY_TYPE_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.COMMODITY_TYPE_MASTER_DATA, fields);
                    break;
                case Constants.CURRENCY_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.CURRENCY_MASTER_DATA, fields);
                    break;
                case Constants.CONTAINER_TYPE_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.CONTAINER_TYPE_MASTER_DATA, fields);
                    break;
                case Constants.CARRIER_MASTER_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.CARRIER_MASTER_DATA, fields);
                    break;
                case Constants.DG_SUBSTANCE:
                    addFieldIfDedicatedMasterData(field, Constants.DG_SUBSTANCE, fields);
                    break;
                case Constants.WARE_HOUSE_DATA:
                    addFieldIfDedicatedMasterData(field, Constants.WARE_HOUSE_DATA, fields);
                    break;
                case Constants.ACTIVITY_TYPE:
                    addFieldIfDedicatedMasterData(field, Constants.ACTIVITY_TYPE, fields);
                    break;
                case Constants.SALES_AGENT:
                    addFieldIfDedicatedMasterData(field, Constants.SALES_AGENT, fields);
                    break;
                case Constants.TENANT_MASTER_DATA:
                    if (field.isAnnotationPresent(TenantIdData.class))
                        fields.add(field.getName());
                    break;
                case Constants.UNLOCATIONS:
                    if (field.isAnnotationPresent(UnlocationData.class))
                        fields.add(field.getName());
                    break;
                case Constants.MASTER_DATA:
                    if (field.isAnnotationPresent(MasterData.class))
                        fields.add(field.getName());
                    break;
                case Constants.ORGANIZATIONS:
                    if (field.isAnnotationPresent(OrganizationMasterData.class))
                        fields.add(field.getName());
                    break;
                default:
            }
        }
        return fields;
    }

    private void addFieldIfDedicatedMasterData(Field field, String expectedType, List<String> fields) {
        if (field.isAnnotationPresent(DedicatedMasterData.class) &&
                field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(expectedType)) {
            fields.add(field.getName());
        }
    }

    public Map<String, EntityTransferVessels> fetchInBulkVessels(Set<String> requests) {
        Map<String, EntityTransferVessels> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || VesselsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchVesselData(request);

            List<EntityTransferVessels> vesselsList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
            vesselsList.forEach(vessel -> keyMasterDataMap.put(vessel.getGuid().toString(), vessel));
        }
        return keyMasterDataMap;
    }

    public <T> List<String> createInBulkCarriersRequest (IRunnerResponse entityPayload, Class<T> mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("CarrierMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.CARRIER_MASTER_DATA);
        for (String field: fields){
            try {
                log.info("CarrierField: "+field);
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String itemValue = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CARRIER, itemValue));
                if(itemValue != null && !itemValue.isEmpty()) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferCarrier> fetchInBulkCarriers(Set<String> requests) {
        Map<String, EntityTransferCarrier> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {}, CarrierList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            CarrierListObject carrierListObject = new CarrierListObject();
            carrierListObject.setListObject(request);
            V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);

            List<EntityTransferCarrier> vesselsList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
            vesselsList.forEach(vessel -> keyMasterDataMap.put(vessel.getItemValue(), vessel));
        }
        return keyMasterDataMap;
    }

    public Map<String, EntityTransferCarrier> fetchInBulkCarriersBySCACCode(List<String> requests) {
        Map<String, EntityTransferCarrier> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {}, CarrierList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.IDENTIFIER1));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            CarrierListObject carrierListObject = new CarrierListObject();
            carrierListObject.setListObject(request);
            V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);

            List<EntityTransferCarrier> carriers = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
            carriers.forEach(carrier -> keyMasterDataMap.put(carrier.getIdentifier1(), carrier));
        }
        return keyMasterDataMap;
    }

    public void pushToCache (Map<String, ?> v1Data, String type, Set<String> keys, Object object, Map<String, Object> cacheMap) {
        if (Objects.isNull(v1Data))
            return;
        for (var key : v1Data.keySet()) {
            cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).put(keyGenerator.customCacheKeyForMasterData(type, key), v1Data.get(key));
            if(!Objects.isNull(cacheMap))
                cacheMap.put(key, v1Data.get(key));
        }
        if(!Objects.equals(v1Data.size(), keys.size())) {
            for(String key : keys) {
                if (!v1Data.containsKey(key)) {
                    cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).put(keyGenerator.customCacheKeyForMasterData(type, key), object);
                    if (!Objects.isNull(cacheMap))
                        cacheMap.put(key, object);
                }
            }
        }
    }

    public Map<String, String> setMasterData (Map<String, String> fieldNameKeyMap, String masterDataType, boolean isBooking, Map<String, Object> cacheMap) {
        return setMasterDataImpl(fieldNameKeyMap, masterDataType, isBooking, cacheMap);
    }

    public Map<String, String> setMasterData (Map<String, String> fieldNameKeyMap, String masterDataType, Map<String, Object> cacheMap) {
        return setMasterDataImpl(fieldNameKeyMap, masterDataType, false, cacheMap);
    }

    public Map<String, String> setMasterDataImpl (Map<String, String> fieldNameKeyMap, String masterDataType, boolean isBooking, Map<String, Object> cacheMap) {
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameMasterDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            Object cache = null;
            cache = getCacheValue(masterDataType, cacheMap, value, cache);
            if(!Objects.isNull(cache)) {
                switch (masterDataType) {
                    case CacheConstants.UNLOCATIONS:
                        EntityTransferUnLocations object = (EntityTransferUnLocations) cache;
                        fieldNameMasterDataMap.put(key, object.lookupDesc);
                        fieldNameMasterDataMap.put(key + Constants.COUNTRY, object.Country);
                        fieldNameMasterDataMap.put(key + Constants.NAME, object.NameWoDiacritics);
                        fieldNameMasterDataMap.put(key + Constants.CODE, object.LocCode);

                        break;
                    case CacheConstants.UNLOCATIONS_AWB:
                        EntityTransferUnLocations obj = (EntityTransferUnLocations) cache;
                        fieldNameMasterDataMap.put(key, obj.NameWoDiacritics);
                        fieldNameMasterDataMap.put(key + Constants.COUNTRY, obj.Country);
                        break;
                    case CacheConstants.CONTAINER_TYPE:
                        EntityTransferContainerType object1 = (EntityTransferContainerType) cache;
                        fieldNameMasterDataMap.put(key, String.format("%s - %s", object1.getCode(), object1.getDescription()));
                        break;
                    case CacheConstants.CHARGE_TYPE:
                        EntityTransferChargeType object2 = (EntityTransferChargeType) cache;
                        fieldNameMasterDataMap.put(key, object2.getDescription());
                        break;
                    case CacheConstants.MASTER_LIST:
                        EntityTransferMasterLists object3 = (EntityTransferMasterLists) cache;
                        populateFieldNameMasterDataForMasterLists(isBooking, key, fieldNameMasterDataMap, object3);
                        break;
                    case CacheConstants.VESSELS:
                        EntityTransferVessels object4 = (EntityTransferVessels) cache;
                        fieldNameMasterDataMap.put(key, object4.getName());
                        break;
                    case CacheConstants.CARRIER:
                        EntityTransferCarrier object5 = (EntityTransferCarrier) cache;
                        fieldNameMasterDataMap.put(key, object5.getItemDescription());
                        break;
                    case CacheConstants.CURRENCIES:
                        EntityTransferCurrency object6 = (EntityTransferCurrency) cache;
                        fieldNameMasterDataMap.put(key, object6.getCurrenyDescription());
                        break;
                    case CacheConstants.TENANTS:
                        TenantModel object7 = (TenantModel) cache;
                        fieldNameMasterDataMap.put(key, object7.tenantName);
                        fieldNameMasterDataMap.put(key + Constants.CODE, object7.code);
                        fieldNameMasterDataMap.put(key + Constants.DISPLAY_NAME, object7.displayName);
                        break;
                    case CacheConstants.WAREHOUSES:
                        WareHouseResponse object8 = (WareHouseResponse) cache;
                        fieldNameMasterDataMap.put(key, object8.getWarehouseDepotCode() + " - " + object8.getWarehouseDepotName());
                        break;
                    case CacheConstants.ACTIVITY_TYPE:
                        ActivityMasterResponse object9 = (ActivityMasterResponse) cache;
                        fieldNameMasterDataMap.put(key, object9.getActivityCode() + " - " + object9.getActivityName());
                        break;
                    case CacheConstants.SALES_AGENT:
                        SalesAgentResponse object10 = (SalesAgentResponse) cache;
                        fieldNameMasterDataMap.put(key, object10.getSalesAgentName());
                        break;
                    case CacheConstants.COMMODITY:
                        EntityTransferCommodityType object11 = (EntityTransferCommodityType) cache;
                        fieldNameMasterDataMap.put(key, object11.getDescription());
                        break;
                    case CacheConstants.ORGANIZATIONS:
                        EntityTransferOrganizations object12 = (EntityTransferOrganizations) cache;
                        fieldNameMasterDataMap.put(key, object12.getFullName());
                        break;
                    default:
                }

            }
        });
        return fieldNameMasterDataMap;
    }

    private void populateFieldNameMasterDataForMasterLists(boolean isBooking, String key, Map<String, String> fieldNameMasterDataMap, EntityTransferMasterLists object3) {
        if(isBooking)
            fieldNameMasterDataMap.put(key, object3.getItemDescription());
        else {
            if(!isStringNullOrEmpty(object3.getValuenDesc()))
                fieldNameMasterDataMap.put(key, object3.getValuenDesc());
            else
                fieldNameMasterDataMap.put(key, object3.getItemDescription());
        }
    }

    private Object getCacheValue(String masterDataType, Map<String, Object> cacheMap, String value, Object cache) {
        if(Objects.isNull(cacheMap) || cacheMap.isEmpty()) {
            var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(masterDataType.equalsIgnoreCase(CacheConstants.UNLOCATIONS_AWB) ? CacheConstants.UNLOCATIONS : masterDataType, value));
            if(!Objects.isNull(resp))
                return resp.get();
        } else {
            return cacheMap.get(value);
        }
        return cache;
    }

    public <T> List<String> createInBulkCurrencyRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.CURRENCY_MASTER_DATA);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String currencyCode = (String) field1.get(entityPayload);
                Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CURRENCIES, currencyCode));
                if(currencyCode != null && !currencyCode.isEmpty()) {
                    if (Objects.isNull(cacheValue)) requests.add(currencyCode);
                    else cacheMap.put(currencyCode, cacheValue.get());
                    fieldNameKeyMap.put(field, currencyCode);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferCurrency> fetchInCurrencyList(Set<String> requests) {
        Map<String, EntityTransferCurrency> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || CurrencyList: {}", LoggerHelper.getRequestIdFromMDC(), requests);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);

            V1DataResponse response = v1Service.fetchCurrenciesData(request);
            List<EntityTransferCurrency> currencyList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
            currencyList.forEach(currency -> keyMasterDataMap.put(currency.getCurrenyCode(), currency));
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkTenantsRequest(IRunnerResponse entityPayload, Class<?> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code,
            Map<String, Object> cacheMap) {
        // List to store tenant IDs that need to be fetched
        List<String> requests = new ArrayList<>();

        // Return an empty list if entityPayload is null
        if (Objects.isNull(entityPayload)) {
            return requests;
        }

        // Map to hold field-to-tenant ID mappings for the given code
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        // Fetching the cache instance for tenant data
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

        // Handle fields marked as Tenant Master Data
        List<String> tenantMasterDataFields = fetchFieldsMap(mainClass, Constants.TENANT_MASTER_DATA);
        for (String tenantMasterDataField : tenantMasterDataFields) {
            try {
                // Access the field in the entityPayload object
                Field field = entityPayload.getClass().getDeclaredField(tenantMasterDataField);
                field.setAccessible(true);
                Object fieldValue = field.get(entityPayload);

                // Process the field value if it's not null or empty
                if (fieldValue != null && !isStringNullOrEmpty(fieldValue.toString())) {
                    Long tenantId = Long.parseLong(fieldValue.toString());
                    // Add tenant ID to requests or cache, and map it to the field name
                    processTenantId(tenantId, tenantMasterDataField, requests, cache, cacheMap, fieldNameKeyMap);
                }
            } catch (Exception e) {
                // Handle any errors that occur while processing this field
                handleFieldProcessingError(tenantMasterDataField, e);
            }
        }

        // Handle fields representing collection tables (e.g., lists)
        handleCollectionTableFields(entityPayload, mainClass, cacheMap, requests, cache, fieldNameKeyMap);

        // Add the field-to-tenant ID mapping for this code to the main map
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);

        return requests;
    }

    private void handleCollectionTableFields(IRunnerResponse entityPayload, Class<?> mainClass, Map<String, Object> cacheMap, List<String> requests, Cache cache, Map<String, String> fieldNameKeyMap) {
        List<String> collectionTableFields = fetchFieldsMap(mainClass, "CollectionTable");
        for (String collectionTableField : collectionTableFields) {
            try {
                // Access the collection field in the entityPayload object
                if (isFieldPresent(entityPayload, collectionTableField)) {
                    Field field = entityPayload.getClass().getDeclaredField(collectionTableField);
                    field.setAccessible(true);
                    Object fieldValue = field.get(entityPayload);

                    handleListFields(cacheMap, requests, cache, fieldNameKeyMap, collectionTableField, fieldValue);
                }

            } catch (Exception e) {
                handleFieldProcessingError(collectionTableField, e);
            }
        }
    }

    private void handleListFields(Map<String, Object> cacheMap, List<String> requests, Cache cache, Map<String, String> fieldNameKeyMap, String collectionTableField, Object fieldValue) {
        // Check if the field value is a List
        if (fieldValue instanceof List<?> fieldValueList) {
            // Iterate over each item in the list
            for (int i = 0; i < fieldValueList.size(); i++) {
                Object item = fieldValueList.get(i);

                // Handle items of type TriangulationPartnerResponse
                if (item instanceof TriangulationPartnerResponse partner) {
                    Long tenantId = partner.getTriangulationPartner();

                    // Process tenant ID if it exists
                    if (tenantId != null) {
                        // Create a unique field key for the collection item
                        String fieldKey = collectionTableField + "_item" + (i + 1);
                        processTenantId(tenantId, fieldKey, requests, cache, cacheMap, fieldNameKeyMap);
                    }
                }
            }
        }
    }

    private boolean isFieldPresent(Object entityPayload, String fieldName) {
        try {
            // Attempt to retrieve the field
            entityPayload.getClass().getDeclaredField(fieldName);
            return true;
        } catch (NoSuchFieldException e) {
            return false;
        }
    }

    /**
     * Process a tenant ID: checks cache, updates requests and cacheMap, and maps field-to-tenant ID.
     */
    private void processTenantId(Long tenantId, String fieldKey, List<String> requests, Cache cache, Map<String, Object> cacheMap, Map<String, String> fieldNameKeyMap) {
        // Generate cache key and check if tenant data is already cached
        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.TENANTS, StringUtility.convertToString(tenantId)));
        if (Objects.isNull(cacheValue)) {
            // Add tenant ID to requests if it's not found in the cache
            requests.add(StringUtility.convertToString(tenantId));
        } else {
            // Store the cached tenant data in cacheMap
            cacheMap.put(StringUtility.convertToString(tenantId), cacheValue.get());
        }
        // Map the field key to the tenant ID
        fieldNameKeyMap.put(fieldKey, StringUtility.convertToString(tenantId));
    }

    private void handleFieldProcessingError(String fieldName, Exception e) {
        throw new GenericException("Error processing field: " + fieldName, e);
    }

    public Map<String, TenantModel> fetchInTenantsList(Set<String> requests) {
        Map<String, TenantModel> keyMasterDataMap = new HashMap<>();
        requests = requests.stream().filter(c -> Objects.nonNull(c) && !Objects.equals(c, "0")).collect(Collectors.toSet());
        if(!requests.isEmpty()) {
            log.info("Request: {} || TenantsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.TENANT_ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.listCousinBranches(request);
            List<TenantModel> tenantModelList = commonUtils.convertToList((List<?>) response.entities, TenantModel.class);
            tenantModelList.forEach(tenantModel -> keyMasterDataMap.put(StringUtility.convertToString(tenantModel.tenantId), tenantModel));
        }
        return keyMasterDataMap;
    }

    public <T> List<String> createInBulkDGSubstanceRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.DG_SUBSTANCE);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                Long dgSubstanceId = null;
                if(field1.get(entityPayload) != null && !isStringNullOrEmpty(field1.get(entityPayload).toString()))
                    dgSubstanceId = Long.parseLong(field1.get(entityPayload).toString());

                if(dgSubstanceId != null) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.DG_SUBSTANCES, StringUtility.convertToString(dgSubstanceId)));
                    if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(dgSubstanceId));
                    else cacheMap.put(StringUtility.convertToString(dgSubstanceId), cacheValue.get());
                    fieldNameKeyMap.put(field, StringUtility.convertToString(dgSubstanceId));
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferDGSubstance> fetchInDGSubstanceList(List<String> requests) {
        Map<String, EntityTransferDGSubstance> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || DGSubstanceList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchDangerousGoodData(request);

            List<EntityTransferDGSubstance> dgSubstanceList = jsonHelper.convertValueToList(response.entities, EntityTransferDGSubstance.class);
            dgSubstanceList.forEach(dgSubstance -> keyMasterDataMap.put(StringUtility.convertToString(dgSubstance.getId()), dgSubstance));
        }
        return keyMasterDataMap;
    }

    public <T> List<String> createInBulkWareHouseRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.WARE_HOUSE_DATA);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                Long wareHouseId = (Long) field1.get(entityPayload);
                if(wareHouseId != null) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.WAREHOUSES, StringUtility.convertToString(wareHouseId)));
                    if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(wareHouseId));
                    else cacheMap.put(StringUtility.convertToString(wareHouseId), cacheValue.get());
                    fieldNameKeyMap.put(field, StringUtility.convertToString(wareHouseId));
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, WareHouseResponse> fetchInWareHousesList(List<String> requests) {
        Map<String, WareHouseResponse> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || WareHousesList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchWarehouseData(request);
            List<WareHouseResponse> wareHousesList = jsonHelper.convertValueToList(response.entities, WareHouseResponse.class);
            wareHousesList.forEach(warehouse -> keyMasterDataMap.put(StringUtility.convertToString(warehouse.getId()), warehouse));
        }
        return keyMasterDataMap;
    }

    public List<MasterDataDescriptionResponse> getMasterDataDescription(ShipmentSettingsDetails tenantSetting) throws ClassNotFoundException, IllegalAccessException, NoSuchFieldException {
        ShipmentSettingsDetailsResponse shipmentSettingsDetailsResponse = jsonHelper.convertValue(tenantSetting, ShipmentSettingsDetailsResponse.class);
        List<MasterDataDescriptionResponse> res = new ArrayList<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

        List<MasterListRequest> listRequests = new ArrayList<>(createInBulkMasterListRequest(shipmentSettingsDetailsResponse, ShipmentSettingsDetails.class, fieldNameKeyMap, ShipmentSettingsDetails.class.getSimpleName(), null));
        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        Map<String, EntityTransferMasterLists> masterListsMap = fetchInBulkMasterList(masterListRequestV2);

        for(Field field : ShipmentSettingsDetails.class.getDeclaredFields()) {
            if(field.isAnnotationPresent(MasterData.class)) {
                Field field1 = Class.forName(shipmentSettingsDetailsResponse.getClass().getName()).getDeclaredField(field.getName());
                field1.setAccessible(true);
                Object fieldValue = field1.get(shipmentSettingsDetailsResponse);
                String itemTypeName = field.getDeclaredAnnotation(MasterData.class).type().name();
                String itemDescription = null;
                if(fieldValue != null && masterListsMap.get((String) fieldValue + '#' + itemTypeName) != null){
                    itemDescription = masterListsMap.get((String) fieldValue + '#' + itemTypeName).getItemDescription();
                }
                res.add(MasterDataDescriptionResponse.builder()
                        .fieldName(field.getName())
                        .fieldValue(fieldValue)
                        .itemDescription(itemDescription)
                        .build()
                );
            }
        }

        return res;
    }

    public Map<String, ActivityMasterResponse> fetchInActivityMasterList(List<String> requests) {
        Map<String, ActivityMasterResponse> keyMasterDataMap = new HashMap<>();
        if(!requests.isEmpty()) {
            log.info("Request: {} || ActivityTypeList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ACTIVITY_CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchActivityMaster(request);

            List<ActivityMasterResponse> activityMasterResponseList = jsonHelper.convertValueToList(response.entities, ActivityMasterResponse.class);
            activityMasterResponseList.forEach(activityMaster -> keyMasterDataMap.put(activityMaster.getActivityCode(), activityMaster));
        }
        return keyMasterDataMap;
    }

    public <T> List<String> createInBulkActivityTypeRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.ACTIVITY_TYPE);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                String activityId = (String) field1.get(entityPayload);
                if(StringUtility.isNotEmpty(activityId)) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.ACTIVITY_TYPE, activityId));
                    if (Objects.isNull(cacheValue)) requests.add(activityId);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(StringUtility.convertToString(activityId), cacheValue.get());
                    fieldNameKeyMap.put(field, activityId);
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public <T> List<String> createInBulkSalesAgentRequest (IRunnerResponse entityPayload, Class<T> mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.SALES_AGENT);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                Long salesAgentId = (Long) field1.get(entityPayload);
                if(salesAgentId != null) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.SALES_AGENT, StringUtility.convertToString(salesAgentId)));
                    if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(salesAgentId));
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(StringUtility.convertToString(salesAgentId), cacheValue.get());
                    fieldNameKeyMap.put(field, StringUtility.convertToString(salesAgentId));
                }
            } catch (Exception e) {
                throw new GenericException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }


    public Map<String, SalesAgentResponse> fetchInSalesAgentList(List<String> requests) {
        Map<String, SalesAgentResponse> keyMasterDataMap = new HashMap<>();
        if (!requests.isEmpty()) {
            log.info("Request: {} || SalesAgentList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchSalesAgentData(request);

            List<SalesAgentResponse> salesAgentResponseList = jsonHelper.convertValueToList(response.entities, SalesAgentResponse.class);
            salesAgentResponseList.forEach(salesAgentResponse -> keyMasterDataMap.put(StringUtility.convertToString(salesAgentResponse.getId()), salesAgentResponse));
        }
        return keyMasterDataMap;
    }

    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        var tenantId = TenantContext.getCurrentTenant();
        var userContext = UserContext.getUser();
        return () -> {
            try {
                if(mdc!=null)
                    MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(userContext);
                runnable.run();
            } finally {
                MDC.clear();
                RequestAuthContext.removeToken();
                TenantSettingsDetailsContext.remove();
                TenantContext.removeTenant();
                UserContext.removeUser();
            }

        };
    }

    public UnlocationsResponse getUNLocRow(String unLocCode) {
        if(StringUtility.isEmpty(unLocCode))
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "=",
                unLocCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse response = v1Service.fetchUnlocation(commonV1ListRequest);

        List<UnlocationsResponse> unLocationsList = jsonHelper.convertValueToList(response.entities, UnlocationsResponse.class);
        return unLocationsList.isEmpty() ? null : unLocationsList.get(0);
    }

    public Map<String, EntityTransferMasterLists> getCommodityGroupDataFromCache(Set<String> commodityGroups) {
        if(Objects.isNull(commodityGroups))
            return new HashMap<>();
        Map<String, EntityTransferMasterLists> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        Set<String> commodityGroupCodesFetchFromV1 = new HashSet<>();
        for(String commodityGroup: commodityGroups) {
            String key = commodityGroup + "#" + MasterDataType.COMMODITY_GROUP;
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, key));
            if(Objects.isNull(value))
                commodityGroupCodesFetchFromV1.add(commodityGroup);
            else
                responseMap.put(key, (EntityTransferMasterLists) value.get());
        }
        if(!commodityGroupCodesFetchFromV1.isEmpty()) {
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            List<MasterListRequest> masterListRequestV2s = new ArrayList<>();
            commodityGroupCodesFetchFromV1.forEach(e -> masterListRequestV2s.add(MasterListRequest.builder().ItemType(MasterDataType.COMMODITY_GROUP.getDescription()).ItemValue(e).build()));
            masterListRequestV2.setMasterListRequests(masterListRequestV2s);
            masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", MasterDataConstants.ITEM_DESCRIPTION));
            Map<String, EntityTransferMasterLists> masterListsMap = fetchInBulkMasterList(masterListRequestV2);
            responseMap.putAll(masterListsMap);
            commodityGroupCodesFetchFromV1 = new HashSet<>();
            commonUtils.createMasterDataKeysList(new HashSet<>(masterListRequestV2s), commodityGroupCodesFetchFromV1);
            pushToCache(masterListsMap, CacheConstants.MASTER_LIST, commodityGroupCodesFetchFromV1, new EntityTransferUnLocations(), null);
        }
        return responseMap;
    }

    /*
     * Gets the location data from cache and populates into supplied unLocationsMap
     */
    public void getLocationDataFromCache(Set<String> locCodes, Map<String, EntityTransferUnLocations> unLocationsMap) {
        unLocationsMap.putAll(getLocationDataFromCache(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID));
    }

    public Map<String, EntityTransferUnLocations> getLocationDataFromCache(Set<String> locCodes, String fieldName) {
        if(Objects.isNull(locCodes))
            return new HashMap<>();
        Map<String, EntityTransferUnLocations> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        Set<String> locCodesFetchFromV1 = new HashSet<>();
        String customCacheKey = Objects.equals(fieldName, EntityTransferConstants.NAME)?CacheConstants.UNLOCATIONS_AWB:CacheConstants.UNLOCATIONS;
        for(String locCode: locCodes) {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(customCacheKey, locCode));
            if(Objects.isNull(value))
                locCodesFetchFromV1.add(locCode);
            else
                responseMap.put(locCode, (EntityTransferUnLocations) value.get());
        }
        if(!locCodesFetchFromV1.isEmpty()) {
            Map<String, EntityTransferUnLocations> unLocationsMap = fetchInBulkUnlocations(locCodesFetchFromV1, fieldName);
            responseMap.putAll(unLocationsMap);
            pushToCache(unLocationsMap, customCacheKey, locCodesFetchFromV1, new EntityTransferUnLocations(), null);
        }
        return responseMap;
    }

    public Map<String, EntityTransferVessels> getVesselDataFromCache(Set<String> vesselGuids) {
        if(Objects.isNull(vesselGuids))
            return new HashMap<>();
        Map<String, EntityTransferVessels> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        Set<String> fetchVeseelFromV1 = new HashSet<>();
        String customCacheKey = CacheConstants.VESSELS;
        for(String guid: vesselGuids) {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(customCacheKey, guid));
            if(Objects.isNull(value))
                fetchVeseelFromV1.add(guid);
            else
                responseMap.put(guid, (EntityTransferVessels) value.get());
        }
        if(!fetchVeseelFromV1.isEmpty()) {
            Map<String, EntityTransferVessels> entityTransferVesselsMap = fetchInBulkVessels(fetchVeseelFromV1);
            responseMap.putAll(entityTransferVesselsMap);
            pushToCache(entityTransferVesselsMap, customCacheKey, fetchVeseelFromV1, new EntityTransferVessels(), null);
        }
        return responseMap;
    }

    public Map<String, EntityTransferCarrier> getCarrierDataFromCache(Set<String> carrierSet) {
        if(Objects.isNull(carrierSet))
            return new HashMap<>();
        Map<String, EntityTransferCarrier> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        Set<String> fetchCarrierFromV1 = new HashSet<>();
        String customCacheKey = CacheConstants.CARRIER;
        for(String carrier: carrierSet) {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(customCacheKey, carrier));
            if(Objects.isNull(value))
                fetchCarrierFromV1.add(carrier);
            else
                responseMap.put(carrier, (EntityTransferCarrier) value.get());
        }
        if(!fetchCarrierFromV1.isEmpty()) {
            Map<String, EntityTransferCarrier> unLocationsMap = fetchInBulkCarriers(fetchCarrierFromV1);
            responseMap.putAll(unLocationsMap);
            pushToCache(unLocationsMap, customCacheKey, fetchCarrierFromV1, new EntityTransferCarrier(), null);
        }
        return responseMap;
    }

    public Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (Objects.isNull(locCodes))
            return locationMap;

        int batchSize = take;
        if (!locCodes.isEmpty()) {
            List<String> locCodeList = new ArrayList<>(locCodes);
            int totalBatches = (int) Math.ceil((double) locCodeList.size() / batchSize);

            for (int i = 0; i < totalBatches; i++) {

                List<String> batch = locCodeList.stream()
                    .skip((long) i * batchSize)
                    .limit(batchSize)
                    .toList();

                List<Object> criteria = Arrays.asList(
                    List.of(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "In",
                    List.of(batch)
                );

                CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
                V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
                List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
                if (!Objects.isNull(unlocationsResponse))
                    unlocationsResponse.forEach(
                        location -> locationMap.put(location.getLocationsReferenceGUID(), location));
            }
        }
        return locationMap;
    }

    public Map<String, CarrierMasterData> getCarriersData(Set<String> carrierCodes) {
        Map<String, CarrierMasterData> carrierMap = new HashMap<>();
        if (Objects.isNull(carrierCodes))
            return carrierMap;
        if (!carrierCodes.isEmpty()) {
            List<Object> criteria = Arrays.asList(
                    List.of(EntityTransferConstants.ITEM_VALUE),
                    "In",
                    List.of(carrierCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            CarrierListObject carrierListObject = new CarrierListObject();
            carrierListObject.setListObject(commonV1ListRequest);
            carrierListObject.setIsList(true);
            V1DataResponse v1DataResponse = v1Service.fetchCarrierMasterData(carrierListObject, true);
            List<CarrierMasterData> carriersResponse = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierMasterData.class);
            if (!Objects.isNull(carriersResponse))
                carriersResponse.forEach(carrier ->  carrierMap.put(carrier.getItemValue(), carrier));
        }
        return carrierMap;
    }

    public EntityTransferDGSubstance fetchDgSubstanceRow(Integer dgSubstanceId) {
        var dgSubstanceRow = new EntityTransferDGSubstance();
        if(Objects.isNull(dgSubstanceId))
            return dgSubstanceRow;
        List<Object> criteria = Arrays.asList(List.of("Id"), "=", dgSubstanceId);
        CommonV1ListRequest listRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchDangerousGoodData(listRequest);

        if(v1DataResponse.entities != null) {
            dgSubstanceRow = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferDGSubstance.class).get(0);
        }

        return dgSubstanceRow;
    }

    public Map<String, Object> getPartiesOrgInfoFromCache(List<Parties> partiesList) {
        if (Objects.isNull(partiesList)) {
            return Collections.emptyMap();
        }

        Map<String, Map<String, Object>> organizationAddressMap = new HashMap<>();
        List<Parties> partiesToFetch = new ArrayList<>();
        Set<String> partiesOrgIdsToFetch = new HashSet<>();
        Cache cache = Objects.requireNonNull(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA));
        String customCacheKey = CacheConstants.ORGANIZATIONS_WITH_ADDRESSES;

        for (Parties party : partiesList) {
            if (party == null || party.getOrgCode() == null) continue;
            String key = party.getOrgCode();
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(customCacheKey, key));

            if (value == null) {
                partiesToFetch.add(party);
                partiesOrgIdsToFetch.add(key);
            } else {
                Map<String, Object> cacheResponse = (Map<String, Object>) value.get();
                organizationAddressMap.put(key, cacheResponse);

                if (cacheResponse == null || isAddressAbsent(cacheResponse, party.getAddressCode())) {
                    partiesToFetch.add(party);
                    partiesOrgIdsToFetch.add(key);
                }
            }
        }

        if (!partiesToFetch.isEmpty()) {
            fetchAndUpdateOrganizationsFromV1(partiesToFetch, organizationAddressMap, customCacheKey, partiesOrgIdsToFetch);
        }

        return buildResponseMap(partiesList, organizationAddressMap);
    }


    private boolean isAddressAbsent(Map<String, Object> cacheResponse, String addressCode) {
        Object orgAddressObj = cacheResponse.get(Constants.ORG_ADDRESS);
        if (orgAddressObj instanceof List<?>) {
            return ((List<Map<String, Object>>) orgAddressObj).stream().filter(Objects::nonNull)
                    .noneMatch(address -> Objects.equals(address.get(Constants.ADDRESS_SHORT_CODE), addressCode));
        }
        return true;
    }

    private void fetchAndUpdateOrganizationsFromV1(
            List<Parties> partiesToFetch,
            Map<String, Map<String, Object>> organizationAddressMap,
            String customCacheKey,
            Set<String> partiesOrgIdsToFetch
    ) {
        OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(partiesToFetch);
        Map<String, Map<String, Object>> organizationMap = orgAddressResponse.getOrganizations();
        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();

        if(addressMap!=null && !addressMap.isEmpty()){
            for (Map.Entry<String, Map<String, Object>> entry : addressMap.entrySet()) {
                String orgCode = entry.getKey().split("#")[0];
                Map<String, Object> orgDetails = organizationMap.get(orgCode);
                if (orgDetails != null && !orgDetails.isEmpty()) {
                    organizationAddressMap.computeIfAbsent(orgCode, k -> orgDetails)
                            .compute(Constants.ORG_ADDRESS, (k, v) -> mergeAddresses(v, entry.getValue()));
                }
            }
        }
        pushToCache(organizationAddressMap, customCacheKey, partiesOrgIdsToFetch, new HashMap<>(), null);
    }

    private Object mergeAddresses(Object existingAddresses, Map<String, Object> newAddress) {
        List<Map<String, Object>> addressList = existingAddresses instanceof List<?>
                ? new ArrayList<>((List<Map<String, Object>>) existingAddresses)
                : new ArrayList<>();
        if (addressList.stream().filter(Objects::nonNull).noneMatch(addr -> Objects.equals(addr.get(Constants.ADDRESS_SHORT_CODE), newAddress.get(Constants.ADDRESS_SHORT_CODE)))) {
            addressList.add(newAddress);
        }
        return addressList;
    }

    private Map<String, Object> buildResponseMap(List<Parties> partiesList, Map<String, Map<String, Object>> organizationAddressMap) {
        Map<String, Object> responseMap = new HashMap<>();
        for (Parties party : partiesList) {
            if (party == null || party.getOrgCode() == null) continue;
            String orgCode = party.getOrgCode();
            if (!organizationAddressMap.isEmpty() && organizationAddressMap.containsKey(orgCode)) {
                responseMap.put(orgCode, organizationAddressMap.get(orgCode));
            }
        }
        return responseMap;
    }

    public List<EntityTransferOrganizations> fetchOrganizations(Object field, Object value) {
        List<EntityTransferOrganizations> response = null;
        try {
            CommonV1ListRequest orgRequest = new CommonV1ListRequest();
            List<Object> orgField = new ArrayList<>(List.of(field));
            String operator = "=";
            List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator));
            orgCriteria.add(value);
            orgRequest.setCriteriaRequests(orgCriteria);
            V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
            response = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        } catch (Exception e) {
            log.error("Error while fetchOrganizations: {}", e.getMessage(), e);
        }
        return response;
    }

    public Map<String, WareHouseResponse> fetchWareHouseData(List<Long> request) {
        return fetchInWareHousesList(request.stream().filter(Objects::nonNull)
                .map(StringUtility::convertToString).toList());
    }

    /**
     * * Used to Fetch Bill Info from V1 for Shipments
     * @param shipmentDetails
     * @param responseList
     */
    public void fetchBillDataForShipments(List<ShipmentDetails> shipmentDetails, List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
            for (IRunnerResponse response : responseList)
                dataMap.put(((ShipmentListResponse)response).getId(), (ShipmentListResponse)response);

            if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
                List<UUID> guidsList = createBillRequest(shipmentDetails, cacheMap);
                if (!guidsList.isEmpty()) {
                    ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder()
                            .guidsList(guidsList).build();
                    ShipmentBillingListResponse shipmentBillingListResponse = getShipmentBillingListResponse(shipmentBillingListRequest);
                    pushToCache(shipmentBillingListResponse.getData(), CacheConstants.BILLING, guidsList.stream().map(UUID::toString).collect(Collectors.toSet()), new ShipmentBillingListResponse.BillingData(), cacheMap);
                }

                for (ShipmentDetails details: shipmentDetails) {
                    Object cache = null;
                    cache = getCacheValue(CacheConstants.BILLING, cacheMap, details.getGuid().toString(), cache);

                    if (!Objects.isNull(cache)) {
                        var billingData = (ShipmentBillingListResponse.BillingData) cache;
                        if (!Objects.isNull(billingData)) {
                            ShipmentListResponse shipmentListResponse = dataMap.get(details.getId());

                            shipmentListResponse.setBillStatus(details.getJobStatus());
                            shipmentListResponse.setTotalEstimatedCost(billingData.getTotalEstimatedCost());
                            shipmentListResponse.setTotalEstimatedRevenue(billingData.getTotalEstimatedRevenue());
                            shipmentListResponse.setTotalEstimatedProfit(billingData.getTotalEstimatedProfit());
                            shipmentListResponse.setTotalEstimatedProfitPercent(billingData.getTotalEstimatedProfitPercent());
                            shipmentListResponse.setTotalCost(billingData.getTotalCost());
                            shipmentListResponse.setTotalRevenue(billingData.getTotalRevenue());
                            shipmentListResponse.setTotalProfit(billingData.getTotalProfit());
                            shipmentListResponse.setTotalProfitPercent(billingData.getTotalProfitPercent());
                            shipmentListResponse.setTotalPostedCost(billingData.getTotalPostedCost());
                            shipmentListResponse.setTotalPostedRevenue(billingData.getTotalPostedRevenue());
                            shipmentListResponse.setTotalPostedProfit(billingData.getTotalPostedProfit());
                            shipmentListResponse.setTotalPostedProfitPercent(billingData.getTotalPostedProfitPercent());
                        }

                    }
                }
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchBillDataForShipments in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private ShipmentBillingListResponse getShipmentBillingListResponse(ShipmentBillingListRequest shipmentBillingListRequest) {
        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
            return billingServiceAdapter.fetchShipmentBillingData(shipmentBillingListRequest);
        }
        return v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
    }

    private List<UUID> createBillRequest(List<ShipmentDetails> shipmentDetails, Map<String, Object> cacheMap) {
        List<UUID> guidsList = new ArrayList<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        shipmentDetails.forEach(shipment -> {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.BILLING, shipment.getGuid().toString()));
            if (Objects.isNull(value)) guidsList.add(shipment.getGuid());
            else cacheMap.put(shipment.getGuid().toString(), value.get());
        });
        return guidsList;
    }

    public com.dpw.runner.shipment.services.masterdata.dto.MasterData getMasterListData(MasterDataType type, String itemValue)
    {
        if (itemValue == null || StringUtility.isEmpty(itemValue)) return null;
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(type.getDescription()).ItemValue(itemValue).build();
        MasterListRequestV2 masterListRequests = new MasterListRequestV2();
        masterListRequests.getMasterListRequests().add(masterListRequest);
        Object masterDataList = v1Service.fetchMultipleMasterData(masterListRequests).getEntities();
        List<com.dpw.runner.shipment.services.masterdata.dto.MasterData> masterData = new ArrayList<>();
        if (masterDataList != null)
            masterData = jsonHelper.convertValueToList(masterDataList, com.dpw.runner.shipment.services.masterdata.dto.MasterData.class);
        if (masterData.isEmpty())
            return null;
        return masterData.get(0);
    }

    public String getVesselName(String code) {
        if (StringUtility.isEmpty(code))
            return null;
        var resp = fetchInBulkVessels(Set.of(code));
        return resp.containsKey(code) ? resp.get(code).getName() : null;
    }

    public String getCarrierName(String code) {
        if (StringUtility.isEmpty(code))
            return null;
        var resp = fetchInBulkCarriers(Set.of(code));
        return resp.containsKey(code) ? resp.get(code).getItemDescription() : null;
    }

    public List<UnlocationsResponse> fetchUnlocationByOneIdentifier(String onField, String value) {
        if (StringUtility.isEmpty(value))
            return null;
        List<Object>  criteria = Arrays.asList(
                Arrays.asList(onField),
                "=",
                value
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        return jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
    }

    public BigDecimal setContainerTeuDataWithContainers(List<Containers> containerResponses) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            Cache cacheQueue = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
            if(!Objects.isNull(containerResponses))
                containerResponses.forEach(r -> {
                    Cache.ValueWrapper cacheValue = cacheQueue.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, r.getContainerCode()));
                    if (Objects.isNull(cacheValue))
                        containerTypes.add(r.getContainerCode());
                    else cacheMap.put(r.getContainerCode(), cacheValue.get());
                });

            Map<String, EntityTransferContainerType> v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
            pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            BigDecimal teu;
            teu = BigDecimal.ZERO;
            if (containerResponses != null) {
                teu = calculateTeuForContainers(containerResponses, cacheMap);
            }
            return teu;
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
        return BigDecimal.ZERO;
    }

    @NotNull
    private BigDecimal calculateTeuForContainers(List<Containers> containers, Map<String, Object> cacheMap) {
        BigDecimal teu = BigDecimal.ZERO;
        for(Containers c : containers) {
            if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount()) && cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA) != null) {
                Object cache = null;
                cache = getCacheValue(CacheConstants.CONTAINER_TYPE, cacheMap, c.getContainerCode(), cache);
                if (!Objects.isNull(cache)) {
                    EntityTransferContainerType object = (EntityTransferContainerType) cache;
                    if (object != null && !Objects.isNull(object.getTeu()))
                        teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                }
            }
        }
        return teu;
    }

    @NotNull
    private BigDecimal calculateTeuForContainers(Set<Containers> containers, Map<String, Object> cacheMap) {
        BigDecimal teu = BigDecimal.ZERO;
        for(Containers c : containers) {
            if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount()) && cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA) != null) {
                Object cache = null;
                cache = getCacheValue(CacheConstants.CONTAINER_TYPE, cacheMap, c.getContainerCode(), cache);
                if (!Objects.isNull(cache)) {
                    EntityTransferContainerType object = (EntityTransferContainerType) cache;
                    if (object != null && !Objects.isNull(object.getTeu()))
                        teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                }
            }
        }
        return teu;
    }
}
