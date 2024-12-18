package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.*;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Slf4j
@Component
public class MasterDataUtils{

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
            double _start = System.currentTimeMillis();
            Set<String> locCodes = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                if (response instanceof CustomerBookingResponse bookingResponse) {
                    if (bookingResponse.getCarrierDetails() != null) {
                        locCodes.addAll(createInBulkUnLocationsRequest(bookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + bookingResponse.getCarrierDetails().getId(), cacheMap));
                    }
                }
                else if (response instanceof ShipmentListResponse shipmentListResponse) {
                    if (shipmentListResponse.getCarrierDetails() != null) {
                        locCodes.addAll(createInBulkUnLocationsRequest(shipmentListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId(), cacheMap));
                    }
                    if (shipmentListResponse.getAdditionalDetails() != null)
                        locCodes.addAll(createInBulkUnLocationsRequest(shipmentListResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() + shipmentListResponse.getAdditionalDetails().getId(), cacheMap));
                }
                else if (response instanceof ConsolidationListResponse consolidationListResponse && consolidationListResponse.getCarrierDetails() != null) {
                    locCodes.addAll(createInBulkUnLocationsRequest(consolidationListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId(), cacheMap));
                }
                else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && consolidationDetailsResponse.getCarrierDetails() != null) {
                    locCodes.addAll(createInBulkUnLocationsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + consolidationDetailsResponse.getCarrierDetails().getId(), cacheMap));
                }
            }

            Map<String, EntityTransferUnLocations> v1Data = fetchInBulkUnlocations(locCodes, onField);
            pushToCache(v1Data, CacheConstants.UNLOCATIONS, locCodes, new EntityTransferUnLocations(), cacheMap);

            for (IRunnerResponse response : responseList) {
                if (response instanceof CustomerBookingResponse bookingResponse) {
                    if (bookingResponse.getCarrierDetails() != null)
                        bookingResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + bookingResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));

                } else if (response instanceof ShipmentListResponse shipmentListResponse) {
                    if (shipmentListResponse.getCarrierDetails() != null)
                        shipmentListResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));

                    if (shipmentListResponse.getAdditionalDetails() != null)
                        shipmentListResponse.getAdditionalDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName() + shipmentListResponse.getAdditionalDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));
                }
                 else if (response instanceof ConsolidationListResponse consolidationListResponse && consolidationListResponse.getCarrierDetails() != null) {
                    consolidationListResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));
                }
                else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && consolidationDetailsResponse.getCarrierDetails() != null) {
                    consolidationDetailsResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationDetailsResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS, cacheMap));
                }
            }
            log.info("Time taken to fetch location Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setLocationData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    public void fetchVesselForList(List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double _start = System.currentTimeMillis();
            Set<String> vessels = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    if (shipmentListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentListResponse.getCarrierDetails().getVessel())) {
                        vessels.addAll(createInBulkVesselsRequest(shipmentListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId(), cacheMap));
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

            Map<String, EntityTransferVessels> v1Data = fetchInBulkVessels(vessels);
            pushToCache(v1Data, CacheConstants.VESSELS, vessels, new EntityTransferVessels(), cacheMap);

            for (IRunnerResponse response : responseList) {
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    if (shipmentListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentListResponse.getCarrierDetails().getVessel()))
                        shipmentListResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
                }
                else if (response instanceof ConsolidationListResponse consolidationListResponse) {
                    if (consolidationListResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationListResponse.getCarrierDetails().getVessel()))
                        consolidationListResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
                }
                else if (response instanceof ConsolidationDetailsResponse consolidationDetailsResponse && consolidationDetailsResponse.getCarrierDetails() != null && StringUtility.isNotEmpty(consolidationDetailsResponse.getCarrierDetails().getVessel())) {
                    consolidationDetailsResponse.getCarrierDetails().setVesselsMasterData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationDetailsResponse.getCarrierDetails().getId()), CacheConstants.VESSELS, cacheMap));
                }
            }
            log.info("Time taken to fetch vessel Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchVesselForList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    public void fetchTenantIdForList(List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double _start = System.currentTimeMillis();
            Set<String> tenantIdList = new HashSet<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            for (IRunnerResponse response : responseList) {
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    if (shipmentListResponse.getTenantId() != null) {
                        tenantIdList.addAll(createInBulkTenantsRequest(shipmentListResponse, MultiTenancy.class, fieldNameKeyMap, MultiTenancy.class.getSimpleName() + shipmentListResponse.getId(), cacheMap));
                    }
                    tenantIdList.addAll(createInBulkTenantsRequest(shipmentListResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() + shipmentListResponse.getId(), cacheMap));
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
            }

            Map<String, TenantModel> v1Data = fetchInTenantsList(tenantIdList);
            pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            for (IRunnerResponse response : responseList) {
                if (response instanceof ShipmentListResponse shipmentListResponse) {
                    shipmentListResponse.setTenantMasterData(new HashMap<>());
                    if (shipmentListResponse.getTenantId() != null)
                        shipmentListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(MultiTenancy.class.getSimpleName() + shipmentListResponse.getId()), CacheConstants.TENANTS, cacheMap));
                    shipmentListResponse.getTenantMasterData().putAll(setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName() + shipmentListResponse.getId()), CacheConstants.TENANTS, cacheMap));
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
            }
            log.info("Time taken to fetch Tenant Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchTenantIdForList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    public void setContainerTeuData(List<ShipmentDetails> shipmentDetailsList, List<IRunnerResponse> responseList) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            double _start = System.currentTimeMillis();
            Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
            for (IRunnerResponse response : responseList)
                dataMap.put(((ShipmentListResponse) response).getId(), (ShipmentListResponse) response);

            Set<String> containerTypes = new HashSet<>();
            Cache cacheQueue = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

            for(ShipmentDetails shipment : shipmentDetailsList) {
                if(!Objects.isNull(shipment.getContainersList()))
                    shipment.getContainersList().forEach(r -> {
                        Cache.ValueWrapper cacheValue = cacheQueue.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, r.getContainerCode()));
                        if (Objects.isNull(cacheValue))
                            containerTypes.add(r.getContainerCode());
                        else cacheMap.put(r.getContainerCode(), cacheValue.get());
                    });
            }

            Map<String, EntityTransferContainerType> v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
            pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            BigDecimal teu;
            for(ShipmentDetails shipment : shipmentDetailsList) {
                containerCountUpdate(shipment, dataMap.get(shipment.getId()));
                teu = BigDecimal.ZERO;
                if (shipment.getContainersList() != null) {
                    for(Containers c : shipment.getContainersList()) {
                        if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount())) {
                            Object cache = null;
                            if(cacheMap.isEmpty()) {
                                var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, c.getContainerCode()));
                                if(!Objects.isNull(resp)) cache = resp.get();
                            } else {
                                cache = cacheMap.get(c.getContainerCode());
                            }
                            if (!Objects.isNull(cache)) {
                                EntityTransferContainerType object = (EntityTransferContainerType) cache;
                                if (!Objects.isNull(object.getTeu()))
                                    teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                            }
                        }
                    }
                }
                dataMap.get(shipment.getId()).setTeuCount(teu);
            }
            log.info("Time taken to fetch COntainer Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    }
                    if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
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
                    for(Containers c : consolidationDetails.getContainersList()) {
                        if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount())) {
                            Object cache = null;
                            if(cacheMap.isEmpty()) {
                                var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, c.getContainerCode()));
                                if(!Objects.isNull(resp)) cache = resp.get();
                            } else {
                                cache = cacheMap.get(c.getContainerCode());
                            }
                            if (!Objects.isNull(cache)) {
                                EntityTransferContainerType object = (EntityTransferContainerType) cache;
                                if (!Objects.isNull(object.getTeu()))
                                    teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                            }
                        }
                    }
                }
                dataMap.get(consolidationDetails.getId()).setTeuCount(teu);
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setConsolidationContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
    }

    public List<MasterListRequest> createInBulkMasterListRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                e.printStackTrace();
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
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
                    String key = masterData.ItemValue + '#' + (Objects.isNull(MasterDataType.masterData(masterData.ItemType)) ? StringUtility.getEmptyString() : MasterDataType.masterData(masterData.ItemType).name());
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
                "ItemDescription",
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
        if(!IsStringNullOrEmpty(key)) {
            String value = null;

            if(!IsStringNullOrEmpty(masterLists.getValuenDesc()))
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
    public List<String> createInBulkUnLocationsRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(locCode != null && !locCode.equals("")) {
                    if (Objects.isNull(cacheValue))  locCodesList.add(locCode);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(locCode, cacheValue.get());
                    fieldNameKeyMap.put(field, locCode);
                }
            } catch (Exception e) {
                log.error("Error in createInBulkUnLocationsRequest : {}", e.getMessage());
                e.printStackTrace();
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return locCodesList;
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

                unLocationsList.forEach(location -> keyMasterDataMap.put(onField.equals(EntityTransferConstants.UNLOCATION_CODE) ? location.LocCode : location.LocationsReferenceGUID, location));
            }
        }

        return keyMasterDataMap;
    }


    // Fetch All Charge Master in single call from V1
    public List<String> createInBulkChargeTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(itemValue != null && !itemValue.equals("")) {
                    if(Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }
    public Map<String, EntityTransferChargeType> fetchInBulkChargeTypes(List<String> requests) {
        Map<String, EntityTransferChargeType> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0){
            log.info("Request: {} || ChargesList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CHARGE_CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchChargeCodeData(request);
            List<EntityTransferChargeType> chargeCodeList = jsonHelper.convertValueToList(response.entities, EntityTransferChargeType.class);
            chargeCodeList.forEach(chargeCode -> {
                keyMasterDataMap.put(chargeCode.getChargeCode(), chargeCode);
            });
        }
        return keyMasterDataMap;
    }


    // Fetch All Charge Master in single call from V1
    public List<String> createInBulkContainerTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(itemValue != null && !itemValue.equals("")) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }
    public Map<String, EntityTransferContainerType> fetchInBulkContainerTypes(Set<String> requests) {
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || ContainersList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);

            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerTypesList.forEach(containerType -> {
                keyMasterDataMap.put(containerType.getCode(), containerType);
            });
        }
        return keyMasterDataMap;
    }

    // Fetch All Commodity Master in single call from V1
    public List<String> createInBulkCommodityTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferCommodityType> fetchInBulkCommodityTypes(List<String> requests) {
        Map<String, EntityTransferCommodityType> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || CommoditiesList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);

            List<EntityTransferCommodityType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            containerTypesList.forEach(containerType -> {
                keyMasterDataMap.put(containerType.getCode(), containerType);
            });
        }
        return keyMasterDataMap;
    }


    public List<String> createInBulkVesselsRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public List<String> fetchFieldsMap(Class mainClass,String masterDataType) {
        if(entityFieldsMasterDataMap.containsKey(mainClass.getSimpleName()) && entityFieldsMasterDataMap.get(mainClass.getSimpleName()).containsKey(masterDataType)){
            return entityFieldsMasterDataMap.get(mainClass.getSimpleName()).get(masterDataType);
        } else {
            List<String> fields = new ArrayList<>();
            for (Field field : mainClass.getDeclaredFields()) {
                switch (masterDataType) {
                    case Constants.VESSEL_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.VESSEL_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.CHARGE_TYPE_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CHARGE_TYPE_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.COMMODITY_TYPE_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.COMMODITY_TYPE_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.CURRENCY_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CURRENCY_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.CONTAINER_TYPE_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CONTAINER_TYPE_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.CARRIER_MASTER_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CARRIER_MASTER_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.DG_SUBSTANCE:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.DG_SUBSTANCE))
                            fields.add(field.getName());
                        break;
                    case Constants.WARE_HOUSE_DATA:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.WARE_HOUSE_DATA))
                            fields.add(field.getName());
                        break;
                    case Constants.ACTIVITY_TYPE:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.ACTIVITY_TYPE))
                            fields.add(field.getName());
                        break;
                    case Constants.SALES_AGENT:
                        if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.SALES_AGENT))
                            fields.add(field.getName());
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
                    default:
                }
            }
            if(!entityFieldsMasterDataMap.containsKey(mainClass.getSimpleName())){
                entityFieldsMasterDataMap.put(mainClass.getSimpleName(), new HashMap<>());
            }
            entityFieldsMasterDataMap.get(mainClass.getSimpleName()).put(masterDataType, fields);
            return fields;
        }
    }

    public Map<String, EntityTransferVessels> fetchInBulkVessels(Set<String> requests) {
        Map<String, EntityTransferVessels> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || VesselsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchVesselData(request);

            List<EntityTransferVessels> vesselsList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
            vesselsList.forEach(vessel -> {
                keyMasterDataMap.put(vessel.getGuid().toString(), vessel);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkCarriersRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(itemValue != null && !itemValue.equals("")) {
                    if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                    else if (!Objects.isNull(cacheMap)) cacheMap.put(itemValue, cacheValue.get());
                    fieldNameKeyMap.put(field, itemValue);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferCarrier> fetchInBulkCarriers(Set<String> requests) {
        Map<String, EntityTransferCarrier> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
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
            vesselsList.forEach(vessel -> {
                keyMasterDataMap.put(vessel.getItemValue(), vessel);
            });
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
            if(Objects.isNull(cacheMap)) {
                var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(masterDataType.equalsIgnoreCase(CacheConstants.UNLOCATIONS_AWB) ? CacheConstants.UNLOCATIONS : masterDataType, value));
                if(!Objects.isNull(resp))
                    cache = resp.get();
            } else {
                cache = cacheMap.get(value);
            }
            if(!Objects.isNull(cache)) {
                switch (masterDataType) {
                    case CacheConstants.UNLOCATIONS:
                        EntityTransferUnLocations object = (EntityTransferUnLocations) cache;
                        if(isBooking) fieldNameMasterDataMap.put(key, object.LocCode + " " + object.NameWoDiacritics);
                        else fieldNameMasterDataMap.put(key, object.lookupDesc);
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
                        if(isBooking)
                            fieldNameMasterDataMap.put(key, object3.getItemDescription());
                        else {
                            if(!IsStringNullOrEmpty(object3.getValuenDesc()))
                                fieldNameMasterDataMap.put(key, object3.getValuenDesc());
                            else
                                fieldNameMasterDataMap.put(key, object3.getItemDescription());
                        }
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
                    default:
                }

            }
        });
        return fieldNameMasterDataMap;
    }

    public List<String> createInBulkCurrencyRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(currencyCode != null && !currencyCode.equals("")) {
                    if (Objects.isNull(cacheValue)) requests.add(currencyCode);
                    else cacheMap.put(currencyCode, cacheValue.get());
                    fieldNameKeyMap.put(field, currencyCode);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferCurrency> fetchInCurrencyList(Set<String> requests) {
        Map<String, EntityTransferCurrency> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || CurrencyList: {}", LoggerHelper.getRequestIdFromMDC(), requests);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);

            V1DataResponse response = v1Service.fetchCurrenciesData(request);
            List<EntityTransferCurrency> currencyList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
            currencyList.forEach(currency -> {
                keyMasterDataMap.put(currency.getCurrenyCode(), currency);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkTenantsRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        List<String> fields = fetchFieldsMap(mainClass, Constants.TENANT_MASTER_DATA);
        for (String field: fields){
            try {
                Field field1 = entityPayload.getClass().getDeclaredField(field);
                field1.setAccessible(true);
                Long tenantId = null;
                if(field1.get(entityPayload) != null) {
                    if(!IsStringNullOrEmpty(field1.get(entityPayload).toString()))
                        tenantId = Long.parseLong(field1.get(entityPayload).toString());
                }
                if(tenantId != null) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.TENANTS, StringUtility.convertToString(tenantId)));
                    if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(tenantId));
                    else cacheMap.put(StringUtility.convertToString(tenantId), cacheValue.get());
                    fieldNameKeyMap.put(field, StringUtility.convertToString(tenantId));
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
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

    public List<String> createInBulkDGSubstanceRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                if(field1.get(entityPayload) != null) {
                    if(!IsStringNullOrEmpty(field1.get(entityPayload).toString()))
                        dgSubstanceId = Long.parseLong(field1.get(entityPayload).toString());
                }
                if(dgSubstanceId != null) {
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.DG_SUBSTANCES, StringUtility.convertToString(dgSubstanceId)));
                    if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(dgSubstanceId));
                    else cacheMap.put(StringUtility.convertToString(dgSubstanceId), cacheValue.get());
                    fieldNameKeyMap.put(field, StringUtility.convertToString(dgSubstanceId));
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferDGSubstance> fetchInDGSubstanceList(List<String> requests) {
        Map<String, EntityTransferDGSubstance> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
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

    public List<String> createInBulkWareHouseRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, WareHouseResponse> fetchInWareHousesList(List<String> requests) {
        Map<String, WareHouseResponse> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
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

    public List<MasterDataDescriptionResponse> getMasterDataDescription(ShipmentSettingsDetails tenantSetting) throws RunnerException, ClassNotFoundException, IllegalAccessException, NoSuchFieldException {
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
        if(requests.size() > 0) {
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

    public List<String> createInBulkActivityTypeRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public List<String> createInBulkSalesAgentRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code, Map<String, Object> cacheMap) {
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
                throw new RuntimeException(e);
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }


    public Map<String, SalesAgentResponse> fetchInSalesAgentList(List<String> requests) {
        Map<String, SalesAgentResponse> keyMasterDataMap = new HashMap<>();
        if (requests.size() > 0) {
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

    public UnlocationsResponse getUNLocRow(String UNLocCode) {
        if(StringUtility.isEmpty(UNLocCode))
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "=",
                UNLocCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse response = v1Service.fetchUnlocation(commonV1ListRequest);

        List<UnlocationsResponse> unLocationsList = jsonHelper.convertValueToList(response.entities, UnlocationsResponse.class);
        return unLocationsList.isEmpty() ? null : unLocationsList.get(0);
    }

    public Map<String, EntityTransferUnLocations> getLocationDataFromCache(Set<String> locCodes) {
        if(Objects.isNull(locCodes))
            return new HashMap<>();
        Map<String, EntityTransferUnLocations> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        assert !Objects.isNull(cache);
        Set<String> locCodesFetchFromV1 = new HashSet<>();
        for(String locCode: locCodes) {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.UNLOCATIONS, locCode));
            if(Objects.isNull(value))
                locCodesFetchFromV1.add(locCode);
            else
                responseMap.put(locCode, (EntityTransferUnLocations) value.get());
        }
        if(!locCodesFetchFromV1.isEmpty()) {
            Map<String, EntityTransferUnLocations> unLocationsMap = fetchInBulkUnlocations(locCodesFetchFromV1, EntityTransferConstants.LOCATION_SERVICE_GUID);
            responseMap.putAll(unLocationsMap);
            pushToCache(unLocationsMap, CacheConstants.UNLOCATIONS, locCodesFetchFromV1, new EntityTransferUnLocations(), null);
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
        } catch (Exception e) { }
        return response;
    }

    public Map<String, WareHouseResponse> fetchWareHouseData(List<Long> request) {
        return fetchInWareHousesList(request.stream().filter(Objects::nonNull)
                .map(StringUtility::convertToString).collect(Collectors.toList()));
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

            if(shipmentDetails != null && shipmentDetails.size() > 0) {
                List<UUID> guidsList = createBillRequest(shipmentDetails, cacheMap);
                if (!guidsList.isEmpty()) {
                    ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder()
                            .guidsList(guidsList).build();
                    ShipmentBillingListResponse shipmentBillingListResponse = getShipmentBillingListResponse(shipmentBillingListRequest);
                    pushToCache(shipmentBillingListResponse.getData(), CacheConstants.BILLING, guidsList.stream().map(UUID::toString).collect(Collectors.toSet()), new ShipmentBillingListResponse.BillingData(), cacheMap);
                }

                for (ShipmentDetails details: shipmentDetails) {
                    Object cache = null;
                    if(cacheMap.isEmpty()) {
                        var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).
                                get(keyGenerator.customCacheKeyForMasterData(CacheConstants.BILLING, details.getGuid().toString()));
                        if(!Objects.isNull(resp)) cache = resp.get();
                    } else {
                        cache = cacheMap.get(details.getGuid().toString());
                    }

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

    //TODO- Server to server api we need to remove this
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

    public com.dpw.runner.shipment.services.masterdata.dto.MasterData getMasterListData(MasterDataType type, String ItemValue)
    {
        if (ItemValue == null || StringUtility.isEmpty(ItemValue)) return null;
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(type.getDescription()).ItemValue(ItemValue).build();
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
                for(Containers c : containerResponses) {
                    if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount()) && cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA) != null) {
                        Object cache = null;
                        if(cacheMap.isEmpty()) {
                            var resp = Objects.requireNonNull(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, c.getContainerCode()));
                            if(!Objects.isNull(resp)) cache = resp.get();
                        } else {
                            cache = cacheMap.get(c.getContainerCode());
                        }
                        if (!Objects.isNull(cache)) {
                            EntityTransferContainerType object = (EntityTransferContainerType) cache;
                            if (object != null && !Objects.isNull(object.getTeu()))
                                teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                        }
                    }
                }
            }
            return teu;
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: setContainerTeuData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataUtils.class.getSimpleName(), ex.getMessage());
        }
        return BigDecimal.ZERO;
    }
}
