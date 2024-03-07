package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
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
    private JsonHelper jsonHelper;

    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private CommonUtils commonUtils;

    public Map<String, String> carrierMasterData (IRunnerResponse entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameCarrierDataMap = new HashMap<>();
        Map<String, String> keyCarrierDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("CarrierMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CARRIER_MASTER_DATA))
            {
                try {
                    log.info("CarrierField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    if(itemValue != null && !itemValue.equals("")) {
                        itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(itemValueList.size() > 0){
            log.info("CarrierList: "+itemValueList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            CarrierListObject carrierListObject = new CarrierListObject();
            carrierListObject.setListObject(request);
            V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);

            List<EntityTransferCarrier> carrierList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
            if (Objects.isNull(carrierList))
                return null;
            carrierList.forEach(carrier -> {
                keyCarrierDataMap.put(carrier.getItemValue(), carrier.ItemDescription);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCarrierDataMap.containsKey(value))
                    fieldNameCarrierDataMap.put(key, keyCarrierDataMap.get(value));
            });
            return fieldNameCarrierDataMap;
        }
        return null;
    }

    public Map<String, String> currencyMasterData (BaseEntity entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameCurrencyDataMap = new HashMap<>();
        Map<String, String> keyCurrencyDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> currencyCodeList = new ArrayList<>();
        log.info("CurrencyMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CURRENCY_MASTER_DATA))
            {
                try {
                    log.info("CurrencyField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String currencyCode = (String) field1.get(entityPayload);
                    if(currencyCode != null && !currencyCode.equals("")) {
                        currencyCodeList.add(currencyCode);
                        fieldNameKeyMap.put(field.getName(), currencyCode);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(currencyCodeList.size() > 0){
            log.info("CurrencyList: "+currencyCodeList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(currencyCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCurrenciesData(request);

            List<EntityTransferCurrency> currencyList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
            currencyList.forEach(currency -> {
                keyCurrencyDataMap.put(currency.getCurrenyCode(), currency.CurrenyDescription);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCurrencyDataMap.containsKey(value))
                    fieldNameCurrencyDataMap.put(key, keyCurrencyDataMap.get(value));
            });
            return fieldNameCurrencyDataMap;
        }
        return null;
    }

    public Map<String, String> addMasterData (IRunnerResponse entityPayload, Class mainClass) {
        if (Objects.isNull(entityPayload))
            return null;

        List<MasterListRequest> requests = new ArrayList<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Map<String, String> keyMasterDataMap = new HashMap<>();
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        for(Field field : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(MasterData.class))
            {
                try {
                    Field field1 = Class.forName(entityPayload.getClass().getName()).getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    String itemType = field.getDeclaredAnnotation(MasterData.class).type().getDescription();
                    String itemTypeName = field.getDeclaredAnnotation(MasterData.class).type().name();
                    String cascadeField = field.getDeclaredAnnotation(MasterData.class).cascade();
                    String cascade = null;

                    if(!cascadeField.equals("")){
                        Field field2 = entityPayload.getClass().getDeclaredField(cascadeField);
                        field2.setAccessible(true);
                        cascade = (String) field2.get(entityPayload);
                    }
                    if(itemValue != null) {
                        requests.add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).Cascade(cascade).build());
                        String key = itemValue + '#' + itemTypeName;
                        fieldNameKeyMap.put(field.getName(), key);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(requests.size() > 0) {
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(requests);
            V1DataResponse response = v1Service.fetchMultipleMasterData(masterListRequestV2);
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            masterLists.forEach(masterData -> {
                String key = masterData.ItemValue + '#' + MasterDataType.masterData(masterData.ItemType).name();
                keyMasterDataMap.put(key, masterData.getItemDescription());
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyMasterDataMap.containsKey(value))
                    fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value));
            });
            return fieldNameMasterDataMap;
        }
        return null;
    }

    public Map<String, String> addUnlocationData (IRunnerResponse entityPayload, Class baseClass, String onField) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameUnlocationDataMap = new HashMap<>();
        Map<String, EntityTransferUnLocations> keyUnlocationDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> locCodesList = new ArrayList<>();
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(UnlocationData.class))
            {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String locCode = (String) field1.get(entityPayload);
                    if(locCode != null && !locCode.equals("")) {
                        locCodesList.add(locCode);
                        fieldNameKeyMap.put(field.getName(), locCode);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(locCodesList.size() > 0){
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(onField));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(locCodesList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchUnlocation(request);

            List<EntityTransferUnLocations> unLocationsList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
            unLocationsList.forEach(unloc -> {
                keyUnlocationDataMap.put(onField == EntityTransferConstants.UNLOCATION_CODE ? unloc.LocCode : unloc.LocationsReferenceGUID, unloc);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyUnlocationDataMap.containsKey(value)) {
                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value).LocCode + " " + keyUnlocationDataMap.get(value).NameWoDiacritics);
                    fieldNameUnlocationDataMap.put(key + Constants.COUNTRY, keyUnlocationDataMap.get(value).Country);
                }
            });
            return fieldNameUnlocationDataMap;
        }
        return null;
    }

    public Map<String, String> commodityMasterData (IRunnerResponse entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameCommodityDataMap = new HashMap<>();
        Map<String, String> keyCommodityDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("commodityMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.COMMODITY_TYPE_MASTER_DATA))
            {
                try {
                    log.info("CommodityField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    if(itemValue != null && !itemValue.equals("")) {
                        itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(itemValueList.size() > 0){
            log.info("Commodity: "+itemValueList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);

            List<EntityTransferCommodityType> commodityList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            commodityList.forEach(commodity -> {
                keyCommodityDataMap.put(commodity.getCode(), commodity.getDescription());
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCommodityDataMap.containsKey(value))
                    fieldNameCommodityDataMap.put(key, keyCommodityDataMap.get(value));
            });
            return fieldNameCommodityDataMap;
        }
        return null;
    }

    public Map<String, String> containerCodeMasterData (IRunnerResponse entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameContainerCodeDataMap = new HashMap<>();
        Map<String, String> keyContainerCodeDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("containerCodeMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CONTAINER_TYPE_MASTER_DATA))
            {
                try {
                    log.info("ContainerField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    if(itemValue != null && !itemValue.equals("")) {
                        itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(itemValueList.size() > 0){
            log.info("Container: "+itemValueList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);

            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerTypesList.forEach(containerType -> {
                keyContainerCodeDataMap.put(containerType.getCode(), containerType.getDescription());
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyContainerCodeDataMap.containsKey(value))
                    fieldNameContainerCodeDataMap.put(key, keyContainerCodeDataMap.get(value));
            });
            return fieldNameContainerCodeDataMap;
        }
        return null;
    }

    public Map<String, String> vesselsMasterData (IRunnerResponse entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameVesselDataMap = new HashMap<>();
        Map<String, String> keyVesselDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("vesselsMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.VESSEL_MASTER_DATA))
            {
                try {
                    log.info("VesselField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    if(itemValue != null && !itemValue.equals("")) {
                        itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(itemValueList.size() > 0){
            log.info("Vessel: "+itemValueList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.MMSI));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchVesselData(request);

            List<EntityTransferVessels> vesselsList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
            vesselsList.forEach(vessel -> {
                keyVesselDataMap.put(vessel.getMmsi(), vessel.getName());
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyVesselDataMap.containsKey(value))
                    fieldNameVesselDataMap.put(key, keyVesselDataMap.get(value));
            });
            return fieldNameVesselDataMap;
        }
        return null;
    }

    public Map<String, String> chargeTypeMasterData (IRunnerResponse entityPayload, Class baseClass) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameChargeTypeDataMap = new HashMap<>();
        Map<String, String> keyChargeTypeDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("chargeTypeMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CHARGE_TYPE_MASTER_DATA))
            {
                try {
                    log.info("ChargeField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    if(itemValue != null && !itemValue.equals("")) {
                        itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(itemValueList.size() > 0){
            log.info("Charge: "+itemValueList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CHARGE_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchChargeCodeData(request);

            List<EntityTransferChargeType> chargeCodeList = jsonHelper.convertValueToList(response.entities, EntityTransferChargeType.class);
            chargeCodeList.forEach(chargeCode -> {
                keyChargeTypeDataMap.put(chargeCode.getChargeCode(), chargeCode.getDescription());
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyChargeTypeDataMap.containsKey(value))
                    fieldNameChargeTypeDataMap.put(key, keyChargeTypeDataMap.get(value));
            });
            return fieldNameChargeTypeDataMap;
        }
        return null;
    }

    public Map<String, EntityTransferChargeType> getChargeTypes(List<String> chargeCode) {
        if (Objects.isNull(chargeCode) || chargeCode.isEmpty())
            return null;
        List<Object> criteria = new ArrayList<>();
        Map<String, EntityTransferChargeType> response = new HashMap<>();
        List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CHARGE_CODE));
        String operator = Operators.IN.getValue();
        criteria.addAll(List.of(field, operator, List.of(chargeCode)));
        V1DataResponse v1DataResponse = v1Service.fetchChargeCodeData(CommonV1ListRequest.builder().criteriaRequests(criteria).build());
        List<EntityTransferChargeType> list = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferChargeType.class);
        log.info("ChargeCodes list from V1: " + jsonHelper.convertToJson(list));
        return list.stream().collect(Collectors.toMap(EntityTransferChargeType::getChargeCode, Function.identity(), (oldValue, newValue) -> newValue));

    }

    /**
     * Master-data methods for list calls*
     */
    public void setLocationData(List<IRunnerResponse> responseList, String onField) {
        Set<String> locCodes = new HashSet<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        for (IRunnerResponse response : responseList) {
            if (response instanceof CustomerBookingResponse) {
                CustomerBookingResponse bookingResponse = ((CustomerBookingResponse) response);
                if (bookingResponse != null && bookingResponse.getCarrierDetails() != null) {
                    locCodes.addAll(createInBulkUnLocationsRequest(bookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + bookingResponse.getCarrierDetails().getId()));
                }
            }
            else if (response instanceof ShipmentListResponse) {
                ShipmentListResponse shipmentListResponse = (ShipmentListResponse) response;
                if (shipmentListResponse != null && shipmentListResponse.getCarrierDetails() != null) {
                    locCodes.addAll(createInBulkUnLocationsRequest(shipmentListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId()));
                }
            }
            else if (response instanceof ConsolidationListResponse) {
                ConsolidationListResponse consolidationListResponse = (ConsolidationListResponse) response;
                if (consolidationListResponse != null && consolidationListResponse.getCarrierDetails() != null) {
                    locCodes.addAll(createInBulkUnLocationsRequest(consolidationListResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId()));
                }
            }
        }

        Map<String, EntityTransferUnLocations> v1Data = fetchInBulkUnlocations(locCodes.stream().toList(), onField);
        pushToCache(v1Data, CacheConstants.UNLOCATIONS);

        for (IRunnerResponse response : responseList) {
            if (response instanceof CustomerBookingResponse) {
                CustomerBookingResponse bookingResponse = ((CustomerBookingResponse) response);
                if (bookingResponse != null && bookingResponse.getCarrierDetails() != null) {
                    bookingResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + bookingResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS));
                }
            }
            else if (response instanceof ShipmentListResponse) {
                ShipmentListResponse shipmentListResponse = (ShipmentListResponse) response;
                if (shipmentListResponse != null && shipmentListResponse.getCarrierDetails() != null) {
                    shipmentListResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + shipmentListResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS));
                }
            }
            else if (response instanceof ConsolidationListResponse) {
                ConsolidationListResponse consolidationListResponse = (ConsolidationListResponse) response;
                if (consolidationListResponse != null && consolidationListResponse.getCarrierDetails() != null) {
                    consolidationListResponse.getCarrierDetails().setUnlocationData(setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName() + consolidationListResponse.getCarrierDetails().getId()), CacheConstants.UNLOCATIONS));
                }
            }
        }
    }

    public void setContainerTeuData(List<ShipmentDetails> shipmentDetailsList, List<IRunnerResponse> responseList) {
        Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList)
            dataMap.put(((ShipmentListResponse) response).getId(), (ShipmentListResponse) response);

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();

        for(ShipmentDetails shipment : shipmentDetailsList) {
            if(!Objects.isNull(shipment.getContainersList()) && !shipment.getContainersList().isEmpty())
                shipment.getContainersList().forEach(r -> containerTypes.add(r.getContainerCode()));
        }

        Map v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).toList());
        pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

        BigDecimal teu;
        for(ShipmentDetails shipment : shipmentDetailsList) {
            teu = BigDecimal.ZERO;
            if (shipment.getContainersList() != null) {
                for(Containers c : shipment.getContainersList()) {
                    if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount())) {
                        var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, c.getContainerCode()));
                        if (!Objects.isNull(cache)) {
                            EntityTransferContainerType object = (EntityTransferContainerType) cache.get();
                            if (!Objects.isNull(object.getTeu()))
                                teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                        }
                    }
                }
            }
            dataMap.get(shipment.getId()).setTeuCount(teu);
        }
    }

    public void setConsolidationContainerTeuData(List<ConsolidationDetails> consolidationDetailsList, List<IRunnerResponse> responseList) {
        Map<Long, ConsolidationListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList)
            dataMap.put(((ConsolidationListResponse) response).getId(), (ConsolidationListResponse) response);

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();

        for(ConsolidationDetails consolidationDetails : consolidationDetailsList) {
            if(!Objects.isNull(consolidationDetails.getContainersList()) && !consolidationDetails.getContainersList().isEmpty())
                consolidationDetails.getContainersList().forEach(r -> containerTypes.add(r.getContainerCode()));
        }

        Map v1Data = fetchInBulkContainerTypes(containerTypes.stream().filter(Objects::nonNull).toList());
        pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

        BigDecimal teu;
        for(ConsolidationDetails consolidationDetails : consolidationDetailsList) {
            teu = BigDecimal.ZERO;
            if (consolidationDetails.getContainersList() != null) {
                for(Containers c : consolidationDetails.getContainersList()) {
                    if (!Objects.isNull(c.getContainerCode()) && !Objects.isNull(c.getContainerCount())) {
                        var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, c.getContainerCode()));
                        if (!Objects.isNull(cache)) {
                            EntityTransferContainerType object = (EntityTransferContainerType) cache.get();
                            if (!Objects.isNull(object.getTeu()))
                                teu = teu.add(BigDecimal.valueOf(object.getTeu()).multiply(BigDecimal.valueOf(c.getContainerCount())));
                        }
                    }
                }
            }
            dataMap.get(consolidationDetails.getId()).setTeuCount(teu);
        }
    }

    public List<MasterListRequest> createInBulkMasterListRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<MasterListRequest> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(MasterData.class))
            {
                try {
                    Field field1 = Class.forName(entityPayload.getClass().getName()).getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    String itemType = field.getDeclaredAnnotation(MasterData.class).type().getDescription();
                    String itemTypeName = field.getDeclaredAnnotation(MasterData.class).type().name();
                    String cascadeField = field.getDeclaredAnnotation(MasterData.class).cascade();
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
                        fieldNameKeyMap.put(field.getName(), key);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }
    public Map<String, EntityTransferMasterLists> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        if(requests.getMasterListRequests() != null && requests.getMasterListRequests().size() > 0) {
            log.info("Request: {} || MasterListsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            masterLists.forEach(masterData -> {
                String key = masterData.ItemValue + '#' + MasterDataType.masterData(masterData.ItemType).name();
                keyMasterDataMap.put(key, masterData);
            });
        }
        return keyMasterDataMap;
    }

    public Map<String, String> setInBulkMasterList (Map<String, String> fieldNameKeyMap, Map<String, EntityTransferMasterLists> keyMasterDataMap) {
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameMasterDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            if(keyMasterDataMap.containsKey(value))
                fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value).getItemDescription());
        });

        return fieldNameMasterDataMap;
    }

    // Fetch All Locations in single call from V1
    public List<String> createInBulkUnLocationsRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> locCodesList = new ArrayList<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(UnlocationData.class))
            {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String locCode = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.UNLOCATIONS, locCode));
                    if(locCode != null && !locCode.equals("")) {
                        if (Objects.isNull(cacheValue))  locCodesList.add(locCode);
                        fieldNameKeyMap.put(field.getName(), locCode);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return locCodesList;
    }
    public Map<String, EntityTransferUnLocations> fetchInBulkUnlocations(List<String> requests, String onField) {
        Map<String, EntityTransferUnLocations> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || UnLocationsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(onField));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchUnlocation(request);
            List<EntityTransferUnLocations> unLocationsList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
            if (Objects.isNull(unLocationsList))
                return keyMasterDataMap;

            unLocationsList.forEach(location -> {
                keyMasterDataMap.put(onField.equals(EntityTransferConstants.UNLOCATION_CODE) ? location.LocCode : location.LocationsReferenceGUID, location);
            });
        }
        return keyMasterDataMap;
    }
    public Map<String, String> setInBulkUnlocations (Map<String, String> fieldNameKeyMap, Map<String, EntityTransferUnLocations> keyMasterDataMap) {
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameMasterDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            if(keyMasterDataMap.containsKey(value)) {
                fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value).LocCode + " " + keyMasterDataMap.get(value).NameWoDiacritics);
                fieldNameMasterDataMap.put(key + Constants.COUNTRY, keyMasterDataMap.get(value).Country);
            }
        });
        return fieldNameMasterDataMap;
    }

    // Fetch All Charge Master in single call from V1
    public List<String> createInBulkChargeTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("chargeTypeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CHARGE_TYPE_MASTER_DATA))
            {
                try {
                    log.info("ChargeField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CHARGE_TYPE, itemValue));
                    if(itemValue != null && !itemValue.equals("")) {
                        if(Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
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
    public Map<String, String> setInBulkChargeTypes (Map<String, String> fieldNameKeyMap, Map<String, EntityTransferChargeType> keyChargeTypeDataMap) {
        Map<String, String> fieldNameChargeTypeDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameChargeTypeDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            if(keyChargeTypeDataMap.containsKey(value))
                fieldNameChargeTypeDataMap.put(key, keyChargeTypeDataMap.get(value).getDescription());
        });
        return fieldNameChargeTypeDataMap;
    }


    // Fetch All Charge Master in single call from V1
    public List<String> createInBulkContainerTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("containerCodeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CONTAINER_TYPE_MASTER_DATA))
            {
                try {
                    log.info("ContainerField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, itemValue));
                    if(itemValue != null && !itemValue.equals("")) {
                        if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }
    public Map<String, EntityTransferContainerType> fetchInBulkContainerTypes(List<String> requests) {
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || ContainersList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);

            List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerTypesList.forEach(containerType -> {
                keyMasterDataMap.put(containerType.getCode(), containerType);
            });
        }
        return keyMasterDataMap;
    }
    public Map<String, String> setInBulkContainerTypes (Map<String, String> fieldNameKeyMap, Map<String, EntityTransferContainerType> keyContainerCodeDataMap) {
        Map<String, String> fieldNameContainerCodeDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameContainerCodeDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            if(keyContainerCodeDataMap.containsKey(value))
                fieldNameContainerCodeDataMap.put(key, keyContainerCodeDataMap.get(value).getDescription());
        });
        return fieldNameContainerCodeDataMap;
    }

    // Fetch All Commodity Master in single call from V1
    public List<String> createInBulkCommodityTypeRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("commodityCodeMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.COMMODITY_TYPE_MASTER_DATA))
            {
                try {
                    log.info("CommodityField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.COMMODITY, itemValue));
                    if(itemValue != null && !itemValue.equals("")) {
                        if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
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
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);

            List<EntityTransferCommodityType> containerTypesList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            containerTypesList.forEach(containerType -> {
                keyMasterDataMap.put(containerType.getCode(), containerType);
            });
        }
        return keyMasterDataMap;
    }

    public Map<String, String> setInBulkCommodityTypes (Map<String, String> fieldNameKeyMap, Map<String, EntityTransferCommodityType> keyCommodityCodeDataMap) {
        Map<String, String> fieldNameContainerCodeDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameContainerCodeDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            if(keyCommodityCodeDataMap.containsKey(value))
                fieldNameContainerCodeDataMap.put(key, keyCommodityCodeDataMap.get(value).getDescription());
        });
        return fieldNameContainerCodeDataMap;
    }

    public List<String> createInBulkVesselsRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("vesselsMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.VESSEL_MASTER_DATA))
            {
                try {
                    log.info("VesselField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.VESSELS, itemValue));
                    if(itemValue != null && !itemValue.equals("")) {
                        if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferVessels> fetchInBulkVessels(List<String> requests) {
        Map<String, EntityTransferVessels> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || VesselsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.MMSI));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchVesselData(request);

            List<EntityTransferVessels> vesselsList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
            vesselsList.forEach(vessel -> {
                keyMasterDataMap.put(vessel.getMmsi(), vessel);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkCarriersRequest (IRunnerResponse entityPayload, Class mainClass,  Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        if (Objects.isNull(entityPayload))
            return null;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> itemValueList = new ArrayList<>();
        log.info("CarrierMasterData");
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field  : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CARRIER_MASTER_DATA))
            {
                try {
                    log.info("CarrierField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String itemValue = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CARRIER, itemValue));
                    if(itemValue != null && !itemValue.equals("")) {
                        if (Objects.isNull(cacheValue)) itemValueList.add(itemValue);
                        fieldNameKeyMap.put(field.getName(), itemValue);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return itemValueList;
    }

    public Map<String, EntityTransferCarrier> fetchInBulkCarriers(List<String> requests) {
        Map<String, EntityTransferCarrier> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {}, CarrierList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(requests)));
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

    public void pushToCache (Map<String, ?> v1Data, String type) {
        if (Objects.isNull(v1Data) || v1Data.isEmpty())
            return;
        for (var key : v1Data.keySet()) {
            cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).put(keyGenerator.customCacheKeyForMasterData(type, key), v1Data.get(key));
        }
    }

    public Map<String, String> setMasterData (Map<String, String> fieldNameKeyMap, String masterDataType, boolean isBooking) {
        return setMasterDataImpl(fieldNameKeyMap, masterDataType, isBooking);
    }

    public Map<String, String> setMasterData (Map<String, String> fieldNameKeyMap, String masterDataType) {
        return setMasterDataImpl(fieldNameKeyMap, masterDataType, false);
    }

    public Map<String, String> setMasterDataImpl (Map<String, String> fieldNameKeyMap, String masterDataType, boolean isBooking) {
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameMasterDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(masterDataType.equalsIgnoreCase(CacheConstants.UNLOCATIONS_AWB) ? CacheConstants.UNLOCATIONS : masterDataType, value));
            if(!Objects.isNull(cache)) {
                switch (masterDataType) {
                    case CacheConstants.UNLOCATIONS:
                        EntityTransferUnLocations object = (EntityTransferUnLocations) cache.get();
                        fieldNameMasterDataMap.put(key, object.LocCode + " " + object.NameWoDiacritics);
                        fieldNameMasterDataMap.put(key + Constants.COUNTRY, object.Country);
                        break;
                    case CacheConstants.UNLOCATIONS_AWB:
                        EntityTransferUnLocations obj = (EntityTransferUnLocations) cache.get();
                        fieldNameMasterDataMap.put(key, obj.NameWoDiacritics);
                        fieldNameMasterDataMap.put(key + Constants.COUNTRY, obj.Country);
                        break;
                    case CacheConstants.CONTAINER_TYPE:
                        EntityTransferContainerType object1 = (EntityTransferContainerType) cache.get();
                        fieldNameMasterDataMap.put(key, String.format("%s - %s", object1.getCode(), object1.getDescription()));
                        break;
                    case CacheConstants.CHARGE_TYPE:
                        EntityTransferChargeType object2 = (EntityTransferChargeType) cache.get();
                        fieldNameMasterDataMap.put(key, object2.getDescription());
                        break;
                    case CacheConstants.MASTER_LIST:
                        EntityTransferMasterLists object3 = (EntityTransferMasterLists) cache.get();
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
                        EntityTransferVessels object4 = (EntityTransferVessels) cache.get();
                        fieldNameMasterDataMap.put(key, object4.getName());
                        break;
                    case CacheConstants.CARRIER:
                        EntityTransferCarrier object5 = (EntityTransferCarrier) cache.get();
                        fieldNameMasterDataMap.put(key, object5.getItemDescription());
                        break;
                    case CacheConstants.CURRENCIES:
                        EntityTransferCurrency object6 = (EntityTransferCurrency) cache.get();
                        fieldNameMasterDataMap.put(key, object6.getCurrenyDescription());
                        break;
                    case CacheConstants.TENANTS:
                        TenantModel object7 = (TenantModel) cache.get();
                        fieldNameMasterDataMap.put(key, object7.tenantName);
                        break;
                    case CacheConstants.WAREHOUSES:
                        WareHouseResponse object8 = (WareHouseResponse) cache.get();
                        fieldNameMasterDataMap.put(key, object8.getWarehouseDepotCode() + " - " + object8.getWarehouseDepotName());
                        break;
                    case CacheConstants.ACTIVITY_TYPE:
                        ActivityMasterResponse object9 = (ActivityMasterResponse) cache.get();
                        fieldNameMasterDataMap.put(key, object9.getActivityCode() + " - " + object9.getActivityName());
                        break;
                    case CacheConstants.SALES_AGENT:
                        SalesAgentResponse object10 = (SalesAgentResponse) cache.get();
                        fieldNameMasterDataMap.put(key, object10.getSalesAgentName());
                        break;
                    case CacheConstants.COMMODITY:
                        EntityTransferCommodityType object11 = (EntityTransferCommodityType) cache.get();
                        fieldNameMasterDataMap.put(key, object11.getDescription());
                        break;
                    default:
                }

            }
        });
        return fieldNameMasterDataMap;
    }

    public List<String> createInBulkCurrencyRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CURRENCY_MASTER_DATA))
            {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String currencyCode = (String) field1.get(entityPayload);
                    Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CURRENCIES, currencyCode));
                    if(currencyCode != null && !currencyCode.equals("")) {
                        if (Objects.isNull(cacheValue)) requests.add(currencyCode);
                        fieldNameKeyMap.put(field.getName(), currencyCode);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, EntityTransferCurrency> fetchInCurrencyList(List<String> requests) {
        Map<String, EntityTransferCurrency> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || CurrencyList: {}", LoggerHelper.getRequestIdFromMDC(), requests);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);

            V1DataResponse response = v1Service.fetchCurrenciesData(request);
            List<EntityTransferCurrency> currencyList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
            currencyList.forEach(currency -> {
                keyMasterDataMap.put(currency.getCurrenyCode(), currency);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkTenantsRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field : mainClass.getDeclaredFields()) {
            if (field.isAnnotationPresent(TenantIdData.class)) {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    Long tenantId = null;
                    if(field1.get(entityPayload) != null) {
                        if(!IsStringNullOrEmpty(field1.get(entityPayload).toString()))
                            tenantId = Long.parseLong(field1.get(entityPayload).toString());
                    }
                    if(tenantId != null) {
                        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.TENANTS, StringUtility.convertToString(tenantId)));
                        if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(tenantId));
                        fieldNameKeyMap.put(field.getName(), StringUtility.convertToString(tenantId));
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public Map<String, TenantModel> fetchInTenantsList(List<String> requests) {
        Map<String, TenantModel> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
            log.info("Request: {} || TenantsList: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requests));
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.TENANT_ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.listCousinBranches(request);

            List<TenantModel> tenantModelList = commonUtils.convertToList((List<?>) response.entities, TenantModel.class);
            tenantModelList.forEach(tenantModel -> {
                keyMasterDataMap.put(StringUtility.convertToString(tenantModel.tenantId), tenantModel);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkDGSubstanceRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        for(Field field : mainClass.getDeclaredFields()) {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.DG_SUBSTANCE)) {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    Long dgSubstanceId = null;
                    if(field1.get(entityPayload) != null) {
                        if(!IsStringNullOrEmpty(field1.get(entityPayload).toString()))
                            dgSubstanceId = Long.parseLong(field1.get(entityPayload).toString());
                    }
                    if(dgSubstanceId != null) {
                        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.DG_SUBSTANCES, StringUtility.convertToString(dgSubstanceId)));
                        if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(dgSubstanceId));
                        fieldNameKeyMap.put(field.getName(), StringUtility.convertToString(dgSubstanceId));
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
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

            List<EntityTransferDGSubstance> dgSubstanceList = commonUtils.convertToList((List<?>) response.entities, EntityTransferDGSubstance.class);
            dgSubstanceList.forEach(dgSubstance -> {
                keyMasterDataMap.put(StringUtility.convertToString(dgSubstance.getId()), dgSubstance);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkWareHouseRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

        for(Field field : mainClass.getDeclaredFields()) {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.WARE_HOUSE_DATA))  {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    Long wareHouseId = (Long) field1.get(entityPayload);
                    if(wareHouseId != null) {
                        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.WAREHOUSES, StringUtility.convertToString(wareHouseId)));
                        if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(wareHouseId));
                        fieldNameKeyMap.put(field.getName(), StringUtility.convertToString(wareHouseId));
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
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
            List<WareHouseResponse> wareHousesList = commonUtils.convertToList((List<?>) response.entities, WareHouseResponse.class);
            wareHousesList.forEach(warehouse -> {
                keyMasterDataMap.put(StringUtility.convertToString(warehouse.getId()), warehouse);
            });
        }
        return keyMasterDataMap;
    }

    public List<MasterDataDescriptionResponse> getMasterDataDescription(ShipmentSettingsDetails tenantSetting) throws RunnerException, ClassNotFoundException, IllegalAccessException, NoSuchFieldException {
        ShipmentSettingsDetailsResponse shipmentSettingsDetailsResponse = jsonHelper.convertValue(tenantSetting, ShipmentSettingsDetailsResponse.class);
        List<MasterDataDescriptionResponse> res = new ArrayList<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

        List<MasterListRequest> listRequests = new ArrayList<>(createInBulkMasterListRequest(shipmentSettingsDetailsResponse, ShipmentSettingsDetails.class, fieldNameKeyMap, ShipmentSettingsDetails.class.getSimpleName()));
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
            List<ActivityMasterResponse> activityMasterResponseList = commonUtils.convertToList((List<?>) response.entities, ActivityMasterResponse.class);
            activityMasterResponseList.forEach(activityMaster -> {
                keyMasterDataMap.put(activityMaster.getActivityCode(), activityMaster);
            });
        }
        return keyMasterDataMap;
    }

    public List<String> createInBulkActivityTypeRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

        for(Field field : mainClass.getDeclaredFields()) {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.ACTIVITY_TYPE))  {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    String activityId = (String) field1.get(entityPayload);
                    if(StringUtility.isNotEmpty(activityId)) {
                        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.ACTIVITY_TYPE, activityId));
                        if (Objects.isNull(cacheValue)) requests.add(activityId);
                        fieldNameKeyMap.put(field.getName(), activityId);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public List<String> createInBulkSalesAgentRequest (IRunnerResponse entityPayload, Class mainClass, Map<String, Map<String, String>> fieldNameMainKeyMap, String code) {
        List<String> requests = new ArrayList<>();
        if (Objects.isNull(entityPayload))
            return requests;

        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

        for(Field field : mainClass.getDeclaredFields()) {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.SALES_AGENT))  {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    field1.setAccessible(true);
                    Long salesAgentId = (Long) field1.get(entityPayload);
                    if(salesAgentId != null) {
                        Cache.ValueWrapper cacheValue = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.SALES_AGENT, StringUtility.convertToString(salesAgentId)));
                        if (Objects.isNull(cacheValue)) requests.add(StringUtility.convertToString(salesAgentId));
                        fieldNameKeyMap.put(field.getName(), StringUtility.convertToString(salesAgentId));
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        fieldNameMainKeyMap.put(code, fieldNameKeyMap);
        return requests;
    }

    public MasterListRequest createMasterListRequest(MasterDataType itemType, String itemValue) {
        if (StringUtility.isEmpty(itemValue)) return null;
        return MasterListRequest.builder().ItemType(itemType.getDescription()).ItemValue(itemValue).build();
    }

    public List<MasterListRequest> createMasterListsRequestFromShipment(ShipmentModel shipmentModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, shipmentModel.getPaymentTerms()));
        request.add(createMasterListRequest(MasterDataType.SERVICE_MODE, shipmentModel.getServiceType()));
        request.add(createMasterListRequest(MasterDataType.TRANSPORT_MODE, shipmentModel.getTransportMode()));
        request.add(createMasterListRequest(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipmentModel.getDirection()));
        request.add(createMasterListRequest(MasterDataType.PACKS_UNIT, shipmentModel.getPacksUnit()));
        request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, shipmentModel.getVolumeUnit()));
        request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, shipmentModel.getNetWeightUnit()));
        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(createMasterListRequest(MasterDataType.RELEASE_TYPE, shipmentModel.getAdditionalDetails().getReleaseType()));
        }
        return request;
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
            List<SalesAgentResponse> salesAgentResponseList = commonUtils.convertToList((List<?>) response.entities, SalesAgentResponse.class);
            salesAgentResponseList.forEach(salesAgentResponse -> {
                keyMasterDataMap.put(StringUtility.convertToString(salesAgentResponse.getId()), salesAgentResponse);
            });
        }
        return keyMasterDataMap;
    }

    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        return () -> {
            MDC.setContextMap(mdc);
            RequestAuthContext.setAuthToken(token);
            runnable.run();
        };
    }

    public UnlocationsResponse getUNLocRow(String UNLocCode) {
        if(UNLocCode == null || UNLocCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "=",
                UNLocCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse response = v1Service.fetchUnlocation(commonV1ListRequest);

        List<UnlocationsResponse> unLocationsList = jsonHelper.convertValueToList(response.entities, UnlocationsResponse.class);
        if(unLocationsList.size() > 0)
            return unLocationsList.get(0);
        return null;
    }

    public Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (Objects.isNull(locCodes))
            return locationMap;
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    List.of("LocationsReferenceGUID"),
                    "In",
                    List.of(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {
                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation);
                }
            }
        }
        return locationMap;
    }

    public EntityTransferDGSubstance fetchDgSubstanceRow(Integer dgSubstanceId) {
        var dgSubstanceRow = new EntityTransferDGSubstance();
        if(dgSubstanceId == null)
            return dgSubstanceRow;
        List<Object> criteria = Arrays.asList(List.of("Id"), "=", dgSubstanceId);
        CommonV1ListRequest listRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
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
            List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, value));
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
        Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList)
            dataMap.put(((ShipmentListResponse)response).getId(), (ShipmentListResponse)response);

        if(shipmentDetails != null && shipmentDetails.size() > 0) {
            List<UUID> guidsList = createBillRequest(shipmentDetails);
            if (!guidsList.isEmpty()) {
                ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder()
                        .guidsList(guidsList).build();
                ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
                pushToCache(shipmentBillingListResponse.getData(), CacheConstants.BILLING);
            }

            for (ShipmentDetails details: shipmentDetails) {
                var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).
                        get(keyGenerator.customCacheKeyForMasterData(CacheConstants.BILLING, details.getGuid().toString()));

                if (!Objects.isNull(cache)) {
                    var billingData = (ShipmentBillingListResponse.BillingData) cache.get();
                    if (!Objects.isNull(billingData)) {
                        ShipmentListResponse shipmentListResponse = dataMap.get(details.getId());

                        shipmentListResponse.setBillStatus(billingData.getBillStatus());
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
                        shipmentListResponse.setWayBillNumber(billingData.getWayBillNumber());
                    }

                }
            }
        }
    }

    private List<UUID> createBillRequest(List<ShipmentDetails> shipmentDetails) {
        List<UUID> guidsList = new ArrayList<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        shipmentDetails.forEach(shipment -> {
            Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.BILLING, shipment.getGuid().toString()));
            if (Objects.isNull(value)) guidsList.add(shipment.getGuid());
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
        if (masterDataList != null) {
            for (Object data : (ArrayList<?>) masterDataList) {
                com.dpw.runner.shipment.services.masterdata.dto.MasterData masterDataObject = modelMapper.map(data, com.dpw.runner.shipment.services.masterdata.dto.MasterData.class);
                masterData.add(masterDataObject);
            }
        }
        if (masterData.isEmpty())
            return null;
        return masterData.get(0);
    }

    public String getVesselName(String code) {
        if (StringUtility.isEmpty(code))
            return null;
        var resp = fetchInBulkVessels(Arrays.asList(code));
        return resp.containsKey(code) ? resp.get(code).getName() : null;
    }

    public String getCarrierName(String code) {
        if (StringUtility.isEmpty(code))
            return null;
        var resp = fetchInBulkCarriers(Arrays.asList(code));
        return resp.containsKey(code) ? resp.get(code).getItemDescription() : null;
    }
}
