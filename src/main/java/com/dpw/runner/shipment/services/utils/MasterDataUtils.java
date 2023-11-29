package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.ActivityMasterResponse;
import com.dpw.runner.shipment.services.dto.v1.response.SalesAgentResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
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
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
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
                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value).LocationsReferenceGUID + " " + keyUnlocationDataMap.get(value).NameWoDiacritics);
                    fieldNameUnlocationDataMap.put(key + "_country", keyUnlocationDataMap.get(value).Country);
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
                shipment.getContainersList().forEach(r -> containerTypes.addAll(createInBulkContainerTypeRequest(jsonHelper.convertValue(jsonHelper.convertToJson(r), ContainerResponse.class), Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));
        }

        Map v1Data = fetchInBulkContainerTypes(containerTypes.stream().toList());
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
    public Map<String, EntityTransferMasterLists> fetchInBulkMasterList(List<MasterListRequest> requests) {
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
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
                fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value).LocationsReferenceGUID + " " + keyMasterDataMap.get(value).NameWoDiacritics);
                fieldNameMasterDataMap.put(key + "_country", keyMasterDataMap.get(value).Country);
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

    public Map<String, String> setMasterData (Map<String, String> fieldNameKeyMap, String masterDataType) {
        Map<String, String> fieldNameMasterDataMap = new HashMap<>();
        if (Objects.isNull(fieldNameKeyMap) || fieldNameKeyMap.isEmpty())
            return fieldNameMasterDataMap;

        fieldNameKeyMap.forEach((key, value) -> {
            var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(masterDataType, value));
            if(!Objects.isNull(cache)) {
                switch (masterDataType) {
                    case CacheConstants.UNLOCATIONS:
                        EntityTransferUnLocations object = (EntityTransferUnLocations) cache.get();
                        fieldNameMasterDataMap.put(key, object.LocCode + " " + object.NameWoDiacritics);
                        fieldNameMasterDataMap.put(key + "_country", object.Country);
                        break;
                    case CacheConstants.CONTAINER_TYPE:
                        EntityTransferContainerType object1 = (EntityTransferContainerType) cache.get();
                        fieldNameMasterDataMap.put(key, object1.getDescription());
                        break;
                    case CacheConstants.CHARGE_TYPE:
                        EntityTransferChargeType object2 = (EntityTransferChargeType) cache.get();
                        fieldNameMasterDataMap.put(key, object2.getDescription());
                        break;
                    case CacheConstants.MASTER_LIST:
                        EntityTransferMasterLists object3 = (EntityTransferMasterLists) cache.get();
                        fieldNameMasterDataMap.put(key, object3.getItemDescription());
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

    public List<MasterDataDescriptionResponse> getMasterDataDescription(ShipmentSettingsDetails tenantSetting) throws Exception {
        ShipmentSettingsDetailsResponse shipmentSettingsDetailsResponse = jsonHelper.convertValue(tenantSetting, ShipmentSettingsDetailsResponse.class);
        List<MasterDataDescriptionResponse> res = new ArrayList<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

        List<MasterListRequest> listRequests = new ArrayList<>(createInBulkMasterListRequest(shipmentSettingsDetailsResponse, ShipmentSettingsDetails.class, fieldNameKeyMap, ShipmentSettingsDetails.class.getSimpleName()));
        Map<String, EntityTransferMasterLists> masterListsMap = fetchInBulkMasterList(listRequests);

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

    public Map<String, SalesAgentResponse> fetchInSalesAgentList(List<String> requests) {
        Map<String, SalesAgentResponse> keyMasterDataMap = new HashMap<>();
        if(requests.size() > 0) {
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
}
