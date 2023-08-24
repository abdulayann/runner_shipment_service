package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCurrency;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.lang.reflect.Field;
import java.util.*;

@Slf4j
@Component
public class MasterDataUtils{

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;

    public Map<String, String> carrierMasterData (IRunnerResponse entityPayload, Class baseClass) {
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
            V1DataResponse response = v1Service.fetchCarrierMasterData(request);

            List<EntityTransferCarrier> carrierList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
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
        Map<String, String> fieldNameUnlocationDataMap = new HashMap<>();
        Map<String, String> keyUnlocationDataMap = new HashMap<>();
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
                keyUnlocationDataMap.put(onField == EntityTransferConstants.UNLOCATION_CODE ? unloc.LocCode : unloc.LocationsReferenceGUID, unloc.NameWoDiacritics);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyUnlocationDataMap.containsKey(value))
                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value));
            });
            return fieldNameUnlocationDataMap;
        }
        return null;
    }
}
