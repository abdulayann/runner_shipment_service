package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.lang.reflect.Field;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Component
public class MasterDataUtils{

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;

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
                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value).LocCode + " " + keyUnlocationDataMap.get(value).NameWoDiacritics);
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

    public void setLocationData(List<IRunnerResponse> responseList) {
        Set<String> locCodes = new HashSet<>();
        for (IRunnerResponse response : responseList) {
            if (((CustomerBookingResponse) response).getCarrierDetails() != null) {
                if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getOriginPort())) {
                    locCodes.add(((CustomerBookingResponse) response).getCarrierDetails().getOriginPort());
                }
                if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getDestinationPort())) {
                    locCodes.add(((CustomerBookingResponse) response).getCarrierDetails().getDestinationPort());
                }
                if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getOrigin())) {
                    locCodes.add(((CustomerBookingResponse) response).getCarrierDetails().getOrigin());
                }
                if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getDestination())) {
                    locCodes.add(((CustomerBookingResponse) response).getCarrierDetails().getDestination());
                }
            }
        }

        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList("LocationsReferenceGUID"),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {
                Map<String, String> locationMap = new HashMap<>();
                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation.getLocCode() + " " + unlocation.getNameWoDiacritics());
                }

                for (IRunnerResponse response : responseList) {
                    if (((CustomerBookingResponse) response).getCarrierDetails() != null) {
                        Map<String, String> unlocationData = new HashMap<>();
                        if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getOriginPort())) {
                            unlocationData.put("originPort", locationMap.get(((CustomerBookingResponse) response).getCarrierDetails().getOriginPort()));
                        }
                        if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getDestinationPort())) {
                            unlocationData.put("destinationPort", locationMap.get(((CustomerBookingResponse) response).getCarrierDetails().getDestinationPort()));
                        }
                        if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getOrigin())) {
                            unlocationData.put("origin", locationMap.get(((CustomerBookingResponse) response).getCarrierDetails().getOrigin()));
                        }
                        if (StringUtility.isNotEmpty(((CustomerBookingResponse) response).getCarrierDetails().getDestination())) {
                            unlocationData.put("destination", locationMap.get(((CustomerBookingResponse) response).getCarrierDetails().getDestination()));
                        }
                        ((CustomerBookingResponse) response).getCarrierDetails().setUnlocationData(unlocationData);
                    }
                }
            }
        }
    }
}
