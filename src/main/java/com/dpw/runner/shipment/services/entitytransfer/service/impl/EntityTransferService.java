package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1SaveRequest;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ImportShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import lombok.extern.slf4j.Slf4j;
import org.bouncycastle.pqc.crypto.ExhaustedPrivateKeyException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;


import javax.transaction.Transactional;
import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class EntityTransferService implements IEntityTransferService {
    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;
    @Transactional
    @Override
    public ResponseEntity<?> sendShipment(CommonRequestModel commonRequestModel) {
        SendShipmentRequest sendShipmentRequest = (SendShipmentRequest) commonRequestModel.getData();
        Long shipId = sendShipmentRequest.getShipId();
        List<Integer> sendToBranch = sendShipmentRequest.getSendToBranch();
        List<Long> additionalDocs = sendShipmentRequest.getAdditionalDocs();
        List<String> sendToOrg = sendShipmentRequest.getSendToOrg();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipId);
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for Id {} with Request Id {}", shipId, LoggerHelper.getRequestIdFromMDC());
//            LoggerHelper.debug("Shipment Details is null for Id {}" , shipId);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(shipmentDetails.isPresent()) {
            var fileRepoList = shipmentDetails.get().getFileRepoList().stream().filter(fileRepo -> {
                return additionalDocs.indexOf(fileRepo.getId()) != -1;
            }).collect(Collectors.toList());
            shipmentDetails.get().setFileRepoList(fileRepoList);
            EntityTransferShipmentDetails entityTransferShipmentDetails = modelMapper.map(shipmentDetails.get(), EntityTransferShipmentDetails.class);

            this.createShipmentPayload(entityTransferShipmentDetails);
            log.info("Shipment Payload Created.");

            List<Integer> failedTenantIds = new ArrayList<>();
            List<Integer> successTenantIds = new ArrayList<>();
            this.createTasks(sendToBranch, failedTenantIds, successTenantIds, entityTransferShipmentDetails, shipmentDetails.get(),false);

            List<String> failedOrg = new ArrayList<>();
            List<Integer> tenantIdsFromOrg = tenantIdFromOrganizations(sendToOrg, failedOrg);
            log.info("Org TenantId: "+ tenantIdsFromOrg);
            this.createTasks(tenantIdsFromOrg, failedTenantIds, successTenantIds, entityTransferShipmentDetails, shipmentDetails.get(),true);

            SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds).failedTenantIds(failedTenantIds).failedOrgName(failedOrg).entityTransferShipmentDetails(entityTransferShipmentDetails).build();
            return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
        }
        return ResponseHelper.buildFailedResponse(EntityTransferConstants.SEND_SHIPMENT_NO_SHIPMENT_FOUND +shipId);
    }
    private void createTasks(List<Integer> tenantIdsList, List<Integer> failedTenantIds, List<Integer> successTenantIds, EntityTransferShipmentDetails entityTransferShipmentDetails,ShipmentDetails shipmentDetails, Boolean sendToOrganization){
        for (int tenantId: tenantIdsList) {
            Integer approverRoleId = getShipmentImportApprovalRole(tenantId);
            if(approverRoleId == 0){
                failedTenantIds.add(tenantId);
            }
            else{
                SendEntityResponse response = this.sendTaskToV1(tenantId, approverRoleId, tenantIdsList, entityTransferShipmentDetails, false, entityTransferShipmentDetails.getShipmentId(), entityTransferShipmentDetails.getHouseBill(), entityTransferShipmentDetails.getMasterBill(), shipmentDetails.getId());
                if(response.getIsCreated() == true){
                    successTenantIds.add(tenantId);
                }
            }
        }
    }

    private Integer getShipmentImportApprovalRole(int tenantId) {
        return shipmentSettingsDao.getShipmentConsoleImportApprovarRole(tenantId);
    }

    private List<Integer> tenantIdFromOrganizations (List<String> sendToOrg, List<String> failedOrg) {
        List<String> guidList = new ArrayList<>();
        CommonV1ListRequest orgRequest = new CommonV1ListRequest();
        List<Object> orgCriteria = new ArrayList<>();
        List<Object> orgField = new ArrayList<>(List.of("OrganizationCode"));
        String operator = Operators.IN.getValue();
        orgCriteria.addAll(List.of(orgField, operator, List.of(sendToOrg)));
        orgRequest.setCriteriaRequests(orgCriteria);
        V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
        List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        orgList.forEach(org -> {
            if(org.WhitelistedTenantGUID != null)
                guidList.add(org.WhitelistedTenantGUID);
            else {
                failedOrg.add(org.FullName);
            }
        });
        log.info("Guids list: "+ guidList);

        List<Integer> tenantIds = new ArrayList<>();
        guidList.forEach(guid -> {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
            criteria.addAll(List.of(field, "=", guid));
            request.setCriteriaRequests(criteria);
            TenantIdResponse tenantId = v1Service.tenantByGuid(request);
            tenantIds.add(tenantId.getId());
        });
        return tenantIds;
    }

    private void createShipmentPayload (EntityTransferShipmentDetails entityTransferShipmentDetails) {
        this.addAllMasterDatas(entityTransferShipmentDetails);
        this.addAllUnlocationDatas(entityTransferShipmentDetails);
        this.addDedicatedMasterData(entityTransferShipmentDetails);
    }
    private void addAllMasterDatas (EntityTransferShipmentDetails shipmentDetails) {
        shipmentDetails.setMasterData(addMasterData(shipmentDetails, ShipmentDetails.class));
        shipmentDetails.getAdditionalDetails().setMasterData(addMasterData(shipmentDetails.getAdditionalDetails(), AdditionalDetails.class));
        shipmentDetails.getCarrierDetails().setMasterData(addMasterData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
        var BookingCarriagesList = shipmentDetails.getBookingCarriagesList();
        BookingCarriagesList.forEach(bookingCarriage -> {
            bookingCarriage.setMasterData(addMasterData(bookingCarriage, BookingCarriage.class));
        });
        var containers = shipmentDetails.getContainersList();
        containers.forEach(cont -> {
            cont.setMasterData(addMasterData(cont, Containers.class));
        });
        var packs = shipmentDetails.getPackingList();
        packs.forEach(pack -> {
            pack.setMasterData(addMasterData(pack, Packing.class));
        });
        var referenceNumbers = shipmentDetails.getReferenceNumbersList();
        referenceNumbers.forEach(referenceNumber -> {
            referenceNumber.setMasterData(addMasterData(referenceNumber, ReferenceNumbers.class));
        });
        var serviceDetails = shipmentDetails.getServicesList();
        serviceDetails.forEach(service -> {
            service.setMasterData(addMasterData(service, ServiceDetails.class));
        });
    }

    private Map<String, EntityTransferMasterLists> addMasterData (IEntityTranferBaseEntity entityPayload, Class mainClass) {
        List<MasterListRequest> requests = new ArrayList<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        Map<String, EntityTransferMasterLists> fieldNameMasterDataMap = new HashMap<>();
        for(Field field : mainClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(MasterData.class))
            {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    String itemValue = (String) field1.get(entityPayload);
                    String itemType = field.getDeclaredAnnotation(MasterData.class).type().getDescription();
                    String cascadeField = field.getDeclaredAnnotation(MasterData.class).cascade();
                    String cascade = null;

                    if(!cascadeField.equals("")){
                        Field field2 = entityPayload.getClass().getDeclaredField(cascadeField);
                        cascade = (String) field2.get(entityPayload);
                    }
                    if(itemValue != null) {
                        requests.add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).Cascade(cascade).build());
                        String key = itemValue + '#' + itemType;
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
                String key = masterData.ItemValue + '#' + MasterDataType.masterData(masterData.ItemType).getDescription();
                keyMasterDataMap.put(key, masterData);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyMasterDataMap.containsKey(value))
                    fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value));
            });
            return fieldNameMasterDataMap;
        }
        return null;
    }

    private void addAllUnlocationDatas (EntityTransferShipmentDetails shipmentDetails) {
        shipmentDetails.getAdditionalDetails().setUnlocationData(addUnlocationData(shipmentDetails.getAdditionalDetails(), AdditionalDetails.class));
        shipmentDetails.getCarrierDetails().setUnlocationData(addUnlocationData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
    }

    private Map<String, EntityTransferUnLocations> addUnlocationData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
        Map<String, EntityTransferUnLocations> fieldNameUnlocationDataMap = new HashMap<>();
        Map<String, EntityTransferUnLocations> keyUnlocationDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> locCodesList = new ArrayList<>();
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(UnlocationData.class))
            {
                try {
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
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
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.UNLOCATION_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(locCodesList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchUnlocation(request);

            List<EntityTransferUnLocations> unLocationsList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
            unLocationsList.forEach(unloc -> {
                keyUnlocationDataMap.put(unloc.LocCode, unloc);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyUnlocationDataMap.containsKey(value))
                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value));
            });
            return fieldNameUnlocationDataMap;
        }
        return null;
    }

    private void addDedicatedMasterData (EntityTransferShipmentDetails shipmentDetails) {
        shipmentDetails.getCarrierDetails().setCarrierMasterData(carrierMasterData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
        shipmentDetails.setCurrenciesMasterData(currencyMasterData(shipmentDetails, ShipmentDetails.class));
        var containers = shipmentDetails.getContainersList();
        containers.forEach(cont -> {
            cont.setContainerTypeMasterData(containerTypeMasterData(cont, Containers.class));
            cont.setCommodityTypeMasterData(commodityTypeMasterData(cont, Containers.class));
        });

    }
    private Map<String, EntityTransferCarrier> carrierMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
        Map<String, EntityTransferCarrier> fieldNameCarrierDataMap = new HashMap<>();
        Map<String, EntityTransferCarrier> keyCarrierDataMap = new HashMap<>();
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
                keyCarrierDataMap.put(carrier.getItemValue(), carrier);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCarrierDataMap.containsKey(value))
                    fieldNameCarrierDataMap.put(key, keyCarrierDataMap.get(value));
            });
            return fieldNameCarrierDataMap;
        }
        return null;
    }
    private Map<String, EntityTransferContainerType> containerTypeMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
        Map<String, EntityTransferContainerType> fieldNameContainerTypeDataMap = new HashMap<>();
        Map<String, EntityTransferContainerType> keyContainerTypeDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> containerCodeList = new ArrayList<>();
        log.info("ContainerTypeMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CONTAINER_TYPE_MASTER_DATA))
            {
                try {
                    log.info("ContainerTypeField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    String containerCode = (String) field1.get(entityPayload);
                    if(containerCode != null && !containerCode.equals("")) {
                        containerCodeList.add(containerCode);
                        fieldNameKeyMap.put(field.getName(), containerCode);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(containerCodeList.size() > 0){
            log.info("ContainerTypeList: "+containerCodeList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(containerCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);

            List<EntityTransferContainerType> containerTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerTypeList.forEach(cont -> {
                keyContainerTypeDataMap.put(cont.getCode(), cont);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyContainerTypeDataMap.containsKey(value))
                    fieldNameContainerTypeDataMap.put(key, keyContainerTypeDataMap.get(value));
            });
            return fieldNameContainerTypeDataMap;
        }
        return null;
    }
    private Map<String, EntityTransferCurrency> currencyMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
        Map<String, EntityTransferCurrency> fieldNameCurrencyDataMap = new HashMap<>();
        Map<String, EntityTransferCurrency> keyCurrencyDataMap = new HashMap<>();
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
                keyCurrencyDataMap.put(currency.getCurrenyCode(), currency);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCurrencyDataMap.containsKey(value))
                    fieldNameCurrencyDataMap.put(key, keyCurrencyDataMap.get(value));
            });
            return fieldNameCurrencyDataMap;
        }
        return null;
    }
    private Map<String,EntityTransferCommodityType> commodityTypeMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
        Map<String, EntityTransferCommodityType> fieldNameCommodityTypeDataMap = new HashMap<>();
        Map<String, EntityTransferCommodityType> keyCommodityTypeDataMap = new HashMap<>();
        Map<String, String> fieldNameKeyMap = new HashMap<>();
        List<String> commodityCodeList = new ArrayList<>();
        log.info("CommodityTypeMasterData");
        for(Field field  : baseClass.getDeclaredFields())
        {
            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.COMMODITY_TYPE_MASTER_DATA))
            {
                try {
                    log.info("commodityTypeField: "+field.getName());
                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
                    String code = (String) field1.get(entityPayload);
                    if(code != null && !code.equals("")) {
                        commodityCodeList.add(code);
                        fieldNameKeyMap.put(field.getName(), code);
                    }
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        if(commodityCodeList.size() > 0){
            log.info("commodityTypeList: "+commodityCodeList);
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(commodityCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);

            List<EntityTransferCommodityType> commodityTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            commodityTypeList.forEach(commodity -> {
                keyCommodityTypeDataMap.put(commodity.getCode(), commodity);
            });
            fieldNameKeyMap.forEach((key, value) -> {
                if(keyCommodityTypeDataMap.containsKey(value))
                    fieldNameCommodityTypeDataMap.put(key, keyCommodityTypeDataMap.get(value));
            });
            return fieldNameCommodityTypeDataMap;
        }
        return null;
    }


    private SendEntityResponse sendTaskToV1 (int tenantId, int approverRole, List<Integer> tenantIds, EntityTransferShipmentDetails entityTransferShipmentDetails, Boolean sendToOrganization, String shipmentId, String houseBill, String masterBill, long id) {
        CreateShipmentTaskRequest createShipmentTaskRequest = CreateShipmentTaskRequest.builder()
                .tenantId(tenantId).approverRole(approverRole).tenantIds(tenantIds)
                .shipmentData(entityTransferShipmentDetails).sendToOrganization(sendToOrganization)
                .shipmentId(shipmentId).houseBill(houseBill).masterBill(masterBill).id(id).build();
        SendEntityResponse response = v1Service.sendShipmentTask(createShipmentTaskRequest);
        return response;
    }

    @Override
    public ResponseEntity<?> sendConsolidation(CommonRequestModel commonRequestModel) {
        return null;
    }
    @Transactional
    @Override
    public ResponseEntity<?> importShipment (CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        ImportShipmentRequest importShipmentRequest = (ImportShipmentRequest) commonRequestModel.getData();
        EntityTransferShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityTransferShipmentDetails();
        String ShipmentId = null;
        try {
            ShipmentId =  this.createShipment(entityTransferShipmentDetails);
            this.createShipmentMasterData(entityTransferShipmentDetails);
            return ResponseHelper.buildSuccessResponse(ImportShipmentResponse.builder().ShipmentId(ShipmentId).build());

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
//    @Transactional
    private String createShipment (EntityTransferShipmentDetails entityTransferShipmentDetails) throws Exception{
        ShipmentRequest request = jsonHelper.convertValue(entityTransferShipmentDetails, ShipmentRequest.class);

        String Hbl = request.getHouseBill();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByHouseBill(Hbl);
        if(shipmentDetails.isPresent()){
            ShipmentDetails shipDetails = shipmentDetails.get();
            log.info("Update payload: "+shipDetails.toString());
            modelMapper.map(entityTransferShipmentDetails, shipDetails);
//            request = jsonHelper.convertValue(shipDetails, ShipmentRequest.class);
            try {
                ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> response = (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.completeUpdate(CommonRequestModel.buildRequest(request));
                log.info("Update payload: "+request);
                return response.getBody().getData().getShipmentId();
            } catch (Exception e) {
                throw new Exception(e);
            }
        }else {
            ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> response = (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.create(CommonRequestModel.buildRequest(request));
            return response.getBody().getData().getShipmentId();
        }
    }
    @Transactional
    private void createShipmentMasterData (EntityTransferShipmentDetails entityTransferShipmentDetails) {
        this.createAllMasterData(entityTransferShipmentDetails);
        this.createAllUnlocationData(entityTransferShipmentDetails);
        this.createAllDedicatedMasterData(entityTransferShipmentDetails);
    }
    @Transactional
    private void createAllMasterData (EntityTransferShipmentDetails entityTransferShipmentDetails) {
        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        if(entityTransferShipmentDetails.getMasterData() != null)
            masterDataList.addAll(entityTransferShipmentDetails.getMasterData().values());
        if(entityTransferShipmentDetails.getAdditionalDetails().getMasterData() != null)
            masterDataList.addAll(entityTransferShipmentDetails.getAdditionalDetails().getMasterData().values());
        if(entityTransferShipmentDetails.getCarrierDetails().getMasterData() != null)
            masterDataList.addAll(entityTransferShipmentDetails.getCarrierDetails().getMasterData().values());

        var BookingCarriagesList = entityTransferShipmentDetails.getBookingCarriagesList();
        BookingCarriagesList.forEach(bookingCarriage -> {
            masterDataList.addAll(bookingCarriage.getMasterData().values());
        });
        var containers = entityTransferShipmentDetails.getContainersList();
        containers.forEach(cont -> {
            if(cont.getMasterData() != null)
                masterDataList.addAll(cont.getMasterData().values());
        });
        var packs = entityTransferShipmentDetails.getPackingList();
        packs.forEach(pack -> {
            if(pack.getMasterData() != null)
                masterDataList.addAll(pack.getMasterData().values());
        });
        var referenceNumbers = entityTransferShipmentDetails.getReferenceNumbersList();
        referenceNumbers.forEach(referenceNumber -> {
            if(referenceNumber.getMasterData() != null)
                masterDataList.addAll(referenceNumber.getMasterData().values());
        });
        var serviceDetails = entityTransferShipmentDetails.getServicesList();
        serviceDetails.forEach(service -> {
            if(service.getMasterData() != null)
                masterDataList.addAll(service.getMasterData().values());
        });

        this.createMasterData(masterDataList);
    }
    @Transactional
    private void createMasterData (List<EntityTransferMasterLists> masterData) {
        List<MasterListRequest> masterListRequest = new ArrayList<>();
        Set<String> masterDataKey = new HashSet<>();
        masterData.forEach(value -> {
            masterListRequest.add(MasterListRequest.builder().ItemType(MasterDataType.masterData(value.ItemType).getDescription()).ItemValue(value.ItemValue).Cascade(value.Cascade).build());
            String key = MasterDataType.masterData(value.ItemType).getDescription() + '#' + value.getItemValue();
            masterDataKey.add(key);
        });
        if (masterListRequest.size() > 0) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(masterListRequest);
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            masterLists.forEach(val -> {
                String key = MasterDataType.masterData(val.ItemType).getDescription() + '#' + val.getItemValue();
                if(masterDataKey.contains(key))
                    masterDataKey.remove(key);
            });
        }
        masterData.forEach(value -> {
            String key = MasterDataType.masterData(value.ItemType).getDescription() + '#' + value.getItemValue();
            if (masterDataKey.contains(key)) {
                V1SaveRequest save = V1SaveRequest.builder().Entity(value).build();
                V1DataResponse response = v1Service.createMasterData(save);
            }
        });
    }
    @Transactional
    private void createAllUnlocationData (EntityTransferShipmentDetails entityTransferShipmentDetails) {
        List<EntityTransferUnLocations> unLocationsList = new ArrayList<>();
        if(entityTransferShipmentDetails.getAdditionalDetails().getUnlocationData() != null)
            unLocationsList.addAll(entityTransferShipmentDetails.getAdditionalDetails().getUnlocationData().values());
        if(entityTransferShipmentDetails.getCarrierDetails().getUnlocationData() != null)
            unLocationsList.addAll(entityTransferShipmentDetails.getCarrierDetails().getUnlocationData().values());

        this.createUnlocationData(unLocationsList);
    }
    @Transactional
    private void createUnlocationData (List<EntityTransferUnLocations> unlocationData) {
        Set<String> locCodesList = new HashSet<>();
        locCodesList.addAll(unlocationData.stream().map(x->x.getLocCode()).collect(Collectors.toSet()));
        if (locCodesList.size() > 0) {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.UNLOCATION_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(locCodesList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchUnlocation(request);
            List<EntityTransferUnLocations> unlocationDataList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
            locCodesList.removeAll(unlocationDataList.stream().map(x->x.getLocCode()).collect(Collectors.toSet()));
        }

        unlocationData.forEach(unlocData -> {
            if(locCodesList.contains(unlocData.getLocCode())){
                V1SaveRequest save = V1SaveRequest.builder().Entity(unlocData).build();
                log.info("Create Unlocation: "+save);
                V1DataResponse response = v1Service.createUnlocationData(save);
            }
        });
    }
    @Transactional
    private void createAllDedicatedMasterData (EntityTransferShipmentDetails entityTransferShipmentDetails) {
        List<EntityTransferCarrier> carrierList = new ArrayList<>();
        List<EntityTransferContainerType> containerTypeList = new ArrayList<>();
        List<EntityTransferCurrency> currencyList = new ArrayList<>();
        List<EntityTransferCommodityType> commodityTypeList = new ArrayList<>();

        if(entityTransferShipmentDetails.getCarrierDetails().getCarrierMasterData() != null)
            carrierList.addAll(entityTransferShipmentDetails.getCarrierDetails().getCarrierMasterData().values());
        if(entityTransferShipmentDetails.getCurrenciesMasterData() != null)
            currencyList.addAll(entityTransferShipmentDetails.getCurrenciesMasterData().values());

        var containers = entityTransferShipmentDetails.getContainersList();
        containers.forEach(cont -> {
            if(cont.getContainerTypeMasterData() != null)
                containerTypeList.addAll(cont.getContainerTypeMasterData().values());
            if(cont.getCommodityTypeMasterData() != null)
                commodityTypeList.addAll(cont.getCommodityTypeMasterData().values());
        });

        this.createCarrierMasterData(carrierList);
        this.createContainerTypeMasterData(containerTypeList);
        this.createCurrencyMasterData(currencyList);
        this.createCommodityTypeMasterData(commodityTypeList);

    }
    @Transactional
    private void createCarrierMasterData (List<EntityTransferCarrier> carrierData) {
        Set<String> itemValueList = new HashSet<>();
        itemValueList.addAll(carrierData.stream().map(x->x.getItemValue()).collect(Collectors.toSet()));
        if (itemValueList.size() > 0) {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCarrierMasterData(request);
            List<EntityTransferCarrier> carrierDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
            itemValueList.removeAll(carrierDataList.stream().map(x->x.getItemValue()).collect(Collectors.toSet()));
        }

        carrierData.forEach(carrier -> {
            if(itemValueList.contains(carrier.getItemValue())){
                V1SaveRequest save = V1SaveRequest.builder().Entity(carrier).build();
                V1DataResponse response = v1Service.createCarrierMasterData(save);
            }
        });
    }
    @Transactional
    private void createContainerTypeMasterData (List<EntityTransferContainerType> containerData) {
        Set<String> containerCodeList = new HashSet<>();
        containerCodeList.addAll(containerData.stream().map(x->x.getCode()).collect(Collectors.toSet()));
        if (containerCodeList.size() > 0) {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(containerCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchContainerTypeData(request);
            List<EntityTransferContainerType> containerTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
            containerCodeList.removeAll(containerTypeList.stream().map(x->x.getCode()).collect(Collectors.toSet()));
        }

        containerData.forEach(cont -> {
            if(containerCodeList.contains(cont.getCode())){
                V1SaveRequest save = V1SaveRequest.builder().Entity(cont).build();
                V1DataResponse response = v1Service.createContainerTypeData(save);
            }
        });
    }
    @Transactional
    private void createCurrencyMasterData (List<EntityTransferCurrency> currencyData) {
        Set<String> currCodeList = new HashSet<>();
        currCodeList.addAll(currencyData.stream().map(x->x.getCurrenyCode()).collect(Collectors.toSet()));
        if (currCodeList.size() > 0) {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(currCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCurrenciesData(request);
            List<EntityTransferCurrency> currencyDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
            currCodeList.removeAll(currencyDataList.stream().map(x->x.getCurrenyCode()).collect(Collectors.toSet()));
        }

        currencyData.forEach(curr -> {
            if(currCodeList.contains(curr.getCurrenyCode())){
                V1SaveRequest save = V1SaveRequest.builder().Entity(curr).build();
                V1DataResponse response = v1Service.createCurrenciesData(save);
            }
        });
    }
    @Transactional
    private void createCommodityTypeMasterData (List<EntityTransferCommodityType> commodityData) {
        Set<String> commodityCodeList = new HashSet<>();
        commodityCodeList.addAll(commodityData.stream().map(x->x.getCode()).collect(Collectors.toSet()));
        if (commodityCodeList.size() > 0) {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> criteria = new ArrayList<>();
            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
            String operator = Operators.IN.getValue();
            criteria.addAll(List.of(field, operator, List.of(commodityCodeList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse response = v1Service.fetchCommodityData(request);
            List<EntityTransferCommodityType> commodityDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
            commodityCodeList.removeAll(commodityDataList.stream().map(x->x.getCode()).collect(Collectors.toSet()));
        }

        commodityData.forEach(commodity -> {
            if(commodityCodeList.contains(commodity.getCode())){
                V1SaveRequest save = V1SaveRequest.builder().Entity(commodity).build();
                V1DataResponse response = v1Service.createCommodityData(save);
            }
        });
    }


    @Override
    public ResponseEntity<?> importConsolidation (CommonRequestModel commonRequestModel) {
        return null;
    }

}
