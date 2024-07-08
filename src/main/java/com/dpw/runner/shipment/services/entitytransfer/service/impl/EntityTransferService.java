package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.*;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.google.common.base.Strings;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class EntityTransferService implements IEntityTransferService {
    public static final String SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Shipment Details is null for Id {} with Request Id {}";
    public static final String CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Consolidation Details is null for Id {} with Request Id {}";
    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    private IConsolidationService consolidationService;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private TenantContext tenantContext;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IHblDao hblDao;
    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private IEventDao eventDao;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private ILogsHistoryService logsHistoryService;
    @Autowired
    MasterDataFactory masterDataFactory;
    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> sendShipment(CommonRequestModel commonRequestModel) {
        SendShipmentRequest sendShipmentRequest = (SendShipmentRequest) commonRequestModel.getData();
        Long shipId = sendShipmentRequest.getShipId();
        List<Integer> sendToBranch = sendShipmentRequest.getSendToBranch();
        List<String> additionalDocs = sendShipmentRequest.getAdditionalDocs();
        List<String> sendToOrg = sendShipmentRequest.getSendToOrg();
        if((sendToBranch == null || sendToBranch.size() == 0) && (sendToOrg == null || sendToOrg.size() == 0)){
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipId);
        if (!shipmentDetails.isPresent()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, shipId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipment = shipmentDetails.get();
//            if(additionalDocs != null) {
//                var fileRepoList = shipmentDetails.get().getFileRepoList().stream().filter(fileRepo -> {
//                    return additionalDocs.indexOf(fileRepo.getId()) != -1;
//                }).toList();
//                shipmentDetails.get().setFileRepoList(fileRepoList);
//            } else {
//                shipmentDetails.get().setFileRepoList(null);
//            }
        List<Integer> successTenantIds = new ArrayList<>();
        // TODO Only V1 Shipment Task is triggered for current requirement
        if(true) {
            List<Integer> tenantIdsFromOrg = new ArrayList<>();
            if(sendToOrg != null && !sendToOrg.isEmpty())
                tenantIdsFromOrg = tenantIdFromOrganizations(sendToOrg);
            CreateV1ShipmentTaskFromV2Request request = CreateV1ShipmentTaskFromV2Request.builder()
                    .shipmentId(shipmentDetails.get().getShipmentId())
                    .sendToBranch(sendToBranch)
                    .sendToOrg(sendToOrg)
                    .additionalDocs(additionalDocs)
                    .build();
            log.info("Entity Transfer V1 Shipment Request Created:" + jsonHelper.convertToJson(request));
            try {
                SendEntityResponse v1ShipmentTaskResponse = v1Service.sendV1ShipmentTask(request);
                if (v1ShipmentTaskResponse.getIsCreated()) {
                    if(sendToBranch != null && !sendToBranch.isEmpty())
                        successTenantIds.addAll(sendToBranch);
                    if(sendToOrg != null && !sendToOrg.isEmpty()) {
                        successTenantIds.addAll(tenantIdsFromOrg);
                    }
                } else {
                    log.error("Entity Transfer failed Send V1 shipment: " + v1ShipmentTaskResponse.getError());
                    throw new RuntimeException(v1ShipmentTaskResponse.getError());
                }
            } catch (Exception ex) {
                log.error("Entity Transfer failed Send V1 shipment: " + ex);
                throw new RuntimeException(ex.getMessage());
            }
        } else {
//                EntityTransferShipmentDetails entityTransferShipmentDetails = modelMapper.map(shipmentDetails.get(), EntityTransferShipmentDetails.class);
//
//                this.createShipmentPayload(entityTransferShipmentDetails);
//                log.info("Shipment Payload Created.");
//
//
//                if (sendToBranch != null && sendToBranch.size() != 0) {
//                    this.createTasks(sendToBranch, successTenantIds, entityTransferShipmentDetails, shipmentDetails.get(), false);
//                }
//
//                if (sendToOrg != null && sendToOrg.size() != 0) {
//                    List<Integer> tenantIdsFromOrg = tenantIdFromOrganizations(sendToOrg);
//                    log.info("Org TenantId: " + tenantIdsFromOrg);
//                    if (tenantIdsFromOrg != null && tenantIdsFromOrg.size() != 0) {
//                        this.createTasks(tenantIdsFromOrg, successTenantIds, entityTransferShipmentDetails, shipmentDetails.get(), true);
//                    }
//                }
        }

        List<String> tenantName = getTenantName(successTenantIds);
        createSendEvent(tenantName, shipment.getReceivingBranch(), shipment.getTriangulationPartner(), shipment.getDocumentationPartner(), shipId.toString(), Constants.SHIPMENT_SENT, Constants.SHIPMENT, null);
        if(Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
            shipmentDao.saveEntityTransfer(shipId, Boolean.TRUE);

        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds).build();
        return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
    }
//    private void createTasks(List<Integer> tenantIdsList, List<Integer> successTenantIds, EntityTransferShipmentDetails entityTransferShipmentDetails,ShipmentDetails shipmentDetails, Boolean sendToOrganization){
//        for (int tenantId: tenantIdsList) {
//            Integer approverRoleId = getShipmentConsoleImportApprovalRole(tenantId);
//            if(approverRoleId == null || approverRoleId == 0){
//                throw new RuntimeException(EntityTransferConstants.APPROVAL_ROLE_NOT_ASSIGNED + tenantId);
//            }
//            else{
//                SendEntityResponse response = this.sendTaskToV1(tenantId, approverRoleId, tenantIdsList, entityTransferShipmentDetails, sendToOrganization, entityTransferShipmentDetails.getShipmentId(), entityTransferShipmentDetails.getHouseBill(), entityTransferShipmentDetails.getMasterBill(), shipmentDetails.getId());
//                if(response.getIsCreated() == true){
//                    successTenantIds.add(tenantId);
//                }
//            }
//        }
//    }

//    private Integer getShipmentConsoleImportApprovalRole(int tenantId) {
//        return shipmentSettingsDao.getShipmentConsoleImportApprovarRole(tenantId);
//    }

    private List<Integer> tenantIdFromOrganizations (List<String> sendToOrg) {
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
                throw new ValidationException("No WhiteListedGuid is attached with org: " + org.FullName);
            }
        });
        log.info("Guids list: "+ guidList);

        List<Integer> tenantIds = new ArrayList<>();
        if(guidList != null || guidList.size() != 0) {
            guidList.forEach(guid -> {
                CommonV1ListRequest request = new CommonV1ListRequest();
                List<Object> criteria = new ArrayList<>();
                List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
                criteria.addAll(List.of(field, "=", guid));
                request.setCriteriaRequests(criteria);
                TenantIdResponse tenantId = v1Service.tenantByGuid(request);
                tenantIds.add(tenantId.getId());
            });
        }
        return tenantIds;
    }

//    private void createShipmentPayload (EntityTransferShipmentDetails entityTransferShipmentDetails) {
//        this.addAllMasterDatas(entityTransferShipmentDetails);
//        this.addAllUnlocationDatas(entityTransferShipmentDetails);
//        this.addDedicatedMasterData(entityTransferShipmentDetails);
//    }
//    private void addAllMasterDatas (EntityTransferShipmentDetails shipmentDetails) {
//        if(shipmentDetails != null) {
//            shipmentDetails.setMasterData(addMasterData(shipmentDetails, ShipmentDetails.class));
//        }
//        if(shipmentDetails != null && shipmentDetails.getAdditionalDetails() != null) {
//            shipmentDetails.getAdditionalDetails().setMasterData(addMasterData(shipmentDetails.getAdditionalDetails(), AdditionalDetails.class));
//        }
//        if(shipmentDetails != null && shipmentDetails.getCarrierDetails() != null) {
//            shipmentDetails.getCarrierDetails().setMasterData(addMasterData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//        var bookingCarriagesList = shipmentDetails != null ? shipmentDetails.getBookingCarriagesList() : null;
//        if(bookingCarriagesList != null) {
//            bookingCarriagesList.forEach(bookingCarriage -> {
//                bookingCarriage.setMasterData(addMasterData(bookingCarriage, BookingCarriage.class));
//            });
//        }
//        var containers = shipmentDetails != null ? shipmentDetails.getContainersList() : null;
//        if(containers != null) {
//            containers.forEach(cont -> {
//                cont.setMasterData(addMasterData(cont, Containers.class));
//            });
//        }
//        var packs = shipmentDetails != null ? shipmentDetails.getPackingList() : null;
//        if(packs != null) {
//            packs.forEach(pack -> {
//                pack.setMasterData(addMasterData(pack, Packing.class));
//            });
//        }
//        var referenceNumbers = shipmentDetails != null ? shipmentDetails.getReferenceNumbersList(): null;
//        if (referenceNumbers != null) {
//            referenceNumbers.forEach(referenceNumber -> {
//                referenceNumber.setMasterData(addMasterData(referenceNumber, ReferenceNumbers.class));
//            });
//        }
//        var serviceDetails = shipmentDetails != null ? shipmentDetails.getServicesList() : null;
//        if (serviceDetails != null) {
//            serviceDetails.forEach(service -> {
//                service.setMasterData(addMasterData(service, ServiceDetails.class));
//            });
//        }
//    }
//
//    private Map<String, EntityTransferMasterLists> addMasterData (IEntityTranferBaseEntity entityPayload, Class mainClass) {
//        MasterListRequestV2 requests = new MasterListRequestV2();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
//        Map<String, EntityTransferMasterLists> fieldNameMasterDataMap = new HashMap<>();
//
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field : mainClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(MasterData.class) && allFields.contains(field.getName()))
//            {
//                try {
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String itemValue = (String) field1.get(entityPayload);
//                    String itemType = field.getDeclaredAnnotation(MasterData.class).type().getDescription();
//                    String cascadeField = field.getDeclaredAnnotation(MasterData.class).cascade();
//                    String cascade = null;
//
//                    if(!cascadeField.equals("")){
//                        Field field2 = entityPayload.getClass().getDeclaredField(cascadeField);
//                        field2.setAccessible(true);
//                        cascade = (String) field2.get(entityPayload);
//                    }
//                    if(itemValue != null) {
//                        requests.getMasterListRequests().add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).Cascade(cascade).build());
//                        String key = itemValue + '#' + itemType;
//                        fieldNameKeyMap.put(field.getName(), key);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(requests.getMasterListRequests().size() > 0) {
//            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
//            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
//            masterLists.forEach(masterData -> {
//                String key = masterData.ItemValue + '#' + MasterDataType.masterData(masterData.ItemType).getDescription();
//                keyMasterDataMap.put(key, masterData);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyMasterDataMap.containsKey(value))
//                    fieldNameMasterDataMap.put(key, keyMasterDataMap.get(value));
//            });
//            return fieldNameMasterDataMap;
//        }
//        return null;
//    }
//
//    private void addAllUnlocationDatas (EntityTransferShipmentDetails shipmentDetails) {
//        if(shipmentDetails.getAdditionalDetails() != null) {
//            shipmentDetails.getAdditionalDetails().setUnlocationData(addUnlocationData(shipmentDetails.getAdditionalDetails(), AdditionalDetails.class));
//        }
//        if(shipmentDetails.getCarrierDetails() != null) {
//            shipmentDetails.getCarrierDetails().setUnlocationData(addUnlocationData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//    }
//
//    private Map<String, EntityTransferUnLocations> addUnlocationData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferUnLocations> fieldNameUnlocationDataMap = new HashMap<>();
//        Map<String, EntityTransferUnLocations> keyUnlocationDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> locCodesList = new ArrayList<>();
//
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(UnlocationData.class) && allFields.contains(field.getName()))
//            {
//                try {
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String locCode = (String) field1.get(entityPayload);
//                    if(locCode != null && !locCode.equals("")) {
//                        locCodesList.add(locCode);
//                        fieldNameKeyMap.put(field.getName(), locCode);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(locCodesList.size() > 0){
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.UNLOCATION_CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(locCodesList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchUnlocation(request);
//
//            List<EntityTransferUnLocations> unLocationsList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
//            unLocationsList.forEach(unloc -> {
//                keyUnlocationDataMap.put(unloc.LocationsReferenceGUID, unloc);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyUnlocationDataMap.containsKey(value))
//                    fieldNameUnlocationDataMap.put(key, keyUnlocationDataMap.get(value));
//            });
//            return fieldNameUnlocationDataMap;
//        }
//        return null;
//    }
//
//    private void addDedicatedMasterData (EntityTransferShipmentDetails shipmentDetails) {
//        if (shipmentDetails.getCarrierDetails() != null) {
//            shipmentDetails.getCarrierDetails().setCarrierMasterData(carrierMasterData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
//            shipmentDetails.getCarrierDetails().setVesselsMasterData(vesselMasterData(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//        if(shipmentDetails != null) {
//            shipmentDetails.setCurrenciesMasterData(currencyMasterData(shipmentDetails, ShipmentDetails.class));
//        }
//        var containers = shipmentDetails.getContainersList();
//        if(containers != null) {
//            containers.forEach(cont -> {
//                cont.setContainerTypeMasterData(containerTypeMasterData(cont, Containers.class));
//                cont.setCommodityTypeMasterData(commodityTypeMasterData(cont, Containers.class));
//            });
//        }
//        if(shipmentDetails.getBookingCarriagesList() != null) {
//            shipmentDetails.getBookingCarriagesList().forEach(bookingCarriage -> {
//                bookingCarriage.setVesselsMasterData(vesselMasterData(bookingCarriage, BookingCarriage.class));
//            });
//        }
//
//    }
//    private Map<String, EntityTransferCarrier> carrierMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferCarrier> fieldNameCarrierDataMap = new HashMap<>();
//        Map<String, EntityTransferCarrier> keyCarrierDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> itemValueList = new ArrayList<>();
//        log.info("CarrierMasterData");
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CARRIER_MASTER_DATA) && allFields.contains(field.getName()))
//            {
//                try {
//                    log.info("CarrierField: "+field.getName());
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String itemValue = (String) field1.get(entityPayload);
//                    if(itemValue != null && !itemValue.equals("")) {
//                        itemValueList.add(itemValue);
//                        fieldNameKeyMap.put(field.getName(), itemValue);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(itemValueList.size() > 0){
//            log.info("CarrierList: "+itemValueList);
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
//            request.setCriteriaRequests(criteria);
//            CarrierListObject carrierListObject = new CarrierListObject();
//            carrierListObject.setListObject(request);
//            V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
//
//            List<EntityTransferCarrier> carrierList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
//            carrierList.forEach(carrier -> {
//                keyCarrierDataMap.put(carrier.getItemValue(), carrier);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyCarrierDataMap.containsKey(value))
//                    fieldNameCarrierDataMap.put(key, keyCarrierDataMap.get(value));
//            });
//            return fieldNameCarrierDataMap;
//        }
//        return null;
//    }
//    private Map<String, EntityTransferVessels> vesselMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferVessels> fieldNameVesselDataMap = new HashMap<>();
//        Map<String, EntityTransferVessels> keyVesselDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> GuidList = new ArrayList<>();
//        log.info("VesselMasterData");
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.VESSEL_MASTER_DATA) && allFields.contains(field.getName()))
//            {
//                try {
//                    log.info("VesselField: "+field.getName());
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String Guid = (String) field1.get(entityPayload);
//                    if(Guid != null && !Guid.equals("")) {
//                        GuidList.add(Guid);
//                        fieldNameKeyMap.put(field.getName(), Guid);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(GuidList.size() > 0){
//            log.info("VesselList: "+GuidList);
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(GuidList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchVesselData(request);
//
//            List<EntityTransferVessels> vesselList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
//            vesselList.forEach(vessel -> {
//                keyVesselDataMap.put(vessel.getGuid().toString(), vessel);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyVesselDataMap.containsKey(value))
//                    fieldNameVesselDataMap.put(key, keyVesselDataMap.get(value));
//            });
//            return fieldNameVesselDataMap;
//        }
//        return null;
//    }
//    private Map<String, EntityTransferContainerType> containerTypeMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferContainerType> fieldNameContainerTypeDataMap = new HashMap<>();
//        Map<String, EntityTransferContainerType> keyContainerTypeDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> containerCodeList = new ArrayList<>();
//        log.info("ContainerTypeMasterData");
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CONTAINER_TYPE_MASTER_DATA) && allFields.contains(field.getName()))
//            {
//                try {
//                    log.info("ContainerTypeField: "+field.getName());
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String containerCode = (String) field1.get(entityPayload);
//                    if(containerCode != null && !containerCode.equals("")) {
//                        containerCodeList.add(containerCode);
//                        fieldNameKeyMap.put(field.getName(), containerCode);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(containerCodeList.size() > 0){
//            log.info("ContainerTypeList: "+containerCodeList);
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(containerCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchContainerTypeData(request);
//
//            List<EntityTransferContainerType> containerTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
//            containerTypeList.forEach(cont -> {
//                keyContainerTypeDataMap.put(cont.getCode(), cont);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyContainerTypeDataMap.containsKey(value))
//                    fieldNameContainerTypeDataMap.put(key, keyContainerTypeDataMap.get(value));
//            });
//            return fieldNameContainerTypeDataMap;
//        }
//        return null;
//    }
//    private Map<String, EntityTransferCurrency> currencyMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferCurrency> fieldNameCurrencyDataMap = new HashMap<>();
//        Map<String, EntityTransferCurrency> keyCurrencyDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> currencyCodeList = new ArrayList<>();
//        log.info("CurrencyMasterData");
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.CURRENCY_MASTER_DATA) && allFields.contains(field.getName()))
//            {
//                try {
//                    log.info("CurrencyField: "+field.getName());
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String currencyCode = (String) field1.get(entityPayload);
//                    if(currencyCode != null && !currencyCode.equals("")) {
//                        currencyCodeList.add(currencyCode);
//                        fieldNameKeyMap.put(field.getName(), currencyCode);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(currencyCodeList.size() > 0){
//            log.info("CurrencyList: "+currencyCodeList);
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(currencyCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchCurrenciesData(request);
//
//            List<EntityTransferCurrency> currencyList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
//            currencyList.forEach(currency -> {
//                keyCurrencyDataMap.put(currency.getCurrenyCode(), currency);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyCurrencyDataMap.containsKey(value))
//                    fieldNameCurrencyDataMap.put(key, keyCurrencyDataMap.get(value));
//            });
//            return fieldNameCurrencyDataMap;
//        }
//        return null;
//    }
//    private Map<String,EntityTransferCommodityType> commodityTypeMasterData (IEntityTranferBaseEntity entityPayload, Class baseClass) {
//        Map<String, EntityTransferCommodityType> fieldNameCommodityTypeDataMap = new HashMap<>();
//        Map<String, EntityTransferCommodityType> keyCommodityTypeDataMap = new HashMap<>();
//        Map<String, String> fieldNameKeyMap = new HashMap<>();
//        List<String> commodityCodeList = new ArrayList<>();
//        log.info("CommodityTypeMasterData");
//        Set<String> allFields = Arrays.stream(entityPayload.getClass().getDeclaredFields()).map(Field::getName).collect(Collectors.toSet());
//        for(Field field  : baseClass.getDeclaredFields())
//        {
//            if (field.isAnnotationPresent(DedicatedMasterData.class) && field.getDeclaredAnnotation(DedicatedMasterData.class).type().equals(Constants.COMMODITY_TYPE_MASTER_DATA) && allFields.contains(field.getName()))
//            {
//                try {
//                    log.info("commodityTypeField: "+field.getName());
//                    Field field1 = entityPayload.getClass().getDeclaredField(field.getName());
//                    field1.setAccessible(true);
//                    String code = (String) field1.get(entityPayload);
//                    if(code != null && !code.equals("")) {
//                        commodityCodeList.add(code);
//                        fieldNameKeyMap.put(field.getName(), code);
//                    }
//                } catch (Exception e) {
//                    throw new RuntimeException(e);
//                }
//            }
//        }
//        if(commodityCodeList.size() > 0){
//            log.info("commodityTypeList: "+commodityCodeList);
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(commodityCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchCommodityData(request);
//
//            List<EntityTransferCommodityType> commodityTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
//            commodityTypeList.forEach(commodity -> {
//                keyCommodityTypeDataMap.put(commodity.getCode(), commodity);
//            });
//            fieldNameKeyMap.forEach((key, value) -> {
//                if(keyCommodityTypeDataMap.containsKey(value))
//                    fieldNameCommodityTypeDataMap.put(key, keyCommodityTypeDataMap.get(value));
//            });
//            return fieldNameCommodityTypeDataMap;
//        }
//        return null;
//    }
//
//
//    private SendEntityResponse sendTaskToV1 (int tenantId, int approverRole, List<Integer> tenantIds, EntityTransferShipmentDetails entityTransferShipmentDetails, Boolean sendToOrganization, String shipmentId, String houseBill, String masterBill, long id) {
//        CreateShipmentTaskRequest createShipmentTaskRequest = CreateShipmentTaskRequest.builder()
//                .tenantId(tenantId).approverRole(approverRole).tenantIds(tenantIds)
//                .shipmentData(entityTransferShipmentDetails).sendToOrganization(sendToOrganization)
//                .shipmentId(shipmentId).houseBill(houseBill).masterBill(masterBill).id(id).build();
//        SendEntityResponse response = v1Service.sendShipmentTask(createShipmentTaskRequest);
//        return response;
//    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> sendConsolidation(CommonRequestModel commonRequestModel) {
        SendConsolidationRequest sendConsolidationRequest = (SendConsolidationRequest) commonRequestModel.getData();
        Long consolId = sendConsolidationRequest.getConsolId();
        List<Integer> sendToBranch = sendConsolidationRequest.getSendToBranch();
        List<String> additionalDocs = sendConsolidationRequest.getAdditionalDocs();
        Map<String, List<String>> shipAdditionalDocs = sendConsolidationRequest.getShipAdditionalDocs();
        List<String> sendToOrg = sendConsolidationRequest.getSendToOrg();

        if((sendToBranch == null || sendToBranch.size() == 0) && (sendToOrg == null || sendToOrg.size() == 0)){
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolId);
        if (!consolidationDetails.isPresent()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
//            if(additionalDocs != null) {
//                var fileRepoList = consolidationDetails.get().getFileRepoList().stream().filter(fileRepo -> {
//                    return additionalDocs.indexOf(fileRepo.getId()) != -1;
//                }).toList();
//                consolidationDetails.get().setFileRepoList(fileRepoList);
//            } else {
//                consolidationDetails.get().setFileRepoList(null);
//            }

        Map<Long, UUID> idVsGuidMap = new HashMap<>();
        Map<UUID, List<UUID>> containerVsShipmentGuid = new HashMap<>();

        List<String> shipId = new ArrayList<>();
        List<List<String>> docList = new ArrayList<>();

        List<String> houseBills = new ArrayList<>();
        List<String> shipmentIds = new ArrayList<>();
        if(consolidationDetails.get().getShipmentsList() != null && consolidationDetails.get().getShipmentsList().size()>0) {
            consolidationDetails.get().getShipmentsList().forEach(shipment -> {
                shipmentIds.add(shipment.getShipmentId());
                houseBills.add(shipment.getHouseBill());
                // TODO For V2 payload creation
//                    shipment.setGuid(UUID.randomUUID());
//                    shipment.getContainersList().forEach(cont -> {
//                        if(idVsGuidMap.containsKey(cont.getId())){
//                            cont.setGuid(idVsGuidMap.get(cont.getId()));
//                            if(!containerVsShipmentGuid.containsKey(cont.getGuid())) {
//                                containerVsShipmentGuid.put(cont.getGuid(), List.of(shipment.getGuid()));
//                            } else {
//                                containerVsShipmentGuid.get(cont.getGuid()).add(shipment.getGuid());
//                            }
//                        } else {
//                            cont.setGuid(UUID.randomUUID());
//                            idVsGuidMap.put(cont.getId(), cont.getGuid());
//                            containerVsShipmentGuid.put(cont.getGuid(), List.of(shipment.getGuid()));
//                        }
//                    });
                if(shipAdditionalDocs != null && shipAdditionalDocs.containsKey(shipment.getGuid().toString())){
                    if(shipAdditionalDocs.get(shipment.getGuid().toString()) != null) {
//                            var shipFileRepoList = shipment.getFileRepoList().stream().filter(fileRepo -> {
//                                return shipAdditionalDocs.get(shipment.getGuid().toString()).indexOf(fileRepo.getId()) != -1;
//                            }).toList();
//                            shipment.setFileRepoList(shipFileRepoList);
                        shipId.add(shipment.getShipmentId());
                        docList.add(shipAdditionalDocs.get(shipment.getGuid().toString()));
                    }
                }
            });
        }

        List<Integer> successTenantIds = new ArrayList<>();

        // TODO Only V1 Consolidation Task is triggered for current requirement
        if(true) {
            List<Integer> tenantIdsFromOrg = new ArrayList<>();
            if(sendToOrg != null && !sendToOrg.isEmpty())
                tenantIdsFromOrg = tenantIdFromOrganizations(sendToOrg);
            CreateV1ConsolidationTaskFromV2Request request = CreateV1ConsolidationTaskFromV2Request.builder()
                    .consoleId(consolidationDetails.get().getConsolidationNumber())
                    .sendToBranch(sendToBranch)
                    .sendToOrg(sendToOrg)
                    .additionalDocs(additionalDocs)
                    .shipId(shipId)
                    .docList(docList)
                    .build();
            log.info("Entity Transfer Send V1 Consolidation Request Created:" + jsonHelper.convertToJson(request));
            try {
                SendEntityResponse v1ConsoleTaskResponse = v1Service.sendV1ConsolidationTask(request);;
                if (v1ConsoleTaskResponse.getIsCreated()) {
                    if(sendToBranch != null && !sendToBranch.isEmpty())
                        successTenantIds.addAll(sendToBranch);
                    if(sendToOrg != null && !sendToOrg.isEmpty()) {
                        successTenantIds.addAll(tenantIdsFromOrg);
                    }
                } else {
                    log.error("Entity Transfer failed Send V1 Consolidation: " + v1ConsoleTaskResponse.getError());
                    throw new RuntimeException(v1ConsoleTaskResponse.getError());
                }
            } catch (Exception ex) {
                log.error("Entity Transfer failed Send V1 Consolidation: " + ex);
                throw new RuntimeException(ex.getMessage());
            }
        } else {

//                if (consolidationDetails.get().getContainersList() != null) {
//                    consolidationDetails.get().getContainersList().forEach(cont -> {
//                        if (idVsGuidMap.containsKey(cont.getId())) {
//                            cont.setGuid(idVsGuidMap.get(cont.getId()));
//                        }
//                    });
//                }
//                EntityTransferConsolidationDetails entityTransferConsolidationDetails = modelMapper.map(consolidationDetails.get(), EntityTransferConsolidationDetails.class);
//
//                entityTransferConsolidationDetails.setContainerVsShipmentGuid(containerVsShipmentGuid);
//                this.createConsolidationPayload(entityTransferConsolidationDetails);
//                log.info("Consolidation Payload Created.");
//
//
//                if (sendToBranch != null || sendToBranch.size() != 0) {
//                    this.createConsoleTasks(sendToBranch, successTenantIds, entityTransferConsolidationDetails, consolidationDetails.get(), false, houseBills, shipmentIds);
//                }
//
//                if (sendToOrg != null || sendToOrg.size() != 0) {
//                    List<Integer> tenantIdsFromOrg = tenantIdFromOrganizations(sendToOrg);
//                    if (tenantIdsFromOrg != null || tenantIdsFromOrg.size() != 0) {
//                        this.createConsoleTasks(tenantIdsFromOrg, successTenantIds, entityTransferConsolidationDetails, consolidationDetails.get(), true, houseBills, shipmentIds);
//                    }
//                }
        }
        this.createAutoEvent(consolidationDetails.get().getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.CONSOLIDATION);
        List<String> tenantName = getTenantName(successTenantIds);
        String consolDesc = createSendEvent(tenantName, consolidationDetails.get().getReceivingBranch(), consolidationDetails.get().getTriangulationPartner(), consolidationDetails.get().getDocumentationPartner(), consolidationDetails.get().getId().toString(), Constants.CONSOLIDATION_SENT, Constants.CONSOLIDATION, null);
        for (var shipment: consolidationDetails.get().getShipmentsList()) {
            this.createAutoEvent(shipment.getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.SHIPMENT);
            createSendEvent(tenantName, shipment.getReceivingBranch(), shipment.getTriangulationPartner(), shipment.getDocumentationPartner(), shipment.getId().toString(), Constants.SHIPMENT_SENT, Constants.SHIPMENT, consolDesc);
            if(Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
                shipmentDao.saveEntityTransfer(shipment.getId(), Boolean.TRUE);
        }
        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(successTenantIds).build();
        return ResponseHelper.buildSuccessResponse(sendConsolidationResponse);
    }
//    private void createConsoleTasks (List<Integer> tenantIdsList, List<Integer> successTenantIds, EntityTransferConsolidationDetails entityTransferConsolidationDetails,ConsolidationDetails consolidationDetails, Boolean sendToOrganization, List<String> houseBill, List<String> shipmentIds) {
//        for (int tenantId: tenantIdsList) {
//            Integer approverRoleId = getShipmentConsoleImportApprovalRole(tenantId);
//            if(approverRoleId == null || approverRoleId == 0){
//                throw new ValidationException(EntityTransferConstants.APPROVAL_ROLE_NOT_ASSIGNED+ tenantId);
//            }
//            else{
//                SendEntityResponse response = this.sendConsoleTaskToV1(tenantId, approverRoleId, tenantIdsList, entityTransferConsolidationDetails, sendToOrganization, entityTransferConsolidationDetails.getConsolidationNumber(), houseBill, entityTransferConsolidationDetails.getMawb(), consolidationDetails.getId(), shipmentIds);
//                if(response.getIsCreated()){
//                    successTenantIds.add(tenantId);
//                }
//            }
//        }
//    }
//    private SendEntityResponse sendConsoleTaskToV1 (int tenantId, int approverRole, List<Integer> tenantIds, EntityTransferConsolidationDetails entityTransferConsolidationDetails, Boolean sendToOrganization, String consolidationNumber, List<String> houseBill, String MAWB, long id, List<String> shipmentIds) {
//        CreateConsolidationTaskRequest createConsolidationTaskRequest = CreateConsolidationTaskRequest.builder()
//                .tenantId(tenantId).approverRole(approverRole).tenantIds(tenantIds)
//                .consoleRow(entityTransferConsolidationDetails).sendToOrganization(sendToOrganization)
//                .mawb(MAWB).consolidationNumber(consolidationNumber).houseBill(houseBill).id(id)
//                .shipmentIds(shipmentIds)
//                .build();
//        SendEntityResponse response = v1Service.sendConsolidationTask(createConsolidationTaskRequest);
//        return response;
//    }
//
//        private void createConsolidationPayload (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        addConsolidationMasterData(entityTransferConsolidationDetails);
//        addConsolidationUnlocationDatas(entityTransferConsolidationDetails);
//        addConsolidationDedicatedMasterData(entityTransferConsolidationDetails);
//    }
//    private void addConsolidationMasterData (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        if(entityTransferConsolidationDetails != null) {
//            entityTransferConsolidationDetails.setMasterData(addMasterData(entityTransferConsolidationDetails, ConsolidationDetails.class));
//        }
//        if (entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getAchievedQuantities() != null) {
//            entityTransferConsolidationDetails.getAchievedQuantities().setMasterData(addMasterData(entityTransferConsolidationDetails.getAchievedQuantities(), AchievedQuantities.class));
//        }
//        if (entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getAllocations() != null) {
//            entityTransferConsolidationDetails.getAllocations().setMasterData(addMasterData(entityTransferConsolidationDetails.getAllocations(), Allocations.class));
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getArrivalDepartureDetails() != null) {
//            entityTransferConsolidationDetails.getArrivalDepartureDetails().setMasterData(addMasterData(entityTransferConsolidationDetails.getArrivalDepartureDetails(), ArrivalDepartureDetails.class));
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getCarrierDetails() != null) {
//            entityTransferConsolidationDetails.getCarrierDetails().setMasterData(addMasterData(entityTransferConsolidationDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getRoutingsList() != null) {
//            entityTransferConsolidationDetails.getRoutingsList().forEach(routing -> {
//                routing.setMasterData(addMasterData(routing, Routings.class));
//            });
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getContainersList() != null) {
//            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
//                cont.setMasterData(addMasterData(cont, Containers.class));
//            });
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getPackingList() != null) {
//            entityTransferConsolidationDetails.getPackingList().forEach(pack -> {
//                pack.setMasterData(addMasterData(pack, Packing.class));
//            });
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getReferenceNumbersList() != null) {
//            entityTransferConsolidationDetails.getReferenceNumbersList().forEach(referenceNumber -> {
//                referenceNumber.setMasterData(addMasterData(referenceNumber, ReferenceNumbers.class));
//            });
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getShipmentsList() != null) {
//            entityTransferConsolidationDetails.getShipmentsList().forEach(this::addAllMasterDatas);
//        }
//    }
//    private void addConsolidationUnlocationDatas (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        if(entityTransferConsolidationDetails != null) {
//            entityTransferConsolidationDetails.setUnlocationData(addUnlocationData(entityTransferConsolidationDetails, ConsolidationDetails.class));
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getArrivalDepartureDetails() != null) {
//            entityTransferConsolidationDetails.getArrivalDepartureDetails().setUnlocationData(addUnlocationData(entityTransferConsolidationDetails.getArrivalDepartureDetails(), ArrivalDepartureDetails.class));
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getRoutingsList() != null) {
//            entityTransferConsolidationDetails.getRoutingsList().forEach(routing -> {
//                routing.setUnlocationData(addUnlocationData(routing, Routings.class));
//            });
//        }
//        if(entityTransferConsolidationDetails != null && entityTransferConsolidationDetails.getCarrierDetails() != null) {
//            entityTransferConsolidationDetails.getCarrierDetails().setUnlocationData(addUnlocationData(entityTransferConsolidationDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//    }
//    private void addConsolidationDedicatedMasterData (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        if(entityTransferConsolidationDetails.getCarrierDetails() != null) {
//            entityTransferConsolidationDetails.getCarrierDetails().setCarrierMasterData(carrierMasterData(entityTransferConsolidationDetails.getCarrierDetails(), CarrierDetails.class));
//            entityTransferConsolidationDetails.getCarrierDetails().setVesselsMasterData(vesselMasterData(entityTransferConsolidationDetails.getCarrierDetails(), CarrierDetails.class));
//        }
//        if(entityTransferConsolidationDetails.getPackingList() != null) {
//            entityTransferConsolidationDetails.getPackingList().forEach(pack -> {
//                pack.setCommodityTypeMasterData(commodityTypeMasterData(pack, Packing.class));
//            });
//        }
//        if(entityTransferConsolidationDetails.getContainersList() != null) {
//            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
//                cont.setCommodityTypeMasterData(commodityTypeMasterData(cont, Containers.class));
//                cont.setContainerTypeMasterData(containerTypeMasterData(cont, Containers.class));
//            });
//        }
//    }
//    @Transactional
//    @Override
//    public ResponseEntity<IRunnerResponse> importShipment (CommonRequestModel commonRequestModel) throws RunnerException {
//        String responseMsg;
//        ImportShipmentRequest importShipmentRequest = (ImportShipmentRequest) commonRequestModel.getData();
//        EntityTransferShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityTransferShipmentDetails();
//        String ShipmentId = null;
//        try {
//            ShipmentDetailsResponse shipmentDetailsResponse =  this.createShipment(entityTransferShipmentDetails);
//            ShipmentId = shipmentDetailsResponse.getShipmentId();
//            this.createShipmentMasterData(entityTransferShipmentDetails);
//            return ResponseHelper.buildSuccessResponse(ImportShipmentResponse.builder().ShipmentId(ShipmentId).build());
//
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//            throw new RunnerException(e.getMessage());
//        }
//    }
//    @Transactional
//    public ShipmentDetailsResponse createShipment(EntityTransferShipmentDetails entityTransferShipmentDetails) throws RunnerException {
//        ShipmentRequest request = jsonHelper.convertValue(entityTransferShipmentDetails, ShipmentRequest.class);
//
//        String Hbl = request.getHouseBill();
//        List<ShipmentDetails> shipmentDetails = null;
//        if (Hbl != null && !Hbl.equalsIgnoreCase("")) {
//            shipmentDetails = shipmentDao.findByHouseBill(Hbl);
//        }
//        if(shipmentDetails != null && shipmentDetails.size() > 0){
//            request.setId(shipmentDetails.get(0).getId());
//            try {
//                ResponseEntity<IRunnerResponse> response = shipmentService.completeUpdate(CommonRequestModel.buildRequest(request));
//                log.info("Update payload: "+request);
//                if(response == null || response.getBody() == null)
//                    throw new RunnerException("Response body from shipment service v1 for Complete Update is null");
//                return (ShipmentDetailsResponse) ((RunnerResponse)response.getBody()).getData();
//            } catch (Exception e) {
//                throw new RuntimeException(e);
//            }
//        }else {
//            ResponseEntity<IRunnerResponse> response = shipmentService.create(CommonRequestModel.buildRequest(request));
//            if(response == null || response.getBody() == null)
//                throw new RunnerException("Response body from shipment service v1 for Complete Update is null");
//            return (ShipmentDetailsResponse) ((RunnerResponse)response.getBody()).getData();
//        }
//    }
//    @Transactional
//    public void createShipmentMasterData(EntityTransferShipmentDetails entityTransferShipmentDetails) {
//        this.createAllMasterData(entityTransferShipmentDetails);
//        this.createAllUnlocationData(entityTransferShipmentDetails);
//        this.createAllDedicatedMasterData(entityTransferShipmentDetails);
//    }
//    @Transactional
//    public void createAllMasterData(EntityTransferShipmentDetails entityTransferShipmentDetails) {
//        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
//        if(entityTransferShipmentDetails.getMasterData() != null)
//            masterDataList.addAll(entityTransferShipmentDetails.getMasterData().values());
//        if(entityTransferShipmentDetails.getAdditionalDetails() != null && entityTransferShipmentDetails.getAdditionalDetails().getMasterData() != null)
//            masterDataList.addAll(entityTransferShipmentDetails.getAdditionalDetails().getMasterData().values());
//        if(entityTransferShipmentDetails.getCarrierDetails() != null && entityTransferShipmentDetails.getCarrierDetails().getMasterData() != null)
//            masterDataList.addAll(entityTransferShipmentDetails.getCarrierDetails().getMasterData().values());
//
//        var BookingCarriagesList = entityTransferShipmentDetails.getBookingCarriagesList();
//        if(BookingCarriagesList != null) {
//            BookingCarriagesList.forEach(bookingCarriage -> {
//                if (bookingCarriage.getMasterData() != null)
//                    masterDataList.addAll(bookingCarriage.getMasterData().values());
//            });
//        }
//        var containers = entityTransferShipmentDetails.getContainersList();
//        if(containers != null) {
//            containers.forEach(cont -> {
//                if (cont.getMasterData() != null)
//                    masterDataList.addAll(cont.getMasterData().values());
//            });
//        }
//        var packs = entityTransferShipmentDetails.getPackingList();
//        if(packs != null) {
//            packs.forEach(pack -> {
//                if (pack.getMasterData() != null)
//                    masterDataList.addAll(pack.getMasterData().values());
//            });
//        }
//        var referenceNumbers = entityTransferShipmentDetails.getReferenceNumbersList();
//        if(referenceNumbers != null) {
//            referenceNumbers.forEach(referenceNumber -> {
//                if (referenceNumber.getMasterData() != null)
//                    masterDataList.addAll(referenceNumber.getMasterData().values());
//            });
//        }
//        var serviceDetails = entityTransferShipmentDetails.getServicesList();
//        if(serviceDetails != null) {
//            serviceDetails.forEach(service -> {
//                if (service.getMasterData() != null)
//                    masterDataList.addAll(service.getMasterData().values());
//            });
//        }
//
//        this.createMasterData(masterDataList);
//    }
//    @Transactional
//    public void createMasterData(List<EntityTransferMasterLists> masterData) {
//        MasterListRequestV2 masterListRequest = new MasterListRequestV2();
//        Set<String> masterDataKey = new HashSet<>();
//        masterData.forEach(value -> {
//            masterListRequest.getMasterListRequests().add(MasterListRequest.builder().ItemType(MasterDataType.masterData(value.ItemType).getDescription()).ItemValue(value.ItemValue).Cascade(value.Cascade).build());
//            String key = MasterDataType.masterData(value.ItemType).getDescription() + '#' + value.getItemValue();
//            masterDataKey.add(key);
//        });
//        if (masterListRequest.getMasterListRequests().size() > 0) {
//            V1DataResponse response = v1Service.fetchMultipleMasterData(masterListRequest);
//            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
//            masterLists.forEach(val -> {
//                String key = MasterDataType.masterData(val.ItemType).getDescription() + '#' + val.getItemValue();
//                if(masterDataKey.contains(key))
//                    masterDataKey.remove(key);
//            });
//        }
//        masterData.forEach(value -> {
//            String key = MasterDataType.masterData(value.ItemType).getDescription() + '#' + value.getItemValue();
//            if (masterDataKey.contains(key)) {
//                V1SaveRequest save = V1SaveRequest.builder().Entity(value).build();
//                V1DataResponse response = v1Service.createMasterData(save);
//            }
//        });
//    }
//    @Transactional
//    public void createAllUnlocationData(EntityTransferShipmentDetails entityTransferShipmentDetails) {
//        List<EntityTransferUnLocations> unLocationsList = new ArrayList<>();
//        if(entityTransferShipmentDetails.getAdditionalDetails() != null && entityTransferShipmentDetails.getAdditionalDetails().getUnlocationData() != null)
//            unLocationsList.addAll(entityTransferShipmentDetails.getAdditionalDetails().getUnlocationData().values());
//        if(entityTransferShipmentDetails.getCarrierDetails() != null && entityTransferShipmentDetails.getCarrierDetails().getUnlocationData() != null)
//            unLocationsList.addAll(entityTransferShipmentDetails.getCarrierDetails().getUnlocationData().values());
//
//        this.createUnlocationData(unLocationsList);
//    }
//    @Transactional
//    public void createUnlocationData(List<EntityTransferUnLocations> unlocationData) {
//        Set<String> locCodesList = new HashSet<>();
//        locCodesList.addAll(unlocationData.stream().map(x->x.getLocCode()).collect(Collectors.toSet()));
//        if (locCodesList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.UNLOCATION_CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(locCodesList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchUnlocation(request);
//            List<EntityTransferUnLocations> unlocationDataList = jsonHelper.convertValueToList(response.entities, EntityTransferUnLocations.class);
//            locCodesList.removeAll(unlocationDataList.stream().map(x->x.getLocCode()).collect(Collectors.toSet()));
//        }
//
//        unlocationData.forEach(unlocData -> {
//            if(locCodesList.contains(unlocData.getLocCode())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(unlocData).build();
//                log.info("Create Unlocation: "+save);
//                V1DataResponse response = v1Service.createUnlocationData(save);
//            }
//        });
//    }
//    @Transactional
//    public void createAllDedicatedMasterData(EntityTransferShipmentDetails entityTransferShipmentDetails) {
//        List<EntityTransferCarrier> carrierList = new ArrayList<>();
//        List<EntityTransferContainerType> containerTypeList = new ArrayList<>();
//        List<EntityTransferCurrency> currencyList = new ArrayList<>();
//        List<EntityTransferCommodityType> commodityTypeList = new ArrayList<>();
//        List<EntityTransferVessels> vesselsList = new ArrayList<>();
//
//        if(entityTransferShipmentDetails.getCarrierDetails() != null && entityTransferShipmentDetails.getCarrierDetails().getCarrierMasterData() != null)
//            carrierList.addAll(entityTransferShipmentDetails.getCarrierDetails().getCarrierMasterData().values());
//        if(entityTransferShipmentDetails.getCurrenciesMasterData() != null)
//            currencyList.addAll(entityTransferShipmentDetails.getCurrenciesMasterData().values());
//
//        var containers = entityTransferShipmentDetails.getContainersList();
//        if(containers != null) {
//            containers.forEach(cont -> {
//                if (cont.getContainerTypeMasterData() != null)
//                    containerTypeList.addAll(cont.getContainerTypeMasterData().values());
//                if (cont.getCommodityTypeMasterData() != null)
//                    commodityTypeList.addAll(cont.getCommodityTypeMasterData().values());
//            });
//        }
//        if(entityTransferShipmentDetails.getCarrierDetails() != null && entityTransferShipmentDetails.getCarrierDetails().getVesselsMasterData() != null) {
//            vesselsList.addAll(entityTransferShipmentDetails.getCarrierDetails().getVesselsMasterData().values());
//        }
//        if(entityTransferShipmentDetails.getBookingCarriagesList() != null) {
//            entityTransferShipmentDetails.getBookingCarriagesList().forEach(bookingCarriage -> {
//                if(bookingCarriage.getVesselsMasterData() != null) {
//                    vesselsList.addAll(bookingCarriage.getVesselsMasterData().values());
//                }
//            });
//        }
//
//        this.createVesselMasterData(vesselsList);
//        this.createCarrierMasterData(carrierList);
//        this.createContainerTypeMasterData(containerTypeList);
//        this.createCurrencyMasterData(currencyList);
//        this.createCommodityTypeMasterData(commodityTypeList);
//
//    }
//    @Transactional
//    public void createCarrierMasterData(List<EntityTransferCarrier> carrierData) {
//        Set<String> itemValueList = new HashSet<>();
//        itemValueList.addAll(carrierData.stream().map(x->x.getItemValue()).collect(Collectors.toSet()));
//        if (itemValueList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(itemValueList)));
//            request.setCriteriaRequests(criteria);
//            CarrierListObject carrierListObject = new CarrierListObject();
//            carrierListObject.setListObject(request);
//            V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
//            List<EntityTransferCarrier> carrierDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
//            itemValueList.removeAll(carrierDataList.stream().map(x->x.getItemValue()).collect(Collectors.toSet()));
//        }
//
//        carrierData.forEach(carrier -> {
//            if(itemValueList.contains(carrier.getItemValue())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(carrier).build();
//                V1DataResponse response = v1Service.createCarrierMasterData(save);
//            }
//        });
//    }
//    @Transactional
//    public void createContainerTypeMasterData(List<EntityTransferContainerType> containerData) {
//        Set<String> containerCodeList = new HashSet<>();
//        containerCodeList.addAll(containerData.stream().map(x->x.getCode()).collect(Collectors.toSet()));
//        if (containerCodeList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(containerCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchContainerTypeData(request);
//            List<EntityTransferContainerType> containerTypeList = jsonHelper.convertValueToList(response.entities, EntityTransferContainerType.class);
//            containerCodeList.removeAll(containerTypeList.stream().map(x->x.getCode()).collect(Collectors.toSet()));
//        }
//
//        containerData.forEach(cont -> {
//            if(containerCodeList.contains(cont.getCode())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(cont).build();
//                V1DataResponse response = v1Service.createContainerTypeData(save);
//            }
//        });
//    }
//    @Transactional
//    public void createCurrencyMasterData(List<EntityTransferCurrency> currencyData) {
//        Set<String> currCodeList = new HashSet<>();
//        currCodeList.addAll(currencyData.stream().map(x->x.getCurrenyCode()).collect(Collectors.toSet()));
//        if (currCodeList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CURRENCY_CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(currCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchCurrenciesData(request);
//            List<EntityTransferCurrency> currencyDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCurrency.class);
//            currCodeList.removeAll(currencyDataList.stream().map(x->x.getCurrenyCode()).collect(Collectors.toSet()));
//        }
//
//        currencyData.forEach(curr -> {
//            if(currCodeList.contains(curr.getCurrenyCode())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(curr).build();
//                V1DataResponse response = v1Service.createCurrenciesData(save);
//            }
//        });
//    }
//    @Transactional
//    public void createCommodityTypeMasterData(List<EntityTransferCommodityType> commodityData) {
//        Set<String> commodityCodeList = new HashSet<>();
//        commodityCodeList.addAll(commodityData.stream().map(x->x.getCode()).collect(Collectors.toSet()));
//        if (commodityCodeList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.CODE));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(commodityCodeList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchCommodityData(request);
//            List<EntityTransferCommodityType> commodityDataList = jsonHelper.convertValueToList(response.entities, EntityTransferCommodityType.class);
//            commodityCodeList.removeAll(commodityDataList.stream().map(x->x.getCode()).collect(Collectors.toSet()));
//        }
//
//        commodityData.forEach(commodity -> {
//            if(commodityCodeList.contains(commodity.getCode())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(commodity).build();
//                V1DataResponse response = v1Service.createCommodityData(save);
//            }
//        });
//    }
//    private void createVesselMasterData (List<EntityTransferVessels> vesselData) {
//        Set<String> GuidList = new HashSet<>();
//        GuidList.addAll(vesselData.stream().map(x->x.getGuid().toString()).collect(Collectors.toSet()));
//        if (GuidList.size() > 0) {
//            CommonV1ListRequest request = new CommonV1ListRequest();
//            List<Object> criteria = new ArrayList<>();
//            List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
//            String operator = Operators.IN.getValue();
//            criteria.addAll(List.of(field, operator, List.of(GuidList)));
//            request.setCriteriaRequests(criteria);
//            V1DataResponse response = v1Service.fetchVesselData(request);
//            List<EntityTransferVessels> vesselList = jsonHelper.convertValueToList(response.entities, EntityTransferVessels.class);
//            GuidList.removeAll(vesselList.stream().map(x->x.getGuid().toString()).collect(Collectors.toSet()));
//        }
//
//        vesselData.forEach(vessel -> {
//            if(GuidList.contains(vessel.getGuid().toString())){
//                V1SaveRequest save = V1SaveRequest.builder().Entity(vessel).build();
//                V1DataResponse response = v1Service.createVesselData(save);
//            }
//        });
//    }
//
//
//    @Override
//    @Transactional
//    public ResponseEntity<IRunnerResponse> importConsolidation (CommonRequestModel commonRequestModel) {
//        String responseMsg;
//        ImportConsolidationRequest importConsolidationRequest = (ImportConsolidationRequest) commonRequestModel.getData();
//        if(importConsolidationRequest == null || importConsolidationRequest.getEntityTransferConsolidationDetails() == null) {
//            throw new ValidationException("No consolidation is attached please check");
//        }
//        EntityTransferConsolidationDetails entityTransferConsolidationDetails = importConsolidationRequest.getEntityTransferConsolidationDetails();
//        String consolidationNumber = null;
//        try {
//            ConsolidationDetailsResponse consolidationDetailsResponse = this.createConsolidation(entityTransferConsolidationDetails);
//            if(consolidationDetailsResponse == null) {
//                throw new RunnerException("Create Consolidation failed for " + commonRequestModel.getId());
//            }
//            consolidationNumber = consolidationDetailsResponse.getConsolidationNumber();
//
//            this.createAllConsolidationMasterData(entityTransferConsolidationDetails);
//            return ResponseHelper.buildSuccessResponse(ImportConsolidationResponse.builder().consolidationNumber(consolidationNumber).build());
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//            throw new RuntimeException(e);
//        }
//    }
//
//    private ConsolidationDetailsResponse createConsolidation (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        ConsolidationDetailsRequest request = jsonHelper.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
//        Map<UUID, List<UUID>> containerVsShipmentGuid = entityTransferConsolidationDetails.getContainerVsShipmentGuid();
//        Map<UUID, Long> shipmentGuidVsIdMap = new HashMap<>();
//        List<Long> shipmentIds = new ArrayList<>();
//        if(entityTransferConsolidationDetails.getShipmentsList() != null){
//            entityTransferConsolidationDetails.getShipmentsList().forEach(shipment -> {
//                shipment.setContainersList(null);
//                ShipmentDetailsResponse shipmentDetailsResponse = null;
//                try {
//                    shipmentDetailsResponse = createShipment(shipment);
//                } catch (RunnerException e) {
//                    throw new RuntimeException(e);
//                }
//                shipmentGuidVsIdMap.put(shipmentDetailsResponse.getGuid(), shipmentDetailsResponse.getId());
//                shipmentIds.add(shipmentDetailsResponse.getId());
//            });
//        }
//        String mbl = request.getBol();
//        List<ConsolidationDetails> consolidationDetails = null;
//        if(mbl != null && !mbl.equalsIgnoreCase("")) {
//            consolidationDetails = consolidationDetailsDao.findByBol(mbl);
//        }
//        ResponseEntity<IRunnerResponse> response;
//        ConsolidationDetailsResponse consolidationDetailsResponse = null;
//        if(consolidationDetails != null && consolidationDetails.size() > 0) {
//            request.setId(consolidationDetails.get(0).getId());
//            try {
//                response = consolidationService.completeUpdate(CommonRequestModel.buildRequest(request));
//            } catch (Exception e) {
//                throw new RuntimeException(e);
//            }
//        } else {
//            try {
//                request.setPackingList(null);
//                response = consolidationService.create(CommonRequestModel.buildRequest(request));
//            } catch (Exception e) {
//                throw new RuntimeException(e);
//            }
//        }
//        if(response != null && response.hasBody()) {
//            consolidationDetailsResponse = ((ConsolidationDetailsResponse)((RunnerResponse)response.getBody()).getData());
//            consolidationDetailsResponse.getContainersList().forEach(cont -> {
//                List<Long> newShipmentIds = new ArrayList<>();
//                if(containerVsShipmentGuid.containsKey(cont.getGuid())) {
//                    List<UUID> shipmentGuids = containerVsShipmentGuid.get(cont.getGuid());
//                    newShipmentIds = shipmentGuids.stream().map(x -> shipmentGuidVsIdMap.get(x)).toList();
//                    shipmentsContainersMappingDao.assignShipments(cont.getId(), newShipmentIds, false);
//                }
//            });
//            try {
//                consolidationService.attachShipments(consolidationDetailsResponse.getId(), shipmentIds);
//            } catch (Exception e) {
//                throw new RuntimeException(e);
//            }
//        }
//        return consolidationDetailsResponse;
//    }
//    private void createAllConsolidationMasterData (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        createConsolidationMasterDatas(entityTransferConsolidationDetails);
//        createConsolidationUnlocationData(entityTransferConsolidationDetails);
//        createConsolidationDedicatedMasterData(entityTransferConsolidationDetails);
//        if(entityTransferConsolidationDetails.getShipmentsList() != null) {
//            entityTransferConsolidationDetails.getShipmentsList().forEach(shipment -> {
//                createShipmentMasterData(shipment);
//            });
//        }
//    }
//
//    private void createConsolidationMasterDatas (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
//        if(entityTransferConsolidationDetails.getMasterData() != null) {
//            masterDataList.addAll(entityTransferConsolidationDetails.getMasterData().values());
//        }
//        if(entityTransferConsolidationDetails.getAllocations() != null && entityTransferConsolidationDetails.getAllocations().getMasterData() != null) {
//            masterDataList.addAll(entityTransferConsolidationDetails.getAllocations().getMasterData().values());
//        }
//        if(entityTransferConsolidationDetails.getAchievedQuantities() != null && entityTransferConsolidationDetails.getAchievedQuantities().getMasterData() != null) {
//            masterDataList.addAll(entityTransferConsolidationDetails.getAchievedQuantities().getMasterData().values());
//        }
//        if(entityTransferConsolidationDetails.getArrivalDepartureDetails() != null && entityTransferConsolidationDetails.getArrivalDepartureDetails().getMasterData() != null) {
//            masterDataList.addAll(entityTransferConsolidationDetails.getArrivalDepartureDetails().getMasterData().values());
//        }
//        if(entityTransferConsolidationDetails.getCarrierDetails() != null &&  entityTransferConsolidationDetails.getCarrierDetails().getMasterData() != null) {
//            masterDataList.addAll(entityTransferConsolidationDetails.getCarrierDetails().getMasterData().values());
//        }
//        if(entityTransferConsolidationDetails.getRoutingsList() != null) {
//            entityTransferConsolidationDetails.getRoutingsList().forEach(routing -> {
//                if (routing.getMasterData() != null) {
//                    masterDataList.addAll(routing.getMasterData().values());
//                }
//            });
//        }
//        if(entityTransferConsolidationDetails.getContainersList() != null) {
//            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
//                if (cont.getMasterData() != null) {
//                    masterDataList.addAll(cont.getMasterData().values());
//                }
//            });
//        }
//        if(entityTransferConsolidationDetails.getPackingList() != null) {
//            entityTransferConsolidationDetails.getPackingList().forEach(pack -> {
//                if (pack.getMasterData() != null) {
//                    masterDataList.addAll(pack.getMasterData().values());
//                }
//            });
//        }
//        if(entityTransferConsolidationDetails.getReferenceNumbersList() != null) {
//            entityTransferConsolidationDetails.getReferenceNumbersList().forEach(referenceNumber -> {
//                if (referenceNumber.getMasterData() != null) {
//                    masterDataList.addAll(referenceNumber.getMasterData().values());
//                }
//            });
//        }
//
//        this.createMasterData(masterDataList);
//    }
//
//    private void createConsolidationUnlocationData (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        List<EntityTransferUnLocations> unLocationsList = new ArrayList<>();
//        if(entityTransferConsolidationDetails.getUnlocationData() != null)
//            unLocationsList.addAll(entityTransferConsolidationDetails.getUnlocationData().values());
//        if(entityTransferConsolidationDetails.getArrivalDepartureDetails() != null && entityTransferConsolidationDetails.getArrivalDepartureDetails().getUnlocationData() != null)
//            unLocationsList.addAll(entityTransferConsolidationDetails.getArrivalDepartureDetails().getUnlocationData().values());
//        if(entityTransferConsolidationDetails.getCarrierDetails() != null && entityTransferConsolidationDetails.getCarrierDetails().getUnlocationData() != null) {
//            unLocationsList.addAll(entityTransferConsolidationDetails.getCarrierDetails().getUnlocationData().values());
//        }
//        if(entityTransferConsolidationDetails.getRoutingsList() != null) {
//            entityTransferConsolidationDetails.getRoutingsList().forEach(routing -> {
//                if(routing.getUnlocationData() != null) {
//                    unLocationsList.addAll(routing.getUnlocationData().values());
//                }
//            });
//        }
//
//        this.createUnlocationData(unLocationsList);
//    }
//
//    private void createConsolidationDedicatedMasterData (EntityTransferConsolidationDetails entityTransferConsolidationDetails) {
//        List<EntityTransferCarrier> carrierList = new ArrayList<>();
//        List<EntityTransferContainerType> containerTypeList = new ArrayList<>();
//        List<EntityTransferCurrency> currencyList = new ArrayList<>();
//        List<EntityTransferCommodityType> commodityTypeList = new ArrayList<>();
//        List<EntityTransferVessels> vesselsList = new ArrayList<>();
//
//        if(entityTransferConsolidationDetails.getCarrierDetails() != null && entityTransferConsolidationDetails.getCarrierDetails().getCarrierMasterData() != null)
//            carrierList.addAll(entityTransferConsolidationDetails.getCarrierDetails().getCarrierMasterData().values());
//        if(entityTransferConsolidationDetails.getPackingList() != null) {
//            entityTransferConsolidationDetails.getPackingList().forEach(pack -> {
//                if(pack.getCommodityTypeMasterData() != null) {
//                    commodityTypeList.addAll(pack.getCommodityTypeMasterData().values());
//                }
//            });
//        }
//
//        if(entityTransferConsolidationDetails.getContainersList() != null) {
//            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
//                if (cont.getContainerTypeMasterData() != null)
//                    containerTypeList.addAll(cont.getContainerTypeMasterData().values());
//                if (cont.getCommodityTypeMasterData() != null)
//                    commodityTypeList.addAll(cont.getCommodityTypeMasterData().values());
//            });
//        }
//        if(entityTransferConsolidationDetails.getCarrierDetails() != null && entityTransferConsolidationDetails.getCarrierDetails().getVesselsMasterData() != null){
//            vesselsList.addAll(entityTransferConsolidationDetails.getCarrierDetails().getVesselsMasterData().values());
//        }
//
//        this.createVesselMasterData(vesselsList);
//        this.createCarrierMasterData(carrierList);
//        this.createContainerTypeMasterData(containerTypeList);
//        this.createCurrencyMasterData(currencyList);
//        this.createCommodityTypeMasterData(commodityTypeList);
//    }


    @Override
    public ResponseEntity<IRunnerResponse> sendConsolidationValidation(CommonRequestModel commonRequestModel) {
        ValidateSendConsolidationRequest request = (ValidateSendConsolidationRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsoleId());
        if (!consolidationDetails.isPresent()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getConsoleId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(consolidationDetails.get().getReceivingBranch() == null) {
            throw new ValidationException(EntityTransferConstants.MISSING_RECEIVING_BRANCH_VALIDATION);
        }

        if(consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
        {
            String flightNumber = null;
            String voyage = null;
            String bol = null;
            if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                flightNumber = consolidationDetails.get().getCarrierDetails().getVessel();
                voyage = consolidationDetails.get().getCarrierDetails().getVoyage();
                bol = consolidationDetails.get().getBol();
            } else if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                flightNumber = consolidationDetails.get().getCarrierDetails().getFlightNumber();
                voyage = consolidationDetails.get().getCarrierDetails().getShippingLine();
                bol = consolidationDetails.get().getBol();
            }
            LocalDateTime eta = consolidationDetails.get().getCarrierDetails().getEta();
            LocalDateTime etd = consolidationDetails.get().getCarrierDetails().getEtd();
            String polId = consolidationDetails.get().getCarrierDetails().getOriginPort();
            String podId = consolidationDetails.get().getCarrierDetails().getDestinationPort();
            List<String> missingField = new ArrayList<>();
            if(Strings.isNullOrEmpty(bol) || Strings.isNullOrEmpty(voyage) || Strings.isNullOrEmpty(flightNumber) ||
                    eta == null || etd == null || Strings.isNullOrEmpty(polId) || Strings.isNullOrEmpty(podId)) {
                if(Strings.isNullOrEmpty(bol) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("Mawb Number");
                if(Strings.isNullOrEmpty(bol) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Master Bill");
                if(Strings.isNullOrEmpty(voyage) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("Flight Carrier");
                if(Strings.isNullOrEmpty(flightNumber) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("Flight Number");
                if(Strings.isNullOrEmpty(voyage) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Voyage");
                if(Strings.isNullOrEmpty(flightNumber) && consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Vessel");
                if(eta == null)
                    missingField.add("Eta");
                if(etd == null)
                    missingField.add("Etd");
                if(Strings.isNullOrEmpty(polId))
                    missingField.add("Origin Port");
                if(Strings.isNullOrEmpty(podId))
                    missingField.add("Destination Port");
                String joinMissingField = String.join(",", missingField);
                throw new ValidationException("Please validate these fields before sending consolidation: " + joinMissingField);
            }
            else {
                Boolean sendConsolidationError = false;
                Boolean hblGenerationError = false;
                for (var shipment: consolidationDetails.get().getShipmentsList()) {
                    if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                            shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                        String shipFlightNumber = null;
                        String shipVoyage = null;
                        if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)){
                            shipFlightNumber = shipment.getCarrierDetails().getVessel();
                            shipVoyage = shipment.getCarrierDetails().getVoyage();
                        } else if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                            shipFlightNumber = shipment.getCarrierDetails().getFlightNumber();
                            shipVoyage = shipment.getCarrierDetails().getShippingLine();
                        }

                        if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) &&
                                shipment.getDirection().equals(Constants.DIRECTION_EXP) &&
                                !Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
                            List<Hbl> hbls = hblDao.findByShipmentId(shipment.getId());
                            if(hbls.isEmpty())
                                hblGenerationError = true;
                        }

                        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
                        TenantModel tenantModel = modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
                        // TODO Need to set that.tenant.IATAAgent = true condition for Air
                        if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) &&
                                shipment.getDirection().equals(Constants.DIRECTION_EXP) && tenantModel.IATAAgent){
                            List<Awb> awbs = awbDao.findByShipmentId(shipment.getId());
                            if(awbs.isEmpty())
                                hblGenerationError = true;
                        }

                        LocalDateTime shipEta = shipment.getCarrierDetails().getEta();
                        LocalDateTime shipEtd = shipment.getCarrierDetails().getEtd();
                        String shipPolId = shipment.getCarrierDetails().getOriginPort();
                        String shipPodId = shipment.getCarrierDetails().getDestinationPort();
                        if((Strings.isNullOrEmpty(shipment.getHouseBill()) && !Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_DRT)) || Strings.isNullOrEmpty(shipment.getMasterBill()) ||
                                shipVoyage == null || shipFlightNumber == null || shipEta == null || shipEtd == null ||
                                Strings.isNullOrEmpty(shipPolId) || Strings.isNullOrEmpty(shipPodId)) {
                            sendConsolidationError = true;
                        }
                        if(hblGenerationError){
                            sendConsolidationError = true;
                        }
                    }
                }


                if(sendConsolidationError){
                    if(hblGenerationError){
                        if(consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                            throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, Vessel & Voyage details in the attached shipments and generate the Original HBL before sending consolidation");
                        } else if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                            throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, Airline and Flight number details in the attached shipments and generate the Original HAWB before sending consolidation");
                        }
                    } else {
                        if(consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                            throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, Vessel & Voyage details in the attached shipments before sending consolidation");
                        } else if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                            throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, Airline and Flight number details in the attached shipments before sending consolidation");
                        }
                    }
                }
            }
        }
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<IRunnerResponse> sendShipmentValidation(CommonRequestModel commonRequestModel) {
        ValidateSendShipmentRequest request = (ValidateSendShipmentRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipId());
        if (!shipmentDetails.isPresent()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getShipId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(shipmentDetails.get().getReceivingBranch() == null) {
            throw new ValidationException(EntityTransferConstants.MISSING_RECEIVING_BRANCH_VALIDATION);
        }

        if(shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
        {
            String flightNumber = null;
            String voyage = null;
            if (shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                flightNumber = shipmentDetails.get().getCarrierDetails().getVessel();
                voyage = shipmentDetails.get().getCarrierDetails().getVoyage();
            } else if (shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                flightNumber = shipmentDetails.get().getCarrierDetails().getFlightNumber();
                voyage = shipmentDetails.get().getCarrierDetails().getShippingLine();
            }
            LocalDateTime eta = shipmentDetails.get().getCarrierDetails().getEta();
            LocalDateTime etd = shipmentDetails.get().getCarrierDetails().getEtd();
            String polId = shipmentDetails.get().getCarrierDetails().getOriginPort();
            String podId = shipmentDetails.get().getCarrierDetails().getDestinationPort();
            List<String> missingField = new ArrayList<>();
            if(Strings.isNullOrEmpty(voyage) || Strings.isNullOrEmpty(flightNumber) ||
                    eta == null || etd == null || Strings.isNullOrEmpty(polId) || Strings.isNullOrEmpty(podId) ||
                    Strings.isNullOrEmpty(shipmentDetails.get().getHouseBill()) ||
                    Strings.isNullOrEmpty(shipmentDetails.get().getMasterBill())) {
                if(Strings.isNullOrEmpty(voyage) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("Flight Carrier");
                if(Strings.isNullOrEmpty(flightNumber) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("Flight Number");
                if(Strings.isNullOrEmpty(voyage) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Voyage");
                if(Strings.isNullOrEmpty(flightNumber) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Vessel");
                if(eta == null)
                    missingField.add("Eta");
                if(etd == null)
                    missingField.add("Etd");
                if(Strings.isNullOrEmpty(polId))
                    missingField.add("Origin Port");
                if(Strings.isNullOrEmpty(podId))
                    missingField.add("Destination Port");
                if(Strings.isNullOrEmpty(shipmentDetails.get().getHouseBill()) && Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.get().getTransportMode())) {
                    if(!Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.get().getJobType())) {
                        missingField.add("HAWB Number");
                    }
                }
                if(Strings.isNullOrEmpty(shipmentDetails.get().getMasterBill()) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                    missingField.add("MAWB Number");
                if(Strings.isNullOrEmpty(shipmentDetails.get().getHouseBill()) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)
                        && !Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.get().getJobType()))
                    missingField.add("House Bill");
                if(Strings.isNullOrEmpty(shipmentDetails.get().getMasterBill()) && shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                    missingField.add("Master Bill");
                String joinMissingField = String.join(",", missingField);
                if(StringUtility.isNotEmpty(joinMissingField))
                    throw new ValidationException("Please validate these fields before sending shipment: " + joinMissingField);
            }
            else {
                var shipment = shipmentDetails.get();
                if(shipment.getDirection().equals(Constants.DIRECTION_EXP)) {
                    if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                        List<Hbl> hbls = hblDao.findByShipmentId(shipment.getId());
                        if(hbls.isEmpty())
                            throw new ValidationException("Please generate original HBL before sending shipment");
                    }
                    DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
                    TenantModel tenantModel = modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
                    if(shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && tenantModel.IATAAgent){
                        List<Awb> awbs = awbDao.findByShipmentId(shipment.getId());
                        if(awbs.isEmpty())
                            throw new ValidationException("Please generate original HAWB before sending shipment");
                    }
                }
            }
        }
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkTaskExist(CommonRequestModel commonRequestModel) {
        CheckTaskExistRequest request = (CheckTaskExistRequest) commonRequestModel.getData();
        CheckTaskExistV1Request requestV1 = CheckTaskExistV1Request.builder().entityType(request.getEntityType())
                .sendToBranch(request.getSendToBranch())
                .sendToOrg(request.getSendToOrg())
                .build();
        if(request.getSendToBranch() == null){
            requestV1.setSendToBranch(new ArrayList<>());
        }
        if(request.getSendToOrg() == null){
            requestV1.setSendToOrg(new ArrayList<>());
        }
        if(request.getEntityType().equals(Constants.Shipments)){
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getEntityId());
            if (!shipmentDetails.isPresent()) {
                log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getEntityId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            requestV1.setShipId(shipmentDetails.get().getShipmentId());
        } else if (request.getEntityType().equals(Constants.Consolidations)) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getEntityId());
            if (!consolidationDetails.isPresent()) {
                log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getEntityId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            requestV1.setConsoleId(consolidationDetails.get().getConsolidationNumber());
        }
        CheckTaskExistResponse response = new CheckTaskExistResponse();
        try {
            response = v1Service.checkTaskExist(requestV1);
        }
        catch (Exception ex) {
            log.error("Check Task exist failed to check from V1: " + ex);
            throw new RuntimeException("Check Task exist failed to check from V1: " + ex);
        }
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void createAutoEvent(String entityId, String eventCode, String entityType) {
        if (StringUtility.isNotEmpty(entityId)) {
            CustomAutoEventRequest eventReq = new CustomAutoEventRequest();
            eventReq.entityId = Long.parseLong(entityId);
            eventReq.entityType = entityType;
            eventReq.eventCode = eventCode;
            eventDao.autoGenerateEvents(eventReq);
        }
    }

    private List<String> getTenantName(List<Integer> tenantIds) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(CustomerBookingConstants.TENANT_ID));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(tenantIds)));
        request.setCriteriaRequests(criteria);
        V1DataResponse tenantName = v1Service.tenantNameByTenantId(request);

        List<V1TenantResponse> v1TenantResponse = jsonHelper.convertValueToList(tenantName.entities, V1TenantResponse.class);
        if(v1TenantResponse != null) {
            return v1TenantResponse.stream().map(V1TenantResponse::getTenantName).toList();
        }
        return null;
    }

    private String createSendEvent(List<String> tenantNameList, Long receivingBranch, Long triangulationPartner, Long documentationPartner,
                                   String entityId, String eventCode, String entityType, String consolPlaceDescription) {
        String tenantName = null;
        if(tenantNameList != null) {
            tenantName = String.join(",", tenantNameList);
        }
        String placeDescription = "";
        if (receivingBranch != null)
            placeDescription = placeDescription + "Receiving Branch";
        if (triangulationPartner != null)
        {
            if (!placeDescription.equals(""))
                placeDescription = placeDescription + ',';
            placeDescription = placeDescription + "Triangulation Partner ";
        }

        if (documentationPartner != null)
        {
            if (!placeDescription.equals(""))
                placeDescription = placeDescription + ',';
            placeDescription = placeDescription + "Documentation Partner";
        }

        CustomAutoEventRequest eventReq = new CustomAutoEventRequest();
        eventReq.entityId = Long.parseLong(entityId);
        eventReq.entityType = entityType;
        eventReq.eventCode = eventCode;
        eventReq.isActualRequired = true;
        eventReq.placeName = tenantName;
        if (placeDescription.equalsIgnoreCase(""))
        {
            eventReq.placeDesc = placeDescription;
        }
        else
        {
            eventReq.placeDesc = consolPlaceDescription;
        }
        eventReq.createDuplicate = true;
        eventDao.autoGenerateEvents(eventReq);
        return placeDescription;
    }

    @Override
    public ResponseEntity<IRunnerResponse> postArValidation(CommonRequestModel commonRequestModel) throws RunnerException {
        PostArValidationRequest request =  (PostArValidationRequest)commonRequestModel.getData();
        if(request.getShipmentGuids() == null || request.getShipmentGuids().isEmpty()) {
            log.error("Guids are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("GUID can't be null. Please provide any one !");
        }
        Set<UUID> requestGuids = new HashSet<>(request.getShipmentGuids());
        List<IRunnerResponse> responseList = new ArrayList<>();

        List<ShipmentDetails> shipmentDetailsList = findShipmentsFromLogsHistory(requestGuids.stream().toList(), request.getTimestamp());

        Map<UUID, ConsolidationDetails> consolidationDetailsMap;
        if (!shipmentDetailsList.isEmpty()){
            Set<UUID> consoleGuids = shipmentDetailsList.stream().filter(x-> (x.getConsolidationList() != null && !x.getConsolidationList().isEmpty())).map(x->x.getConsolidationList().get(0).getGuid()).collect(Collectors.toCollection(LinkedHashSet::new));

            List<ConsolidationDetails> consolidationDetailsList = findConsolidationFromLogsHistory(consoleGuids.stream().toList(), request.getTimestamp());
            consolidationDetailsMap = consolidationDetailsList.stream().collect(Collectors.toMap(ConsolidationDetails::getGuid, Function.identity()));

            Set<UUID> sourceGuids = new HashSet<>();
            Set<UUID> shipmentGuids = new HashSet<>();
            Set<String> locationRefGuids = new HashSet<>();
            for (var shipmentDetails: shipmentDetailsList) {
                if(Objects.equals(shipmentDetails.getJobType(),  Constants.SHIPMENT_TYPE_DRT)){
                    continue;
                }
                if(shipmentDetails.getSourceGuid() != null){
                    sourceGuids.add(shipmentDetails.getSourceGuid());
                }
                else if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()){
                    ConsolidationDetails consolidationDetails;
                    if(consolidationDetailsMap.containsKey(shipmentDetails.getConsolidationList().get(0).getGuid())) {
                        consolidationDetails = consolidationDetailsMap.get(shipmentDetails.getConsolidationList().get(0).getGuid());

                        var receivingAgent = consolidationDetails.getReceivingBranch();
                        var triangulationPartner = consolidationDetails.getTriangulationPartner();
                        if (receivingAgent == null && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
                            var destination = shipmentDetails.getCarrierDetails().getDestination();
                            if (destination != null)
                                locationRefGuids.add(destination);
                        }
                        if (receivingAgent != null && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP) &&
                                !shipmentDetails.getTenantId().equals(receivingAgent.intValue())) {
                            shipmentGuids.add(shipmentDetails.getGuid());
                        }
                        if (triangulationPartner != null && !Objects.equals(triangulationPartner, receivingAgent) && !shipmentDetails.getTenantId().equals(triangulationPartner.intValue())) {
                            shipmentGuids.add(shipmentDetails.getGuid());
                        }
                    }
                }
            }
            Map<UUID, List<ShipmentDetails>> destinationShipmentsMap = new HashMap<>();
            Map<UUID, ShipmentDetails> originShipmentsMap = new HashMap<>();
            List<ShipmentDetails> originShipments = !sourceGuids.isEmpty() ? findShipmentsFromLogsHistory(sourceGuids.stream().toList(), request.getTimestamp()) : null;

            Map<UUID, ConsolidationDetails> originConsoleMap = new HashMap<>();
            if(originShipments != null && !originShipments.isEmpty()) {
                shipmentGuids.addAll(originShipments.stream().map(ShipmentDetails::getGuid).collect(Collectors.toSet()));
                Set<UUID> originConsoleGuids = originShipments.stream().filter(x-> (x.getConsolidationList() != null && !x.getConsolidationList().isEmpty())).map(x->x.getConsolidationList().get(0).getGuid()).collect(Collectors.toSet());
                List<ConsolidationDetails> originConsolidationDetails = findConsolidationFromLogsHistory(originConsoleGuids.stream().toList(), request.getTimestamp());
                originConsoleMap = originConsolidationDetails.stream().collect(Collectors.toMap(ConsolidationDetails::getGuid, Function.identity()));
            }

            List<ShipmentDetails> destinationShip = !shipmentGuids.isEmpty() ? shipmentDao.findShipmentsBySourceGuids(shipmentGuids) : null;
            Set<UUID> destinationShipmentsGuids = destinationShip != null ? destinationShip.stream().map(ShipmentDetails::getGuid).collect(Collectors.toSet()) : null;
            List<ShipmentDetails> destinationShipments = destinationShipmentsGuids != null ? findShipmentsFromLogsHistory(destinationShipmentsGuids.stream().toList(), request.getTimestamp()) : null;


            if(destinationShipments != null && !destinationShipments.isEmpty()){
                destinationShipmentsMap = destinationShipments.stream().collect(Collectors.groupingBy(ShipmentDetails::getSourceGuid));
            }
            if(originShipments != null && !originShipments.isEmpty()){
                originShipmentsMap = originShipments.stream().collect(Collectors.toMap(ShipmentDetails::getGuid, Function.identity()));
            }
            Map<String, UnlocationsResponse> unlocationsResponseMap = !locationRefGuids.isEmpty() ? masterDataUtils.getLocationData(locationRefGuids) : new HashMap<>();

            for (var shipmentDetails: shipmentDetailsList) {
                ArValidationResponse arValidationResponse = new ArValidationResponse();
                arValidationResponse.setShipmentGuid(shipmentDetails.getGuid());
                arValidationResponse.setConsolidationType(shipmentDetails.getJobType());
                arValidationResponse.setSourceBranch(shipmentDetails.getTenantId());
                if (Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
                    responseList.add(arValidationResponse);
                    continue;
                }
                if(shipmentDetails.getSourceGuid() != null) {
                    if(originShipmentsMap.containsKey(shipmentDetails.getSourceGuid())){
                        ShipmentDetails originShipment = originShipmentsMap.get(shipmentDetails.getSourceGuid());
                        ConsolidationDetails consolidationDetails;
                        if(originShipment.getConsolidationList() != null && !originShipment.getConsolidationList().isEmpty() &&
                                originConsoleMap.containsKey(originShipment.getConsolidationList().get(0).getGuid())){
                            consolidationDetails = originConsoleMap.get(originShipment.getConsolidationList().get(0).getGuid());
                            var receivingAgent = consolidationDetails.getReceivingBranch();
                            var triangulationPartner = consolidationDetails.getTriangulationPartner();
                            ArValidationResponse.ProfitShareShipmentData originShipmentData = mapShipmentDataToProfitShare(originShipment);
                            arValidationResponse.setOrigin(originShipment.getTenantId());
                            arValidationResponse.setReceivingAgent(receivingAgent);
                            arValidationResponse.setTriangulationPartner(triangulationPartner);
                            arValidationResponse.setOriginShipment(originShipmentData);
                            if (receivingAgent != null) {
                                if (shipmentDetails.getTenantId().equals(receivingAgent.intValue())) {
                                    ArValidationResponse.ProfitShareShipmentData receivingShipmentData = mapShipmentDataToProfitShare(shipmentDetails);
                                    arValidationResponse.setTransferToReceivingAgent(true);
                                    arValidationResponse.setReceivingShipment(receivingShipmentData);
                                } else if (destinationShipmentsMap.containsKey(originShipment.getGuid())) {
                                    var ships = destinationShipmentsMap.get(originShipment.getGuid());
                                    var isShip = ships.stream().filter(x -> x.getTenantId().equals(receivingAgent.intValue())).findAny();
                                    if (isShip.isPresent()) {
                                        ArValidationResponse.ProfitShareShipmentData receivingShipmentData = mapShipmentDataToProfitShare(isShip.get());
                                        arValidationResponse.setTransferToReceivingAgent(true);
                                        arValidationResponse.setReceivingShipment(receivingShipmentData);
                                    }
                                }
                            }
                            if (triangulationPartner != null) {
                                if (shipmentDetails.getTenantId().equals(triangulationPartner.intValue())) {
                                    ArValidationResponse.ProfitShareShipmentData triangulationShipmentData = mapShipmentDataToProfitShare(shipmentDetails);
                                    arValidationResponse.setTransferToTriangulationPartner(true);
                                    arValidationResponse.setTriangulationShipment(triangulationShipmentData);
                                } else if (destinationShipmentsMap.containsKey(originShipment.getGuid())) {
                                    var ships = destinationShipmentsMap.get(originShipment.getGuid());
                                    var isShip = ships.stream().filter(x -> x.getTenantId().equals(triangulationPartner.intValue())).findAny();
                                    if (isShip.isPresent()) {
                                        ArValidationResponse.ProfitShareShipmentData triangulationShipmentData = mapShipmentDataToProfitShare(shipmentDetails);
                                        arValidationResponse.setTransferToTriangulationPartner(true);
                                        arValidationResponse.setTriangulationShipment(triangulationShipmentData);
                                    }
                                }
                            }
                        }
                    }
                }
                else if (shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()) {
                    ConsolidationDetails consolidationDetails;
                    if (consolidationDetailsMap.containsKey(shipmentDetails.getConsolidationList().get(0).getGuid())) {
                        consolidationDetails = consolidationDetailsMap.get(shipmentDetails.getConsolidationList().get(0).getGuid());
                        var receivingAgent = consolidationDetails.getReceivingBranch();
                        var triangulationPartner = consolidationDetails.getTriangulationPartner();
                        if (receivingAgent == null && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
                            var destination = shipmentDetails.getCarrierDetails().getDestination();
                            if (destination != null && unlocationsResponseMap.containsKey(destination)) {
                                arValidationResponse.setDestinationCountry(unlocationsResponseMap.get(destination).getCountry());
                            }
                        }
                        arValidationResponse.setReceivingAgent(receivingAgent);
                        arValidationResponse.setTriangulationPartner(triangulationPartner);
                        arValidationResponse.setOrigin(shipmentDetails.getTenantId());
                        ArValidationResponse.ProfitShareShipmentData originShipmentData = mapShipmentDataToProfitShare(shipmentDetails);
                        arValidationResponse.setOriginShipment(originShipmentData);
                        if (receivingAgent != null && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP) &&
                                !shipmentDetails.getTenantId().equals(receivingAgent.intValue()) && destinationShipmentsMap.containsKey(shipmentDetails.getGuid())) {
                            var ships = destinationShipmentsMap.get(shipmentDetails.getGuid());
                            var isShip = ships.stream().filter(x -> x.getTenantId().equals(receivingAgent.intValue())).findAny();
                            if (isShip.isPresent()) {
                                arValidationResponse.setTransferToReceivingAgent(true);
                                ArValidationResponse.ProfitShareShipmentData receivingShipmentData = mapShipmentDataToProfitShare(isShip.get());
                                arValidationResponse.setReceivingShipment(receivingShipmentData);
                            }
                        }

                        if (triangulationPartner != null) {
                            if (Objects.equals(triangulationPartner, receivingAgent)) {
                                arValidationResponse.setTransferToTriangulationPartner(arValidationResponse.getTransferToReceivingAgent());
                                arValidationResponse.setTriangulationShipment(arValidationResponse.getReceivingShipment());
                            } else if (!shipmentDetails.getTenantId().equals(triangulationPartner.intValue()) && destinationShipmentsMap.containsKey(shipmentDetails.getGuid())) {
                                var ships = destinationShipmentsMap.get(shipmentDetails.getGuid());
                                var isShip = ships.stream().filter(x -> x.getTenantId().equals(triangulationPartner.intValue())).findAny();
                                if (isShip.isPresent()) {
                                    arValidationResponse.setTransferToTriangulationPartner(true);
                                    ArValidationResponse.ProfitShareShipmentData triangulationData = mapShipmentDataToProfitShare(isShip.get());
                                    arValidationResponse.setTriangulationShipment(triangulationData);
                                }
                            }
                        }
                    }
                }
                responseList.add(arValidationResponse);
            }

        }
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    private List<ShipmentDetails> findShipmentsFromLogsHistory(List<UUID> guids, LocalDateTime timeStamp) throws RunnerException {
        List<LogHistoryResponse> logHistoryResponses = logsHistoryService.findByEntityGuidsAndTimeStamp(guids, timeStamp);
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        Set<UUID> remainingGuids = new HashSet<>(guids);
        if(!logHistoryResponses.isEmpty())
            logHistoryResponses.forEach(
                    log -> {
                        shipmentDetailsList.add(jsonHelper.readFromJson(log.getEntityPayload(), ShipmentDetails.class));
                        remainingGuids.remove(log.getEntityGuid());
                    });

        if(!Objects.equals(shipmentDetailsList.size(), guids.size())){
            List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByGuids(remainingGuids);
            shipmentDetailsList.addAll(shipmentDetails);
        }

        return shipmentDetailsList;
    }

    private List<ConsolidationDetails> findConsolidationFromLogsHistory(List<UUID> guids, LocalDateTime timeStamp) throws RunnerException {
        List<LogHistoryResponse> logHistoryResponsesForConsole = logsHistoryService.findByEntityGuidsAndTimeStamp(guids, timeStamp);
        List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();
        Set<UUID> remainingGuids = new HashSet<>(guids);
        if(!logHistoryResponsesForConsole.isEmpty())
            logHistoryResponsesForConsole.forEach(log -> {
                consolidationDetailsList.add(jsonHelper.readFromJson(log.getEntityPayload(), ConsolidationDetails.class));
                remainingGuids.remove(log.getEntityGuid());
            });

        if(!Objects.equals(consolidationDetailsList.size(), guids.size())){
            List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByGuids(remainingGuids);
            consolidationDetailsList.addAll(consolidationDetails);
        }
        return consolidationDetailsList;
    }

    private ArValidationResponse.ProfitShareShipmentData mapShipmentDataToProfitShare(ShipmentDetails shipmentDetails) {
        return  ArValidationResponse.ProfitShareShipmentData.builder()
                .shipmentType(shipmentDetails.getShipmentType())
                .transportMode(shipmentDetails.getTransportMode())
                .shipmentGuid(shipmentDetails.getGuid())
                .createdBy(shipmentDetails.getCreatedBy())
                .hbl(shipmentDetails.getHouseBill())
                .mbl(shipmentDetails.getMasterBill())
                .shipmentId(shipmentDetails.getShipmentId())
                .createdAt(shipmentDetails.getCreatedAt())
                .bookingType(shipmentDetails.getBookingType())
                .bookingReference(shipmentDetails.getBookingReference())
                .status(shipmentDetails.getStatus() != null ?ShipmentStatus.fromValue(shipmentDetails.getStatus()).name(): null)
                .jobType(shipmentDetails.getJobType())
                .orderNumber(shipmentDetails.getOrderNumber())
                .build();
    }

}
