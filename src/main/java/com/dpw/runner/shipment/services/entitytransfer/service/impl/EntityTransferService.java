package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchTenantIdContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchTenantIdDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.request.CheckTaskExistV1Request;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
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
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;


import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Service
@Slf4j
@NoArgsConstructor
public class EntityTransferService implements IEntityTransferService {
    public static final String SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Shipment Details is null for Id {} with Request Id {}";
    public static final String CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Consolidation Details is null for Id {} with Request Id {}";
    private IShipmentSettingsDao shipmentSettingsDao;
    private IShipmentDao shipmentDao;
    private IShipmentService shipmentService;
    private IConsolidationService consolidationService;
    private IConsolidationDetailsDao consolidationDetailsDao;
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    private ModelMapper modelMapper;
    private IV1Service v1Service;
    private JsonHelper jsonHelper;
    private IHblDao hblDao;
    private IAwbDao awbDao;
    private IEventDao eventDao;
    private MasterDataUtils masterDataUtils;
    private ILogsHistoryService logsHistoryService;
    private IContainerDao containerDao;
    private IPackingDao packingDao;
    private MasterDataFactory masterDataFactory;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private CommonUtils commonUtils;
    private IV1Service iv1Service;
    private V1ServiceUtil v1ServiceUtil;
    private ITasksService tasksService;
    private INotificationService notificationService;
    private ExecutorService executorService;
    @Autowired
    DocumentManagerRestClient documentManagerRestClient;

    @Autowired
    public EntityTransferService(IShipmentSettingsDao shipmentSettingsDao, IShipmentDao shipmentDao, IShipmentService shipmentService, IConsolidationService consolidationService, IConsolidationDetailsDao consolidationDetailsDao, IShipmentsContainersMappingDao shipmentsContainersMappingDao, ModelMapper modelMapper, IV1Service v1Service, JsonHelper jsonHelper, IHblDao hblDao, IAwbDao awbDao, IEventDao eventDao, MasterDataUtils masterDataUtils, ILogsHistoryService logsHistoryService, IContainerDao containerDao, IPackingDao packingDao, MasterDataFactory masterDataFactory, CommonUtils commonUtils, IV1Service iv1Service, V1ServiceUtil v1ServiceUtil, ITasksService tasksService, INotificationService notificationService, ExecutorService executorService) {
        this.shipmentSettingsDao = shipmentSettingsDao;
        this.shipmentDao = shipmentDao;
        this.shipmentService = shipmentService;
        this.consolidationService = consolidationService;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.shipmentsContainersMappingDao = shipmentsContainersMappingDao;
        this.modelMapper = modelMapper;
        this.v1Service = v1Service;
        this.jsonHelper = jsonHelper;
        this.hblDao = hblDao;
        this.awbDao = awbDao;
        this.eventDao = eventDao;
        this.masterDataUtils = masterDataUtils;
        this.logsHistoryService = logsHistoryService;
        this.containerDao = containerDao;
        this.packingDao = packingDao;
        this.masterDataFactory = masterDataFactory;
        this.commonUtils = commonUtils;
        this.iv1Service = iv1Service;
        this.v1ServiceUtil = v1ServiceUtil;
        this.tasksService = tasksService;
        this.notificationService = notificationService;
        this.executorService = executorService;
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> sendShipment(CommonRequestModel commonRequestModel) {
        SendShipmentRequest sendShipmentRequest = (SendShipmentRequest) commonRequestModel.getData();
        Long shipId = sendShipmentRequest.getShipId();
        List<Integer> sendToBranch = sendShipmentRequest.getSendToBranch();
        List<String> additionalDocs = sendShipmentRequest.getAdditionalDocs();
        List<String> sendToOrg = sendShipmentRequest.getSendToOrg();
        if((sendToBranch == null || sendToBranch.isEmpty()) && (sendToOrg == null || sendToOrg.isEmpty())){
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipId);
        if (shipmentDetails.isEmpty()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, shipId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipment = shipmentDetails.get();

        List<Integer> successTenantIds = new ArrayList<>();

        Set<Integer> uniqueDestinationTenants = new HashSet<>(sendShipmentRequest.getSendToBranch());
        validationsBeforeSendTask(uniqueDestinationTenants);
        var tenantMap = getTenantMap(List.of(shipment.getTenantId()));

        CompletableFuture<Void> emailFuture = CompletableFuture.runAsync(() ->
            sendShipmentEmailNotification(shipment, uniqueDestinationTenants.stream().toList())
        );

        List<Integer> destinationTenantList = uniqueDestinationTenants.stream().toList();
        var entityTransferPayload = prepareShipmentPayload(shipment);
        entityTransferPayload.setSourceBranchTenantName(tenantMap.get(shipment.getTenantId()).getTenantName());
        entityTransferPayload.setAdditionalDocs(additionalDocs);

        for(int i = 0; i < destinationTenantList.size(); i++) {
            var tenant = destinationTenantList.get(i);
            var taskPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferShipmentDetails.class);
            if((Long.valueOf(tenant).equals(shipment.getReceivingBranch()))) {
                taskPayload.setDirection(reverseDirection(shipment.getDirection()));
            }
            taskPayload.setSendToBranch(tenant);

            createTask(taskPayload, shipment.getId(), Constants.Shipments, tenant);
            successTenantIds.add(tenant);
        }

        List<String> tenantName = getTenantName(successTenantIds);
        createSendEvent(tenantName, shipment.getReceivingBranch(), shipment.getTriangulationPartner(), shipment.getDocumentationPartner(), shipId.toString(), Constants.SHIPMENT_SENT, Constants.SHIPMENT, null);
        if(Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
            shipmentDao.saveEntityTransfer(shipId, Boolean.TRUE);

        emailFuture.join();

        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds).build();
        return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
    }

    private Integer getShipmentConsoleImportApprovalRole(int tenantId) {
        return shipmentSettingsDao.getShipmentConsoleImportApprovarRole(tenantId);
    }

    private List<Integer> tenantIdFromOrganizations (List<String> sendToOrg) {
        List<String> guidList = new ArrayList<>();
        CommonV1ListRequest orgRequest = new CommonV1ListRequest();
        List<Object> orgField = new ArrayList<>(List.of("OrganizationCode"));
        String operator = Operators.IN.getValue();
        List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, List.of(sendToOrg)));
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
        if(guidList != null || !guidList.isEmpty()) {
            guidList.forEach(guid -> {
                CommonV1ListRequest request = new CommonV1ListRequest();
                List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.GUID));
                List<Object> criteria = new ArrayList<>(List.of(field, "=", guid));
                request.setCriteriaRequests(criteria);
                TenantIdResponse tenantId = v1Service.tenantByGuid(request);
                tenantIds.add(tenantId.getId());
            });
        }
        return tenantIds;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> sendConsolidation(CommonRequestModel commonRequestModel) {
        SendConsolidationRequest sendConsolidationRequest = (SendConsolidationRequest) commonRequestModel.getData();
        Long consolId = sendConsolidationRequest.getConsolId();
        List<Integer> sendToBranch = sendConsolidationRequest.getSendToBranch();
        List<String> sendToOrg = sendConsolidationRequest.getSendToOrg();
        List<Integer> successTenantIds = new ArrayList<>();

        if ((sendToBranch == null || sendToBranch.isEmpty()) && (sendToOrg == null || sendToOrg.isEmpty())) {
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }
        // Validation for importer role in receiving branch only for consolidation
        Set<Integer> tenantIds = new HashSet<>(sendConsolidationRequest.getSendToBranch());
        validationsBeforeSendTask(tenantIds);

        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolId);
        if (!consolidationDetails.isPresent()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails consol = consolidationDetails.get();

        CompletableFuture<Void> emailFuture = CompletableFuture.runAsync(() ->
            sendConsolidationEmailNotification(consol, sendToBranch)
        );

        interBranchValidation(consol, sendConsolidationRequest);

        List<EntityTransferConsolidationDetails> entityTransferConsolList = new ArrayList<>();
        for (int i = 0; i < sendToBranch.size(); i++) {
            var tenant = sendToBranch.get(i);
            var entityTransferPayload = prepareConsolidationPayload(consolidationDetails.get(), i, sendConsolidationRequest);
            entityTransferConsolList.add(entityTransferPayload);
            createTask(entityTransferPayload, consolidationDetails.get().getId(), Constants.Consolidations, tenant);

            successTenantIds.add(tenant);
        }

        // ~~~~~~~~~~ KEEP IT AS IS ~~~~~~~~~~~~~
        this.createAutoEvent(consolidationDetails.get().getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.CONSOLIDATION);
        List<String> tenantName = getTenantName(successTenantIds);
        String consolDesc = createSendEvent(tenantName, consolidationDetails.get().getReceivingBranch(), consolidationDetails.get().getTriangulationPartner(), consolidationDetails.get().getDocumentationPartner(), consolidationDetails.get().getId().toString(), Constants.CONSOLIDATION_SENT, Constants.CONSOLIDATION, null);
        for (var shipment : consolidationDetails.get().getShipmentsList()) {
            this.createAutoEvent(shipment.getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.SHIPMENT);
            createSendEvent(tenantName, shipment.getReceivingBranch(), shipment.getTriangulationPartner(), shipment.getDocumentationPartner(), shipment.getId().toString(), Constants.SHIPMENT_SENT, Constants.SHIPMENT, consolDesc);
            if (Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
                shipmentDao.saveEntityTransfer(shipment.getId(), Boolean.TRUE);
        }
        // ~~~~~~~~~~ END ~~~~~~~~~~~~~
        emailFuture.join();

        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(successTenantIds)
            .jsonString(jsonHelper.convertToJson(entityTransferConsolList))
            .build();
        return ResponseHelper.buildSuccessResponse(sendConsolidationResponse);

    }


    private void interBranchValidation(ConsolidationDetails consol, SendConsolidationRequest sendConsolidationRequest) {
        if(Boolean.TRUE.equals(consol.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
            Set<Integer> uniqueTenants = new HashSet<>(sendConsolidationRequest.getSendToBranch());
            var tenantSettingsMap = v1ServiceUtil.getTenantSettingsMap(uniqueTenants.stream().toList());
            List<Integer> shipmentTenantId = consol.getShipmentsList().stream().map(ShipmentDetails::getTenantId).toList();
            List<Integer> errorTenants = new ArrayList<>();

            for(var tenantSet : tenantSettingsMap.entrySet()) {
                var tenant = tenantSet.getKey();
                var tenantSetting = tenantSettingsMap.get(tenant);
                if(tenantSetting.getColoadingBranchIds() != null && !tenantSetting.getColoadingBranchIds().containsAll(shipmentTenantId)) {
                    errorTenants.add(tenant);
                }
            }

            if(!errorTenants.isEmpty()) {
                throw new ValidationException(String.format("Destination branches %s not having co-loading branch ids", errorTenants));
            }
        }
    }

    private void validationsBeforeSendTask(Set<Integer> sendBranches) {
        List<Integer> nonApprovalTenants = new ArrayList<>();
        for (Integer tenantId: sendBranches) {
            Integer approverRoleId = getShipmentConsoleImportApprovalRole(tenantId);
            if (approverRoleId == null || approverRoleId == 0) {
                nonApprovalTenants.add(tenantId);
            }
        }
        if(nonApprovalTenants.isEmpty())
            return;
        List<String> tenantNames = getTenantName(nonApprovalTenants);
        assert tenantNames != null;
        throw new ValidationException(EntityTransferConstants.APPROVAL_ROLE_NOT_ASSIGNED + String.join(", ", tenantNames));
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> importShipment (CommonRequestModel commonRequestModel) throws RunnerException {
        ImportShipmentRequest importShipmentRequest = (ImportShipmentRequest) commonRequestModel.getData();

//        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.REJECTED.getDescription())){
//            updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.REJECTED, importShipmentRequest.getRejectRemarks());
//            return ResponseHelper.buildSuccessResponse();
//        }

        if (importShipmentRequest.getEntityData() == null) {
            throw new ValidationException("No Shipment payload please check");
        }
        CopyDocumentsRequest copyDocumentsRequest = new CopyDocumentsRequest();
        EntityTransferShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityData();
        ShipmentDetailsResponse shipmentDetailsResponse =  this.createShipment(entityTransferShipmentDetails, copyDocumentsRequest);
        String shipmentId = shipmentDetailsResponse.getShipmentId();

        // TODO: create document
        copyDocumentsRequest.setDeleteExistingDocuments(true);
//                if(copyDocumentsRequest.getDocuments() != null && !copyDocumentsRequest.getDocuments().isEmpty()){
//            // call Doc service
//        }

        // Task status approved
//        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.APPROVED.getDescription())) {
//            updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.APPROVED, importShipmentRequest.getRejectRemarks());
//        }

        var response = ImportShipmentResponse.builder()
                .shipmentId(shipmentId)
                .message("Shipment Imported Successfully with Shipment Number: " + shipmentId)
                .build();
        return ResponseHelper.buildSuccessResponse(response);
    }
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

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> importConsolidation (CommonRequestModel commonRequestModel) throws RunnerException {
        ImportConsolidationRequest importConsolidationRequest = (ImportConsolidationRequest) commonRequestModel.getData();

        // Task status rejected
        if(Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            updateTaskStatus(importConsolidationRequest.getTaskId(), TaskStatus.REJECTED, importConsolidationRequest.getRejectRemarks());
            return ResponseHelper.buildSuccessResponse();
        }
        if (importConsolidationRequest == null || importConsolidationRequest.getEntityData() == null) {
            throw new ValidationException("No consolidation payload please check");
        }
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = importConsolidationRequest.getEntityData();

        ConsolidationDetailsResponse consolidationDetailsResponse = this.createConsolidation(entityTransferConsolidationDetails);
        String consolidationNumber = Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getConsolidationNumber).orElse(null);

        // Task status approved
        if(Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.APPROVED.getDescription())) {
            updateTaskStatus(importConsolidationRequest.getTaskId(), TaskStatus.APPROVED, importConsolidationRequest.getRejectRemarks());
        }

        var response = ImportConsolidationResponse.builder()
                .consolidationNumber(consolidationNumber)
                .message("Consolidation Imported Successfully with Consolidation Number: " + consolidationNumber)
                .build();

        return ResponseHelper.buildSuccessResponse(response);
    }

    private ConsolidationDetailsResponse createConsolidation (EntityTransferConsolidationDetails entityTransferConsolidationDetails) throws RunnerException {

        List<ConsolidationDetails> oldConsolidationDetailsList = consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid());
        Map<UUID, List<UUID>> oldContVsOldShipGuidMap = entityTransferConsolidationDetails.getContainerVsShipmentGuid();
        Map<UUID, UUID> oldPackVsOldContGuidMap = entityTransferConsolidationDetails.getPackingVsContainerGuid();

        Map<UUID, Long> oldVsNewShipIds = new HashMap<>();
        Map<UUID, UUID> newVsOldPackingGuid = new HashMap<>();
        Map<UUID, UUID> newVsOldContainerGuid = new HashMap<>();
        Map<UUID, Long> oldGuidVsNewContainerId = new HashMap<>();

        List<Long> shipmentIds = new ArrayList<>();
        List<UUID> shipmentGuids = new ArrayList<>();
        List<ShipmentDetailsResponse> shipmentDetailsResponseList = new ArrayList<>();
        CopyDocumentsRequest copyDocumentsRequest = new CopyDocumentsRequest();

        List<Long> interBranchShipment = new ArrayList<>();

        // detach all shipment from console
        if(oldConsolidationDetailsList != null && !oldConsolidationDetailsList.isEmpty()) {
            List<Long> detachShipIds = oldConsolidationDetailsList.get(0).getShipmentsList().stream().map(BaseEntity::getId).toList();
            if(!detachShipIds.isEmpty())
                consolidationService.detachShipments(oldConsolidationDetailsList.get(0).getId(), detachShipIds);
        }


        // Create Shipments
        if(!Objects.isNull(entityTransferConsolidationDetails.getShipmentsList()) && !entityTransferConsolidationDetails.getShipmentsList().isEmpty()) {
            entityTransferConsolidationDetails.getShipmentsList().forEach(ship -> {
                try {
                    // container will be created with consolidation
                    ship.setContainersList(null);

                    // replaced old packing guid with new
                    if(ship.getPackingList() != null && !ship.getPackingList().isEmpty()) {
                        ship.getPackingList().forEach(pack -> {
                            UUID newGuid = UUID.randomUUID();
                            newVsOldPackingGuid.put(newGuid, pack.getGuid());
                            pack.setGuid(newGuid);
                        });
                    }

                    if(entityTransferConsolidationDetails.getShipAdditionalDocs() != null && entityTransferConsolidationDetails.getShipAdditionalDocs().containsKey(ship.getGuid().toString())) {
                        ship.setAdditionalDocs(entityTransferConsolidationDetails.getShipAdditionalDocs().get(ship.getGuid().toString()));
                    }
                    ShipmentDetailsResponse shipmentDetailsResponse = createShipment(ship, copyDocumentsRequest);
                    shipmentDetailsResponseList.add(shipmentDetailsResponse);

                    oldVsNewShipIds.put(ship.getGuid(), shipmentDetailsResponse.getId());
                    shipmentIds.add(shipmentDetailsResponse.getId());
                    shipmentGuids.add(shipmentDetailsResponse.getGuid());
                    if(!Objects.equals(shipmentDetailsResponse.getTenantId(), UserContext.getUser().getTenantId())){
                        interBranchShipment.add(shipmentDetailsResponse.getId());
                    }
                } catch (RunnerException e) {
                    throw new RuntimeException(e);
                }
            });
        }
        if(entityTransferConsolidationDetails.getContainersList() != null && !entityTransferConsolidationDetails.getContainersList().isEmpty()) {
            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
                UUID newGuid = UUID.randomUUID();
                newVsOldContainerGuid.put(newGuid, cont.getGuid());
                cont.setGuid(newGuid);
            });
        }
        // Packing will be created with shipment
        entityTransferConsolidationDetails.setPackingList(null);

        ConsolidationDetailsRequest consolidationDetailsRequest =  modelMapper.map(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);

        ConsolidationDetailsResponse consolidationDetailsResponse = null;
        // create or update console
        if(oldConsolidationDetailsList == null || oldConsolidationDetailsList.isEmpty()) {
            consolidationDetailsRequest.setGuid(null);
            consolidationDetailsRequest.setShipmentsList(null);
            consolidationDetailsRequest.setSourceGuid(entityTransferConsolidationDetails.getGuid());

            consolidationDetailsResponse = consolidationService.createConsolidationFromEntityTransfer(consolidationDetailsRequest);
        } else {
            consolidationDetailsRequest = jsonHelper.convertValue(oldConsolidationDetailsList.get(0), ConsolidationDetailsRequest.class);

            Long id = consolidationDetailsRequest.getId();
            UUID guid = consolidationDetailsRequest.getGuid();
            this.cleanAllListEntitiesForConsolidation(consolidationDetailsRequest);
            modelMapper.map(entityTransferConsolidationDetails, consolidationDetailsRequest);
            consolidationDetailsRequest.setId(id);
            consolidationDetailsRequest.setGuid(guid);
            consolidationDetailsRequest.setShipmentsList(null);
            consolidationDetailsRequest.setSourceGuid(entityTransferConsolidationDetails.getGuid());

            consolidationDetailsResponse = consolidationService.completeUpdateConsolidationFromEntityTransfer(consolidationDetailsRequest);
            oldConsolidationDetailsList.get(0).setContainersList(jsonHelper.convertValueToList(consolidationDetailsResponse.getContainersList(), Containers.class));
        }

        // attach console and shipment
        if(consolidationDetailsResponse != null) {
            if(!interBranchShipment.isEmpty()) {
                createShipmentPullRequest(interBranchShipment, consolidationDetailsResponse.getId());
                consolidationService.attachShipments(ShipmentRequestedType.APPROVE, consolidationDetailsResponse.getId(), shipmentIds);
            } else {
                consolidationService.attachShipments(null, consolidationDetailsResponse.getId(), shipmentIds);
            }

            if (consolidationDetailsResponse.getContainersList() != null && !consolidationDetailsResponse.getContainersList().isEmpty()) {
                this.attachShipmentToContainers(consolidationDetailsResponse.getId(), newVsOldContainerGuid, oldContVsOldShipGuidMap, oldVsNewShipIds, oldGuidVsNewContainerId);
            }


            if(!shipmentDetailsResponseList.isEmpty() && !oldPackVsOldContGuidMap.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packingList = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());

                this.attachPackToContainers(packingList.stream().toList(), newVsOldPackingGuid, oldPackVsOldContGuidMap, oldGuidVsNewContainerId);
            }

            this.createImportEvent(entityTransferConsolidationDetails.getSourceBranchTenantName(), consolidationDetailsResponse.getId(), Constants.CONSOLIDATION_IMPORTED, Constants.CONSOLIDATION);

            // Copy docs request for doc service
            if(entityTransferConsolidationDetails.getAdditionalDocs() != null && !entityTransferConsolidationDetails.getAdditionalDocs().isEmpty()) {
                CopyDocumentsRequest.DocumentRequest documentRequest = CopyDocumentsRequest.DocumentRequest.builder()
                        .entityKey(consolidationDetailsResponse.getGuid().toString())
                        .tenantId(consolidationDetailsResponse.getTenantId())
                        .entityType("[Consolidations]")
                        .docGuid(entityTransferConsolidationDetails.getAdditionalDocs()).build();
                copyDocumentsRequest.getDocuments().add(documentRequest);
            }
            copyDocumentsRequest.setDeleteExistingDocuments(true);

            // TODO: create document
//        if(copyDocumentsRequest.getDocuments() != null && !copyDocumentsRequest.getDocuments().isEmpty()){
//            // call Doc service
//        }
        }

        if(!shipmentGuids.isEmpty()) {
            ConsolidationDetailsResponse finalConsolidationDetailsResponse = consolidationDetailsResponse;
            CompletableFuture.runAsync(() -> sendGroupedEmailForShipmentImport(finalConsolidationDetailsResponse, shipmentGuids))
                    .exceptionally(ex -> {
                        log.error("Send consolidated email failure due to: " + ex.getMessage());
                        return null;
                    });
        }

        return consolidationDetailsResponse;
    }


    private ShipmentDetailsResponse createShipment(EntityTransferShipmentDetails entityTransferShipmentDetails, CopyDocumentsRequest copyDocumentsRequest) throws RunnerException {
        ShipmentRequest shipmentRequest = modelMapper.map(entityTransferShipmentDetails, ShipmentRequest.class);
        var tenantId = UserContext.getUser().getTenantId();
        if(Objects.equals(shipmentRequest.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.equals(shipmentRequest.getJobType(), Constants.SHIPMENT_TYPE_DRT) && !Objects.equals(entityTransferShipmentDetails.getSendToBranch(), tenantId)){
            commonUtils.setInterBranchContextForHub();
            InterBranchTenantIdContext.setContext(InterBranchTenantIdDto.builder().tenantId(entityTransferShipmentDetails.getSendToBranch()).build());
        }
        List<ShipmentDetails> oldShipmentDetailsList = shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch());

        ShipmentDetailsResponse shipmentDetailsResponse = null;
        if(oldShipmentDetailsList == null || oldShipmentDetailsList.isEmpty()){
            shipmentRequest.setGuid(null);
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());
            shipmentRequest.setMasterBill(null);

            shipmentDetailsResponse = shipmentService.createShipmentFromEntityTransfer(shipmentRequest);
        } else {
            shipmentRequest = jsonHelper.convertValue(oldShipmentDetailsList.get(0), ShipmentRequest.class);

            Long id = shipmentRequest.getId();
            UUID guid = shipmentRequest.getGuid();
            this.cleanAllListEntitiesForShipment(shipmentRequest);
            modelMapper.map(entityTransferShipmentDetails, shipmentRequest);
            shipmentRequest.setId(id);
            shipmentRequest.setGuid(guid);
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());
            shipmentRequest.setConsolidationList(List.of());
            shipmentRequest.setMasterBill(null);

            shipmentDetailsResponse = shipmentService.completeUpdateShipmentFromEntityTransfer(shipmentRequest);
            oldShipmentDetailsList.get(0).setPackingList(jsonHelper.convertValueToList(shipmentDetailsResponse.getPackingList(), Packing.class));
        }

        this.createImportEvent(entityTransferShipmentDetails.getSourceBranchTenantName(), shipmentDetailsResponse.getId(), Constants.SHIPMENT_IMPORTED, Constants.SHIPMENT);

        //TODO: create document
        // Copy docs request for doc service
        if(entityTransferShipmentDetails.getAdditionalDocs() != null && !entityTransferShipmentDetails.getAdditionalDocs().isEmpty()) {
            CopyDocumentsRequest.DocumentRequest documentRequest = CopyDocumentsRequest.DocumentRequest.builder()
                    .entityKey(shipmentDetailsResponse.getGuid().toString())
                    .tenantId(shipmentDetailsResponse.getTenantId())
                    .entityType("[Shipments]")
                    .docGuid(entityTransferShipmentDetails.getAdditionalDocs()).build();
            copyDocumentsRequest.getDocuments().add(documentRequest);
        }

        InterBranchTenantIdContext.removeContext();

        return shipmentDetailsResponse;
    }

    private void createShipmentPullRequest(List<Long> shipIds, Long consoleId ) {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        for (var shipId: shipIds) {
            ConsoleShipmentMapping entity = ConsoleShipmentMapping.builder()
                    .shipmentId(shipId)
                    .consolidationId(consoleId)
                    .isAttachmentDone(false)
                    .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                    .build();
            consoleShipmentMappingList.add(entity);
        }
        consoleShipmentMappingDao.saveAll(consoleShipmentMappingList);
    }

    private void cleanAllListEntitiesForShipment(ShipmentRequest shipmentRequest){
        shipmentRequest.getPackingList().clear();
        shipmentRequest.getContainersList().clear();
        shipmentRequest.getRoutingsList().clear();
        shipmentRequest.getReferenceNumbersList().clear();
        shipmentRequest.getBookingCarriagesList().clear();
        shipmentRequest.getServicesList().clear();
        shipmentRequest.getShipmentAddresses().clear();
    }

    private void cleanAllListEntitiesForConsolidation(ConsolidationDetailsRequest consolidationDetailsRequest){
        consolidationDetailsRequest.getPackingList().clear();
        consolidationDetailsRequest.getContainersList().clear();
        consolidationDetailsRequest.getRoutingsList().clear();
        consolidationDetailsRequest.getReferenceNumbersList().clear();
        consolidationDetailsRequest.getConsolidationAddresses().clear();
        consolidationDetailsRequest.getShipmentsList().clear();
    }

    private void updateTaskStatus(Long taskId, TaskStatus status, String rejectionRemarks) {

        var taskUpdateRequest = TaskUpdateRequest.builder()
                .id(taskId.toString())
                .status(status.getName())
                .rejectionRemarks(rejectionRemarks)
                .build();
        tasksService.updateTask(CommonRequestModel.buildRequest(taskUpdateRequest));
    }

//    private void importConsolidationDocs(UUID consoleGuid, List<String> consoleDocGuids, Map<String, List<String>> shipmentsDocsGuids, Map<String, String> oldVsNewShipmentGuids) {
//        CopyDocumentsRequest request = CopyDocumentsRequest.builder().documents().build();
//        List<CopyDocumentsRequest.DocumentRequest> documents = new ArrayList<>();
//        CopyDocumentsRequest.DocumentRequest documentRequest = CopyDocumentsRequest.DocumentRequest.builder()
//                .entityKey().entityType().tenantId()
//                .docGuid().build();
//
//        documentManagerRestClient.copyDocuments(CommonRequestModel.buildRequest(request));
//    }

    private void attachShipmentToContainers(Long consoleId, Map<UUID, UUID> newVsOldContainerGuid, Map<UUID, List<UUID>> oldContVsOldShipGuidMap, Map<UUID, Long> oldVsNewShipIds, Map<UUID, Long> oldGuidVsNewContainerId) {
        List<Containers> containersList = containerDao.findByConsolidationId(consoleId);
        containersList.forEach(cont -> {
            List<ShipmentDetails> shipmentDetails = new ArrayList<>();
            List<UUID> shipmentGuids = new ArrayList<>();
            oldGuidVsNewContainerId.put(newVsOldContainerGuid.get(cont.getGuid()), cont.getId());
            if(oldContVsOldShipGuidMap.containsKey(newVsOldContainerGuid.get(cont.getGuid()))) {
                shipmentGuids = oldContVsOldShipGuidMap.get(newVsOldContainerGuid.get(cont.getGuid()));
            }
            if (shipmentGuids != null && !shipmentGuids.isEmpty()) {
                shipmentGuids.forEach(shipGuid -> {
                    var shipmentDetail = new ShipmentDetails();
                    shipmentDetail.setId(oldVsNewShipIds.get(shipGuid));
                    shipmentDetails.add(shipmentDetail);
                });
            }
            cont.setShipmentsList(shipmentDetails);
        });
        containerDao.saveAll(containersList);
    }

    private void attachPackToContainers(List<Packing> packingList, Map<UUID, UUID> newVsOldPackingGuid, Map<UUID, UUID> oldPackVsOldContGuidMap, Map<UUID, Long> oldGuidVsNewContainerId) {
        if(packingList != null && !packingList.isEmpty()) {
            packingList.forEach(pack -> {
                if(oldPackVsOldContGuidMap.containsKey(newVsOldPackingGuid.get(pack.getGuid())) && oldGuidVsNewContainerId.containsKey(oldPackVsOldContGuidMap.get(newVsOldPackingGuid.get(pack.getGuid())))) {
                    Long id = oldGuidVsNewContainerId.get(oldPackVsOldContGuidMap.get(newVsOldPackingGuid.get(pack.getGuid())));
                    pack.setContainerId(id);
                }
            });
            packingDao.saveAll(packingList);
        }
    }


    private void createImportEvent(String tenantName, Long entityId, String eventCode, String entityType) {
            CustomAutoEventRequest eventReq = new CustomAutoEventRequest();
            eventReq.entityId = entityId;
            eventReq.entityType = entityType;
            eventReq.eventCode = eventCode;
            eventReq.isActualRequired = true;
            eventReq.placeName = tenantName;
            eventReq.createDuplicate = true;
            eventDao.autoGenerateEvents(eventReq);
    }




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
                boolean sendConsolidationError = false;
                boolean hblGenerationError = false;
                List<String> shipmentIds = new ArrayList<>();
                for (var shipment: consolidationDetails.get().getShipmentsList()) {
                    boolean isShipmentError = false;
                    if(Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()) && Objects.isNull(shipment.getReceivingBranch()))
                    {
                        sendConsolidationError = true;
                        isShipmentError = true;
                    }
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
                            isShipmentError = true;
                        }
                        if(hblGenerationError){
                            sendConsolidationError = true;
                            isShipmentError = true;
                        }
                    }
                    if(Boolean.TRUE.equals(isShipmentError))
                    {
                        shipmentIds.add(shipment.getShipmentId());
                    }
                }

                if(sendConsolidationError){
                    String interBranch = "";
                    if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
                        interBranch = "Receiving Branch, ";
                    if(hblGenerationError){
                        if(consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                            throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, " + interBranch + "Vessel & Voyage details in the attached shipments: " + String.join(", ", shipmentIds) + " and generate the Original HBL before sending consolidation");
                        } else if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                            throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, " + interBranch + "Airline and Flight number details in the attached shipments: " + String.join(", ", shipmentIds) + " and generate the Original HAWB before sending consolidation");
                        }
                    } else {
                        if(consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                            throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, " + interBranch + "Vessel & Voyage details in the attached shipments: " + String.join(", ", shipmentIds) + " before sending consolidation");
                        } else if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                            throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, " + interBranch + "Airline and Flight number details in the attached shipments: " + String.join(", ", shipmentIds) + " before sending consolidation");
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
    public ResponseEntity<IRunnerResponse> checkTaskExist(CommonRequestModel commonRequestModel) throws RunnerException {
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
            throw new RunnerException("Check Task exist failed to check from V1: " + ex);
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
        return Collections.emptyList();
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
                            arValidationResponse.setSalesBranch(originShipment.getSalesBranch());
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
                        arValidationResponse.setSalesBranch(shipmentDetails.getSalesBranch());
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

    @Override
    public ResponseEntity<IRunnerResponse> checkEntityExists(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CheckEntityExistRequest request = (CheckEntityExistRequest) commonRequestModel.getData();
            if (request == null || Objects.isNull(request.getEntityId()) || Objects.isNull(request.getEntityType())) {
                log.error("Request is empty for Check entity exists with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            boolean isPresent = false;
            switch (request.getEntityType()) {
                case Constants.Shipment -> isPresent = !shipmentDao.findBySourceGuid(UUID.fromString(request.getEntityId())).isEmpty();
                case Constants.Consolidation -> isPresent = !consolidationDetailsDao.findBySourceGuid(UUID.fromString(request.getEntityId())).isEmpty();
                default -> {}
            }

            return ResponseHelper.buildSuccessResponse(CheckEntityExistResponse.builder().isEntityExists(isPresent).message(isPresent ? String.format(EntityTransferConstants.TRANSFERRED_ENTITY_ALREADY_PRESENT, request.getEntityType()) : null).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    private EntityTransferConsolidationDetails prepareConsolidationPayload(ConsolidationDetails consolidationDetails, int index, SendConsolidationRequest sendConsolidationRequest) {
        List<Integer> sendToBranch = sendConsolidationRequest.getSendToBranch();
        Map<String, List<Integer>> shipmentGuidSendToBranch = sendConsolidationRequest.getShipmentGuidSendToBranch();
        List<Integer> tenantIds = new ArrayList<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.addAll(consolidationDetails.getShipmentsList().stream().map(ShipmentDetails::getTenantId).toList());
        var currentTenant = sendToBranch.get(index);
        var tenantMap = getTenantMap(tenantIds);
        EntityTransferConsolidationDetails payload = jsonHelper.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        payload.setSendToBranch(currentTenant);
        boolean reverseDirection = false;
        if(Long.valueOf(currentTenant).equals(consolidationDetails.getReceivingBranch())) {
            payload.setShipmentType(reverseDirection(consolidationDetails.getShipmentType()));
            reverseDirection = true;
        }

        // Map container guid vs List<shipmentGuid>
        Map<UUID, List<UUID>> containerVsShipmentGuid = new HashMap<>();
        // List EntityTransferShipmentDetails
        List<EntityTransferShipmentDetails> transferShipmentDetails = new ArrayList<>();
        if(!consolidationDetails.getShipmentsList().isEmpty()) {
            for(var shipment : consolidationDetails.getShipmentsList()) {
                Long id = shipment.getId();
                UUID guid = shipment.getGuid();
                Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findById(id);
                if(shipmentDetailsOptional.isEmpty()) {
                    log.error("Shipment with id : {}, is not present while creating task payload", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                List<String> shipAdditionalDocs = Collections.emptyList();
                if(sendConsolidationRequest.getShipAdditionalDocs() != null && sendConsolidationRequest.getShipAdditionalDocs().get(guid.toString()) != null) {
                    shipAdditionalDocs = sendConsolidationRequest.getShipAdditionalDocs().get(guid.toString());
                }
                var entityTransferShipment = prepareShipmentPayload(shipmentDetailsOptional.get());
                if(reverseDirection) {
                    entityTransferShipment.setDirection(reverseDirection(shipment.getDirection()));
                }
                if(shipmentGuidSendToBranch != null && shipmentGuidSendToBranch.containsKey(guid.toString()))
                    entityTransferShipment.setSendToBranch(shipmentGuidSendToBranch.get(guid.toString()).get(index));
                else
                    entityTransferShipment.setSendToBranch(sendConsolidationRequest.getSendToBranch().get(index));
                entityTransferShipment.setSourceBranchTenantName(tenantMap.get(shipment.getTenantId()).getTenantName());
                entityTransferShipment.setAdditionalDocs(shipAdditionalDocs);
                transferShipmentDetails.add(entityTransferShipment);


                // populate container vs shipment guid map
                var shipmentGuid = shipmentDetailsOptional.get().getGuid();
                shipmentDetailsOptional.get().getContainersList().stream().map(Containers::getGuid).forEach(
                        containerGuid -> {
                            if(!containerVsShipmentGuid.containsKey(containerGuid)) {
                                containerVsShipmentGuid.put(containerGuid, new ArrayList<>());
                            }
                            containerVsShipmentGuid.get(containerGuid).add(shipmentGuid);
                        }
                );
            }
        }
        payload.setSourceBranchTenantName(tenantMap.get(consolidationDetails.getTenantId()).getTenantName());
        payload.setShipmentsList(transferShipmentDetails);
        payload.setContainerVsShipmentGuid(containerVsShipmentGuid);
        payload.setAdditionalDocs(sendConsolidationRequest.getAdditionalDocs());

        // packing guid vs container guid
        Map<UUID, UUID> packsVsContainerGuid = new HashMap<>();
        consolidationDetails.getContainersList().forEach(container ->
            container.getPacksList().forEach(pack -> packsVsContainerGuid.put(pack.getGuid(), container.getGuid()))
        );
        payload.setPackingVsContainerGuid(packsVsContainerGuid);
        // populate master data and other fields
        payload.setMasterData(getConsolMasterData(consolidationDetails));

        return payload;
    }

    private Map<String, Object> getConsolMasterData(ConsolidationDetails consolidationDetails) {
        ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        return consolidationService.fetchAllMasterDataByKey(consolidationDetails, consolidationDetailsResponse);
    }

    private EntityTransferShipmentDetails prepareShipmentPayload(ShipmentDetails shipmentDetails) {
        EntityTransferShipmentDetails payload = jsonHelper.convertValue(shipmentDetails, EntityTransferShipmentDetails.class);
        // populate master data and other fields
        payload.setMasterData(getShipmentMasterData(shipmentDetails));

        return payload;
    }

    private Map<String, Object> getShipmentMasterData(ShipmentDetails shipmentDetails) {
        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        return shipmentService.fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
    }

    private String reverseDirection(String direction) {
        String res = direction;
        if(Constants.DIRECTION_EXP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_IMP;
        }
        else if(Constants.DIRECTION_IMP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_EXP;
        }
        return res;
    }

    private Map<Integer, V1TenantResponse> getTenantMap(List<Integer> tenantId) {
        var v1Map = v1ServiceUtil.getTenantDetails(tenantId);
        return v1Map.keySet().stream().collect(
            Collectors.toMap(Function.identity(), k -> jsonHelper.convertValue(v1Map.get(k), V1TenantResponse.class))
        );
    }

    private void createTask(Object payload, Long entityId, String entityType, int tenantId) {
        TaskCreateRequest taskCreateRequest = new TaskCreateRequest();
        taskCreateRequest.setTaskJson(jsonHelper.convertToJson(payload));
        taskCreateRequest.setEntityId(entityId.toString());
        taskCreateRequest.setEntityType(entityType);
        taskCreateRequest.setRoleId(StringUtility.convertToString(getShipmentConsoleImportApprovalRole(tenantId)));
        taskCreateRequest.setTenantId(StringUtility.convertToString(tenantId));
        // can be moved as background task
        if(Constants.Consolidations.equalsIgnoreCase(entityType))
            taskCreateRequest.setTaskType(TaskType.CONSOLIDATION_IMPORTER.getDescription());
        else
            taskCreateRequest.setTaskType(TaskType.SHIPMENT_IMPORTER.getDescription());
        tasksService.createTask(CommonRequestModel.buildRequest(taskCreateRequest));
    }

    public void testSendConsolidationEmailNotification(Long consoleId, List<Integer> destinationBranches) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        consolidationDetails.ifPresent(details -> sendConsolidationEmailNotification(details, destinationBranches));
    }

//    public void testSendGroupedEmailForShipmentImport(Long consoleId, List<UUID> shipmentGuids) {
//        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
//        consolidationDetails.ifPresent(details -> sendGroupedEmailForShipmentImport(details, shipmentGuids));
//    }

    public void sendConsolidationEmailNotification(ConsolidationDetails consolidationDetails, List<Integer> destinationBranches) {
        List<String> requests = new ArrayList<>(List.of(CONSOLIDATION_IMPORT_EMAIL_TYPE));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        EmailTemplatesRequest emailTemplateModel = null;
        if(v1DataResponse != null) {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if(emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                for (EmailTemplatesRequest emailTemplate : emailTemplatesRequests) {
                    if(Objects.equals(emailTemplate.getType(), CONSOLIDATION_IMPORT_EMAIL_TYPE)) {
                        emailTemplateModel = emailTemplate;
                    }
                }
            }
        }
        if(emailTemplateModel == null)
            emailTemplateModel = new EmailTemplatesRequest();
        List<String> ccEmails = new ArrayList<>();
        for(Integer roleId: destinationBranches) {
            List<String> emailList = getRoleListByRoleId(roleId);
            if(!emailList.isEmpty()) {
                createConsolidationImportEmailBody(consolidationDetails, emailTemplateModel);
                try {
                    notificationService.sendEmail(emailTemplateModel.getBody(),
                            emailTemplateModel.getSubject(), emailList, ccEmails);
                } catch (Exception ex) {
                    log.error(ex.getMessage());
                }
            }
        }
    }

    public void sendShipmentEmailNotification(ShipmentDetails shipmentDetails, List<Integer> destinationBranches) {
        List<String> requests = new ArrayList<>(List.of(SHIPMENT_IMPORT_EMAIL_TYPE));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        EmailTemplatesRequest emailTemplateModel = null;
        if(v1DataResponse != null) {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if(emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                for (EmailTemplatesRequest emailTemplate : emailTemplatesRequests) {
                    if(Objects.equals(emailTemplate.getType(), SHIPMENT_IMPORT_EMAIL_TYPE)) {
                        emailTemplateModel = emailTemplate;
                    }
                }
            }
        }
        if(emailTemplateModel == null)
            emailTemplateModel = new EmailTemplatesRequest();
        List<String> ccEmails = new ArrayList<>();
        for(Integer roleId: destinationBranches) {
            List<String> emailList = getRoleListByRoleId(roleId);
            if (!emailList.isEmpty()) {
                createShipmentImportEmailBody(shipmentDetails, emailTemplateModel);
                notificationService.sendEmail(emailTemplateModel.getBody(),
                        emailTemplateModel.getSubject(), emailList, ccEmails);
            }
        }
    }

    public void createShipmentImportEmailBody(ShipmentDetails shipmentDetails, EmailTemplatesRequest template) {
        UsersDto user = UserContext.getUser();

        // Subject
        String subject = (template.getSubject() == null) ?
                Constants.DEFAULT_SHIPMENT_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, user.getTenantDisplayName());
        subject = subject.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        // Body
        String body = (template.getBody() == null) ?
                Constants.DEFAULT_SHIPMENT_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, user.getTenantDisplayName());
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, user.getDisplayName());
        body = body.replace(BL_NUMBER_PLACEHOLDER, shipmentDetails.getHouseBill());
        body = body.replace(MBL_NUMBER_PLACEHOLDER, shipmentDetails.getMasterBill());
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        template.setSubject(subject);
        template.setBody(body);
    }

    public void createConsolidationImportEmailBody(ConsolidationDetails consolidationDetails, EmailTemplatesRequest template) {
        UsersDto user = UserContext.getUser();

        String blNumbers = (consolidationDetails.getShipmentsList() == null) ? "" :
                consolidationDetails.getShipmentsList().stream()
                        .map(ShipmentDetails::getHouseBill)
                        .collect(Collectors.joining(","));

        String shipmentNumbers = (consolidationDetails.getShipmentsList() == null) ? "" :
                consolidationDetails.getShipmentsList().stream()
                        .map(ShipmentDetails::getShipmentId)
                        .map(String::valueOf)
                        .collect(Collectors.joining(","));

        // Subject
        String subject = (template.getSubject() == null) ?
                Constants.DEFAULT_CONSOLIDATION_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, user.getTenantDisplayName());
        subject = subject.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, consolidationDetails.getConsolidationNumber());
        subject = subject.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));

        // Body
        String body = (template.getBody() == null) ?
                Constants.DEFAULT_CONSOLIDATION_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, user.getTenantDisplayName());
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, user.getDisplayName());
        body = body.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));
        body = body.replace(BL_NUMBER_PLACEHOLDER, blNumbers);
        body = body.replace(MBL_NUMBER_PLACEHOLDER, "SEA".equals(consolidationDetails.getTransportMode()) ? consolidationDetails.getBol() : consolidationDetails.getMawb());
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, consolidationDetails.getConsolidationNumber());
        body = body.replace(SHIPMENT_NUMBERS_PLACEHOLDER, shipmentNumbers);

        template.setSubject(subject);
        template.setBody(body);
    }

    public List<String> getRoleListByRoleId(Integer roleId) {

        V1UsersEmailRequest request = new V1UsersEmailRequest();
        request.setRoleId(roleId);
        request.setTake(50);
        List<UsersRoleListResponse> usersEmailIds = iv1Service.getUserEmailsByRoleId(request);
        List<String> emailIds = new ArrayList<>();
        usersEmailIds.forEach(e -> emailIds.add(e.getEmail()));

        return emailIds;
    }



    public void sendGroupedEmailForShipmentImport(ConsolidationDetailsResponse consolidationDetailsResponse, List<UUID> shipmentGuids) {
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(consolidationDetailsResponse, ConsolidationDetails.class);
        commonUtils.setInterBranchContextForHub();
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        Set<Integer> tenantIds = new HashSet<>();
        Map<Integer, List<ShipmentDetails>> tenantShipmentMapping = new HashMap<>();
        for(UUID guid: shipmentGuids) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(guid);
            if(shipmentDetails.isPresent()) {
                shipmentDetailsList.add(shipmentDetails.get());
                tenantIds.add(shipmentDetails.get().getTenantId());
            }
        }

        for(ShipmentDetails shipmentDetails: shipmentDetailsList) {
            tenantShipmentMapping.computeIfAbsent(shipmentDetails.getTenantId(), shipmentDetail -> new ArrayList<>()).add(shipmentDetails);
        }

        List<EntityTransferMasterLists> toAndCcMailIds = new ArrayList<>();
        commonUtils.getToAndCCEmailIds(tenantIds, toAndCcMailIds);
        Map<Integer, List<EntityTransferMasterLists>> toAndCCMasterDataMap = toAndCcMailIds.stream().collect(Collectors.groupingBy(EntityTransferMasterLists::getTenantId));
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();


        List<String> requests = new ArrayList<>(List.of(GROUPED_SHIPMENT_IMPORT_EMAIL_TYPE));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        EmailTemplatesRequest emailTemplateModel = null;
        if(v1DataResponse != null) {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if(emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                for (EmailTemplatesRequest emailTemplate : emailTemplatesRequests) {
                    if(Objects.equals(emailTemplate.getType(), GROUPED_SHIPMENT_IMPORT_EMAIL_TYPE)) {
                        emailTemplateModel = emailTemplate;
                    }
                }
            }
        }
        if(emailTemplateModel == null)
            emailTemplateModel = new EmailTemplatesRequest();


        for(Integer tenantId: tenantIds) {
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, tenantId, false);
            List<String> importerEmailIds = getRoleListByRoleId(tenantId);
            List<ShipmentDetails> shipmentDetailsForTenant = tenantShipmentMapping.get(tenantId);

            List<String> toEmailIdsList = new ArrayList<>(toEmailIds);
            importerEmailIds.addAll(toEmailIdsList);
            List<String> ccEmailIdsList = new ArrayList<>(ccEmailIds);

            if (!importerEmailIds.isEmpty()) {
                createGroupedShipmentImportEmailBody(shipmentDetailsForTenant, emailTemplateModel, consolidationDetails);
                notificationService.sendEmail(emailTemplateModel.getBody(),
                        emailTemplateModel.getSubject(), importerEmailIds, ccEmailIdsList);
            }

        }

    }

    public void createGroupedShipmentImportEmailBody(List<ShipmentDetails> shipmentDetailsForTenant, EmailTemplatesRequest template, ConsolidationDetails consolidationDetails) {

        // Body
        String body = (template.getBody() == null) ?
                DEFAULT_GROUPED_SHIPMENT_RECEIVED_BODY : template.getBody();
        template.setBody(body);

        Map<String, Object> tagDetails = new HashMap<>();

        template.setSubject(generateSubject(shipmentDetailsForTenant, consolidationDetails.getConsolidationNumber()));

        populateTagDetails(tagDetails, consolidationDetails.getConsolidationNumber());

        template.setBody(generateEmailBody(tagDetails, shipmentDetailsForTenant, template.getBody()));
    }

    public String generateSubject(List<ShipmentDetails> shipmentDetailsList, String consolidationBranch) {
        String shipmentNumbers = shipmentDetailsList.stream()
                .map(ShipmentDetails::getShipmentId)
                .collect(Collectors.joining(", "));

        String subjectTemplate = "Shipment/s: {#SD_ShipmentDetails}{SD_ShipmentNumber}{/SD_ShipmentDetails} created by consolidating branch – {GS_ConsolidationBranch}";

        return subjectTemplate
                .replace("{#SD_ShipmentDetails}{SD_ShipmentNumber}{/SD_ShipmentDetails}", shipmentNumbers)
                .replace("{GS_ConsolidationBranch}", consolidationBranch);
    }


    public String generateEmailBody(Map<String, Object> tagDetails, List<ShipmentDetails> shipmentDetailsList, String htmlTemplate) {
        String tableTemplate = extractTableTemplate(htmlTemplate);

        String populatedTable = populateTableWithData(tableTemplate, shipmentDetailsList);

        String emailBody = replaceTagsValues(tagDetails, htmlTemplate);
        emailBody = emailBody.replace(tableTemplate, populatedTable);
        return emailBody;
    }

    public String extractTableTemplate(String htmlTemplate) {
        int tableStartIndex = htmlTemplate.indexOf("<table>");
        int tableEndIndex = htmlTemplate.indexOf("</table>") + "</table>".length();

        if (tableStartIndex != -1 && tableEndIndex > tableStartIndex) {
            return htmlTemplate.substring(tableStartIndex, tableEndIndex);
        }

        return "";
    }


    public String populateTableWithData(String tableTemplate, List<ShipmentDetails> shipmentDetailsList) {
        Document document = Jsoup.parse(tableTemplate);
        Element table = document.select("table").first();

        assert table != null;
        Element rowTemplate = table.select("tbody tr").get(1);

        rowTemplate.remove();

        String styleAttribute = "style";
        String paddingValue = "padding: 10px;";

        for (ShipmentDetails shipment : shipmentDetailsList) {
            Element newRow = rowTemplate.clone();
            newRow.select("td").get(0).text(shipment.getShipmentId()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(1).text(String.valueOf(shipment.getReceivingBranch())).attr(styleAttribute, paddingValue);
            newRow.select("td").get(2).text(shipment.getHouseBill()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(3).text(shipment.getMasterBill()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(4).text(shipment.getShipmentCreatedOn().toString()).attr(styleAttribute, paddingValue);

            table.select("tbody").first().appendChild(newRow);
        }

        return table.outerHtml();
    }

    public String replaceTagsValues(Map<String, Object> tagDetails, String htmlElement) {
        for (Map.Entry<String, Object> entry : tagDetails.entrySet()) {
            String tagPattern = "{" + entry.getKey() + "}";
            String value = entry.getValue() == null ? "" : entry.getValue().toString();
            htmlElement = htmlElement.replace(tagPattern, value);
        }
        return htmlElement;
    }


    public void populateTagDetails(Map<String, Object> tagDetails, String consolidationBranch) {
        tagDetails.put("GS_ConsolidationBranch", consolidationBranch);
    }

}
