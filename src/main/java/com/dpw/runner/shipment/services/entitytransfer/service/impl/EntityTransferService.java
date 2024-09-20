package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
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
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.UsersRoleListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
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
import com.dpw.runner.shipment.services.syncing.impl.ConsolidationSync;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;

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
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private CommonUtils commonUtils;
    private IV1Service iv1Service;
    private V1ServiceUtil v1ServiceUtil;
    private ITasksService tasksService;
    private INotificationService notificationService;
    private ExecutorService executorService;
    private DocumentManagerRestClient documentManagerRestClient;
    private ConsolidationSync consolidationSync;
    private ShipmentSync shipmentSync;

    @Autowired
    public EntityTransferService(IShipmentSettingsDao shipmentSettingsDao, IShipmentDao shipmentDao, IShipmentService shipmentService, IConsolidationService consolidationService, IConsolidationDetailsDao consolidationDetailsDao, IShipmentsContainersMappingDao shipmentsContainersMappingDao, ModelMapper modelMapper, IV1Service v1Service, JsonHelper jsonHelper, IHblDao hblDao, IAwbDao awbDao, IEventDao eventDao, MasterDataUtils masterDataUtils, ILogsHistoryService logsHistoryService, IContainerDao containerDao, IPackingDao packingDao, MasterDataFactory masterDataFactory, CommonUtils commonUtils, IV1Service iv1Service, V1ServiceUtil v1ServiceUtil, ITasksService tasksService, INotificationService notificationService, ExecutorService executorService, DocumentManagerRestClient documentManagerRestClient, IConsoleShipmentMappingDao consoleShipmentMappingDao, ConsolidationSync consolidationSync, ShipmentSync shipmentSync) {
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
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.documentManagerRestClient = documentManagerRestClient;
        this.consolidationSync = consolidationSync;
        this.shipmentSync = shipmentSync;
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
        try {
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendShipmentEmailNotification(shipment, uniqueDestinationTenants.stream().toList())), executorService);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }

        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Shipment Sent to branches %s", String.join(", ", getTenantName(successTenantIds))))
                .build();
        return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
    }

    private Integer getShipmentConsoleImportApprovalRole(int tenantId) {
        return shipmentSettingsDao.getShipmentConsoleImportApprovarRole(tenantId);
    }


    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> sendConsolidation(CommonRequestModel commonRequestModel) {
        SendConsolidationRequest sendConsolidationRequest = (SendConsolidationRequest) commonRequestModel.getData();
        Long consolId = sendConsolidationRequest.getConsolId();
        List<Integer> sendToBranch = sendConsolidationRequest.getSendToBranch();
        List<String> sendToOrg = sendConsolidationRequest.getSendToOrg();
        List<Integer> successTenantIds = new ArrayList<>();
        Map<String, List<Integer>> shipmentGuidSendToBranch = sendConsolidationRequest.getShipmentGuidSendToBranch();

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

        interBranchValidation(consol, sendConsolidationRequest);
        EntityTransferConsolidationDetails entityTransferPayload = prepareConsolidationPayload(consol, sendConsolidationRequest);

        for (int index = 0; index < sendToBranch.size(); index++) {
            var tenant = sendToBranch.get(index);

            var consolidationPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferConsolidationDetails.class);
            consolidationPayload.setSendToBranch(tenant);
            boolean reverseDirection = false;
            if(Long.valueOf(tenant).equals(consol.getReceivingBranch())) {
                consolidationPayload.setShipmentType(reverseDirection(consol.getShipmentType()));
                reverseDirection = true;
            }

            if(!consolidationPayload.getShipmentsList().isEmpty()) {
                for(var entityTransferShipment : consolidationPayload.getShipmentsList()) {
                    var guid = entityTransferShipment.getGuid();
                    if(reverseDirection) {
                        entityTransferShipment.setDirection(reverseDirection(entityTransferShipment.getDirection()));
                    }
                    if(shipmentGuidSendToBranch != null && shipmentGuidSendToBranch.containsKey(guid.toString()))
                        entityTransferShipment.setSendToBranch(shipmentGuidSendToBranch.get(guid.toString()).get(index));
                    else
                        entityTransferShipment.setSendToBranch(tenant);
                }
            }

            createTask(consolidationPayload, consol.getId(), Constants.Consolidations, tenant);
            successTenantIds.add(tenant);
        }

        this.createAutoEvent(consolidationDetails.get().getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.CONSOLIDATION);
        List<String> tenantName = getTenantName(successTenantIds);
        String consolDesc = createSendEvent(tenantName, consolidationDetails.get().getReceivingBranch(), consolidationDetails.get().getTriangulationPartner(), consolidationDetails.get().getDocumentationPartner(), consolidationDetails.get().getId().toString(), Constants.CONSOLIDATION_SENT, Constants.CONSOLIDATION, null);
        for (var shipment : consolidationDetails.get().getShipmentsList()) {
            // Set TenantId Context for inter branch shipment for Event creation
            if(Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.equals(TenantContext.getCurrentTenant(), shipment.getTenantId()))
                TenantContext.setCurrentTenant(shipment.getTenantId());
            this.createAutoEvent(shipment.getId().toString(), Constants.PRE_ALERT_EVENT_CODE, Constants.SHIPMENT);
            createSendEvent(tenantName, shipment.getReceivingBranch(), shipment.getTriangulationPartner(), shipment.getDocumentationPartner(), shipment.getId().toString(), Constants.SHIPMENT_SENT, Constants.SHIPMENT, consolDesc);
            TenantContext.setCurrentTenant(UserContext.getUser().getTenantId());

            if (Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
                shipmentDao.saveEntityTransfer(shipment.getId(), Boolean.TRUE);
        }

        try {
            CompletableFuture.runAsync(masterDataUtils.withMdc(() ->  sendConsolidationEmailNotification(consol, sendToBranch, shipmentGuidSendToBranch)), executorService);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }

        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Consolidation Sent to branches %s", String.join(", ", getTenantName(successTenantIds))))
            .build();
        return ResponseHelper.buildSuccessResponse(sendConsolidationResponse);

    }


    private void interBranchValidation(ConsolidationDetails consol, SendConsolidationRequest sendConsolidationRequest) {
        if(Boolean.TRUE.equals(consol.getInterBranchConsole())) {
            var shipmentGuidSendToBranch = sendConsolidationRequest.getShipmentGuidSendToBranch();
            commonUtils.setInterBranchContextForHub();
            Set<Integer> uniqueTenants = new HashSet<>(sendConsolidationRequest.getSendToBranch());
            var tenantSettingsMap = v1ServiceUtil.getTenantSettingsMap(uniqueTenants.stream().toList());
            var coloadInfoMap = v1ServiceUtil.fetchCoLoadInfo(sendConsolidationRequest.getSendToBranch(), EntityTransferConstants.PARENT_TENANT_ID);
            List<Integer> errorTenants = new ArrayList<>();

            for (int i = 0; i < sendConsolidationRequest.getSendToBranch().size(); i++) {
                var consoleReceivingBranch = sendConsolidationRequest.getSendToBranch().get(i);
                var tenantSettings = tenantSettingsMap.get(consoleReceivingBranch);
                if (Objects.isNull(tenantSettings)) {
                    errorTenants.add(consoleReceivingBranch);
                }
                else if (shipmentGuidSendToBranch != null) {
                    for (var set : shipmentGuidSendToBranch.entrySet()) {
                        var list = set.getValue();

                        if (!Objects.equals(consoleReceivingBranch, list.get(i)) &&
                                (!Boolean.TRUE.equals(tenantSettings.getIsColoadingMAWBStationEnabled()) || !coloadInfoMap.containsKey(consoleReceivingBranch) || !coloadInfoMap.get(consoleReceivingBranch).contains(list.get(i))) ) {
                            errorTenants.add(consoleReceivingBranch);
                        }
                    }
                }
            }

            if(!errorTenants.isEmpty()) {
                throw new ValidationException(String.format("Destination branches %s not having co-loading branch relation!!", String.join(", ", getTenantName(errorTenants))));
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

        // Update task status rejected
        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.REJECTED, importShipmentRequest.getRejectRemarks());
            return ResponseHelper.buildSuccessResponse();
        }

        if (importShipmentRequest.getEntityData() == null) {
            throw new ValidationException("No Shipment payload present please check");
        }
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().documents(new ArrayList<>()).build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityData();
        log.info("Import shipment request: {} with RequestId: {}", jsonHelper.convertToJson(entityTransferShipmentDetails), LoggerHelper.getRequestIdFromMDC());

        // Import shipment implementation
        ShipmentDetailsResponse shipmentDetailsResponse =  this.createShipment(entityTransferShipmentDetails, copyDocumentsRequest);
        log.info("Shipment got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());
        String shipmentId = shipmentDetailsResponse.getShipmentId();

        // Call Document Service api for copy docs
        this.sendCopyDocumentRequest(copyDocumentsRequest);

        // Update task status approved
        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.APPROVED.getDescription())) {
            updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.APPROVED, importShipmentRequest.getRejectRemarks());
        }

        var response = ImportShipmentResponse.builder()
                .shipmentId(shipmentId)
                .message("Shipment Imported Successfully with Shipment Number: " + shipmentId)
                .build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> importConsolidation (CommonRequestModel commonRequestModel) throws RunnerException {
        ImportConsolidationRequest importConsolidationRequest = (ImportConsolidationRequest) commonRequestModel.getData();

        // Update task status rejected
        if(Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            updateTaskStatus(importConsolidationRequest.getTaskId(), TaskStatus.REJECTED, importConsolidationRequest.getRejectRemarks());
            return ResponseHelper.buildSuccessResponse();
        }
        if (importConsolidationRequest.getEntityData() == null) {
            throw new ValidationException("No consolidation payload present please check");
        }
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = importConsolidationRequest.getEntityData();

        // Import consolidation implementation
        ConsolidationDetailsResponse consolidationDetailsResponse = this.createConsolidation(entityTransferConsolidationDetails);
        String consolidationNumber = Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getConsolidationNumber).orElse(null);

        // Update task status approved
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
        SyncingContext.setContext(false);
        List<ConsolidationDetails> oldConsolidationDetailsList = consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid());
        Map<UUID, List<UUID>> oldContVsOldShipGuidMap = entityTransferConsolidationDetails.getContainerVsShipmentGuid();
        Map<UUID, UUID> oldPackVsOldContGuidMap = entityTransferConsolidationDetails.getPackingVsContainerGuid();

        Map<UUID, Long> oldVsNewShipIds = new HashMap<>();
        Map<UUID, UUID> newVsOldPackingGuid = new HashMap<>();
        Map<UUID, UUID> newVsOldContainerGuid = new HashMap<>();
        Map<UUID, Long> oldGuidVsNewContainerId = new HashMap<>();

        List<Long> shipmentIds = new ArrayList<>();
        List<UUID> shipmentGuids = new ArrayList<>();
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().documents(new ArrayList<>()).build();

        List<Long> interBranchShipment = new ArrayList<>();

        // Detach all shipment from console
        if(!CommonUtils.listIsNullOrEmpty(oldConsolidationDetailsList)) {
            List<Long> detachShipIds = oldConsolidationDetailsList.get(0).getShipmentsList().stream().map(BaseEntity::getId).toList();
            if(!detachShipIds.isEmpty())
                consolidationService.detachShipments(oldConsolidationDetailsList.get(0).getId(), detachShipIds);
        }

        // Create shipments
        this.createOrUpdateShipment(entityTransferConsolidationDetails, newVsOldPackingGuid, oldVsNewShipIds, shipmentGuids, shipmentIds, interBranchShipment, copyDocumentsRequest);
        log.info("Shipment got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());

        // Created console old vs new guid map
        if(!CommonUtils.listIsNullOrEmpty(entityTransferConsolidationDetails.getContainersList())) {
            entityTransferConsolidationDetails.getContainersList().forEach(cont -> {
                UUID newGuid = UUID.randomUUID();
                newVsOldContainerGuid.put(newGuid, cont.getGuid());
                cont.setGuid(newGuid);
            });
        }
        // Packing got created with shipment
        entityTransferConsolidationDetails.setPackingList(null);

        // Create or update console
        ConsolidationDetailsResponse consolidationDetailsResponse = this.createOrUpdateConsolidation(entityTransferConsolidationDetails, oldConsolidationDetailsList);
        log.info("Consolidation got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());


        if(consolidationDetailsResponse != null) {
            // Attach consolidation and shipment
            if(!interBranchShipment.isEmpty()) {
                commonUtils.setInterBranchContextForHub();
                createShipmentPullRequest(interBranchShipment, consolidationDetailsResponse.getId());
                consolidationService.attachShipments(ShipmentRequestedType.APPROVE, consolidationDetailsResponse.getId(), shipmentIds);
            } else {
                consolidationService.attachShipments(null, consolidationDetailsResponse.getId(), shipmentIds);
            }
            log.info("Shipment and console got attached with RequestId: {}", LoggerHelper.getRequestIdFromMDC());

            // Attach consolidation containers to Shipments
            if (!CommonUtils.listIsNullOrEmpty(consolidationDetailsResponse.getContainersList())) {
                this.attachShipmentToContainers(consolidationDetailsResponse.getId(), newVsOldContainerGuid, oldContVsOldShipGuidMap, oldVsNewShipIds, oldGuidVsNewContainerId);

            }

            // Attach Shipment Packs to containers
            this.attachPackToContainers(shipmentIds, newVsOldPackingGuid, oldPackVsOldContGuidMap, oldGuidVsNewContainerId);

            // Create console import event
            this.createImportEvent(entityTransferConsolidationDetails.getSourceBranchTenantName(), consolidationDetailsResponse.getId(), Constants.CONSOLIDATION_IMPORTED, Constants.CONSOLIDATION);

            // Prepare copy docs request for doc service
            this.prepareCopyDocumentRequest(copyDocumentsRequest, consolidationDetailsResponse.getGuid().toString(), Consolidations, consolidationDetailsResponse.getTenantId(), entityTransferConsolidationDetails.getAdditionalDocs());

            // Call document service api for copy docs
            this.sendCopyDocumentRequest(copyDocumentsRequest);

            // Syncing Imported Shipment & Console to V1
            this.syncToV1(consolidationDetailsResponse.getId(), shipmentIds);
        }

        // Send consolidated shipments email
        if(!shipmentGuids.isEmpty()) {
            try {
                var shipmentDetailsList = shipmentDao.findShipmentsByGuids(new HashSet<>(shipmentGuids));
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendGroupedEmailForShipmentImport(shipmentDetailsList, entityTransferConsolidationDetails.getSourceBranchTenantName())));
            } catch (Exception ex) {
                log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
            }
        }

        return consolidationDetailsResponse;
    }

    private ConsolidationDetailsResponse createOrUpdateConsolidation(EntityTransferConsolidationDetails entityTransferConsolidationDetails, List<ConsolidationDetails> oldConsolidationDetailsList) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest =  modelMapper.map(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse;
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
        return consolidationDetailsResponse;
    }

    private void createOrUpdateShipment(EntityTransferConsolidationDetails entityTransferConsolidationDetails, Map<UUID, UUID> newVsOldPackingGuid, Map<UUID, Long> oldVsNewShipIds, List<UUID> shipmentGuids, List<Long> shipmentIds, List<Long> interBranchShipment, CopyDocumentsRequest copyDocumentsRequest) throws RunnerException {
        if(!CommonUtils.listIsNullOrEmpty(entityTransferConsolidationDetails.getShipmentsList())) {
            for (var ship : entityTransferConsolidationDetails.getShipmentsList()) {
                // Container will be created with consolidation
                ship.setContainersList(List.of());
                ship.setMasterBill(null);

                // Replaced old packing guid with new
                if (!CommonUtils.listIsNullOrEmpty(ship.getPackingList())) {
                    ship.getPackingList().forEach(pack -> {
                        UUID newGuid = UUID.randomUUID();
                        newVsOldPackingGuid.put(newGuid, pack.getGuid());
                        pack.setGuid(newGuid);
                    });
                }

                // Added docs in each shipment payload from common map
                if (entityTransferConsolidationDetails.getShipAdditionalDocs() != null && entityTransferConsolidationDetails.getShipAdditionalDocs().containsKey(ship.getGuid().toString())) {
                    ship.setAdditionalDocs(entityTransferConsolidationDetails.getShipAdditionalDocs().get(ship.getGuid().toString()));
                }

                // Create shipment
                ShipmentDetailsResponse shipmentDetailsResponse = createShipment(ship, copyDocumentsRequest);

                oldVsNewShipIds.put(ship.getGuid(), shipmentDetailsResponse.getId());
                shipmentIds.add(shipmentDetailsResponse.getId());
                shipmentGuids.add(shipmentDetailsResponse.getGuid());
                if (!Objects.equals(shipmentDetailsResponse.getTenantId(), UserContext.getUser().getTenantId())) {
                    interBranchShipment.add(shipmentDetailsResponse.getId());
                }
            }
        }
    }

    private void sendCopyDocumentRequest(CopyDocumentsRequest copyDocumentsRequest) {
        if(!copyDocumentsRequest.getDocuments().isEmpty()){
            copyDocumentsRequest.setDeleteExistingDocuments(true);
            try {
                documentManagerRestClient.copyDocuments(CommonRequestModel.buildRequest(copyDocumentsRequest));
            } catch (Exception ex) {
                log.error("Error in Copy document Api from Document Service: {}", ex.getMessage());
            }
        }
    }

    private void prepareCopyDocumentRequest(CopyDocumentsRequest copyDocumentsRequest, String entityKey, String entityType, Integer tenantId, List<String> docGuids){
        if(docGuids != null && !docGuids.isEmpty()) {
            CopyDocumentsRequest.DocumentRequest documentRequest = CopyDocumentsRequest.DocumentRequest.builder()
                    .entityKey(entityKey)
                    .tenantId(tenantId)
                    .entityType(entityType)
                    .docGuid(docGuids).build();
            copyDocumentsRequest.getDocuments().add(documentRequest);
        }
    }


    private ShipmentDetailsResponse createShipment(EntityTransferShipmentDetails entityTransferShipmentDetails, CopyDocumentsRequest copyDocumentsRequest) throws RunnerException {
        ShipmentRequest shipmentRequest = modelMapper.map(entityTransferShipmentDetails, ShipmentRequest.class);
        var tenantId = UserContext.getUser().getTenantId();

        // Set Inter Branch TenantId in TenantContext for inter branch Shipment
        if(Objects.equals(shipmentRequest.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.equals(shipmentRequest.getJobType(), Constants.SHIPMENT_TYPE_DRT) && !Objects.equals(entityTransferShipmentDetails.getSendToBranch(), tenantId)){
            commonUtils.setInterBranchContextForHub();
            TenantContext.setCurrentTenant(entityTransferShipmentDetails.getSendToBranch());
        }

        // Find old shipment with source guid
        List<ShipmentDetails> oldShipmentDetailsList = shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch());

        ShipmentDetailsResponse shipmentDetailsResponse = null;
        if(oldShipmentDetailsList == null || oldShipmentDetailsList.isEmpty()){
            shipmentRequest.setGuid(null);
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());

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
            oldShipmentDetailsList.get(0).setContainersList(List.of());

            shipmentDetailsResponse = shipmentService.completeUpdateShipmentFromEntityTransfer(shipmentRequest);
            oldShipmentDetailsList.get(0).setPackingList(jsonHelper.convertValueToList(shipmentDetailsResponse.getPackingList(), Packing.class));
        }

        // Create shipment import event
        this.createImportEvent(entityTransferShipmentDetails.getSourceBranchTenantName(), shipmentDetailsResponse.getId(), Constants.SHIPMENT_IMPORTED, Constants.SHIPMENT);

        // Prepare copy docs request for doc service
        this.prepareCopyDocumentRequest(copyDocumentsRequest, shipmentDetailsResponse.getGuid().toString(), Shipments, shipmentDetailsResponse.getTenantId(), entityTransferShipmentDetails.getAdditionalDocs());

        // Clean Inter Branch TenantId from TenantContext for this shipment
        TenantContext.setCurrentTenant(UserContext.getUser().getTenantId());

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
        if(shipmentRequest.getPackingList() != null) shipmentRequest.getPackingList().clear();
        if(shipmentRequest.getContainersList() != null) shipmentRequest.getContainersList().clear();
        if(shipmentRequest.getRoutingsList() != null) shipmentRequest.getRoutingsList().clear();
        if(shipmentRequest.getReferenceNumbersList() != null) shipmentRequest.getReferenceNumbersList().clear();
        if(shipmentRequest.getBookingCarriagesList() != null) shipmentRequest.getBookingCarriagesList().clear();
        if(shipmentRequest.getServicesList() != null) shipmentRequest.getServicesList().clear();
        if(shipmentRequest.getShipmentAddresses() != null) shipmentRequest.getShipmentAddresses().clear();
        if(shipmentRequest.getNotesList() != null) shipmentRequest.getNotesList().clear();
    }

    private void cleanAllListEntitiesForConsolidation(ConsolidationDetailsRequest consolidationDetailsRequest){
        if(consolidationDetailsRequest.getPackingList() != null) consolidationDetailsRequest.getPackingList().clear();
        if(consolidationDetailsRequest.getContainersList() != null) consolidationDetailsRequest.getContainersList().clear();
        if(consolidationDetailsRequest.getRoutingsList() != null) consolidationDetailsRequest.getRoutingsList().clear();
        if(consolidationDetailsRequest.getReferenceNumbersList() != null) consolidationDetailsRequest.getReferenceNumbersList().clear();
        if(consolidationDetailsRequest.getConsolidationAddresses() != null) consolidationDetailsRequest.getConsolidationAddresses().clear();
        if(consolidationDetailsRequest.getShipmentsList() != null) consolidationDetailsRequest.getShipmentsList().clear();
    }

    private void updateTaskStatus(Long taskId, TaskStatus status, String rejectionRemarks) {
        var taskUpdateRequest = TaskUpdateRequest.builder()
                .id(taskId.toString())
                .status(status.getName())
                .rejectionRemarks(rejectionRemarks)
                .build();
        log.info("Update task status request: {}", jsonHelper.convertToJson(taskUpdateRequest));
        tasksService.updateTask(CommonRequestModel.buildRequest(taskUpdateRequest));
    }

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
        log.info("Consolidation containers got attached to Shipment with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
    }

    private void attachPackToContainers(List<Long> shipmentIds, Map<UUID, UUID> newVsOldPackingGuid, Map<UUID, UUID> oldPackVsOldContGuidMap, Map<UUID, Long> oldGuidVsNewContainerId) {
        if(!shipmentIds.isEmpty() && !oldPackVsOldContGuidMap.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
            Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());

            List<Packing> packingList = packings.stream().toList();
            if (!packingList.isEmpty()) {
                packingList.forEach(pack -> {
                    if (oldPackVsOldContGuidMap.containsKey(newVsOldPackingGuid.get(pack.getGuid())) && oldGuidVsNewContainerId.containsKey(oldPackVsOldContGuidMap.get(newVsOldPackingGuid.get(pack.getGuid())))) {
                        Long id = oldGuidVsNewContainerId.get(oldPackVsOldContGuidMap.get(newVsOldPackingGuid.get(pack.getGuid())));
                        pack.setContainerId(id);
                    }
                });
                packingDao.saveAll(packingList);
            }
            log.info("Packs got attached to containers with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
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
            log.info("Import event request: {}", jsonHelper.convertToJson(eventReq));
            eventDao.autoGenerateEvents(eventReq);
            log.info("Import event got created successfully with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
    }


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
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();

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
        if(CommonUtils.listIsNullOrEmpty(request.getSendToBranch())){
            return ResponseHelper.buildSuccessResponse();
        }
        CommonV1ListRequest commonV1ListRequest = createCriteriaTaskListRequest(request.getEntityId().toString(), request.getEntityType(), request.getSendToBranch());
        log.info("V1 task list request: {}" , jsonHelper.convertToJson(commonV1ListRequest));

        CheckTaskExistResponse response = new CheckTaskExistResponse();
        V1DataResponse v1Response;
        try {
            v1Response = v1Service.listTask(commonV1ListRequest);
        }
        catch (Exception ex) {
            log.error("Check Task exist failed to check from V1: " + ex);
            throw new RunnerException("Check Task exist failed to check from V1: " + ex);
        }
        List<TaskCreateRequest> taskCreateRequestList = jsonHelper.convertValueToList(v1Response.getEntities(), TaskCreateRequest.class);

        response.setSendToBranch(taskCreateRequestList.stream().map(x -> Integer.parseInt(x.getTenantId())).collect(Collectors.toSet()));
        return ResponseHelper.buildSuccessResponse(response);
    }

    private CommonV1ListRequest createCriteriaTaskListRequest(Object value1, Object value2, Object value4) {
        List<Object> criteria1 = new ArrayList<>(List.of(List.of("EntityId"), "=", value1));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of("EntityType"), "=", value2));
        List<Object> criteria3 = new ArrayList<>(List.of(List.of("IsCreatedFromV2"), "=", 1));
        List<Object> criteria4 = new ArrayList<>(List.of(List.of("TenantId"), "In", List.of(value4)));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(List.of(criteria1, "and", criteria2), "and", criteria3), "and" , criteria4)).build();
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

    private EntityTransferConsolidationDetails prepareConsolidationPayload(ConsolidationDetails consolidationDetails, SendConsolidationRequest sendConsolidationRequest) {
        List<Integer> tenantIds = new ArrayList<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.addAll(consolidationDetails.getShipmentsList().stream().map(ShipmentDetails::getTenantId).toList());
        var tenantMap = getTenantMap(tenantIds);
        EntityTransferConsolidationDetails payload = jsonHelper.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);

        // Map container guid vs List<shipmentGuid>
        Map<UUID, List<UUID>> containerVsShipmentGuid = new HashMap<>();
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


                entityTransferShipment.setSourceBranchTenantName(tenantMap.get(shipment.getTenantId()).getTenantName());
                entityTransferShipment.setAdditionalDocs(shipAdditionalDocs);
                transferShipmentDetails.add(entityTransferShipment);


                // populate container vs shipment guid map
                var shipmentGuid = shipmentDetailsOptional.get().getGuid();
                if(shipmentDetailsOptional.get().getContainersList() != null) {
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
        }

        payload.setSourceBranchTenantName(tenantMap.get(consolidationDetails.getTenantId()).getTenantName());
        payload.setShipmentsList(transferShipmentDetails);
        payload.setContainerVsShipmentGuid(containerVsShipmentGuid);
        payload.setAdditionalDocs(sendConsolidationRequest.getAdditionalDocs());

        // packing guid vs container guid
        Map<UUID, UUID> packsVsContainerGuid = new HashMap<>();
        if(consolidationDetails.getContainersList() != null) {
            consolidationDetails.getContainersList().forEach(container -> {
                if(container.getPacksList() != null)
                    container.getPacksList().forEach(pack -> packsVsContainerGuid.put(pack.getGuid(), container.getGuid()));
            });
        }
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

    public void sendConsolidationEmailNotification(ConsolidationDetails consolidationDetails, List<Integer> destinationBranches, Map<String, List<Integer>> shipmentGuidSendToBranch) {

        var emailTemplatesRequests = getEmailTemplates(CONSOLIDATION_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());

        // No CC emails are used
        List<String> ccEmails = new ArrayList<>();
        // Fetching role ids corresponding to console destination branches
        var shipDestinationBranchIds = new ArrayList<>(destinationBranches);
        for (var keySet : shipmentGuidSendToBranch.entrySet())
            shipDestinationBranchIds.addAll(keySet.getValue());
        shipDestinationBranchIds.add(TenantContext.getCurrentTenant());
        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(shipDestinationBranchIds));

        for (int i = 0; i < destinationBranches.size(); i++) {
            List<String> emailList = getRoleListByRoleId(getShipmentConsoleImportApprovalRole(destinationBranches.get(i)));
            var template = createConsolidationImportEmailBody(consolidationDetails, emailTemplateModel, shipmentGuidSendToBranch, i, branchIdVsTenantModelMap, destinationBranches);
            sendEmailNotification(template, emailList, ccEmails);
        }
    }

    public void sendShipmentEmailNotification(ShipmentDetails shipmentDetails, List<Integer> destinationBranches) {
        var emailTemplatesRequests = getEmailTemplates(SHIPMENT_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());
        createShipmentImportEmailBody(shipmentDetails, emailTemplateModel);

        // No CC emails are used
        List<String> ccEmails = new ArrayList<>();

        // Fetching role ids corresponding to shipment destination branches
        var shipmentSettingsList = shipmentSettingsDao.getSettingsByTenantIds(destinationBranches);

        for(var shipmentSetting: shipmentSettingsList) {
            List<String> emailList = getRoleListByRoleId(Integer.parseInt(shipmentSetting.getShipmentConsoleImportApproverRole()));
            sendEmailNotification(emailTemplateModel, emailList, ccEmails);
        }
    }

    public void createShipmentImportEmailBody(ShipmentDetails shipmentDetails, EmailTemplatesRequest template) {
        UsersDto user = UserContext.getUser();

        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(List.of(TenantContext.getCurrentTenant())));
        // Subject
        String subject = (template.getSubject() == null) ?
                Constants.DEFAULT_SHIPMENT_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        subject = subject.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        // Body
        String body = (template.getBody() == null) ?
                Constants.DEFAULT_SHIPMENT_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, StringUtility.convertToString(user.getDisplayName()));
        body = body.replace(BL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getHouseBill()));
        body = body.replace(MBL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getMasterBill()));
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        template.setSubject(subject);
        template.setBody(body);
    }

    public EmailTemplatesRequest createConsolidationImportEmailBody(ConsolidationDetails consolidationDetails, EmailTemplatesRequest template, Map<String, List<Integer>> shipmentGuidSendToBranch, int index, Map<Integer, TenantModel> tenantMap, List<Integer> destinationBranches) {
        UsersDto user = UserContext.getUser();

        String blNumbers = (consolidationDetails.getShipmentsList() == null) ? "" :
                consolidationDetails.getShipmentsList().stream()
                        .map(c -> {
                            var branch = shipmentGuidSendToBranch.containsKey(StringUtility.convertToString(c.getGuid())) ? shipmentGuidSendToBranch.get(StringUtility.convertToString(c.getGuid())).get(index) : destinationBranches.get(index);
                            return StringUtility.convertToString(c.getHouseBill()) + " - " + (tenantMap.containsKey(branch) ? tenantMap.get(branch).getTenantName() : StringUtility.getEmptyString());
                        })
                        .collect(Collectors.joining(","));

        String shipmentNumbers = (consolidationDetails.getShipmentsList() == null) ? "" :
                consolidationDetails.getShipmentsList().stream()
                        .map(ShipmentDetails::getShipmentId)
                        .map(String::valueOf)
                        .collect(Collectors.joining(","));

        // Subject
        String subject = (template.getSubject() == null) ?
                Constants.DEFAULT_CONSOLIDATION_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(tenantMap.get(TenantContext.getCurrentTenant()).tenantName));
        subject = subject.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, StringUtility.convertToString(consolidationDetails.getConsolidationNumber()));
        subject = subject.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));

        // Body
        String body = (template.getBody() == null) ?
                Constants.DEFAULT_CONSOLIDATION_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(tenantMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, StringUtility.convertToString(user.getDisplayName()));
        body = body.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));
        body = body.replace(BL_NUMBER_PLACEHOLDER, blNumbers);
        body = body.replace(MBL_NUMBER_PLACEHOLDER, TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) ? StringUtility.convertToString(consolidationDetails.getBol()) : StringUtility.convertToString(consolidationDetails.getMawb()));
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, StringUtility.convertToString(consolidationDetails.getConsolidationNumber()));
        body = body.replace(SHIPMENT_NUMBERS_PLACEHOLDER, StringUtility.convertToString(shipmentNumbers));

        return EmailTemplatesRequest.builder().body(body).subject(subject).build();

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


    public void sendGroupedEmailForShipmentImport(List<ShipmentDetails> shipmentDetailsList, String consoleSourceBranchTenantName) {
        commonUtils.setInterBranchContextForHub();
        Set<Integer> tenantIds = new HashSet<>();
        Set<Integer> sourceTenantIds = new HashSet<>();
        Map<Integer, List<ShipmentDetails>> tenantShipmentMapping = new HashMap<>();

        for(ShipmentDetails shipmentDetails: shipmentDetailsList) {
            tenantShipmentMapping.computeIfAbsent(shipmentDetails.getTenantId(), shipmentDetail -> new ArrayList<>()).add(shipmentDetails);
            tenantIds.add(shipmentDetails.getTenantId());
            sourceTenantIds.add(Integer.parseInt(StringUtility.convertToString(shipmentDetails.getSourceTenantId())));
        }

        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap);

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();


        var emailTemplatesRequests = getEmailTemplates(GROUPED_SHIPMENT_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());
        var tenantMap = getTenantMap(sourceTenantIds.stream().toList());
        for(Integer tenantId: tenantIds) {
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, tenantId, false);
            List<String> importerEmailIds = getRoleListByRoleId(getShipmentConsoleImportApprovalRole(tenantId));
            List<ShipmentDetails> shipmentDetailsForTenant = tenantShipmentMapping.get(tenantId);

            List<String> toEmailIdsList = new ArrayList<>(toEmailIds);
            importerEmailIds.addAll(toEmailIdsList);
            List<String> ccEmailIdsList = new ArrayList<>(ccEmailIds);

            if (!importerEmailIds.isEmpty()) {
                var template = createGroupedShipmentImportEmailBody(shipmentDetailsForTenant, emailTemplateModel, tenantMap, consoleSourceBranchTenantName);
                sendEmailNotification(template, importerEmailIds, ccEmailIdsList);
            }

        }

    }

    public EmailTemplatesRequest createGroupedShipmentImportEmailBody(List<ShipmentDetails> shipmentDetailsForTenant, EmailTemplatesRequest template, Map<Integer, V1TenantResponse> tenantMap, String consoleSourceBranchTenantName) {
        var emailTemplate = EmailTemplatesRequest.builder().build();
        // Body
        String body = (template.getBody() == null) ?
                DEFAULT_GROUPED_SHIPMENT_RECEIVED_BODY : template.getBody();

        Map<String, Object> tagDetails = new HashMap<>();

        emailTemplate.setSubject(generateSubject(shipmentDetailsForTenant, consoleSourceBranchTenantName));

        populateTagDetails(tagDetails, consoleSourceBranchTenantName);

        emailTemplate.setBody(generateEmailBody(tagDetails, shipmentDetailsForTenant, body, tenantMap));
        return emailTemplate;
    }

    private String generateSubject(List<ShipmentDetails> shipmentDetailsList, String consolidationBranch) {
        String shipmentNumbers = shipmentDetailsList.stream()
                .map(ShipmentDetails::getShipmentId)
                .collect(Collectors.joining(", "));

        String subjectTemplate = "Shipment/s: {#SD_ShipmentDetails}{SD_ShipmentNumber}{/SD_ShipmentDetails} created by consolidating branch – {GS_ConsolidationBranch}";

        return subjectTemplate
                .replace("{#SD_ShipmentDetails}{SD_ShipmentNumber}{/SD_ShipmentDetails}", shipmentNumbers)
                .replace("{GS_ConsolidationBranch}", consolidationBranch);
    }


    private String generateEmailBody(Map<String, Object> tagDetails, List<ShipmentDetails> shipmentDetailsList, String htmlTemplate, Map<Integer, V1TenantResponse> tenantMap) {
        String tableTemplate = extractTableTemplate(htmlTemplate);

        String populatedTable = populateTableWithData(tableTemplate, shipmentDetailsList, tenantMap);

        String emailBody = replaceTagsValues(tagDetails, htmlTemplate);
        emailBody = emailBody.replace(tableTemplate, populatedTable);
        return emailBody;
    }

    private String extractTableTemplate(String htmlTemplate) {
        int tableStartIndex = htmlTemplate.indexOf("<table>");
        int tableEndIndex = htmlTemplate.indexOf("</table>") + "</table>".length();

        if (tableStartIndex != -1 && tableEndIndex > tableStartIndex) {
            return htmlTemplate.substring(tableStartIndex, tableEndIndex);
        }

        return "";
    }


    private String populateTableWithData(String tableTemplate, List<ShipmentDetails> shipmentDetailsList, Map<Integer, V1TenantResponse> tenantMap) {
        Document document = Jsoup.parse(tableTemplate);
        Element table = document.select("table").first();

        assert table != null;
        Element rowTemplate = table.select("tbody tr").get(1);

        rowTemplate.remove();

        String styleAttribute = "style";
        String paddingValue = "padding: 10px;";

        for (ShipmentDetails shipment : shipmentDetailsList) {
            Element newRow = rowTemplate.clone();
            var sourceTenantId = Objects.isNull(shipment.getSourceTenantId()) ? null : Integer.parseInt(StringUtility.convertToString(shipment.getSourceTenantId()));
            String sourceBranchName = Objects.isNull(sourceTenantId) && tenantMap.containsKey(sourceTenantId) ? StringUtility.getEmptyString() : tenantMap.get(sourceTenantId).getTenantName();
            newRow.select("td").get(0).text(shipment.getShipmentId()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(1).text(sourceBranchName).attr(styleAttribute, paddingValue);
            newRow.select("td").get(2).text(shipment.getHouseBill()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(3).text(shipment.getMasterBill()).attr(styleAttribute, paddingValue);
            newRow.select("td").get(4).text(shipment.getShipmentCreatedOn().format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"))).attr(styleAttribute, paddingValue);

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

    private List<EmailTemplatesRequest> getEmailTemplates(String templateType) {
        List<String> requests = new ArrayList<>(List.of(templateType));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        return jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
    }

    private void sendEmailNotification(EmailTemplatesRequest emailTemplateModel, List<String> to, List<String> cc) {
        if(!to.isEmpty()) {
            try {
                notificationService.sendEmail(emailTemplateModel.getBody(),
                        emailTemplateModel.getSubject(), to, cc);
            } catch (Exception ex) {
                log.error(ex.getMessage());
            }
        }
    }

    private Map<Integer, TenantModel> convertToTenantModel(Map<Integer, Object> map) {
        var response = new HashMap<Integer, TenantModel>();

        for (var entry : map.entrySet())
            response.put(entry.getKey(), modelMapper.map(entry.getValue(), TenantModel.class));

        return response;
    }

    private void syncToV1(Long id, List<Long> shipmentIds) {
        try {
            SyncingContext.setContext(Boolean.TRUE);
            List<ShipmentDetails> shipments = new ArrayList<>();
            ConsolidationDetails consolidation = consolidationDetailsDao.findById(id).orElse(new ConsolidationDetails());
            consolidationDetailsDao.entityDetach(List.of(consolidation));
            if (!CommonUtils.listIsNullOrEmpty(shipmentIds)) {
                shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
                shipmentDao.entityDetach(shipments);
                shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
            }
            consolidation = consolidationDetailsDao.findById(id).orElse(new ConsolidationDetails());
            Set<Long> containerIds = new HashSet<>();
            for(ShipmentDetails shipment : shipments) {
                if(!listIsNullOrEmpty(shipment.getContainersList()))
                    containerIds.addAll(shipment.getContainersList().stream().map(BaseEntity::getId).toList());
            }
            if(!listIsNullOrEmpty(consolidation.getContainersList())) {
                consolidation.getContainersList().removeIf(e -> containerIds.contains(e.getId()));
            }
            consolidation.setPackingList(null);
            consolidationSync.sync(consolidation, StringUtility.convertToString(consolidation.getGuid()), false);
            for (ShipmentDetails shipment : shipments)
                shipmentSync.sync(shipment, null, null, StringUtility.convertToString(consolidation.getGuid()), false);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_SYNC, ex.getMessage()));
        }
    }

}
