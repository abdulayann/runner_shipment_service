package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.bridgeService.BridgeRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.UsersRoleListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.*;
import com.dpw.runner.shipment.services.entitytransfer.enums.TransferStatus;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.*;
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
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

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
    public static final String CONSOLIDATION_IMPORT = "[Consolidations]";
    public static final String SHIPMENT_IMPORT = "[Shipments]";
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
    private IEventService eventService;
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
    private INetworkTransferService networkTransferService;
    private INetworkTransferDao networkTransferDao;
    private INotificationDao notificationDao;
    private DependentServiceHelper dependentServiceHelper;
    private IBridgeServiceAdapter bridgeServiceAdapter;

    @Autowired
    public EntityTransferService(IShipmentSettingsDao shipmentSettingsDao, IShipmentDao shipmentDao, IShipmentService shipmentService, IConsolidationService consolidationService
            , IConsolidationDetailsDao consolidationDetailsDao, IShipmentsContainersMappingDao shipmentsContainersMappingDao, ModelMapper modelMapper, IV1Service v1Service, JsonHelper jsonHelper, IHblDao hblDao, IAwbDao awbDao, IEventDao eventDao, MasterDataUtils masterDataUtils, ILogsHistoryService logsHistoryService, IContainerDao containerDao, IPackingDao packingDao, MasterDataFactory masterDataFactory, CommonUtils commonUtils, IV1Service iv1Service, V1ServiceUtil v1ServiceUtil, ITasksService tasksService, INotificationService notificationService, ExecutorService executorService, DocumentManagerRestClient documentManagerRestClient, IConsoleShipmentMappingDao consoleShipmentMappingDao, ConsolidationSync consolidationSync, ShipmentSync shipmentSync, INetworkTransferService networkTransferService, INetworkTransferDao networkTransferDao, IEventService eventService, INotificationDao notificationDao,
                                 DependentServiceHelper dependentServiceHelper, IBridgeServiceAdapter bridgeServiceAdapter) {
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
        this.networkTransferService = networkTransferService;
        this.networkTransferDao = networkTransferDao;
        this.notificationDao = notificationDao;
        this.eventService = eventService;
        this.dependentServiceHelper = dependentServiceHelper;
        this.bridgeServiceAdapter = bridgeServiceAdapter;
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
        if(!Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled()))
            approvalRoleTenantValidation(uniqueDestinationTenants);
        var tenantMap = getTenantMap(List.of(shipment.getTenantId()));

        List<Integer> destinationTenantList = uniqueDestinationTenants.stream().toList();
        var entityTransferPayload = prepareShipmentPayload(shipment);
        entityTransferPayload.setSourceBranchTenantName(tenantMap.get(shipment.getTenantId()).getTenantName());
        entityTransferPayload.setAdditionalDocs(additionalDocs);

        for (Integer tenant : destinationTenantList) {
            var taskPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferShipmentDetails.class);
            setDirectionInTaskPayload(tenant, shipment, taskPayload);
            taskPayload.setSendToBranch(tenant);

            if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled())){
                processNetworkTransfer(tenant, shipment, taskPayload, shipId);
            } else {
                createTask(taskPayload, shipment.getId(), Constants.SHIPMENTS_WITH_SQ_BRACKETS, tenant);
            }
            successTenantIds.add(tenant);
        }

        List<String> tenantName = getTenantName(successTenantIds);
        if(Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP))
            shipmentDao.saveEntityTransfer(shipId, Boolean.TRUE);

        try {
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendShipmentEmailNotification(shipment, uniqueDestinationTenants.stream().toList(), sendShipmentRequest.getIsAutomaticTransfer())), executorService);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }

        this.createBulkExportEvent(shipId, EventConstants.PRST, SHIPMENT, successTenantIds, shipment.getTenantId());

        SendShipmentResponse sendShipmentResponse = SendShipmentResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Shipment Sent to branches %s", String.join(", ", tenantName)))
                .build();
        return ResponseHelper.buildSuccessResponse(sendShipmentResponse);
    }

    private void processNetworkTransfer(Integer tenant, ShipmentDetails shipment, EntityTransferShipmentDetails taskPayload, Long shipId) {
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(tenant), shipment.getId(), SHIPMENT);
        Map<String, Object> entityPayload = getNetworkTransferEntityPayload(taskPayload);
        if(optionalNetworkTransfer.isPresent()) {
            if (EntityTransferConstants.RETRANSFER_SET.contains(optionalNetworkTransfer.get().getStatus())) {
                NetworkTransfer oldNetworkTransfer = optionalNetworkTransfer.get();
                oldNetworkTransfer.setStatus(NetworkTransferStatus.RETRANSFERRED);
                oldNetworkTransfer.setEntityPayload(entityPayload);
                oldNetworkTransfer.setUpdatedBy(UserContext.getUser().Username);
                oldNetworkTransfer.setTransferredDate(LocalDateTime.now());
                networkTransferDao.save(oldNetworkTransfer);
            } else {
                networkTransferService.updateNetworkTransferTransferred(optionalNetworkTransfer.get(), entityPayload);
            }
        }
        else {
            networkTransferService.processNetworkTransferEntity(Long.valueOf(tenant), null,
                SHIPMENT, shipment,
                null, taskPayload.getDirection(), entityPayload, false);
        }
        List<Notification> notificationList = notificationDao.findNotificationForEntityTransfer(shipId, SHIPMENT, tenant, List.of(NotificationRequestType.REQUEST_TRANSFER.name(), NotificationRequestType.REASSIGN.name()));
        notificationDao.deleteAll(notificationList);
    }

    private void setDirectionInTaskPayload(Integer tenant, ShipmentDetails shipment, EntityTransferShipmentDetails taskPayload) {
        if ((Long.valueOf(tenant).equals(shipment.getReceivingBranch())))
            taskPayload.setDirection(reverseDirection(shipment.getDirection()));
        else if (ObjectUtils.isNotEmpty(shipment.getTriangulationPartnerList())
                && shipment.getTriangulationPartnerList().stream()
                    .filter(Objects::nonNull)
                    .anyMatch(tp -> Objects.equals(Long.valueOf(tenant), tp.getTriangulationPartner()))
        ) {
            taskPayload.setDirection(Constants.DIRECTION_CTS);
        } else if (CommonUtils.listIsNullOrEmpty(shipment.getTriangulationPartnerList()) && Long.valueOf(tenant).equals(shipment.getTriangulationPartner())) {
            taskPayload.setDirection(Constants.DIRECTION_CTS);
        }
    }

    private Integer getShipmentConsoleImportApprovalRole(int tenantId) {
        return shipmentSettingsDao.getShipmentConsoleImportApprovarRole(tenantId);
    }

    private Map<String, Object> getNetworkTransferEntityPayload(Object taskPayload){
        String payloadString = jsonHelper.convertToJson(taskPayload);
        return jsonHelper.convertJsonToMap(payloadString);
    }

    private Boolean getIsNetworkTransferFeatureEnabled(){
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
    }

    private boolean isTriangulationPartner(Integer tenant, ConsolidationDetails consol) {
        Long tenantId = Long.valueOf(tenant);
        return (ObjectUtils.isNotEmpty(consol.getTriangulationPartnerList())
                && consol.getTriangulationPartnerList().stream()
                .filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tenantId, tp.getTriangulationPartner())))
                || (CommonUtils.listIsNullOrEmpty(consol.getTriangulationPartnerList())
                && tenantId.equals(consol.getTriangulationPartner()));
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

        if (isSendToBranchAndOrgMissing(sendToBranch, sendToOrg)) {
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }
        // Validation for importer role in receiving branch only for consolidation
        Set<Integer> tenantIds = new HashSet<>(sendConsolidationRequest.getSendToBranch());
        if(!Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled()))
            approvalRoleTenantValidation(tenantIds);

        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolId);
        if (!consolidationDetails.isPresent()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails consol = consolidationDetails.get();
        Map<UUID, ShipmentDetails> guidVsShipmentMap = getGuidVsShipmentMap(consol);

        interBranchValidation(consol, sendConsolidationRequest);
        EntityTransferConsolidationDetails entityTransferPayload = prepareConsolidationPayload(consol, sendConsolidationRequest);

        Map<String, List<Integer>> shipmentGuidBranchMap = new HashMap<>();
        for (int index = 0; index < sendToBranch.size(); index++) {
            var tenant = sendToBranch.get(index);

            var consolidationPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferConsolidationDetails.class);
            consolidationPayload.setSendToBranch(tenant);

            processConsoleShipmentList(consolidationPayload, shipmentGuidSendToBranch, index, tenant, shipmentGuidBranchMap, guidVsShipmentMap, consol);

            if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled())) {
                processConsoleNetworkTransfer(tenant, consol, consolidationPayload, consolId);
            }else{
                createTask(consolidationPayload, consol.getId(), Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS, tenant);
            }

            successTenantIds.add(tenant);
        }

        this.createAutoEvent(consolidationDetails.get().getId(), EventConstants.COSN, CONSOLIDATION, successTenantIds, consol.getTenantId());

        for (var shipment : consolidationDetails.get().getShipmentsList()) {
            // Set TenantId Context for inter branch shipment for Event creation
            if(shouldSetTenantContext(shipment))
                TenantContext.setCurrentTenant(shipment.getTenantId());
            TenantContext.setCurrentTenant(UserContext.getUser().getTenantId());

            if (shouldSaveShipment(shipment))
                shipmentDao.saveEntityTransfer(shipment.getId(), Boolean.TRUE);
        }

        try {
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendConsolidationEmailNotification(consol, sendToBranch, shipmentGuidSendToBranch, sendConsolidationRequest.getIsAutomaticTransfer())), executorService);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }

        this.createBulkExportEventForMultipleShipments(consol, shipmentGuidBranchMap);

        SendConsolidationResponse sendConsolidationResponse = SendConsolidationResponse.builder().successTenantIds(successTenantIds)
                .message(String.format("Consolidation Sent to branches %s", String.join(", ", getTenantName(successTenantIds))))
            .build();
        return ResponseHelper.buildSuccessResponse(sendConsolidationResponse);

    }

    public ResponseEntity<IRunnerResponse> sendFileToExternalSystem(CommonRequestModel commonRequestModel) throws RunnerException {
        SendFileToExternalRequest sendFileToExternalRequest = (SendFileToExternalRequest) commonRequestModel.getData();
        if(Objects.equals(sendFileToExternalRequest.getEntityType(), SHIPMENT)){
            sendShipmentToExternalSystem(sendFileToExternalRequest);
        } else if(Objects.equals(sendFileToExternalRequest.getEntityType(), CONSOLIDATION)){
            sendConsolidationToExternalSystem(sendFileToExternalRequest);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    private void sendShipmentToExternalSystem(SendFileToExternalRequest sendFileToExternalRequest) throws RunnerException {
        Long shipId = sendFileToExternalRequest.getEntityId();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipId);
        if (shipmentDetails.isEmpty()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, shipId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipment = shipmentDetails.get();
        var entityTransferPayload = prepareShipmentPayload(shipment);
        entityTransferPayload.setDirection(Constants.IMP);
        entityTransferPayload.setAdditionalDocs(sendFileToExternalRequest.getAdditionalDocs());
        Map<String, Object> entityPayload = getNetworkTransferEntityPayload(entityTransferPayload);
        prepareBridgePayload(entityPayload, entityTransferPayload.getShipmentId(), SHIPMENT, entityTransferPayload.getTransportMode(), entityTransferPayload.getDirection(), sendFileToExternalRequest);

    }
    private void sendConsolidationToExternalSystem(SendFileToExternalRequest sendFileToExternalRequest) throws RunnerException {
        Long consoleId = sendFileToExternalRequest.getEntityId();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        if (consolidationDetails.isEmpty()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consoleId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails console = consolidationDetails.get();
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .additionalDocs(sendFileToExternalRequest.getAdditionalDocs())
                .shipAdditionalDocs(sendFileToExternalRequest.getShipAdditionalDocs())
                .build();

        EntityTransferConsolidationDetails entityTransferPayload = prepareConsolidationPayload(console, sendConsolidationRequest);
        entityTransferPayload.setShipmentType(Constants.IMP);
        if(!entityTransferPayload.getShipmentsList().isEmpty()) {
            for (var ship : entityTransferPayload.getShipmentsList()) {
                ship.setDirection(Constants.IMP);
            }
        }
        Map<String, Object> entityPayload = getNetworkTransferEntityPayload(entityTransferPayload);
        prepareBridgePayload(entityPayload, entityTransferPayload.getConsolidationNumber(), CONSOLIDATION, entityTransferPayload.getTransportMode(), entityTransferPayload.getShipmentType(), sendFileToExternalRequest);
    }

    private void prepareBridgePayload(Map<String, Object> entityPayload, String entityNumber, String entityType, String transportMode, String jobType, SendFileToExternalRequest sendFileToExternalRequest) throws RunnerException {
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setEntityPayload(entityPayload);
        networkTransfer.setStatus(NetworkTransferStatus.TRANSFERRED);
        networkTransfer.setEntityType(entityType);
        networkTransfer.setEntityNumber(entityNumber);
        networkTransfer.setTransportMode(transportMode);
        networkTransfer.setJobType(jobType);

        BridgeRequest request = BridgeRequest.builder()
                .requestCode(sendFileToExternalRequest.getSendToBranch())
                .transactionId(UUID.randomUUID().toString())
                .referenceId(entityNumber)
                .payload(networkTransfer)
                .build();
        log.info("OutBound File Transfer Bridge Service Request: {}", jsonHelper.convertToJson(request));
        BridgeServiceResponse bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.requestOutBoundFileTransfer(CommonRequestModel.buildRequest(request));
        log.info("OutBound File Transfer Bridge Service Response: {}", jsonHelper.convertToJson(bridgeServiceResponse));
    }

    private boolean shouldSaveShipment(ShipmentDetails shipment) {
        return Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipment.getDirection(), Constants.DIRECTION_EXP);
    }

    private boolean shouldSetTenantContext(ShipmentDetails shipment) {
        return Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.equals(TenantContext.getCurrentTenant(), shipment.getTenantId());
    }

    private boolean isSendToBranchAndOrgMissing(List<Integer> sendToBranch, List<String> sendToOrg) {
        return (sendToBranch == null || sendToBranch.isEmpty()) && (sendToOrg == null || sendToOrg.isEmpty());
    }

    private void processConsoleShipmentList(EntityTransferConsolidationDetails consolidationPayload, Map<String, List<Integer>> shipmentGuidSendToBranch, int index, Integer tenant, Map<String, List<Integer>> shipmentGuidBranchMap, Map<UUID, ShipmentDetails> guidVsShipmentMap, ConsolidationDetails consol) {
        boolean reverseDirection = isReverseDirection(tenant, consol);
        boolean sendingToTriangulationPartner = isSendingToTriangulationPartner(tenant, consol);

        updateConsolidationPayload(consolidationPayload, consol, reverseDirection, sendingToTriangulationPartner);

        if (!consolidationPayload.getShipmentsList().isEmpty()) {
            for (var entityTransferShipment : consolidationPayload.getShipmentsList()) {
                var guid = entityTransferShipment.getGuid();
                if (reverseDirection)
                    entityTransferShipment.setDirection(
                        reverseDirection(entityTransferShipment.getDirection()));
                else if (sendingToTriangulationPartner)
                    entityTransferShipment.setDirection(Constants.DIRECTION_CTS);

                if (validShipmentGuids(shipmentGuidSendToBranch, guid))
                    entityTransferShipment.setSendToBranch(
                        shipmentGuidSendToBranch.get(guid.toString()).get(index));
                else
                    entityTransferShipment.setSendToBranch(tenant);
                if (guid != null) {
                    shipmentGuidBranchMap.computeIfAbsent(guid.toString(), k -> new ArrayList<>())
                        .add(entityTransferShipment.getSendToBranch());
                }
                processInterConsoleCase(consolidationPayload, guidVsShipmentMap,
                    entityTransferShipment, guid);
            }
            // Clear all pending shipment notifications for Inter branch console
            removePendingShipmentNotifications(consolidationPayload, consol, tenant);
        }
    }
    private boolean isReverseDirection(Integer tenant, ConsolidationDetails consol) {
        return Long.valueOf(tenant).equals(consol.getReceivingBranch());
    }

    private boolean isSendingToTriangulationPartner(Integer tenant, ConsolidationDetails consol) {
        return isTriangulationPartner(tenant, consol);
    }

    private void updateConsolidationPayload(EntityTransferConsolidationDetails consolidationPayload,
        ConsolidationDetails consol,
        boolean reverseDirection,
        boolean sendingToTriangulationPartner) {
        if (reverseDirection) {
            consolidationPayload.setShipmentType(reverseDirection(consol.getShipmentType()));
        } else if (sendingToTriangulationPartner) {
            consolidationPayload.setShipmentType(Constants.DIRECTION_CTS);
        }
    }

    private void processInterConsoleCase(EntityTransferConsolidationDetails consolidationPayload, Map<UUID, ShipmentDetails> guidVsShipmentMap, EntityTransferShipmentDetails entityTransferShipment, UUID guid) {
        if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled()) && Boolean.TRUE.equals(consolidationPayload.getInterBranchConsole()) && !Objects.equals(entityTransferShipment.getDirection(), Constants.DIRECTION_CTS)) {
            this.sendOverarchingShipmentToNetworkTransfer(entityTransferShipment.getSendToBranch(), entityTransferShipment, guidVsShipmentMap.get(guid));
        }
    }

    private boolean validShipmentGuids(Map<String, List<Integer>> shipmentGuidSendToBranch, UUID guid) {
        return shipmentGuidSendToBranch != null && shipmentGuidSendToBranch.containsKey(guid.toString()) && !CommonUtils.listIsNullOrEmpty(shipmentGuidSendToBranch.get(guid.toString()));
    }

    private void removePendingShipmentNotifications(EntityTransferConsolidationDetails consolidationPayload, ConsolidationDetails consol, Integer tenant) {
        if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled()) && Boolean.TRUE.equals(consolidationPayload.getInterBranchConsole())) {
            List<Long> shipIds = consol.getShipmentsList().stream().map(BaseEntity::getId).toList();
            List<Notification> notificationList = notificationDao.findNotificationByEntityIdsForEntityTransfer(shipIds, SHIPMENT, tenant, List.of(NotificationRequestType.REQUEST_TRANSFER.name(), NotificationRequestType.REASSIGN.name()));
            notificationDao.deleteAll(notificationList);
        }
    }

    private Map<UUID, ShipmentDetails> getGuidVsShipmentMap(ConsolidationDetails consol) {
        Map<UUID, ShipmentDetails> guidVsShipmentMap = new HashMap<>();
        if(!CommonUtils.setIsNullOrEmpty(consol.getShipmentsList()))
            guidVsShipmentMap = consol.getShipmentsList().stream().collect(Collectors.toMap(ShipmentDetails::getGuid, Function.identity()));
        return guidVsShipmentMap;
    }

    private void processConsoleNetworkTransfer(Integer tenant, ConsolidationDetails consol, EntityTransferConsolidationDetails consolidationPayload, Long consolId) {
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(tenant), consol.getId(), CONSOLIDATION);
        Map<String, Object> entityPayload = getNetworkTransferEntityPayload(consolidationPayload);
        boolean isInterBranchConsole = Boolean.TRUE.equals(consolidationPayload.getInterBranchConsole());

        if (optionalNetworkTransfer.isPresent()) {
            if (EntityTransferConstants.RETRANSFER_SET.contains(optionalNetworkTransfer.get().getStatus())) {
                NetworkTransfer oldNetworkTransfer = optionalNetworkTransfer.get();
                oldNetworkTransfer.setStatus(NetworkTransferStatus.RETRANSFERRED);
                oldNetworkTransfer.setEntityPayload(entityPayload);
                oldNetworkTransfer.setUpdatedBy(UserContext.getUser().Username);
                oldNetworkTransfer.setTransferredDate(LocalDateTime.now());
                networkTransferDao.save(oldNetworkTransfer);
            }else {
                networkTransferService.updateNetworkTransferTransferred(
                    optionalNetworkTransfer.get(),
                    entityPayload);
            }
        }
        else {

            networkTransferService.processNetworkTransferEntity(Long.valueOf(tenant), null, CONSOLIDATION,
                    null, consol, consolidationPayload.getShipmentType(), entityPayload, isInterBranchConsole);
        }

        List<Notification> notificationList = notificationDao.findNotificationForEntityTransfer(consolId, CONSOLIDATION, tenant, List.of(NotificationRequestType.REQUEST_TRANSFER.name(), NotificationRequestType.REASSIGN.name()));
        notificationDao.deleteAll(notificationList);
    }

    private void sendOverarchingShipmentToNetworkTransfer(Integer tenant, EntityTransferShipmentDetails entityTransferShipment, ShipmentDetails shipment) {
        Long shipId = shipment.getId();
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(tenant), shipId, SHIPMENT);
        Map<String, Object> entityPayload = getNetworkTransferEntityPayload(entityTransferShipment);
        if (optionalNetworkTransfer.isPresent()) {
            optionalNetworkTransfer.get().setIsInterBranchEntity(true);
            networkTransferService.updateNetworkTransferTransferred(optionalNetworkTransfer.get(), entityPayload);
        }
        else
            networkTransferService.processNetworkTransferEntity(Long.valueOf(tenant), null, SHIPMENT,
                    shipment, null, entityTransferShipment.getShipmentType(), entityPayload, true);

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
                    processShipmentGuidSendToBranch(shipmentGuidSendToBranch, consoleReceivingBranch, i, tenantSettings, coloadInfoMap, errorTenants);
                }
            }

            if(!errorTenants.isEmpty()) {
                throw new ValidationException(String.format("Destination branches %s not having co-loading branch relation!!", String.join(", ", getTenantName(errorTenants))));
            }
        }
    }

    private void processShipmentGuidSendToBranch(Map<String, List<Integer>> shipmentGuidSendToBranch, Integer consoleReceivingBranch, int i, V1TenantSettingsResponse tenantSettings, Map<Integer, Set<Integer>> coloadInfoMap, List<Integer> errorTenants) {
        for (var set : shipmentGuidSendToBranch.entrySet()) {
            var list = set.getValue();

            if (isInvalidConsoleReceivingBranch(list, consoleReceivingBranch, i, tenantSettings, coloadInfoMap)) {
                errorTenants.add(consoleReceivingBranch);
            }
        }
    }

    private boolean isInvalidConsoleReceivingBranch(List<Integer> list, Integer consoleReceivingBranch, int i, V1TenantSettingsResponse tenantSettings, Map<Integer, Set<Integer>> coloadInfoMap) {
        return !CommonUtils.listIsNullOrEmpty(list) && !Objects.equals(consoleReceivingBranch, list.get(i)) &&
                (!Boolean.TRUE.equals(tenantSettings.getIsColoadingMAWBStationEnabled()) || !coloadInfoMap.containsKey(consoleReceivingBranch) || !coloadInfoMap.get(consoleReceivingBranch).contains(list.get(i)));
    }

    private void approvalRoleTenantValidation(Set<Integer> sendBranches) {
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
        this.validateApprovalRoleForImport();
        ImportShipmentRequest importShipmentRequest = (ImportShipmentRequest) commonRequestModel.getData();

        boolean isNetworkTransferFeatureEnabled = Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled());
        // Update task status rejected
        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            if(Boolean.TRUE.equals(importShipmentRequest.getIsFromNte()))
                rejectionForReTransferNte(importShipmentRequest.getTaskId(), importShipmentRequest.getRejectRemarks(), isNetworkTransferFeatureEnabled, Constants.SHIPMENT);
            else
                updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.REJECTED, importShipmentRequest.getRejectRemarks());
            return ResponseHelper.buildSuccessResponse();
        }

        if (importShipmentRequest.getEntityData() == null) {
            throw new ValidationException("No Shipment payload present please check");
        }
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().documents(new ArrayList<>()).build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityData();

        if(isNetworkTransferFeatureEnabled && importShipmentRequest.getAssignedTo()!=null)
            entityTransferShipmentDetails.setAssignedTo(importShipmentRequest.getAssignedTo());
        MutableBoolean isCreateShip = new MutableBoolean(false);
        log.info("Import shipment request: {} with RequestId: {}", jsonHelper.convertToJson(entityTransferShipmentDetails), LoggerHelper.getRequestIdFromMDC());

        // Import shipment implementation
        String department = commonUtils.getAutoPopulateDepartment(entityTransferShipmentDetails.getTransportMode(), entityTransferShipmentDetails.getDirection(), MdmConstants.SHIPMENT_MODULE);
        ShipmentDetailsResponse shipmentDetailsResponse =  this.createShipment(entityTransferShipmentDetails, copyDocumentsRequest, isCreateShip, department);
        log.info("Shipment got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());
        String shipmentId = shipmentDetailsResponse.getShipmentId();

        // Call Document Service api for copy docs
        String authToken = RequestAuthContext.getAuthToken();
        sendCopyDocumentRequest(copyDocumentsRequest, authToken);

        // Push data to dependant service
        pushImportShipmentDataToDependantService(shipmentDetailsResponse.getId(), isCreateShip.isTrue());

        // Update task status approved
        if(Boolean.TRUE.equals(importShipmentRequest.getIsFromNte())) {
            updateNteStatus(isNetworkTransferFeatureEnabled, importShipmentRequest, shipmentDetailsResponse);
        } else if (Objects.equals(importShipmentRequest.getOperation(), TaskStatus.APPROVED.getDescription())) {
            updateTaskStatus(importShipmentRequest.getTaskId(), TaskStatus.APPROVED, importShipmentRequest.getRejectRemarks());
        }

        var response = ImportShipmentResponse.builder()
                .shipmentId(shipmentId)
                .message("Shipment Imported Successfully with Shipment Number: " + shipmentId)
                .build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void rejectionForReTransferNte(Long taskId, String rejectionRemarks, boolean isNetworkTransferFeatureEnabled, String entityType) {
        if(isNetworkTransferFeatureEnabled) {
            var networkTransfer = networkTransferDao.findById(taskId);
            if(networkTransfer.isPresent()) {
                if(!Objects.equals(networkTransfer.get().getStatus(), NetworkTransferStatus.RETRANSFERRED)){
                    throw new ValidationException(EntityTransferConstants.CANCELLATION_FAILURE_MSG);
                }
                // Update status back to accepted
                networkTransferDao.updateStatus(taskId, NetworkTransferStatus.ACCEPTED.name());
                // Trigger Rejection Email
                sendRejectionEmail(networkTransfer.get().getUpdatedBy(), rejectionRemarks, entityType, networkTransfer.get().getEntityNumber());
            }
        }
        else{
            throw new ValidationException(EntityTransferConstants.NETWORK_TRANSFER_NOT_ENABLED_ERROR);
        }
    }

    private void updateNteStatus(boolean isNetworkTransferFeatureEnabled, ImportShipmentRequest importShipmentRequest, ShipmentDetailsResponse shipmentDetailsResponse) {
        if(isNetworkTransferFeatureEnabled) {
            networkTransferService.updateStatusAndCreatedEntityId(importShipmentRequest.getTaskId(), NetworkTransferStatus.ACCEPTED.name(), shipmentDetailsResponse.getId());
            var nte = networkTransferDao.findById(importShipmentRequest.getTaskId());
            if(nte.isPresent() && !Objects.equals(nte.get().getSource(), NetworkTransferSource.EXTERNAL)) {
                Long tenantId = Long.valueOf(TenantContext.getCurrentTenant());
                if (tenantId.equals(shipmentDetailsResponse.getReceivingBranch()))
                    shipmentDao.saveIsTransferredToReceivingBranch(nte.get().getEntityId(), Boolean.TRUE);
                if (shipmentDetailsResponse.getTriangulationPartnerList() != null && shipmentDetailsResponse.getTriangulationPartnerList().stream()
                        .filter(Objects::nonNull)
                        .anyMatch(tp -> Objects.equals(tenantId, tp.getTriangulationPartner())))
                    shipmentDao.updateIsAcceptedTriangulationPartner(nte.get().getEntityId(), tenantId, Boolean.TRUE);
            }
        } else{
            throw new ValidationException("Network Transfer feature is not enabled");
        }
    }

    private void validateApprovalRoleForImport() {
        if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled())){
            var tenantId = TenantContext.getCurrentTenant();
            List<UsersDto> users = v1ServiceUtil.getUsersWithGivenPermission(List.of(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), tenantId);
            if (CommonUtils.listIsNullOrEmpty(users)) {
                throw new ValidationException(EntityTransferConstants.APPROVAL_ROLE_ACTION_NOT_ALLOWED);
            }
            Long userId = UserContext.getUser().getUserId();
            Set<Long> userIds = users.stream()
                    .map(UsersDto::getUserId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());

            if (!userIds.contains(userId)) {
                throw new ValidationException(EntityTransferConstants.APPROVAL_ROLE_ACTION_NOT_ALLOWED);
            }
        }
    }

    private void pushImportShipmentDataToDependantService(Long shipmentId, boolean isCreateShip) {
        try {
            Optional<ShipmentDetails> shipment = shipmentDao.findById(shipmentId);
            shipment.ifPresent(shipmentDetails -> dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, isCreateShip, false, shipmentDetails.getContainersList()));
        } catch (Exception ex) {
            log.error("Error occurred while pushing import shipment data to dependent service : {}", ex.getMessage());
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> importConsolidation (CommonRequestModel commonRequestModel) throws RunnerException {
        this.validateApprovalRoleForImport();
        ImportConsolidationRequest importConsolidationRequest = (ImportConsolidationRequest) commonRequestModel.getData();

        boolean isNetworkTransferFeatureEnabled = Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled());
        // Update task status rejected
        if(Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            if(Boolean.TRUE.equals(importConsolidationRequest.getIsFromNte()))
                rejectionForReTransferNte(importConsolidationRequest.getTaskId(), importConsolidationRequest.getRejectRemarks(), isNetworkTransferFeatureEnabled, CONSOLIDATION);
            else
                updateTaskStatus(importConsolidationRequest.getTaskId(), TaskStatus.REJECTED, importConsolidationRequest.getRejectRemarks());
            return ResponseHelper.buildSuccessResponse();
        }
        if (importConsolidationRequest.getEntityData() == null) {
            throw new ValidationException("No consolidation payload present please check");
        }
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = importConsolidationRequest.getEntityData();

        setAssignedToOfShipments(isNetworkTransferFeatureEnabled, entityTransferConsolidationDetails, importConsolidationRequest);
        Map<UUID, Long> oldVsNewShipIds = new HashMap<>();

        // Import consolidation implementation
        ConsolidationDetailsResponse consolidationDetailsResponse = this.createConsolidation(entityTransferConsolidationDetails, oldVsNewShipIds);
        String consolidationNumber = Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getConsolidationNumber).orElse(null);

        // Update task status approved
        if(Boolean.TRUE.equals(importConsolidationRequest.getIsFromNte())) {
            if (isNetworkTransferFeatureEnabled) {
                networkTransferService.updateStatusAndCreatedEntityId(importConsolidationRequest.getTaskId(), NetworkTransferStatus.ACCEPTED.name(), Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getId).orElse(null));
                var nte = networkTransferDao.findById(importConsolidationRequest.getTaskId());
                if(nte.isPresent() && !Objects.equals(nte.get().getSource(), NetworkTransferSource.EXTERNAL)) {
                    updateNteStatus(nte.get(), consolidationDetailsResponse, oldVsNewShipIds);
                }
            } else {
                throw new ValidationException("Network Transfer feature is not enabled");
            }
        } else if (Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.APPROVED.getDescription())) {
            updateTaskStatus(importConsolidationRequest.getTaskId(), TaskStatus.APPROVED, importConsolidationRequest.getRejectRemarks());
        }

        var response = ImportConsolidationResponse.builder()
                .consolidationNumber(consolidationNumber)
                .message("Consolidation Imported Successfully with Consolidation Number: " + consolidationNumber)
                .build();

        return ResponseHelper.buildSuccessResponse(response);
    }

    private void updateNteStatus(NetworkTransfer nte, ConsolidationDetailsResponse consolidationDetailsResponse, Map<UUID, Long> oldVsNewShipIds) {
        Long consolId = nte.getEntityId();
        Long tenantId = Long.valueOf(TenantContext.getCurrentTenant());
        boolean isRecevingBranch = false;
        if (tenantId.equals(consolidationDetailsResponse.getReceivingBranch())) {
            consolidationDetailsDao.saveIsTransferredToReceivingBranch(consolId, Boolean.TRUE);
            isRecevingBranch = true;
        }
        if (consolidationDetailsResponse.getTriangulationPartnerList() != null && consolidationDetailsResponse.getTriangulationPartnerList().stream()
                .filter(Objects::nonNull).anyMatch(tp -> Objects.equals(tenantId, tp.getTriangulationPartner())))
            consolidationDetailsDao.updateIsAcceptedTriangulationPartner(consolId, tenantId, Boolean.TRUE);
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(consolId);
        if (consolidationDetails.isEmpty()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        // update status for interBranch shipments
        if(Boolean.TRUE.equals(nte.getIsInterBranchEntity())) {
            this.updateInterBranchShipmentStatus(oldVsNewShipIds);
        }
        for (var shipment : consolidationDetails.get().getShipmentsList()) {
            if (isRecevingBranch)
                shipmentDao.saveIsTransferredToReceivingBranch(shipment.getId(), Boolean.TRUE);
            if (shipment.getTriangulationPartnerList() != null && shipment.getTriangulationPartnerList().stream()
                    .filter(Objects::nonNull)
                    .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), tenantId)))
                shipmentDao.updateIsAcceptedTriangulationPartner(shipment.getId(), tenantId, Boolean.TRUE);
        }
    }

    private void setAssignedToOfShipments(boolean isNetworkTransferFeatureEnabled, EntityTransferConsolidationDetails entityTransferConsolidationDetails, ImportConsolidationRequest importConsolidationRequest) {
        if (isNetworkTransferFeatureEnabled && entityTransferConsolidationDetails.getShipmentsList()!=null
        && importConsolidationRequest.getShipmentNumberAssignedToMap()!=null) {
            Map<String, String> shipmentNumberAssignedToMap = importConsolidationRequest.getShipmentNumberAssignedToMap();
            for(EntityTransferShipmentDetails shipmentDetails: entityTransferConsolidationDetails.getShipmentsList()){
                String assignedTo = shipmentNumberAssignedToMap.get(shipmentDetails.getShipmentId());
                if(assignedTo!=null)
                    shipmentDetails.setAssignedTo(assignedTo);
            }
        }
    }

    private void updateInterBranchShipmentStatus(Map<UUID, Long> oldVsNewShipIds) {
        var shipGuids = oldVsNewShipIds.keySet();
        var shipNteList = networkTransferDao.findByEntityGuids(shipGuids.stream().toList());
        if(!CommonUtils.listIsNullOrEmpty(shipNteList)) {
            var res = shipNteList.stream().filter(x->!Objects.equals(x.getJobType(), DIRECTION_CTS))
                    .map(x-> {
                        if(oldVsNewShipIds.containsKey(x.getEntityGuid())) {
                            x.setCreatedEntityId(oldVsNewShipIds.get(x.getEntityGuid()));
                            x.setStatus(NetworkTransferStatus.ACCEPTED);
                        }
                        return x;
                    }).toList();
            networkTransferDao.saveAll(res);
        }
    }

    private ConsolidationDetailsResponse createConsolidation (EntityTransferConsolidationDetails entityTransferConsolidationDetails, Map<UUID, Long> oldVsNewShipIds) throws RunnerException {
        SyncingContext.setContext(false);
        List<ConsolidationDetails> oldConsolidationDetailsList = consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid());
        Map<UUID, List<UUID>> oldContVsOldShipGuidMap = Optional.ofNullable(entityTransferConsolidationDetails.getContainerVsShipmentGuid()).orElse(new HashMap<>());
        Map<UUID, UUID> oldPackVsOldContGuidMap = Optional.ofNullable(entityTransferConsolidationDetails.getPackingVsContainerGuid()).orElse(new HashMap<>());

        Map<UUID, UUID> newVsOldPackingGuid = new HashMap<>();
        Map<UUID, UUID> newVsOldContainerGuid = new HashMap<>();
        Map<UUID, Long> oldGuidVsNewContainerId = new HashMap<>();

        List<Long> shipmentIds = new ArrayList<>();
        List<UUID> shipmentGuids = new ArrayList<>();
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().documents(new ArrayList<>()).build();
        MutableBoolean isCreateConsole = new MutableBoolean(false);
        Map<Long, Boolean> isCreateShipMap = new HashMap<>();

        List<Long> interBranchShipment = new ArrayList<>();

        // Detach all shipment from console
        if(!CommonUtils.listIsNullOrEmpty(oldConsolidationDetailsList)) {
            List<Long> detachShipIds = oldConsolidationDetailsList.get(0).getShipmentsList().stream().map(BaseEntity::getId).toList();
            if(!detachShipIds.isEmpty())
                consolidationService.detachShipments(oldConsolidationDetailsList.get(0), detachShipIds);
        }

        // Create shipments
        this.createOrUpdateShipment(entityTransferConsolidationDetails, newVsOldPackingGuid, oldVsNewShipIds, shipmentGuids, shipmentIds, interBranchShipment, copyDocumentsRequest, isCreateShipMap);
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
        // Setting this to true to bypass condition - no dg shipment available
        entityTransferConsolidationDetails.setCreatingFromDgShipment(entityTransferConsolidationDetails.getHazardous());

        // Create or update console
        ConsolidationDetailsResponse consolidationDetailsResponse = this.createOrUpdateConsolidation(entityTransferConsolidationDetails, oldConsolidationDetailsList, isCreateConsole);
        log.info("Consolidation got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());


        if(consolidationDetailsResponse != null) {
            // Attach consolidation and shipment
            if(!interBranchShipment.isEmpty()) {
                commonUtils.setInterBranchContextForHub();
                createShipmentPullRequest(interBranchShipment, consolidationDetailsResponse.getId());
                consolidationService.attachShipments(ShipmentRequestedType.APPROVE, consolidationDetailsResponse.getId(), shipmentIds, true);
            } else {
                consolidationService.attachShipments(null, consolidationDetailsResponse.getId(), shipmentIds, true);
            }
            log.info("Shipment and console got attached with RequestId: {}", LoggerHelper.getRequestIdFromMDC());

            // Attach consolidation containers to Shipments
            if (!CommonUtils.listIsNullOrEmpty(consolidationDetailsResponse.getContainersList())) {
                this.attachShipmentToContainers(consolidationDetailsResponse.getId(), newVsOldContainerGuid, oldContVsOldShipGuidMap, oldVsNewShipIds, oldGuidVsNewContainerId);

            }

            // Attach Shipment Packs to containers
            this.attachPackToContainers(shipmentIds, newVsOldPackingGuid, oldPackVsOldContGuidMap, oldGuidVsNewContainerId);

            // Create console import event
            this.createImportEvent(entityTransferConsolidationDetails.getSourceBranchTenantName(), consolidationDetailsResponse.getId(), EventConstants.TCOA, CONSOLIDATION);

            // Prepare copy docs request for doc service
            this.prepareCopyDocumentRequest(copyDocumentsRequest, consolidationDetailsResponse.getGuid().toString(), CONSOLIDATIONS_WITH_SQ_BRACKETS, entityTransferConsolidationDetails.getSendToBranch(), entityTransferConsolidationDetails.getAdditionalDocs());

            // Call document service api for copy docs
            String authToken = RequestAuthContext.getAuthToken();
            sendCopyDocumentRequest(copyDocumentsRequest, authToken);

            // Syncing Imported Shipment & Console to V1
            this.syncToV1(consolidationDetailsResponse.getId(), shipmentIds);

            // Push data to dependant service
            pushImportConsoleDataToDependantService(consolidationDetailsResponse.getId(), shipmentIds, isCreateConsole.isTrue(), isCreateShipMap);
        }

        // Send consolidated shipments email
        if(!shipmentGuids.isEmpty()) {
            try {
                var shipmentDetailsList = shipmentDao.findShipmentsByGuids(new HashSet<>(shipmentGuids));
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendGroupedEmailForShipmentImport(shipmentDetailsList, entityTransferConsolidationDetails.getSourceBranchTenantName())), executorService);
            } catch (Exception ex) {
                log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
            }
        }

        return consolidationDetailsResponse;
    }

    private void pushImportConsoleDataToDependantService(Long consoleId, List<Long> shipmentIds, boolean isCreateConsole, Map<Long, Boolean> isCreateShipMap) {
        try {
            Optional<ConsolidationDetails> consolidation = consolidationDetailsDao.findById(consoleId);
            consolidation.ifPresent(consolidationDetails -> consolidationService.pushShipmentDataToDependentService(consolidationDetails, isCreateConsole, consolidationDetails));

            List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
            for (ShipmentDetails shipment : shipments) {
                dependentServiceHelper.pushShipmentDataToDependentService(shipment, isCreateShipMap.containsKey(shipment.getId()) && Boolean.TRUE.equals(isCreateShipMap.get(shipment.getId())), false, shipment.getContainersList());
            }
        } catch (Exception ex) {
            log.error("Error occurred while pushing import console data to dependent service : {}", ex.getMessage());
        }
    }

    private ConsolidationDetailsResponse createOrUpdateConsolidation(EntityTransferConsolidationDetails entityTransferConsolidationDetails, List<ConsolidationDetails> oldConsolidationDetailsList, MutableBoolean isCreateConsole) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest =  modelMapper.map(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse;
        consolidationDetailsRequest.setDepartment(commonUtils.getAutoPopulateDepartment(consolidationDetailsRequest.getTransportMode(), consolidationDetailsRequest.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE));
        if(oldConsolidationDetailsList == null || oldConsolidationDetailsList.isEmpty()) {
            consolidationDetailsRequest.setGuid(null);
            consolidationDetailsRequest.setShipmentsList(null);
            consolidationDetailsRequest.setSourceGuid(entityTransferConsolidationDetails.getGuid());

            consolidationDetailsResponse = consolidationService.createConsolidationFromEntityTransfer(consolidationDetailsRequest);
            isCreateConsole.setTrue();
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
            isCreateConsole.setFalse();
        }
        return consolidationDetailsResponse;
    }

    private void createOrUpdateShipment(EntityTransferConsolidationDetails entityTransferConsolidationDetails, Map<UUID, UUID> newVsOldPackingGuid, Map<UUID, Long> oldVsNewShipIds, List<UUID> shipmentGuids, List<Long> shipmentIds, List<Long> interBranchShipment, CopyDocumentsRequest copyDocumentsRequest, Map<Long, Boolean> isCreateShipMap) throws RunnerException {
        if(!CommonUtils.listIsNullOrEmpty(entityTransferConsolidationDetails.getShipmentsList())) {
            String department = commonUtils.getAutoPopulateDepartment(entityTransferConsolidationDetails.getShipmentsList().get(0).getTransportMode(), entityTransferConsolidationDetails.getShipmentsList().get(0).getDirection(), MdmConstants.SHIPMENT_MODULE);
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
                MutableBoolean isCreateShip = new MutableBoolean(false);

                // Create shipment
                ShipmentDetailsResponse shipmentDetailsResponse = createShipment(ship, copyDocumentsRequest, isCreateShip, department);
                isCreateShipMap.put(shipmentDetailsResponse.getId(), isCreateShip.booleanValue());

                oldVsNewShipIds.put(ship.getGuid(), shipmentDetailsResponse.getId());
                shipmentIds.add(shipmentDetailsResponse.getId());
                shipmentGuids.add(shipmentDetailsResponse.getGuid());
                if (!Objects.equals(shipmentDetailsResponse.getTenantId(), UserContext.getUser().getTenantId())) {
                    interBranchShipment.add(shipmentDetailsResponse.getId());
                }
            }
        }
    }


    private void sendCopyDocumentRequest(CopyDocumentsRequest copyDocumentsRequest, String authToken) {

        if(!copyDocumentsRequest.getDocuments().isEmpty()){
            copyDocumentsRequest.setDeleteExistingDocuments(true);
            try {
                documentManagerRestClient.copyDocuments(CommonRequestModel.buildRequest(copyDocumentsRequest), authToken);
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


    private ShipmentDetailsResponse createShipment(EntityTransferShipmentDetails entityTransferShipmentDetails, CopyDocumentsRequest copyDocumentsRequest, MutableBoolean isCreateShip, String department) throws RunnerException {
        ShipmentRequest shipmentRequest = modelMapper.map(entityTransferShipmentDetails, ShipmentRequest.class);
        var tenantId = UserContext.getUser().getTenantId();
        shipmentRequest.setDepartment(department);
        if(entityTransferShipmentDetails.getSendToBranch() == null) {
            entityTransferShipmentDetails.setSendToBranch(tenantId);
        }
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
            shipmentRequest.setShipmentCreatedOn(LocalDateTime.now());
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());

            shipmentDetailsResponse = shipmentService.createShipmentFromEntityTransfer(shipmentRequest);
            isCreateShip.setTrue();
        } else {
            shipmentRequest = jsonHelper.convertValue(oldShipmentDetailsList.get(0), ShipmentRequest.class);

            Long id = shipmentRequest.getId();
            UUID guid = shipmentRequest.getGuid();
            this.cleanAllListEntitiesForShipment(shipmentRequest);
            modelMapper.map(entityTransferShipmentDetails, shipmentRequest);
            shipmentRequest.setId(id);
            shipmentRequest.setGuid(guid);
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());
            shipmentRequest.setConsolidationList(Set.of());
            oldShipmentDetailsList.get(0).setContainersList(Set.of());

            shipmentDetailsResponse = shipmentService.completeUpdateShipmentFromEntityTransfer(shipmentRequest);
            oldShipmentDetailsList.get(0).setPackingList(jsonHelper.convertValueToList(shipmentDetailsResponse.getPackingList(), Packing.class));
            isCreateShip.setFalse();
        }

        // Create shipment import event
        this.createImportEvent(entityTransferShipmentDetails.getSourceBranchTenantName(), shipmentDetailsResponse.getId(), EventConstants.TSHA, Constants.SHIPMENT);

        // Prepare copy docs request for doc service
        this.prepareCopyDocumentRequest(copyDocumentsRequest, shipmentDetailsResponse.getGuid().toString(), SHIPMENTS_WITH_SQ_BRACKETS, shipmentDetailsResponse.getTenantId(), entityTransferShipmentDetails.getAdditionalDocs());

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
            Set<ShipmentDetails> shipmentDetails = new HashSet<>();
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

    public void createBulkExportEvent(Long entityId, String eventCode, String entityType, List<Integer> tenantIds, Integer currentTenant) {
        if (Objects.isNull(entityId) || CommonUtils.listIsNullOrEmpty(tenantIds))
            return;
        var tenantMap = v1ServiceUtil.getTenantDetails(tenantIds);
        Map<Integer, Object> branchMap = new HashMap<>();
        if (isAutomaticTransfer()) {
            branchMap = v1ServiceUtil.getTenantDetails(Collections.singletonList(currentTenant));
        }
        List<EventsRequest> events = prepareEvents(entityId, eventCode, entityType, tenantIds, tenantMap, branchMap, currentTenant);
        eventService.saveAllEvent(events);
    }

    private List<EventsRequest> prepareEvents(Long entityId, String eventCode, String entityType, List<Integer> tenantIds, Map<Integer, Object> tenantMap, Map<Integer, Object> branchMap, Integer currentTenant) {
        List<EventsRequest> events = new ArrayList<>();
        if (isAutomaticTransfer() && (branchMap.isEmpty() || !branchMap.containsKey(currentTenant))) {
            var v1Map = v1ServiceUtil.getTenantDetails(Collections.singletonList(currentTenant));
            if (!v1Map.isEmpty() && v1Map.containsKey(currentTenant)) {
                branchMap.put(currentTenant, v1Map.get(currentTenant));
            }
        }
        for (Integer tenantId : tenantIds) {
            var tenantDetails = jsonHelper.convertValue(tenantMap.getOrDefault(tenantId, new V1TenantResponse()), V1TenantResponse.class);
            EventsRequest eventsRequest = new EventsRequest();
            eventsRequest.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
            eventsRequest.setEntityId(entityId);
            eventsRequest.setEntityType(entityType);
            eventsRequest.setEventCode(eventCode);
            if (tenantDetails != null && !CommonUtils.isStringNullOrEmpty(tenantDetails.getCode()))
                eventsRequest.setPlaceName(tenantDetails.getCode());
            eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
            if (isAutomaticTransfer()) {
                updateUserFieldsInEvent(eventsRequest, branchMap, currentTenant);
            }
            events.add(eventsRequest);
        }
        return events;
    }

    private void updateUserFieldsInEvent(EventsRequest event, Map<Integer, Object> branchMap, Integer tenantId) {
        event.setUserName(EventConstants.SYSTEM_GENERATED);
        event.setUserEmail(null);
        if (!branchMap.isEmpty() && branchMap.containsKey(tenantId)) {
            var tenantDetails = jsonHelper.convertValue(branchMap.getOrDefault(tenantId, new V1TenantResponse()), V1TenantResponse.class);
            event.setBranch(tenantDetails.getCode());
        }
    }

    private boolean isAutomaticTransfer() {
        String automaticTransfer = MDC.get(LoggingConstants.AUTOMATIC_TRANSFER);
        return automaticTransfer != null && automaticTransfer.equals("true");
    }

    public void createBulkExportEventForMultipleShipments(ConsolidationDetails consolidationDetails, Map<String, List<Integer>> shipmentGuidBranchMap) {
        if (CollectionUtils.isEmpty(shipmentGuidBranchMap) || CommonUtils.setIsNullOrEmpty(consolidationDetails.getShipmentsList())) {
            return;
        }

        // Group shipments by Tenant ID to reduce unnecessary context switching
        Map<Integer, List<ShipmentDetails>> shipmentsByTenant = consolidationDetails.getShipmentsList().stream()
                .collect(Collectors.groupingBy(ShipmentDetails::getTenantId));

        Integer originalTenant = TenantContext.getCurrentTenant();

        Set<Integer> tenantIds = shipmentGuidBranchMap.values().stream()
                .flatMap(List::stream)
                .collect(Collectors.toSet());

        var tenantMap = v1ServiceUtil.getTenantDetails(tenantIds.stream().toList());
        Map<Integer, Object> branchMap = new HashMap<>();
        if (isAutomaticTransfer()) {
            Set<Integer> shipmentUniqueTenantIds = consolidationDetails.getShipmentsList().stream()
                    .map(ShipmentDetails::getTenantId)
                    .collect(Collectors.toSet());
            branchMap = v1ServiceUtil.getTenantDetails(shipmentUniqueTenantIds.stream().toList());
        }

        Map<Integer, Object> finalBranchMap = branchMap;
        shipmentsByTenant.forEach((tenantId, shipments) -> {
            if (!Objects.equals(originalTenant, tenantId)) {
                TenantContext.setCurrentTenant(tenantId);
            }
            List<EventsRequest> eventsList = new ArrayList<>();
            for (ShipmentDetails shipment : shipments) {
                List<EventsRequest> events = prepareEvents(
                        shipment.getId(),
                        EventConstants.PRST,
                        SHIPMENT,
                        shipmentGuidBranchMap.getOrDefault(shipment.getGuid().toString(), Collections.emptyList()),
                        tenantMap,
                        finalBranchMap,
                        tenantId
                );
                if (!CommonUtils.listIsNullOrEmpty(events)) {
                    eventsList.addAll(events);
                }
            }
            eventService.saveAllEvent(eventsList);
        });

        // Restore the original tenant
        TenantContext.setCurrentTenant(originalTenant);
    }


    private void createImportEvent(String tenantName, Long entityId, String eventCode, String entityType) {
        if (ObjectUtils.isNotEmpty(entityId)) {
            EventsRequest eventsRequest = new EventsRequest();
            eventsRequest.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
            eventsRequest.setEntityId(entityId);
            eventsRequest.setEntityType(entityType);
            eventsRequest.setEventCode(eventCode);
            eventsRequest.setPlaceName(tenantName);
            eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
            log.info("Import event request: {}", jsonHelper.convertToJson(eventsRequest));
            eventService.saveEvent(eventsRequest);
            log.info("Import {} event got created successfully with RequestId: {}", eventCode, LoggerHelper.getRequestIdFromMDC());
        }
    }


    @Override
    public ResponseEntity<IRunnerResponse> sendConsolidationValidation(CommonRequestModel commonRequestModel) {
        ValidateSendConsolidationRequest request = (ValidateSendConsolidationRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsoleId());
        if (consolidationDetails.isEmpty()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getConsoleId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        validateTransportMode(consolidationDetails.get());
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()) && Objects.equals(consolidationDetails.get().getShipmentType(), Constants.DIRECTION_EXP)) {
            processNTEValidations(consolidationDetails.get());
        }
        else {
            if (consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                    consolidationDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                String flightNumber = getFlightNumber(consolidationDetails.get());
                String voyage = getVoyage(consolidationDetails.get());
                String bol;
                bol = consolidationDetails.get().getBol();
                LocalDateTime eta = consolidationDetails.get().getCarrierDetails().getEta();
                LocalDateTime etd = consolidationDetails.get().getCarrierDetails().getEtd();
                String polId = consolidationDetails.get().getCarrierDetails().getOriginPort();
                String podId = consolidationDetails.get().getCarrierDetails().getDestinationPort();
                List<TriangulationPartner> triangulationPartnerList = consolidationDetails.get().getTriangulationPartnerList();
                Long triangulationBranch = consolidationDetails.get().getTriangulationPartner();
                Long receivingBranch = consolidationDetails.get().getReceivingBranch();
                boolean entityTransferDetails = isShipmentEntityTransferDetails(receivingBranch, triangulationBranch, triangulationPartnerList);
                List<String> missingField = new ArrayList<>();
                if (isAnyRequiredFieldMissing(bol, voyage, flightNumber, eta, etd, polId, podId, entityTransferDetails)) {
                    getMissingFieldsForSeaAir(bol, consolidationDetails.get(), missingField, voyage, flightNumber, eta, etd, polId, podId, entityTransferDetails);
                    String joinMissingField = String.join(",", missingField);
                    throw new ValidationException("Please validate these fields before sending consolidation: " + joinMissingField);
                } else {
                    boolean sendConsolidationError = false;
                    boolean hblGenerationError = false;
                    List<String> shipmentIds = new ArrayList<>();
                    ConsoleValidation consoleValidation = processConsoleShipments(consolidationDetails.get(), receivingBranch, sendConsolidationError, hblGenerationError, shipmentIds);

                    sendConsolidationErrorIfPresent(consoleValidation.sendConsolidationError(), consolidationDetails.get(), receivingBranch, consoleValidation.hblGenerationError(), shipmentIds);
                }
            }
        }
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    private ConsoleValidation processConsoleShipments(ConsolidationDetails consolidationDetails, Long receivingBranch, boolean sendConsolidationError, boolean hblGenerationError, List<String> shipmentIds) {
        for (var shipment : consolidationDetails.getShipmentsList()) {
            boolean isShipmentError = false;
            if (isInterBranchConsoleCase(shipment, consolidationDetails, receivingBranch)) {
                sendConsolidationError = true;
                isShipmentError = true;
            }
            if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                    shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                String shipVoyage = getShipVoyage(shipment);
                String shipFlightNumber = getShipFlightNumber(shipment);
                boolean requiredFieldsMissingError = isCarrierDetailsAndFlightDetailsMissing(shipment, shipVoyage, shipFlightNumber);
                if (requiredFieldsMissingError) {
                    sendConsolidationError = true;
                    isShipmentError = true;
                }
                hblGenerationError = isHblGenerationError(shipment, hblGenerationError);
                if (hblGenerationError) {
                    sendConsolidationError = true;
                    isShipmentError = true;
                }
            }
            if (Boolean.TRUE.equals(isShipmentError)) {
                shipmentIds.add(shipment.getShipmentId());
            }
        }
        return new ConsoleValidation(sendConsolidationError, hblGenerationError);
    }

    private record ConsoleValidation(boolean sendConsolidationError, boolean hblGenerationError) {
    }

    private boolean isShipmentEntityTransferDetails(Long receivingBranch, Long triangulationBranch, List<TriangulationPartner> triangulationPartnerList) {
        boolean entityTransferDetails = false;
        if (Objects.isNull(receivingBranch) && Objects.isNull(triangulationBranch)) {
            entityTransferDetails = ObjectUtils.isEmpty(triangulationPartnerList);
        }
        return entityTransferDetails;
    }

    private String getVoyage(ConsolidationDetails consolidationDetails) {
        String voyage;
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            voyage = consolidationDetails.getCarrierDetails().getVoyage();
        } else {
            voyage = consolidationDetails.getCarrierDetails().getShippingLine();
        }
        return voyage;
    }

    private String getFlightNumber(ConsolidationDetails consolidationDetails) {
        String flightNumber;
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            flightNumber = consolidationDetails.getCarrierDetails().getVessel();
        } else {
            flightNumber = consolidationDetails.getCarrierDetails().getFlightNumber();
        }
        return flightNumber;
    }

    private void validateTransportMode(ConsolidationDetails consolidationDetails) {
        if (Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_RAI)) {
            throw new ValidationException("File transfer is not allowed for Rail Transport Mode");
        }
    }

    private String getShipFlightNumber(ShipmentDetails shipment) {
        String shipFlightNumber = null;
        if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            shipFlightNumber = shipment.getCarrierDetails().getVessel();
        } else {
            shipFlightNumber = shipment.getCarrierDetails().getFlightNumber();
        }
        return shipFlightNumber;
    }

    private String getShipVoyage(ShipmentDetails shipment) {
        String shipVoyage = null;
        if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            shipVoyage = shipment.getCarrierDetails().getVoyage();
        } else {
            shipVoyage = shipment.getCarrierDetails().getShippingLine();
        }
        return shipVoyage;
    }

    private boolean isInterBranchConsoleCase(ShipmentDetails shipment, ConsolidationDetails consolidationDetails, Long receivingBranch) {
        return Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole()) && Objects.isNull(shipment.getReceivingBranch()) && !Objects.isNull(receivingBranch);
    }

    private boolean isAnyRequiredFieldMissing(String bol, String voyage, String flightNumber, LocalDateTime eta, LocalDateTime etd, String polId, String podId, boolean entityTransferDetails) {
        return Strings.isNullOrEmpty(bol) || Strings.isNullOrEmpty(voyage) || Strings.isNullOrEmpty(flightNumber) ||
                eta == null || etd == null || Strings.isNullOrEmpty(polId) || Strings.isNullOrEmpty(podId) || entityTransferDetails;
    }

    private boolean isCarrierDetailsAndFlightDetailsMissing(ShipmentDetails shipment, String shipVoyage, String shipFlightNumber) {
        LocalDateTime shipEta = shipment.getCarrierDetails().getEta();
        LocalDateTime shipEtd = shipment.getCarrierDetails().getEtd();
        String shipPolId = shipment.getCarrierDetails().getOriginPort();
        String shipPodId = shipment.getCarrierDetails().getDestinationPort();
        return (Strings.isNullOrEmpty(shipment.getHouseBill()) && !Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_DRT)) || Strings.isNullOrEmpty(shipment.getMasterBill()) ||
                shipVoyage == null || shipFlightNumber == null || shipEta == null || shipEtd == null ||
                Strings.isNullOrEmpty(shipPolId) || Strings.isNullOrEmpty(shipPodId);
    }

    private boolean isHblGenerationError(ShipmentDetails shipment, boolean hblGenerationError) {
        if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) &&
                shipment.getDirection().equals(Constants.DIRECTION_EXP) &&
                !Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipment.getId());
            if (hbls.isEmpty())
                hblGenerationError = true;
        }

        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
        TenantModel tenantModel = modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
        // LATER Need to set that.tenant.IATAAgent = true condition for Air
        if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) &&
                shipment.getDirection().equals(Constants.DIRECTION_EXP) && tenantModel.IATAAgent &&
                Objects.equals(shipment.getJobType(), SHIPMENT_TYPE_STD)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipment.getId());
            if (awbs.isEmpty())
                hblGenerationError = true;
        }
        return hblGenerationError;
    }

    private void processNTEValidations(ConsolidationDetails consolidationDetails) {
        SendConsoleValidationResponse response;
        if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            response = this.networkTransferValidationsForAirConsolidation(consolidationDetails, false);
        else if (Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_SEA))
            response = this.networkTransferValidationsForSeaConsolidation(consolidationDetails, false);
        else
            response = this.networkTransferValidationsForOtherTransportConsolidation(consolidationDetails, false);
        if(Boolean.TRUE.equals(response.getIsError())) {
            throw new ValidationException(response.getConsoleErrorMessage());
        }
    }

    private void sendConsolidationErrorIfPresent(boolean sendConsolidationError, ConsolidationDetails consolidationDetails, Long receivingBranch, boolean hblGenerationError, List<String> shipmentIds) {
        if (sendConsolidationError) {
            String interBranch = "";
            if (Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole()) && !Objects.isNull(receivingBranch))
                interBranch = "Receiving Branch, ";
            if (hblGenerationError) {
                if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                    throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, " + interBranch + "Vessel & Voyage details in the attached shipments: " + String.join(", ", shipmentIds) + " and generate the Original HBL before sending consolidation");
                } else if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                    throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, " + interBranch + "Airline and Flight number details in the attached shipments: " + String.join(", ", shipmentIds) + " and generate the Original HAWB before sending consolidation");
                }
            } else {
                if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                    throw new ValidationException("Please enter the HBL, MBL, ETA, ETD, " + interBranch + "Vessel & Voyage details in the attached shipments: " + String.join(", ", shipmentIds) + " before sending consolidation");
                } else if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                    throw new ValidationException("Please enter the HAWB, MAWB, ETA, ETD, " + interBranch + "Airline and Flight number details in the attached shipments: " + String.join(", ", shipmentIds) + " before sending consolidation");
                }
            }
        }
    }

    private void getMissingFieldsForSeaAir(String bol, ConsolidationDetails consolidationDetails, List<String> missingField, String voyage, String flightNumber, LocalDateTime eta, LocalDateTime etd, String polId, String podId, boolean entityTransferDetails) {
        if (Strings.isNullOrEmpty(bol) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            missingField.add("Mawb Number");
        if (Strings.isNullOrEmpty(bol) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
            missingField.add("Master Bill");
        if (Strings.isNullOrEmpty(voyage) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            missingField.add("Flight Carrier");
        if (Strings.isNullOrEmpty(flightNumber) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            missingField.add(EntityTransferConstants.MISSING_FIELD_FLIGHT_NUMBER);
        if (Strings.isNullOrEmpty(voyage) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
            missingField.add(EntityTransferConstants.MISSING_FIELD_VOYAGE);
        if (Strings.isNullOrEmpty(flightNumber) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
            missingField.add(EntityTransferConstants.MISSING_FIELD_VESSEL);
        addEtaEtdMissingFields(missingField, eta, etd);
        if (Strings.isNullOrEmpty(polId))
            missingField.add("Origin Port");
        if (Strings.isNullOrEmpty(podId))
            missingField.add("Destination Port");
        if (entityTransferDetails) {
            missingField.add("Please select one of the branches in the entity transfer details section");
        }
    }

    private void addEtaEtdMissingFields(List<String> missingField, LocalDateTime eta, LocalDateTime etd) {
        if (eta == null)
            missingField.add("Eta");
        if (etd == null)
            missingField.add("Etd");
    }

    @Override
    public SendConsoleValidationResponse automaticTransferConsoleValidation(CommonRequestModel commonRequestModel) {
        ValidateSendConsolidationRequest request = (ValidateSendConsolidationRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsoleId());
        if (consolidationDetails.isEmpty()) {
            log.debug(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getConsoleId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if (Objects.equals(consolidationDetails.get().getTransportMode(), TRANSPORT_MODE_RAI)) {
            throw new ValidationException("File transfer is not allowed for Rail Transport Mode");
        }
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();

        SendConsoleValidationResponse response;
        if(Objects.equals(consolidationDetails.get().getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            response = this.networkTransferValidationsForAirConsolidation(consolidationDetails.get(), true);
        else if (Objects.equals(consolidationDetails.get().getTransportMode(), TRANSPORT_MODE_SEA))
            response = this.networkTransferValidationsForSeaConsolidation(consolidationDetails.get(), true);
        else
            response = this.networkTransferValidationsForOtherTransportConsolidation(consolidationDetails.get(), true);
        return response;
    }
    private SendConsoleValidationResponse networkTransferValidationsForAirConsolidation (ConsolidationDetails consolidationDetails, boolean isAutomaticTransfer) {
        boolean isPrintMawbError = false;
        boolean isPrintHawbError = false;
        boolean isHawbNumberError = false;
        List<String> errorShipments = new ArrayList<>();
        List<Long> errorShipIds = new ArrayList<>();

        // Console Fields validations
        List<String> missingField = this.airConsoleFieldValidations(consolidationDetails, isAutomaticTransfer);
        if(Objects.equals(consolidationDetails.getConsolidationType(), Constants.SHIPMENT_TYPE_STD)) {
            List<Awb> mawbs = awbDao.findByConsolidationId(consolidationDetails.getId());
            if (mawbs.isEmpty())
                isPrintMawbError = true;
        }

        for (var shipment : consolidationDetails.getShipmentsList()) {
            if(Objects.equals(shipment.getJobType(), SHIPMENT_TYPE_STD)) {
                List<Awb> awbs = awbDao.findByShipmentId(shipment.getId());
                if (awbs.isEmpty()) {
                    isPrintHawbError = true;
                    errorShipments.add(shipment.getShipmentId());
                    errorShipIds.add(shipment.getId());
                }
            }
            if (!Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_STD)
                    && !Objects.equals(shipment.getJobType(), Constants.CONSOLIDATION_TYPE_DRT)
                    && Strings.isNullOrEmpty(shipment.getHouseBill())) {
                isHawbNumberError = true;
                errorShipments.add(shipment.getShipmentId());
                errorShipIds.add(shipment.getId());
            }
        }
        StringBuilder shipErrorMsg = new StringBuilder();

        // Error message construction
        String errorMsg = this.errorMsgPreparationForAirConsole(missingField, isPrintMawbError, isPrintHawbError, errorShipments, isHawbNumberError, shipErrorMsg, isAutomaticTransfer);
        if(!errorMsg.isEmpty()){
            return SendConsoleValidationResponse.builder()
                    .consoleErrorMessage(errorMsg)
                    .shipmentErrorMessage(shipErrorMsg.toString())
                    .shipmentIds(errorShipIds)
                    .isError(Boolean.TRUE)
                    .build();
        }
        return SendConsoleValidationResponse.builder()
                .isError(Boolean.FALSE)
                .build();
    }

    private List<String> airConsoleFieldValidations(ConsolidationDetails consolidationDetails, boolean isAutomaticTransfer) {
        List<String> missingField = new ArrayList<>();
        if(Strings.isNullOrEmpty(consolidationDetails.getCarrierDetails().getFlightNumber()))
            missingField.add("Flight Number");
        if (consolidationDetails.getCarrierDetails().getEta() == null)
            missingField.add("Eta");
        if (consolidationDetails.getCarrierDetails().getEtd() == null)
            missingField.add("Etd");
        if(!isAutomaticTransfer) {
            boolean entityTransferDetails = false;
            if (Objects.isNull(consolidationDetails.getReceivingBranch()) && Objects.isNull(consolidationDetails.getTriangulationPartner())) {
                entityTransferDetails = ObjectUtils.isEmpty(consolidationDetails.getTriangulationPartnerList());
            }
            if (entityTransferDetails){
                missingField.add(EntityTransferConstants.SELECT_BRANCH_FOR_ET);
            }
        }
        if(!Objects.equals(consolidationDetails.getConsolidationType(), Constants.SHIPMENT_TYPE_STD)
                && !Objects.equals(consolidationDetails.getConsolidationType(), Constants.CONSOLIDATION_TYPE_DRT)
                && Strings.isNullOrEmpty(consolidationDetails.getBol())){
            missingField.add("MAWB Number");
        }
        return missingField;
    }
    private String errorMsgPreparationForAirConsole(List<String> missingField, boolean isPrintMawbError, boolean isPrintHawbError, List<String> errorShipments, boolean isHawbNumberError, StringBuilder shipErrorMsg, boolean isAutomaticTransfer) {
        String errorMsg = "";
        String msgSuffix = (isAutomaticTransfer? EntityTransferConstants.TO_RE_TRIGGER_THE_TRANSFER : EntityTransferConstants.TO_TRANSFER_THE_FILES);
        if(!missingField.isEmpty()) {
            String missingFieldString = String.join(", ", missingField);
            if(isPrintMawbError)
                missingFieldString = missingFieldString + " and generate MAWB";
            errorMsg = EntityTransferConstants.PLEASE_ENTER_THE + missingFieldString + EntityTransferConstants.FOR_THE_CONSOLIDATION;
        } else if (isPrintMawbError) {
            errorMsg = "Please generate MAWB for the consolidation";
        }
        if(isPrintHawbError) {
            if (!errorMsg.isEmpty()) {
                errorMsg = errorMsg + " and generate HAWB for the shipment/s "+ String.join(", " ,errorShipments);
            } else {
                errorMsg = "Please generate HAWB for the shipment/s " + String.join(", " ,errorShipments);
            }
            shipErrorMsg.setLength(0);
            shipErrorMsg.append("Please generate HAWB to retrigger the transfer.");
        }
        if(isHawbNumberError) {
            if (!errorMsg.isEmpty()) {
                errorMsg = errorMsg + " and enter the HAWB number for the shipment/s "+ String.join(", " ,errorShipments);
            } else {
                errorMsg = "Please enter the HAWB number for the shipment/s " + String.join(", " ,errorShipments);
            }
            shipErrorMsg.setLength(0);
            shipErrorMsg.append("Please enter the HAWB number to retrigger the transfer.");
        }
        if(!errorMsg.isEmpty()) {
            errorMsg = errorMsg + msgSuffix;
        }
        return errorMsg;
    }

    private SendConsoleValidationResponse networkTransferValidationsForSeaConsolidation (ConsolidationDetails consolidationDetails, boolean isAutomaticTransfer) {

        boolean isPrintHblError = false;
        List<String> errorShipments = new ArrayList<>();
        List<Long> errorShipIds = new ArrayList<>();

        // Consolidation Fields Validations
        List<String> missingField = this.seaConsoleFieldValidations(consolidationDetails, isAutomaticTransfer);
        for (var shipment : consolidationDetails.getShipmentsList()) {
            if(!Objects.equals(shipment.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
                List<Hbl> hbls = hblDao.findByShipmentId(shipment.getId());
                if (hbls.isEmpty()) {
                    isPrintHblError = true;
                    errorShipments.add(shipment.getShipmentId());
                    errorShipIds.add(shipment.getId());
                }
            }
        }
        String errorMsg = "";
        String msgSuffix = (isAutomaticTransfer? EntityTransferConstants.TO_RE_TRIGGER_THE_TRANSFER : EntityTransferConstants.TO_TRANSFER_THE_FILES);
        if(!missingField.isEmpty()) {
            String missingFieldString = String.join(", ", missingField);
            errorMsg = EntityTransferConstants.PLEASE_ENTER_THE + missingFieldString + EntityTransferConstants.FOR_THE_CONSOLIDATION;
        }
        String shipErrorMsg = "";
        if(isPrintHblError) {
            if (!errorMsg.isEmpty()) {
                errorMsg = errorMsg + " and generate HBL for the shipment/s "+ String.join(", " ,errorShipments);
            } else {
                errorMsg = "Please generate HBL for the shipment/s " + String.join(", " ,errorShipments);
            }
            shipErrorMsg = "Please generate HBL to retrigger the transfer.";
        }
        if(!errorMsg.isEmpty()){
            errorMsg = errorMsg + msgSuffix;
            return SendConsoleValidationResponse.builder()
                    .consoleErrorMessage(errorMsg)
                    .shipmentErrorMessage(shipErrorMsg)
                    .shipmentIds(errorShipIds)
                    .isError(Boolean.TRUE)
                    .build();
        }
        return SendConsoleValidationResponse.builder()
                .isError(Boolean.FALSE)
                .build();
    }

    private List<String> seaConsoleFieldValidations (ConsolidationDetails consolidationDetails, boolean isAutomaticTransfer) {
        List<String> missingField = new ArrayList<>();
        boolean entityTransferDetails = false;
        if(Strings.isNullOrEmpty(consolidationDetails.getBol()))
            missingField.add("MBL");
        if (consolidationDetails.getCarrierDetails().getEta() == null)
            missingField.add("Eta");
        if (consolidationDetails.getCarrierDetails().getEtd() == null)
            missingField.add("Etd");
        if(Strings.isNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()))
            missingField.add("Shipping line");
        if(Strings.isNullOrEmpty(consolidationDetails.getCarrierDetails().getVessel()))
            missingField.add("Vessel");
        if(Strings.isNullOrEmpty(consolidationDetails.getCarrierDetails().getVoyage()))
            missingField.add("Voyage");
        if(Objects.isNull(consolidationDetails.getSendingAgent()) || Strings.isNullOrEmpty(consolidationDetails.getSendingAgent().getOrgCode()))
            missingField.add("Origin agent");
        if(Objects.isNull(consolidationDetails.getReceivingAgent()) || Strings.isNullOrEmpty(consolidationDetails.getReceivingAgent().getOrgCode()))
            missingField.add("Destination agent");
        if (!isAutomaticTransfer && Objects.isNull(consolidationDetails.getReceivingBranch()) && Objects.isNull(consolidationDetails.getTriangulationPartner())) {
            entityTransferDetails = ObjectUtils.isEmpty(consolidationDetails.getTriangulationPartnerList());
        }
        if (entityTransferDetails){
            missingField.add(EntityTransferConstants.SELECT_BRANCH_FOR_ET);
        }
        return missingField;
    }

    private SendConsoleValidationResponse networkTransferValidationsForOtherTransportConsolidation (ConsolidationDetails consolidationDetails, boolean isAutomaticTransfer) {
        List<String> missingField = new ArrayList<>();
        if (consolidationDetails.getCarrierDetails().getEta() == null)
            missingField.add("Eta");
        if (consolidationDetails.getCarrierDetails().getEtd() == null)
            missingField.add("Etd");
        if(Objects.isNull(consolidationDetails.getSendingAgent()) || Strings.isNullOrEmpty(consolidationDetails.getSendingAgent().getOrgCode()))
            missingField.add("Origin agent");
        if(Objects.isNull(consolidationDetails.getReceivingAgent()) || Strings.isNullOrEmpty(consolidationDetails.getReceivingAgent().getOrgCode()))
            missingField.add("Destination agent");
        if(!isAutomaticTransfer) {
            boolean entityTransferDetails = false;
            if (Objects.isNull(consolidationDetails.getReceivingBranch()) && Objects.isNull(consolidationDetails.getTriangulationPartner())) {
                entityTransferDetails = ObjectUtils.isEmpty(consolidationDetails.getTriangulationPartnerList());
            }
            if (entityTransferDetails){
                missingField.add(EntityTransferConstants.SELECT_BRANCH_FOR_ET);
            }
        }

        String errorMsg = "";
        String msgSuffix = (isAutomaticTransfer? EntityTransferConstants.TO_RE_TRIGGER_THE_TRANSFER : EntityTransferConstants.TO_TRANSFER_THE_FILES);
        if(!missingField.isEmpty()) {
            String missingFieldString = String.join(", ", missingField);
            errorMsg = EntityTransferConstants.PLEASE_ENTER_THE + missingFieldString + EntityTransferConstants.FOR_THE_CONSOLIDATION;
        }
        if(!errorMsg.isEmpty()){
            errorMsg = errorMsg + msgSuffix;
            return SendConsoleValidationResponse.builder()
                    .consoleErrorMessage(errorMsg)
                    .isError(Boolean.TRUE)
                    .build();
        }
        return SendConsoleValidationResponse.builder()
                .isError(Boolean.FALSE)
                .build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> sendShipmentValidation(CommonRequestModel commonRequestModel) {
        ValidateSendShipmentRequest request = (ValidateSendShipmentRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipId());
        if (shipmentDetails.isEmpty()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getShipId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()) && Objects.equals(shipmentDetails.get().getDirection(), Constants.DIRECTION_EXP)) {
            validateNteSendShipmentValidations(shipmentDetails.get());
        }
        else {
            if (shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) ||
                    shipmentDetails.get().getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                String flightNumber = getFlightNumber(shipmentDetails.get());
                String voyage = getVoyage(shipmentDetails.get());

                boolean entityTransferDetails = isShipmentEntityTransferDetails(shipmentDetails.get());
                if (isRequiredFieldsMissing(voyage, flightNumber, shipmentDetails.get().getCarrierDetails(), shipmentDetails.get(), entityTransferDetails)) {
                    List<String> missingField = getMissingFields(voyage, shipmentDetails.get(), flightNumber, shipmentDetails.get().getCarrierDetails(), entityTransferDetails);
                    String joinMissingField = String.join(",", missingField);
                    if (StringUtility.isNotEmpty(joinMissingField))
                        throw new ValidationException("Please validate these fields before sending shipment: " + joinMissingField);
                } else {
                    var shipment = shipmentDetails.get();
                    validateHblAndAwb(shipment);
                }
            }
        }
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void validateNteSendShipmentValidations(ShipmentDetails shipmentDetails) {
        var sendShipmentValidationResponse = this.networkTransferValidationsForShipment(shipmentDetails, false);
        if(Boolean.TRUE.equals(sendShipmentValidationResponse.getIsError())) {
            throw new ValidationException(sendShipmentValidationResponse.getShipmentErrorMessage());
        }
    }

    private String getVoyage(ShipmentDetails shipmentDetails) {
        String voyage;
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            voyage = shipmentDetails.getCarrierDetails().getVoyage();
        } else {
            voyage = shipmentDetails.getCarrierDetails().getShippingLine();
        }
        return voyage;
    }

    private String getFlightNumber(ShipmentDetails shipmentDetails) {
        String flightNumber;
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            flightNumber = shipmentDetails.getCarrierDetails().getVessel();
        } else {
            flightNumber = shipmentDetails.getCarrierDetails().getFlightNumber();
        }
        return flightNumber;
    }

    private boolean isRequiredFieldsMissing(String voyage, String flightNumber, CarrierDetails carrierDetails, ShipmentDetails shipmentDetails, boolean entityTransferDetails) {
        LocalDateTime eta = carrierDetails.getEta();
        LocalDateTime etd = carrierDetails.getEtd();
        String polId = carrierDetails.getOriginPort();
        String podId = carrierDetails.getDestinationPort();
        return Strings.isNullOrEmpty(voyage) || Strings.isNullOrEmpty(flightNumber) ||
                eta == null || etd == null || Strings.isNullOrEmpty(polId) || Strings.isNullOrEmpty(podId) ||
                Strings.isNullOrEmpty(shipmentDetails.getHouseBill()) ||
                Strings.isNullOrEmpty(shipmentDetails.getMasterBill()) || entityTransferDetails;
    }

    private boolean isShipmentEntityTransferDetails(ShipmentDetails shipmentDetails) {
        boolean entityTransferDetails = false;
        if (Objects.isNull(shipmentDetails.getReceivingBranch()) && Objects.isNull(shipmentDetails.getTriangulationPartner())) {
            entityTransferDetails = ObjectUtils.isEmpty(shipmentDetails.getTriangulationPartnerList());
        }
        return entityTransferDetails;
    }

    private void validateHblAndAwb(ShipmentDetails shipment) {
        if (shipment.getDirection().equals(Constants.DIRECTION_EXP)) {
            if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                List<Hbl> hbls = hblDao.findByShipmentId(shipment.getId());
                if (hbls.isEmpty())
                    throw new ValidationException("Please generate original HBL before sending shipment");
            }
            DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
            TenantModel tenantModel = modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
            if (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && tenantModel.IATAAgent) {
                List<Awb> awbs = awbDao.findByShipmentId(shipment.getId());
                if (awbs.isEmpty())
                    throw new ValidationException("Please generate original HAWB before sending shipment");
            }
        }
    }

    private List<String> getMissingFields(String voyage, ShipmentDetails shipmentDetails, String flightNumber, CarrierDetails carrierDetails, boolean entityTransferDetails) {
        List<String> missingField =  new ArrayList<>();
        LocalDateTime eta = carrierDetails.getEta();
        LocalDateTime etd = carrierDetails.getEtd();
        String polId = carrierDetails.getOriginPort();
        String podId = carrierDetails.getDestinationPort();
        checkVoyageAndFlightNumberFieldMissing(voyage, shipmentDetails, missingField, flightNumber);
        checkHouseBillMasterBillFieldsMissing(shipmentDetails, missingField);
        if (eta == null)
            missingField.add("Eta");
        if (etd == null)
            missingField.add("Etd");
        if (Strings.isNullOrEmpty(polId))
            missingField.add("Origin Port");
        if (Strings.isNullOrEmpty(podId))
            missingField.add("Destination Port");
        if (entityTransferDetails) {
            missingField.add("Please select one of the branches in the entity transfer details section");
        }
        return missingField;
    }

    private void checkHouseBillMasterBillFieldsMissing(ShipmentDetails shipmentDetails, List<String> missingField) {
        if(Strings.isNullOrEmpty(shipmentDetails.getHouseBill())){
            if (Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()) && !Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType())) {
                missingField.add("HAWB Number");
            }
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)
                    && !Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType()))
                missingField.add("House Bill");
        }
        if (Strings.isNullOrEmpty(shipmentDetails.getMasterBill())){
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                missingField.add("MAWB Number");
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                missingField.add("Master Bill");
        }
    }

    private void checkVoyageAndFlightNumberFieldMissing(String voyage, ShipmentDetails shipmentDetails, List<String> missingField, String flightNumber) {
        if (Strings.isNullOrEmpty(voyage)){
            if(shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                missingField.add("Flight Carrier");
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                missingField.add(EntityTransferConstants.MISSING_FIELD_VOYAGE);
        }
        if (Strings.isNullOrEmpty(flightNumber)){
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                missingField.add(EntityTransferConstants.MISSING_FIELD_FLIGHT_NUMBER);
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))
                missingField.add(EntityTransferConstants.MISSING_FIELD_VESSEL);
        }
    }

    @Override
    public SendShipmentValidationResponse automaticTransferShipmentValidation(CommonRequestModel commonRequestModel) {
        ValidateSendShipmentRequest request = (ValidateSendShipmentRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipId());
        if (!shipmentDetails.isPresent()) {
            log.debug(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, request.getShipId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return this.networkTransferValidationsForShipment(shipmentDetails.get(), true);
    }

    private SendShipmentValidationResponse networkTransferValidationsForShipment(ShipmentDetails shipmentDetails, boolean isAutomaticTransfer) {
        if(Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipmentDetails.getId());
            boolean isAwbPrintError = false;

            // Shipment Fields Validations
            List<String> missingField = this.airShipmentFieldValidations(shipmentDetails, isAutomaticTransfer);

            if (awbs.isEmpty()) {
                isAwbPrintError = true;
            }

            String responseErrorMsg = getResponseErrorMsg(isAutomaticTransfer, missingField, isAwbPrintError);
            if(!Strings.isNullOrEmpty(responseErrorMsg)) {
                return SendShipmentValidationResponse.builder()
                        .isError(true)
                        .shipmentErrorMessage(responseErrorMsg)
                        .build();
            }
            return SendShipmentValidationResponse.builder()
                    .isError(false)
                    .build();
        } else {
            throw new ValidationException("Entity Transfer not allowed for this shipment.");
        }
    }

    private String getResponseErrorMsg(boolean isAutomaticTransfer, List<String> missingField, boolean isAwbPrintError) {
        String responseErrorMsg = "";
        String msgSuffix = (isAutomaticTransfer ? EntityTransferConstants.TO_RE_TRIGGER_THE_TRANSFER : " to transfer the shipment.");
        if(!missingField.isEmpty()) {
            String missingFieldString = String.join(",", missingField);
            if(isAwbPrintError)
                responseErrorMsg = EntityTransferConstants.PLEASE_ENTER_THE + missingFieldString + " and generate MAWB" + msgSuffix;
            else
                responseErrorMsg = EntityTransferConstants.PLEASE_ENTER_THE + missingFieldString + msgSuffix;
        } else if (isAwbPrintError) {
            responseErrorMsg = "Please generate MAWB" + msgSuffix;
        }
        return responseErrorMsg;
    }

    private List<String> airShipmentFieldValidations(ShipmentDetails shipmentDetails, boolean isAutomaticTransfer) {
        List<String> missingField = new ArrayList<>();
        boolean entityTransferDetails = false;
        if(Strings.isNullOrEmpty(shipmentDetails.getCarrierDetails().getFlightNumber())) {
            missingField.add("Flight number");
        }
        if(shipmentDetails.getCarrierDetails().getEta()==null) {
            missingField.add("Eta");
        }
        if(shipmentDetails.getCarrierDetails().getEtd()==null) {
            missingField.add("Etd");
        }
        if (!isAutomaticTransfer && Objects.isNull(shipmentDetails.getReceivingBranch()) && Objects.isNull(shipmentDetails.getTriangulationPartner())) {
            entityTransferDetails = ObjectUtils.isEmpty(shipmentDetails.getTriangulationPartnerList());
        }
        if(entityTransferDetails) {
            missingField.add(EntityTransferConstants.SELECT_BRANCH_FOR_ET);
        }
        return missingField;
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkTaskExist(CommonRequestModel commonRequestModel) throws RunnerException {
        CheckTaskExistRequest request = (CheckTaskExistRequest) commonRequestModel.getData();
        if(CommonUtils.listIsNullOrEmpty(request.getSendToBranch())){
            return ResponseHelper.buildSuccessResponse();
        }
        CheckTaskExistResponse response = new CheckTaskExistResponse();
        List<TaskCreateRequest> taskCreateRequestList  = retireveTaskFromV1(request.getEntityId().toString(), request.getEntityType(), request.getSendToBranch());
        response.setSendToBranch(taskCreateRequestList.stream().map(x -> Integer.parseInt(x.getTenantId())).collect(Collectors.toSet()));
        return ResponseHelper.buildSuccessResponse(response);
    }

    private List<TaskCreateRequest> retireveTaskFromV1(String entityId, String entityType, Object sendToBranch) throws RunnerException {
        CommonV1ListRequest commonV1ListRequest = createCriteriaTaskListRequest(entityId, entityType, sendToBranch);
        log.info("V1 task list request: {}" , jsonHelper.convertToJson(commonV1ListRequest));

        V1DataResponse v1Response;
        try {
            v1Response = v1Service.listTask(commonV1ListRequest);
        }
        catch (Exception ex) {
            log.error("Check Task exist failed to check from V1: " + ex);
            throw new RunnerException("Check Task exist failed to check from V1: " + ex);
        }
        return jsonHelper.convertValueToList(v1Response.getEntities(), TaskCreateRequest.class);
    }


    private CommonV1ListRequest createCriteriaTaskListRequest(Object value1, Object value2, Object value4) {
        List<Object> criteria1 = new ArrayList<>(List.of(List.of("EntityId"), "=", value1));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of("EntityType"), "=", value2));
        List<Object> criteria3 = new ArrayList<>(List.of(List.of("IsCreatedFromV2"), "=", 1));
        List<Object> criteria4 = new ArrayList<>(List.of(List.of("TenantId"), "In", List.of(value4)));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(List.of(criteria1, "and", criteria2), "and", criteria3), "and" , criteria4)).build();
    }

    public void createAutoEvent(Long entityId, String eventCode, String entityType, List<Integer> tenantIds, Integer currentTenant) {
        if (ObjectUtils.isNotEmpty(entityId) && !CommonUtils.listIsNullOrEmpty(tenantIds)) {
            var tenantMap = v1ServiceUtil.getTenantDetails(tenantIds);
            Map<Integer, Object> branchMap = new HashMap<>();
            if (isAutomaticTransfer()) {
                branchMap = v1ServiceUtil.getTenantDetails(Collections.singletonList(currentTenant));
            }
            List<EventsRequest> events = prepareEvents(entityId, eventCode, entityType, tenantIds, tenantMap, branchMap, currentTenant);
            eventService.saveAllEvent(events);
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

    private boolean shouldAddShipmentToGuidList(List<TriangulationPartner> triangulationPartnerList,
                                                Long triangulationPartner,
                                                Long receivingAgent,
                                                ShipmentDetails shipmentDetails) {
        return (ObjectUtils.isNotEmpty(triangulationPartnerList)
                && triangulationPartnerList.stream()
                .filter(Objects::nonNull)
                .noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), receivingAgent)
                        || Objects.equals(tp.getTriangulationPartner(), shipmentDetails.getTenantId().longValue())))
                || (triangulationPartnerList == null
                && triangulationPartner != null
                && !Objects.equals(triangulationPartner, receivingAgent)
                && !shipmentDetails.getTenantId().equals(triangulationPartner.intValue()));
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

        Map<UUID, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        if (!shipmentDetailsList.isEmpty()){
            Set<UUID> consoleGuids = getConsoleGuids(shipmentDetailsList);
            if (!CollectionUtils.isEmpty(consoleGuids)) {
                List<ConsolidationDetails> consolidationDetailsList = findConsolidationFromLogsHistory(consoleGuids.stream().toList(), request.getTimestamp());
                consolidationDetailsMap = consolidationDetailsList.stream().collect(Collectors.toMap(ConsolidationDetails::getGuid, Function.identity()));
            }

            Set<UUID> sourceGuids = new HashSet<>();
            Set<UUID> shipmentGuids = new HashSet<>();
            Set<String> locationRefGuids = new HashSet<>();
            for (var shipmentDetails: shipmentDetailsList) {
                setGuidsWithShipmentDetails(shipmentDetails, sourceGuids, consolidationDetailsMap, locationRefGuids, shipmentGuids);
            }

            Map<UUID, ShipmentDetails> originShipmentsMap = new HashMap<>();
            List<ShipmentDetails> originShipments = getOriginShipments(sourceGuids, request);

            Map<UUID, ConsolidationDetails> originConsoleMap = getOriginConsoleMap(originShipments, shipmentGuids, request);


            Map<UUID, List<ShipmentDetails>> destinationShipmentsMap = getDestinationShipmentsMap(shipmentGuids, request);
            if(originShipments != null && !originShipments.isEmpty()){
                originShipmentsMap = originShipments.stream().collect(Collectors.toMap(ShipmentDetails::getGuid, Function.identity()));
            }
            Map<String, UnlocationsResponse> unlocationsResponseMap = !locationRefGuids.isEmpty() ? masterDataUtils.getLocationData(locationRefGuids) : new HashMap<>();

            processShipmentDetailsList(shipmentDetailsList, originShipmentsMap, originConsoleMap, destinationShipmentsMap, consolidationDetailsMap, unlocationsResponseMap, responseList);

        }
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

    private void processShipmentDetailsList(List<ShipmentDetails> shipmentDetailsList, Map<UUID, ShipmentDetails> originShipmentsMap, Map<UUID, ConsolidationDetails> originConsoleMap, Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, Map<UUID, ConsolidationDetails> consolidationDetailsMap, Map<String, UnlocationsResponse> unlocationsResponseMap, List<IRunnerResponse> responseList) {
        for (var shipmentDetails: shipmentDetailsList) {
            ArValidationResponse arValidationResponse = new ArValidationResponse();
            arValidationResponse.setShipmentGuid(shipmentDetails.getGuid());
            arValidationResponse.setConsolidationType(shipmentDetails.getJobType());
            arValidationResponse.setSourceBranch(shipmentDetails.getTenantId());
            Long receivingBranch = null;
            List<Long> triangulationList = new ArrayList<>();
            if(!Objects.equals(shipmentDetails.getSourceGuid(), shipmentDetails.getGuid())) {
                if(originShipmentsMap.containsKey(shipmentDetails.getSourceGuid())){
                    ShipmentDetails originShipment = originShipmentsMap.get(shipmentDetails.getSourceGuid());
                    processOriginShipmentConsolidationsList(originConsoleMap, destinationShipmentsMap, shipmentDetails, originShipment, arValidationResponse, receivingBranch, triangulationList);
                }
            }
            else if (shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()) {
                processShipmentConsolidation(destinationShipmentsMap, consolidationDetailsMap, unlocationsResponseMap, shipmentDetails, arValidationResponse, receivingBranch, triangulationList);
            } else if(Objects.equals(shipmentDetails.getTransportMode(), TRANSPORT_MODE_AIR) && Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT)) {
                processAirDrtShipment(destinationShipmentsMap, unlocationsResponseMap, shipmentDetails, arValidationResponse, receivingBranch, triangulationList);

            }
            responseList.add(arValidationResponse);
        }
    }

    private void processOriginShipmentConsolidationsList(Map<UUID, ConsolidationDetails> originConsoleMap, Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, ShipmentDetails originShipment, ArValidationResponse arValidationResponse, Long receivingBranch, List<Long> triangulationList) {
        Long entityId;
        String entityType;
        ConsolidationDetails consolidationDetails = null;
        if(canProcessConsolidationList(originConsoleMap, originShipment)){
            if (!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
                consolidationDetails = originConsoleMap.get(originShipment.getConsolidationList().iterator().next().getGuid());
                entityId = consolidationDetails.getId();
                entityType = getEntityType(CONSOLIDATION_IMPORT, CONSOLIDATION);
            } else {
                entityId = originShipment.getId();
                entityType = getEntityType(SHIPMENT_IMPORT, SHIPMENT);
            }
            var receivingAgent = consolidationDetails != null ? consolidationDetails.getReceivingBranch() : null;
            processDrtShipmentTypeForEmptyAgent(destinationShipmentsMap, shipmentDetails, originShipment, arValidationResponse, receivingAgent);
            List<TriangulationPartner> triangulationPartnerList = consolidationDetails != null ? consolidationDetails.getTriangulationPartnerList() : originShipment.getTriangulationPartnerList();
            var triangulationPartner = consolidationDetails != null ? consolidationDetails.getTriangulationPartner() : shipmentDetails.getTriangulationPartner();
            ArValidationResponse.ProfitShareShipmentData originShipmentData = mapShipmentDataToProfitShare(originShipment);
            arValidationResponse.setOrigin(originShipment.getTenantId());
            arValidationResponse.setSalesBranch(originShipment.getSalesBranch());
            arValidationResponse.setReceivingAgent(receivingAgent);
            arValidationResponse.setTriangulationPartner(triangulationPartner);
            arValidationResponse.setOriginShipment(originShipmentData);
            if (receivingAgent != null) {
                receivingBranch = getReceivingBranchFromAgent(destinationShipmentsMap, shipmentDetails, originShipment, arValidationResponse, receivingBranch, receivingAgent);
            }
            if (ObjectUtils.isNotEmpty(triangulationPartnerList)) {
                processTriangulationPartnerList(destinationShipmentsMap, shipmentDetails, originShipment, arValidationResponse, triangulationList, triangulationPartnerList);
            } else if (isTriangulationPartnerPresent(triangulationPartnerList, triangulationPartner)) {
                updateTriangulationListWithPartner(destinationShipmentsMap, shipmentDetails, originShipment, arValidationResponse, triangulationList, triangulationPartner);
            }
            populateNonAcceptedShipment(receivingBranch, triangulationList, entityId, entityType, arValidationResponse);
        }
    }

    private boolean isTriangulationPartnerPresent(List<TriangulationPartner> triangulationPartnerList, Long triangulationPartner) {
        return triangulationPartnerList == null && triangulationPartner != null;
    }

    private boolean canProcessConsolidationList(Map<UUID, ConsolidationDetails> originConsoleMap, ShipmentDetails originShipment) {
        return (originShipment.getConsolidationList() != null && !originShipment.getConsolidationList().isEmpty() &&
                originConsoleMap.containsKey(originShipment.getConsolidationList().iterator().next().getGuid())) || Objects.equals(originShipment.getJobType(), SHIPMENT_TYPE_DRT);
    }

    private String getEntityType(String importEntityType, String newEntityType) {
        String entityType = importEntityType;
        if (Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled())) {
            entityType = newEntityType;
        }
        return entityType;
    }

    private void processDrtShipmentTypeForEmptyAgent(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, ShipmentDetails originShipment, ArValidationResponse arValidationResponse, Long receivingAgent) {
        if (Objects.isNull(receivingAgent) && Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT)) {
            Long receivingBranch = shipmentDetails.getReceivingBranch();
            if (destinationShipmentsMap.containsKey(originShipment.getGuid())) {
                var ships = destinationShipmentsMap.get(originShipment.getGuid());
                var isShips = ships.stream()
                        .filter(x -> !x.getTenantId().equals(receivingBranch != null ? receivingBranch.intValue() : 0))
                        .toList();

                if (!isShips.isEmpty()) {
                    List<ArValidationResponse.ProfitShareShipmentData> triangulationShipmentDataList = new ArrayList<>();
                    for(var ship: isShips) {
                        ArValidationResponse.ProfitShareShipmentData triangulationShipment = mapShipmentDataToProfitShare(ship);
                        triangulationShipmentDataList.add(triangulationShipment);
                    }
                    arValidationResponse.setTransferToTriangulationPartner(true);
                    arValidationResponse.setTriangulationShipment(triangulationShipmentDataList.get(0));
                    arValidationResponse.setTriangulationShipmentList(triangulationShipmentDataList);
                }
            }
        }
    }

    private Long getReceivingBranchFromAgent(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, ShipmentDetails originShipment, ArValidationResponse arValidationResponse, Long receivingBranch, Long receivingAgent) {
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
            } else {
                receivingBranch = receivingAgent;
            }
        } else {
            receivingBranch = receivingAgent;
        }
        return receivingBranch;
    }

    private void processTriangulationPartnerList(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, ShipmentDetails originShipment, ArValidationResponse arValidationResponse, List<Long> triangulationList, List<TriangulationPartner> triangulationPartnerList) {
        if (destinationShipmentsMap.containsKey(originShipment.getGuid()) || triangulationPartnerList.stream()
                .filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), shipmentDetails.getTenantId().longValue()))) {
            setTriangulationDataInResponse(destinationShipmentsMap, originShipment, triangulationPartnerList, arValidationResponse);
        }
        Set<Integer> branchIds = new HashSet<>();
        if(arValidationResponse.getTriangulationShipmentList() != null) {
            branchIds = arValidationResponse.getTriangulationShipmentList().stream()
                    .filter(Objects::nonNull)
                    .map(ArValidationResponse.ProfitShareShipmentData::getBranchId)
                    .collect(Collectors.toSet());
        }

        for(TriangulationPartner triangulation : triangulationPartnerList) {
            if(!branchIds.contains(triangulation.getTriangulationPartner().intValue())) {
                triangulationList.add(triangulation.getTriangulationPartner());
            }
        }
    }

    private void updateTriangulationListWithPartner(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, ShipmentDetails originShipment, ArValidationResponse arValidationResponse, List<Long> triangulationList, Long triangulationPartner) {
        if (shipmentDetails.getTenantId().equals(triangulationPartner.intValue())) {
            ArValidationResponse.ProfitShareShipmentData triangulationShipmentData = mapShipmentDataToProfitShare(shipmentDetails);
            arValidationResponse.setTransferToTriangulationPartner(true);
            arValidationResponse.setTriangulationShipment(triangulationShipmentData);
            List<ArValidationResponse.ProfitShareShipmentData> triangulationShipmentDataList = new ArrayList<>();
            triangulationShipmentDataList.add(triangulationShipmentData);
            arValidationResponse.setTriangulationShipmentList(triangulationShipmentDataList);
        } else if (destinationShipmentsMap.containsKey(originShipment.getGuid())) {
            var ships = destinationShipmentsMap.get(originShipment.getGuid());
            var isShip = ships.stream().filter(x -> x.getTenantId().equals(triangulationPartner.intValue())).findAny();
            if (isShip.isPresent()) {
                ArValidationResponse.ProfitShareShipmentData triangulationShipmentData = mapShipmentDataToProfitShare(isShip.get());
                arValidationResponse.setTransferToTriangulationPartner(true);
                arValidationResponse.setTriangulationShipment(triangulationShipmentData);
                List<ArValidationResponse.ProfitShareShipmentData> triangulationShipmentDataList = new ArrayList<>();
                triangulationShipmentDataList.add(triangulationShipmentData);
                arValidationResponse.setTriangulationShipmentList(triangulationShipmentDataList);
            } else {
                triangulationList.add(triangulationPartner);
            }
        } else {
            triangulationList.add(triangulationPartner);
        }
    }

    private void setTriangulationDataInResponse(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails originShipment, List<TriangulationPartner> triangulationPartnerList, ArValidationResponse arValidationResponse) {
        var ships = destinationShipmentsMap.get(originShipment.getGuid());
        var isShips = ships.stream()
                .filter(shp -> triangulationPartnerList.stream()
                        .filter(Objects::nonNull)
                        .anyMatch(partner -> Objects.equals(partner.getTriangulationPartner(), shp.getTenantId().longValue())))
                .toList();
        if (!isShips.isEmpty()) {
            List<ArValidationResponse.ProfitShareShipmentData> triangulationShipmentDataList = new ArrayList<>();
            for(var ship: isShips) {
                ArValidationResponse.ProfitShareShipmentData triangulationShipment = mapShipmentDataToProfitShare(ship);
                triangulationShipmentDataList.add(triangulationShipment);
            }
            arValidationResponse.setTransferToTriangulationPartner(true);
            arValidationResponse.setTriangulationShipment(triangulationShipmentDataList.get(0));
            arValidationResponse.setTriangulationShipmentList(triangulationShipmentDataList);
        }
    }

    private void processShipmentConsolidation(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, Map<UUID, ConsolidationDetails> consolidationDetailsMap, Map<String, UnlocationsResponse> unlocationsResponseMap, ShipmentDetails shipmentDetails, ArValidationResponse arValidationResponse, Long receivingBranch, List<Long> triangulationList) {
        String entityType;
        ConsolidationDetails consolidationDetails;
        Long entityId;
        if (consolidationDetailsMap.containsKey(shipmentDetails.getConsolidationList().iterator().next().getGuid())) {
            consolidationDetails = consolidationDetailsMap.get(shipmentDetails.getConsolidationList().iterator().next().getGuid());
            entityId = consolidationDetails.getId();
            entityType = getEntityType(CONSOLIDATION_IMPORT, CONSOLIDATION);
            var receivingAgent = consolidationDetails.getReceivingBranch();
            var triangulationPartnerList = consolidationDetails.getTriangulationPartnerList();
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
            receivingBranch = getReceivingBranchFromAgent(destinationShipmentsMap, shipmentDetails, receivingAgent, arValidationResponse, receivingBranch);
            processTriangulationPartnerList(destinationShipmentsMap, shipmentDetails, triangulationPartnerList, receivingAgent, arValidationResponse, triangulationList, triangulationPartner);

            populateNonAcceptedShipment(receivingBranch, triangulationList, entityId, entityType, arValidationResponse);
        }
    }

    private void processAirDrtShipment(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, Map<String, UnlocationsResponse> unlocationsResponseMap, ShipmentDetails shipmentDetails, ArValidationResponse arValidationResponse, Long receivingBranch, List<Long> triangulationList) {
        String entityType;
        Long entityId;
        entityId = shipmentDetails.getId();
        entityType = getEntityType(SHIPMENT_IMPORT, SHIPMENT);
        var receivingAgent = shipmentDetails.getReceivingBranch();
        var triangulationPartnerList = shipmentDetails.getTriangulationPartnerList();
        var triangulationPartner = shipmentDetails.getTriangulationPartner();
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
        receivingBranch = getReceivingBranchFromAgent(destinationShipmentsMap, shipmentDetails, receivingAgent, arValidationResponse, receivingBranch);
        processTriangulationPartnerList(destinationShipmentsMap, shipmentDetails, triangulationPartnerList, receivingAgent, arValidationResponse, triangulationList, triangulationPartner);

        populateNonAcceptedShipment(receivingBranch, triangulationList, entityId, entityType, arValidationResponse);
    }

    private Long getReceivingBranchFromAgent(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, Long receivingAgent, ArValidationResponse arValidationResponse, Long receivingBranch) {
        if (receivingAgent != null &&
                !shipmentDetails.getTenantId().equals(receivingAgent.intValue()) && destinationShipmentsMap.containsKey(shipmentDetails.getGuid())) {
            var ships = destinationShipmentsMap.get(shipmentDetails.getGuid());
            var isShip = ships.stream().filter(x -> x.getTenantId().equals(receivingAgent.intValue())).findAny();
            if (isShip.isPresent()) {
                arValidationResponse.setTransferToReceivingAgent(true);
                ArValidationResponse.ProfitShareShipmentData receivingShipmentData = mapShipmentDataToProfitShare(isShip.get());
                arValidationResponse.setReceivingShipment(receivingShipmentData);
            } else {
                receivingBranch = receivingAgent;
            }
        } else {
            receivingBranch = receivingAgent;
        }
        return receivingBranch;
    }

    private void processTriangulationPartnerList(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, List<TriangulationPartner> triangulationPartnerList, Long receivingAgent, ArValidationResponse arValidationResponse, List<Long> triangulationList, Long triangulationPartner) {
        if (ObjectUtils.isNotEmpty(triangulationPartnerList)) {
            if (triangulationPartnerList.stream().filter(Objects::nonNull).anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), receivingAgent))) {
                arValidationResponse.setTransferToTriangulationPartner(arValidationResponse.getTransferToReceivingAgent());
                arValidationResponse.setTriangulationShipment(arValidationResponse.getReceivingShipment());
                arValidationResponse.setTriangulationShipmentList(List.of(arValidationResponse.getReceivingShipment()));
            } else if (triangulationPartnerList.stream()
                    .filter(Objects::nonNull)
                    .noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), shipmentDetails.getTenantId().longValue()))
                    && destinationShipmentsMap.containsKey(shipmentDetails.getGuid())
            ) {
                setTriangulationDataInResponse(destinationShipmentsMap, shipmentDetails, triangulationPartnerList, arValidationResponse);
            }
            Set<Integer> branchIds = new HashSet<>();
            if(arValidationResponse.getTriangulationShipmentList() != null) {
                branchIds = arValidationResponse.getTriangulationShipmentList().stream()
                        .filter(Objects::nonNull)
                        .map(ArValidationResponse.ProfitShareShipmentData::getBranchId)
                        .collect(Collectors.toSet());
            }

            for(TriangulationPartner triangulation : triangulationPartnerList) {
                if(!branchIds.contains(triangulation.getTriangulationPartner().intValue())) {
                    triangulationList.add(triangulation.getTriangulationPartner());
                }
            }
        } else if (isTriangulationPartnerPresent(triangulationPartnerList, triangulationPartner)) {
            updateListFromPartner(destinationShipmentsMap, shipmentDetails, receivingAgent, arValidationResponse, triangulationList, triangulationPartner);
        }
    }

    private void updateListFromPartner(Map<UUID, List<ShipmentDetails>> destinationShipmentsMap, ShipmentDetails shipmentDetails, Long receivingAgent, ArValidationResponse arValidationResponse, List<Long> triangulationList, Long triangulationPartner) {
        if (Objects.equals(triangulationPartner, receivingAgent)) {
            arValidationResponse.setTransferToTriangulationPartner(arValidationResponse.getTransferToReceivingAgent());
            arValidationResponse.setTriangulationShipment(arValidationResponse.getReceivingShipment());
            arValidationResponse.setTriangulationShipmentList(List.of(arValidationResponse.getReceivingShipment()));
        } else if (!shipmentDetails.getTenantId().equals(triangulationPartner.intValue()) && destinationShipmentsMap.containsKey(shipmentDetails.getGuid())) {
            var ships = destinationShipmentsMap.get(shipmentDetails.getGuid());
            var isShip = ships.stream().filter(x -> x.getTenantId().equals(triangulationPartner.intValue())).findAny();
            if (isShip.isPresent()) {
                arValidationResponse.setTransferToTriangulationPartner(true);
                ArValidationResponse.ProfitShareShipmentData triangulationData = mapShipmentDataToProfitShare(isShip.get());
                arValidationResponse.setTriangulationShipment(triangulationData);
                arValidationResponse.setTriangulationShipmentList(List.of(triangulationData));
            } else {
                triangulationList.add(triangulationPartner);
            }
        } else {
            triangulationList.add(triangulationPartner);
        }
    }

    private Map<UUID, List<ShipmentDetails>> getDestinationShipmentsMap(Set<UUID> shipmentGuids, PostArValidationRequest request) throws RunnerException {
        Map<UUID, List<ShipmentDetails>> destinationShipmentsMap = new HashMap<>();
        List<ShipmentDetails> destinationShip = !shipmentGuids.isEmpty() ? shipmentDao.findShipmentsBySourceGuids(shipmentGuids) : null;
        Set<UUID> destinationShipmentsGuids = destinationShip != null ? destinationShip.stream().map(ShipmentDetails::getGuid).collect(Collectors.toSet()) : null;
        List<ShipmentDetails> destinationShipments = destinationShipmentsGuids != null ? findShipmentsFromLogsHistory(destinationShipmentsGuids.stream().toList(), request.getTimestamp()) : null;
        if(destinationShipments != null && !destinationShipments.isEmpty()){
            destinationShipmentsMap = destinationShipments.stream().collect(Collectors.groupingBy(ShipmentDetails::getSourceGuid));
        }
        return destinationShipmentsMap;
    }

    private LinkedHashSet<UUID> getConsoleGuids(List<ShipmentDetails> shipmentDetailsList) {
        return shipmentDetailsList.stream().filter(x -> (x.getConsolidationList() != null && !x.getConsolidationList().isEmpty())).map(x -> x.getConsolidationList().iterator().next().getGuid()).collect(Collectors.toCollection(LinkedHashSet::new));
    }


    private Map<UUID, ConsolidationDetails> getOriginConsoleMap(List<ShipmentDetails> originShipments, Set<UUID> shipmentGuids, PostArValidationRequest request) throws RunnerException {
        Map<UUID, ConsolidationDetails> originConsoleMap = new HashMap<>();
        if(originShipments != null && !originShipments.isEmpty()) {
            shipmentGuids.addAll(originShipments.stream().map(ShipmentDetails::getGuid).collect(Collectors.toSet()));
            Set<UUID> originConsoleGuids = originShipments.stream().filter(x-> (x.getConsolidationList() != null && !x.getConsolidationList().isEmpty())).map(x->x.getConsolidationList().iterator().next().getGuid()).collect(Collectors.toSet());
            List<ConsolidationDetails> originConsolidationDetails = findConsolidationFromLogsHistory(originConsoleGuids.stream().toList(), request.getTimestamp());
            originConsoleMap = originConsolidationDetails.stream().collect(Collectors.toMap(ConsolidationDetails::getGuid, Function.identity()));
        }
        return originConsoleMap;
    }

    private List<ShipmentDetails> getOriginShipments(Set<UUID> sourceGuids, PostArValidationRequest request) throws RunnerException {
        return !sourceGuids.isEmpty() ? findShipmentsFromLogsHistory(sourceGuids.stream().toList(), request.getTimestamp()) : null;
    }

    private void setGuidsWithShipmentDetails(ShipmentDetails shipmentDetails, Set<UUID> sourceGuids, Map<UUID, ConsolidationDetails> consolidationDetailsMap, Set<String> locationRefGuids, Set<UUID> shipmentGuids) {
        if(!Objects.equals(shipmentDetails.getSourceGuid(), shipmentDetails.getGuid()) || Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT)){
            sourceGuids.add(shipmentDetails.getSourceGuid());
        }
        else if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()){
            ConsolidationDetails consolidationDetails;
            if(consolidationDetailsMap.containsKey(shipmentDetails.getConsolidationList().iterator().next().getGuid())) {
                consolidationDetails = consolidationDetailsMap.get(shipmentDetails.getConsolidationList().iterator().next().getGuid());

                var receivingAgent = consolidationDetails.getReceivingBranch();
                List<TriangulationPartner> triangulationPartnerList = consolidationDetails.getTriangulationPartnerList();
                var triangulationPartner = consolidationDetails.getTriangulationPartner();
                if (receivingAgent == null && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
                    var destination = shipmentDetails.getCarrierDetails().getDestination();
                    if (destination != null)
                        locationRefGuids.add(destination);
                }
                setShipmentGuids(shipmentDetails, shipmentGuids, receivingAgent, triangulationPartnerList, triangulationPartner);
            }
        }
    }

    private void setShipmentGuids(ShipmentDetails shipmentDetails, Set<UUID> shipmentGuids, Long receivingAgent, List<TriangulationPartner> triangulationPartnerList, Long triangulationPartner) {
        if (receivingAgent != null &&
                !shipmentDetails.getTenantId().equals(receivingAgent.intValue())) {
            shipmentGuids.add(shipmentDetails.getGuid());
        }
        if (shouldAddShipmentToGuidList(triangulationPartnerList, triangulationPartner, receivingAgent, shipmentDetails)) {
            shipmentGuids.add(shipmentDetails.getGuid());
        }
    }


    private void populateNonAcceptedShipment(Long receivingAgent, List<Long> triangulationPartnerList, Long entityId, String entityType, ArValidationResponse response) {
        List<Long> partnerList = new ArrayList<>();
        if(receivingAgent != null) {
            partnerList.add(receivingAgent);
        }
        partnerList.addAll(triangulationPartnerList);
        if(!CollectionUtils.isEmpty(partnerList)) {
            try {
                Set<Integer> tenantIds = getTenantIds(entityId, entityType, partnerList);
                if(CollectionUtils.isEmpty(tenantIds)) {
                    if(receivingAgent != null) {
                        response.setReceivingShipment(mapShipmentDataToProfitShare(TransferStatus.NOT_TRANSFERRED, receivingAgent.intValue()));
                    }
                    for(Long triangulationId : triangulationPartnerList) {
                        response.addTriangulationShipmentList(mapShipmentDataToProfitShare(TransferStatus.NOT_TRANSFERRED, triangulationId.intValue()));
                    }
                } else {
                    processTaskCreateRequestsList(receivingAgent, triangulationPartnerList, response, tenantIds);
                }
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }
    }

    private Set<Integer> getTenantIds(Long entityId, String entityType, List<Long> partnerList) throws RunnerException {
        Set<Integer> tenantIds = new HashSet<>();
        if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled())) {
            List<Integer> partnerListInt = partnerList.stream().map(Long::intValue).toList();
            tenantIds = this.retrieveTaskFromNte(entityId, entityType, partnerListInt);
        } else {
            List<TaskCreateRequest> taskCreateRequests = retireveTaskFromV1(String.valueOf(entityId), entityType, partnerList);
            if(!CollectionUtils.isEmpty(taskCreateRequests))
                tenantIds = taskCreateRequests.stream().map(x -> Integer.parseInt(x.getTenantId())).collect(Collectors.toSet());
        }
        return tenantIds;
    }

    @SuppressWarnings("java:S2259")
    private Set<Integer> retrieveTaskFromNte(Long entityId, String entityType, List<Integer> tenantIds) {
        List<NetworkTransfer> networkTransfers = networkTransferDao.findByEntityAndTenantList(entityId, entityType, tenantIds);
        networkTransfers = ObjectUtils.isNotEmpty(networkTransfers) ?
                networkTransfers.stream().filter(networkTransfer -> NetworkTransferStatus.TRANSFERRED == networkTransfer.getStatus()).toList() : null;

        if (ObjectUtils.isNotEmpty(networkTransfers)) {
            return networkTransfers.stream().map(NetworkTransfer::getTenantId).collect(Collectors.toSet());
        }
        return new HashSet<>();
    }

    private void processTaskCreateRequestsList(Long receivingAgent, List<Long> triangulationPartnerList, ArValidationResponse response, Set<Integer> tenantIds) {
        if (receivingAgent != null) {
            if (tenantIds.contains(receivingAgent.intValue())) {
                response.setReceivingShipment(mapShipmentDataToProfitShare(TransferStatus.TRANSFERRED, receivingAgent.intValue()));
            } else {
                response.setReceivingShipment(mapShipmentDataToProfitShare(TransferStatus.NOT_TRANSFERRED, receivingAgent.intValue()));
            }
        }
        for (Long triangulationId : triangulationPartnerList) {
            if (tenantIds.contains(triangulationId.intValue())) {
                response.addTriangulationShipmentList(mapShipmentDataToProfitShare(TransferStatus.TRANSFERRED, triangulationId.intValue()));
            } else {
                response.addTriangulationShipmentList(mapShipmentDataToProfitShare(TransferStatus.NOT_TRANSFERRED, triangulationId.intValue()));
            }
        }
    }

    private List<ShipmentDetails> findShipmentsFromLogsHistory(List<UUID> guids, LocalDateTime timeStamp) throws RunnerException {
        guids = Optional.ofNullable(guids).orElse(Collections.emptyList());
        guids = guids.stream().filter(Objects::nonNull).toList();
        if(CollectionUtils.isEmpty(guids) || timeStamp == null) {
            return Collections.emptyList();
        }
        List<LogHistoryResponse> logHistoryResponses = logsHistoryService.findByEntityGuidsAndTimeStamp(guids, timeStamp);
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        Set<UUID> remainingGuids = new HashSet<>(guids);
        if(!logHistoryResponses.isEmpty())
            logHistoryResponses.forEach(
                    log -> {
                        UUID guid = log.getEntityGuid();
                        ShipmentDetails entityPayload = jsonHelper.readFromJson(log.getEntityPayload(), ShipmentDetails.class);
                        if(Objects.isNull(entityPayload.getSourceGuid()))
                            entityPayload.setSourceGuid(guid);
                        shipmentDetailsList.add(entityPayload);
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
                .branchId(shipmentDetails.getTenantId())
                .transferStatus(TransferStatus.ACCEPTED)
                .build();
    }

    private ArValidationResponse.ProfitShareShipmentData mapShipmentDataToProfitShare(TransferStatus transferStatus, int branchId) {
        return  ArValidationResponse.ProfitShareShipmentData.builder()
                .branchId(branchId)
                .transferStatus(transferStatus)
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
                case Constants.SHIPMENT_CAMELCASE -> isPresent = !shipmentDao.findBySourceGuid(UUID.fromString(request.getEntityId())).isEmpty();
                case Constants.CONSOLIDATION_CAMELCASE -> isPresent = !consolidationDetailsDao.findBySourceGuid(UUID.fromString(request.getEntityId())).isEmpty();
                default -> log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, request.getEntityType());
            }

            return ResponseHelper.buildSuccessResponse(CheckEntityExistResponse.builder().isEntityExists(isPresent).message(isPresent ? String.format(EntityTransferConstants.TRANSFERRED_ENTITY_ALREADY_PRESENT, request.getEntityType()) : null).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkAcceptedFiles(
        CommonRequestModel commonRequestModel) {
        AcceptedFileRequest acceptedFileRequest = (AcceptedFileRequest) commonRequestModel.getData();
        Long entityId = acceptedFileRequest.getEntityId();
        String entityType = acceptedFileRequest.getEntityType();
        List<Integer> sendToBranch = acceptedFileRequest.getSendToBranch();
        if (sendToBranch == null || sendToBranch.isEmpty()) {
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }

        List<NetworkTransfer> networkTransfers = networkTransferDao.findByEntityNTList(
            entityId, entityType);
        boolean sendingAgain = isNTAlreadySended(networkTransfers, sendToBranch);

        networkTransfers = ObjectUtils.isNotEmpty(networkTransfers) ?
            networkTransfers.stream().filter(
                    networkTransfer -> EntityTransferConstants.RETRANSFER_SET.contains(networkTransfer.getStatus()))
                .toList() : Collections.emptyList();

        String acceptedBranches = "";
        if (ObjectUtils.isNotEmpty(networkTransfers) && sendingAgain) {
            List<Integer> tenantIdList = networkTransfers.stream().map(NetworkTransfer::getTenantId)
                .toList();
            acceptedBranches = String.join(", ", getTenantName(tenantIdList));
        }
        AcceptedFileResponse acceptedFileResponse = AcceptedFileResponse.builder()
            .acceptedBranches(acceptedBranches)
            .build();

        return ResponseHelper.buildSuccessResponse(acceptedFileResponse);
    }

    private boolean isNTAlreadySended(List<NetworkTransfer> networkTransfers, List<Integer> sendToBranch){
        return networkTransfers.stream()
            .map(NetworkTransfer::getTenantId)
            .anyMatch(sendToBranch::contains);
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
                processContainersList(shipmentDetailsOptional.get(), containerVsShipmentGuid, shipmentGuid);
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

    private void processContainersList(ShipmentDetails shipmentDetails, Map<UUID, List<UUID>> containerVsShipmentGuid, UUID shipmentGuid) {
        if(shipmentDetails.getContainersList() != null) {
            shipmentDetails.getContainersList().stream().map(Containers::getGuid).forEach(
                    containerGuid -> {
                        if(!containerVsShipmentGuid.containsKey(containerGuid)) {
                            containerVsShipmentGuid.put(containerGuid, new ArrayList<>());
                        }
                        containerVsShipmentGuid.get(containerGuid).add(shipmentGuid);
                    }
            );
        }
    }

    private Map<String, Object> getConsolMasterData(ConsolidationDetails consolidationDetails) {
        ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        return consolidationService.fetchAllMasterDataByKey(consolidationDetailsResponse);
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
        taskCreateRequest.setUserId(UserContext.getUser().getUserId());
        // can be moved as background task
        if(Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS.equalsIgnoreCase(entityType))
            taskCreateRequest.setTaskType(TaskType.CONSOLIDATION_IMPORTER.getDescription());
        else
            taskCreateRequest.setTaskType(TaskType.SHIPMENT_IMPORTER.getDescription());
        tasksService.createTask(CommonRequestModel.buildRequest(taskCreateRequest));
    }

    public void sendRejectionEmail(String username, String rejectionRemarks, String entityType, String entityNumber) {
        if(username != null) {
            List<String> emailList = new ArrayList<>();
            Map<String, String> usernameEmailsMap = new HashMap<>();
            commonUtils.getUserDetails(new HashSet<>(Set.of(username)), usernameEmailsMap);
            if (usernameEmailsMap.containsKey(username))
                emailList.add(usernameEmailsMap.get(username));
            if (!emailList.isEmpty()) {
                EmailTemplatesRequest template = createRejectionEmailBody(rejectionRemarks, entityType, entityNumber);
                sendEmailNotification(template, emailList, new ArrayList<>());
            }
        }
    }

    private EmailTemplatesRequest createRejectionEmailBody(String rejectionRemarks, String entityType, String entityNumber){
        UsersDto user = UserContext.getUser();
        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(List.of(TenantContext.getCurrentTenant())));
        String body;
        String subject;
        if(SHIPMENT.equals(entityType)) {
            body = EntityTransferConstants.SHIPMENT_REJECTION_EMAIL_BODY;
            subject = EntityTransferConstants.SHIPMENT_REJECTION_EMAIL_SUBJECT;
            subject = subject.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, entityNumber);
            body = body.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, entityNumber);
        } else {
            body = EntityTransferConstants.CONSOLIDATION_REJECTION_EMAIL_BODY;
            subject = EntityTransferConstants.CONSOLIDATION_REJECTION_EMAIL_SUBJECT;
            subject = subject.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, entityNumber);
            body = body.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, entityNumber);
        }

        body = body.replace(EntityTransferConstants.USER_NAME_PLACEHOLDER, user.getDisplayName());
        body = body.replace(EntityTransferConstants.BRANCH_NAME_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(EntityTransferConstants.CANCELLATION_REASON_PLACEHOLDER, rejectionRemarks);
        body = body.replace(EntityTransferConstants.CANCELLED_USER_EMAIL_PLACEHOLDER, user.getEmail());

        return EmailTemplatesRequest.builder().body(body).subject(subject).build();
    }

    public void sendConsolidationEmailNotification(ConsolidationDetails consolidationDetails, List<Integer> destinationBranches, Map<String, List<Integer>> shipmentGuidSendToBranch, Boolean isAutomaticTransfer) {

        var emailTemplatesRequests = getEmailTemplates(CONSOLIDATION_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());

        // No CC emails are used
        List<String> ccEmails = new ArrayList<>();
        // Current user (the sender) in CC for NTE
        UsersDto user = UserContext.getUser();
        if(user.Email!=null)
            ccEmails.add(user.Email);
        // Fetching role ids corresponding to console destination branches
        var shipDestinationBranchIds = new ArrayList<>(destinationBranches);
        if(shipmentGuidSendToBranch == null)
            shipmentGuidSendToBranch = new HashMap<>();
        for (var keySet : shipmentGuidSendToBranch.entrySet())
            shipDestinationBranchIds.addAll(keySet.getValue());
        shipDestinationBranchIds.add(TenantContext.getCurrentTenant());
        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(shipDestinationBranchIds));

        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        for (int i = 0; i < destinationBranches.size(); i++) {
            List<String> emailList;
            if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled())) {
                emailList = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), destinationBranches.get(i));
            } else {
                emailList = getRoleListByRoleId(getShipmentConsoleImportApprovalRole(destinationBranches.get(i)));
            }
            var template = createConsolidationImportEmailBody(consolidationDetails, emailTemplateModel, shipmentGuidSendToBranch, i, branchIdVsTenantModelMap, destinationBranches, isAutomaticTransfer);
            sendEmailNotification(template, emailList, ccEmails);
        }
    }

    public void sendShipmentEmailNotification(ShipmentDetails shipmentDetails, List<Integer> destinationBranches, Boolean isAutomaticTransfer) {
        var emailTemplatesRequests = getEmailTemplates(SHIPMENT_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());
        createShipmentImportEmailBody(shipmentDetails, emailTemplateModel, isAutomaticTransfer);

        // No CC emails are used
        List<String> ccEmails = new ArrayList<>();
        // Current user (the sender) in CC for NTE
        UsersDto user = UserContext.getUser();
        if(Boolean.TRUE.equals(isAutomaticTransfer)) {
            Map<String, String> usernameEmailsMap = new HashMap<>();
            if(!Strings.isNullOrEmpty(shipmentDetails.getAssignedTo())) {
                commonUtils.getUserDetails(new HashSet<>(Set.of(shipmentDetails.getAssignedTo())), usernameEmailsMap);
                if (usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                    ccEmails.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            }
        }
        else if(user.Email!=null)
            ccEmails.add(user.Email);

        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        for(Integer tenantId: destinationBranches) {
            List<String> emailList;
            if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled())) {
                emailList = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), tenantId);
            } else {
                emailList = getRoleListByRoleId(getShipmentConsoleImportApprovalRole(tenantId));
            }
            sendEmailNotification(emailTemplateModel, emailList, ccEmails);
        }
    }

    public void createShipmentImportEmailBody(ShipmentDetails shipmentDetails, EmailTemplatesRequest template, Boolean isAutomaticTransfer) {
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
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, Boolean.TRUE.equals(isAutomaticTransfer) ? "Automated Transfer" : StringUtility.convertToString(user.getDisplayName()));
        body = body.replace(BL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getHouseBill()));
        body = body.replace(MBL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getMasterBill()));
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        template.setSubject(subject);
        template.setBody(body);
    }

    public EmailTemplatesRequest createConsolidationImportEmailBody(ConsolidationDetails consolidationDetails, EmailTemplatesRequest template, Map<String, List<Integer>> shipmentGuidSendToBranch, int index, Map<Integer, TenantModel> tenantMap, List<Integer> destinationBranches, Boolean isAutomaticTransfer) {
        UsersDto user = UserContext.getUser();

        String blNumbers = (consolidationDetails.getShipmentsList() == null) ? "" :
                consolidationDetails.getShipmentsList().stream()
                        .map(c -> {
                            var branch = getBranch(shipmentGuidSendToBranch, index, destinationBranches, c);
                            return StringUtility.convertToString(c.getHouseBill()) + " - " + getTenantName(tenantMap, branch);
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
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, Boolean.TRUE.equals(isAutomaticTransfer) ? "Automated Transfer" : StringUtility.convertToString(user.getDisplayName()));
        body = body.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));
        body = body.replace(BL_NUMBER_PLACEHOLDER, blNumbers);
        body = body.replace(MBL_NUMBER_PLACEHOLDER, TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) ? StringUtility.convertToString(consolidationDetails.getBol()) : StringUtility.convertToString(consolidationDetails.getMawb()));
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, StringUtility.convertToString(consolidationDetails.getConsolidationNumber()));
        body = body.replace(SHIPMENT_NUMBERS_PLACEHOLDER, StringUtility.convertToString(shipmentNumbers));

        return EmailTemplatesRequest.builder().body(body).subject(subject).build();

    }

    private String getTenantName(Map<Integer, TenantModel> tenantMap, Integer branch) {
        return tenantMap.containsKey(branch) ? tenantMap.get(branch).getTenantName() : Constants.EMPTY_STRING;
    }

    private Integer getBranch(Map<String, List<Integer>> shipmentGuidSendToBranch, int index, List<Integer> destinationBranches, ShipmentDetails c) {
        return shipmentGuidSendToBranch.containsKey(StringUtility.convertToString(c.getGuid())) ? shipmentGuidSendToBranch.get(StringUtility.convertToString(c.getGuid())).get(index) : destinationBranches.get(index);
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
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        for(Integer tenantId: tenantIds) {
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, tenantId, false);
            List<String> importerEmailIds;
            if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled())) {
                importerEmailIds = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), tenantId);
            } else {
                importerEmailIds = getRoleListByRoleId(getShipmentConsoleImportApprovalRole(tenantId));
            }
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
            String sourceBranchName = Objects.isNull(sourceTenantId) && tenantMap.containsKey(sourceTenantId) ? Constants.EMPTY_STRING : tenantMap.get(sourceTenantId).getTenantName();
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
        List<Object> criteria = new ArrayList<>();
        List<Object> field1List = new ArrayList<>(List.of(Constants.TYPE));
        criteria.add(new ArrayList<>(List.of(field1List, Operators.IN.getValue(), List.of(requests))));

        List<Object> field2List = new ArrayList<>(List.of("TenantId"));
        criteria.add("and");
        criteria.add(new ArrayList<>(List.of(field2List, Operators.IN.getValue(), List.of(List.of(TenantContext.getCurrentTenant())))));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplatesWithTenantId(request);
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
                if(!CommonUtils.setIsNullOrEmpty(shipment.getContainersList()))
                    containerIds.addAll(shipment.getContainersList().stream().map(BaseEntity::getId).toList());
            }
            if(!CommonUtils.listIsNullOrEmpty(consolidation.getContainersList())) {
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

    private List<String> getEmailsListByPermissionKeysAndTenantId(List<String> permissionKeys, Integer tenantId) {
        UserWithPermissionRequestV1 request = new UserWithPermissionRequestV1();
        request.setUserTenantId(tenantId);
        request.setPermissionKeys(permissionKeys);

        List<UsersDto> usersDtoList = v1Service.getUsersWithGivenPermissions(request);
        List<String> userEmailIds = new ArrayList<>();
        if(!CommonUtils.listIsNullOrEmpty(usersDtoList)) {
            for(UsersDto user: usersDtoList) {
                if (!CommonUtils.isStringNullOrEmpty(user.getEmail())) {
                    userEmailIds.add(user.getEmail());
                }
            }
        }
        return userEmailIds;
    }

}
