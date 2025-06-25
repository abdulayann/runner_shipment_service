package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BL_NUMBER_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CLIENT_NAME_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSIGNEE_NAME_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSIGNOR_NAME_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_NUMBER_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_V3_IMPORT_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DEFAULT_CONSOLIDATION_V3_RECEIVED_BODY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DEFAULT_CONSOLIDATION_V3_RECEIVED_SUBJECT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DEFAULT_GROUPED_SHIPMENT_RECEIVED_BODY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DEFAULT_SHIPMENT_RECEIVED_SUBJECT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DEFAULT_SHIPMENT_V3_RECEIVED_BODY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_CTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_IMP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.EMPTY_STRING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.GROUPED_SHIPMENT_BODY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.GROUPED_SHIPMENT_IMPORT_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MBL_NUMBER_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.NUMBER_OF_SHIPMENTS_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SENDER_USER_NAME_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SENT_DATE_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_IMPORT_V3_EMAIL_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_NUMBER_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_DRT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_BRANCH_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSFERRED_DATE_PLACEHOLDER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TYPE;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.UserWithPermissionRequestV1;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportV3ConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportV3ShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ImportConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferV3Service;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
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

@Service
@Slf4j
@NoArgsConstructor
public class EntityTransferV3Service implements IEntityTransferV3Service {
    public static final String SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Shipment Details is null for Id {} with Request Id {}";
    public static final String CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID = "Consolidation Details is null for Id {} with Request Id {}";
    private IShipmentSettingsDao shipmentSettingsDao;
    private IShipmentDao shipmentDao;
    private IShipmentServiceV3 shipmentService;
    private IConsolidationV3Service consolidationService;
    private IConsolidationDetailsDao consolidationDetailsDao;
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    private ModelMapper modelMapper;
    private IV1Service v1Service;
    private JsonHelper jsonHelper;
    private IHblDao hblDao;
    private IAwbDao awbDao;
    private IEventDao eventDao;
    private IEventsV3Service eventService;
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
    public EntityTransferV3Service(IShipmentSettingsDao shipmentSettingsDao, IShipmentDao shipmentDao, IShipmentServiceV3 shipmentService, IConsolidationV3Service consolidationService
            , IConsolidationDetailsDao consolidationDetailsDao, IShipmentsContainersMappingDao shipmentsContainersMappingDao, ModelMapper modelMapper, IV1Service v1Service, JsonHelper jsonHelper,
                                   IHblDao hblDao, IAwbDao awbDao, IEventDao eventDao, MasterDataUtils masterDataUtils, ILogsHistoryService logsHistoryService, IContainerDao containerDao,
                                   IPackingDao packingDao, MasterDataFactory masterDataFactory, CommonUtils commonUtils, IV1Service iv1Service, V1ServiceUtil v1ServiceUtil, ITasksService tasksService,
                                   INotificationService notificationService, ExecutorService executorService, DocumentManagerRestClient documentManagerRestClient, IConsoleShipmentMappingDao consoleShipmentMappingDao,
                                   ConsolidationSync consolidationSync, ShipmentSync shipmentSync, INetworkTransferService networkTransferService, INetworkTransferDao networkTransferDao, IEventsV3Service eventService, INotificationDao notificationDao,
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
    public List<Integer> sendShipment(CommonRequestModel commonRequestModel) {
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
            logIdNull(SHIPMENT_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, shipId);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipment = shipmentDetails.get();

        List<Integer> successTenantIds = new ArrayList<>();

        Set<Integer> uniqueDestinationTenants = new HashSet<>(sendShipmentRequest.getSendToBranch());
        var tenantMap = getTenantMap(List.of(shipment.getTenantId()));

        List<Integer> destinationTenantList = uniqueDestinationTenants.stream().toList();
        var entityTransferPayload = prepareShipmentPayload(shipment);
        entityTransferPayload.setSourceBranchTenantName(tenantMap.get(shipment.getTenantId()).getTenantName());
        entityTransferPayload.setAdditionalDocs(additionalDocs);

        for (Integer tenant : destinationTenantList) {
            var taskPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferV3ShipmentDetails.class);
            setDirectionInTaskPayload(tenant, shipment, taskPayload);
            taskPayload.setSendToBranch(tenant);
            processNetworkTransfer(tenant, shipment, taskPayload, shipId);
            successTenantIds.add(tenant);
        }

        if (shouldSaveShipment(shipment)) {
            shipmentDao.saveEntityTransfer(shipId, Boolean.TRUE);
        }

        try {
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> sendShipmentEmailNotification(shipment, uniqueDestinationTenants.stream().toList(), sendShipmentRequest.getIsAutomaticTransfer())), executorService);
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }

        this.createBulkExportEvent(shipId, EventConstants.PRST, SHIPMENT, successTenantIds, shipment.getTenantId());

        return successTenantIds;
    }

    private void processNetworkTransfer(Integer tenant, ShipmentDetails shipment, EntityTransferV3ShipmentDetails taskPayload, Long shipId) {
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

    private void setDirectionInTaskPayload(Integer tenant, ShipmentDetails shipment, EntityTransferV3ShipmentDetails taskPayload) {
        boolean isReceivingBranch = false;
        if ((Long.valueOf(tenant).equals(shipment.getReceivingBranch()))) {
            taskPayload.setDirection(reverseDirection(shipment.getDirection()));
            isReceivingBranch = true;
        }
        else if (ObjectUtils.isNotEmpty(shipment.getTriangulationPartnerList())
                && shipment.getTriangulationPartnerList().stream()
                    .filter(Objects::nonNull)
                    .anyMatch(tp -> Objects.equals(Long.valueOf(tenant), tp.getTriangulationPartner()))
        ) {
            taskPayload.setDirection(DIRECTION_CTS);
        } else if (CommonUtils.listIsNullOrEmpty(shipment.getTriangulationPartnerList()) && Long.valueOf(tenant).equals(shipment.getTriangulationPartner())) {
            taskPayload.setDirection(DIRECTION_CTS);
        }
        if(isReceivingBranch)
            updateReceivingBranchFieldsInShipment(taskPayload);
    }

    private void updateReceivingBranchFieldsInShipment(EntityTransferV3ShipmentDetails taskPayload){
        taskPayload.getAdditionalDetails().setBlInstructionReceived(null);
        taskPayload.getAdditionalDetails().setScreeningStatus(null);
        taskPayload.getAdditionalDetails().setAomFreeText(null);
        taskPayload.setSecurityStatus(null);
        taskPayload.getAdditionalDetails().setExemptionCodes(null);
        taskPayload.getAdditionalDetails().setSci(null);
        taskPayload.getAdditionalDetails().setSecurityStatusReceivedFrom(null);
        taskPayload.getAdditionalDetails().setRegulatedEntityCategory(null);
        taskPayload.getAdditionalDetails().setAdditionalSecurityInformation(null);
        taskPayload.getAdditionalDetails().setHouseBillType(null);
        taskPayload.getAdditionalDetails().setReleaseType(null);
        taskPayload.getAdditionalDetails().setPlaceOfSupply(null);
        taskPayload.getAdditionalDetails().setPlaceOfIssue(null);
        taskPayload.getAdditionalDetails().setBLChargesDisplay(null);
        taskPayload.getAdditionalDetails().setPaidPlace(null);
        taskPayload.getAdditionalDetails().setDateOfIssue(null);
        taskPayload.getAdditionalDetails().setDateOfReceipt(null);
        taskPayload.getAdditionalDetails().setOriginal(null);
        taskPayload.getAdditionalDetails().setCopy(null);
    }


    private void updateReceivingBranchFieldsInConsole(EntityTransferV3ConsolidationDetails taskPayload){
        taskPayload.setAdditionalSecurityInformation(null);
        taskPayload.setAomFreeText(null);
        taskPayload.setSecurityStatus(null);
        taskPayload.setScreeningStatus(null);
        taskPayload.setSci(null);
        taskPayload.setExemptionCodes(null);
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
    public List<Integer> sendConsolidation(CommonRequestModel commonRequestModel) {
        SendConsolidationRequest sendConsolidationRequest = (SendConsolidationRequest) commonRequestModel.getData();
        Long consolId = sendConsolidationRequest.getConsolId();
        List<Integer> sendToBranch = sendConsolidationRequest.getSendToBranch();
        List<String> sendToOrg = sendConsolidationRequest.getSendToOrg();
        List<Integer> successTenantIds = new ArrayList<>();
        Map<String, List<Integer>> shipmentGuidSendToBranch = sendConsolidationRequest.getShipmentGuidSendToBranch();

        if (isSendToBranchAndOrgMissing(sendToBranch, sendToOrg)) {
            throw new ValidationException(EntityTransferConstants.SELECT_SENDTOBRANCH_OR_SENDTOORG);
        }

        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolId);
        if (consolidationDetails.isEmpty()) {
            logIdNull(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails consol = consolidationDetails.get();

        Map<UUID, ShipmentDetails> guidVsShipmentMap = getGuidVsShipmentMap(consol);

        interBranchValidation(consol, sendConsolidationRequest);
        EntityTransferV3ConsolidationDetails entityTransferPayload = prepareConsolidationPayload(consol, sendConsolidationRequest);

        Map<String, List<Integer>> shipmentGuidBranchMap = new HashMap<>();
        for (int index = 0; index < sendToBranch.size(); index++) {
            var tenant = sendToBranch.get(index);

            var consolidationPayload = jsonHelper.convertValue(entityTransferPayload, EntityTransferV3ConsolidationDetails.class);
            consolidationPayload.setSendToBranch(tenant);

            processConsoleShipmentList(consolidationPayload, shipmentGuidSendToBranch, index, tenant, shipmentGuidBranchMap, guidVsShipmentMap, consol);

            processConsoleNetworkTransfer(tenant, consol, consolidationPayload, consolId);
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

        return successTenantIds;

    }

    private void logIdNull(String shipmentDetailsIsNullForIdWithRequestId, Long id) {
        log.debug(shipmentDetailsIsNullForIdWithRequestId, id, LoggerHelper.getRequestIdFromMDC());
    }

    private boolean shouldSaveShipment(ShipmentDetails shipment) {
        return Objects.equals(shipment.getTransportMode(), TRANSPORT_MODE_SEA)
                && (Objects.equals(shipment.getDirection(), DIRECTION_EXP) || Objects.equals(shipment.getDirection(), DIRECTION_CTS) );
    }

    private boolean shouldSetTenantContext(ShipmentDetails shipment) {
        return Objects.equals(shipment.getTransportMode(), TRANSPORT_MODE_AIR) && !Objects.equals(TenantContext.getCurrentTenant(), shipment.getTenantId());
    }

    private boolean isSendToBranchAndOrgMissing(List<Integer> sendToBranch, List<String> sendToOrg) {
        return (sendToBranch == null || sendToBranch.isEmpty()) && (sendToOrg == null || sendToOrg.isEmpty());
    }

    private void processConsoleShipmentList(EntityTransferV3ConsolidationDetails consolidationPayload, Map<String, List<Integer>> shipmentGuidSendToBranch, int index, Integer tenant, Map<String, List<Integer>> shipmentGuidBranchMap, Map<UUID, ShipmentDetails> guidVsShipmentMap, ConsolidationDetails consol) {
        boolean reverseDirection = isReverseDirection(tenant, consol);
        boolean sendingToTriangulationPartner = isSendingToTriangulationPartner(tenant, consol);

        updateConsolidationPayload(consolidationPayload, consol, reverseDirection, sendingToTriangulationPartner);

        if (!consolidationPayload.getShipmentsList().isEmpty()) {
            for (var entityTransferShipment : consolidationPayload.getShipmentsList()) {
                var guid = entityTransferShipment.getGuid();
                if (reverseDirection) {
                    entityTransferShipment.setDirection(reverseDirection(entityTransferShipment.getDirection()));
                    updateReceivingBranchFieldsInShipment(entityTransferShipment);
                }
                else if (sendingToTriangulationPartner)
                    entityTransferShipment.setDirection(DIRECTION_CTS);

                if (validShipmentGuids(shipmentGuidSendToBranch, guid))
                    entityTransferShipment.setSendToBranch(shipmentGuidSendToBranch.get(guid.toString()).get(index));
                else
                    entityTransferShipment.setSendToBranch(tenant);
                if (guid != null) {
                    shipmentGuidBranchMap.computeIfAbsent(guid.toString(), k -> new ArrayList<>())
                        .add(entityTransferShipment.getSendToBranch());
                }
                processInterConsoleCase(consolidationPayload, guidVsShipmentMap, entityTransferShipment, guid);
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

    private void updateConsolidationPayload(EntityTransferV3ConsolidationDetails consolidationPayload,
        ConsolidationDetails consol,
        boolean reverseDirection,
        boolean sendingToTriangulationPartner) {
        if (reverseDirection) {
            consolidationPayload.setShipmentType(reverseDirection(consol.getShipmentType()));
            updateReceivingBranchFieldsInConsole(consolidationPayload);
        } else if (sendingToTriangulationPartner) {
            consolidationPayload.setShipmentType(DIRECTION_CTS);
        }
    }

    private void processInterConsoleCase(EntityTransferV3ConsolidationDetails consolidationPayload, Map<UUID, ShipmentDetails> guidVsShipmentMap, EntityTransferV3ShipmentDetails entityTransferShipment, UUID guid) {
        if(Boolean.TRUE.equals(getIsNetworkTransferFeatureEnabled()) && Boolean.TRUE.equals(consolidationPayload.getInterBranchConsole()) && !Objects.equals(entityTransferShipment.getDirection(), DIRECTION_CTS)) {
            this.sendOverarchingShipmentToNetworkTransfer(entityTransferShipment.getSendToBranch(), entityTransferShipment, guidVsShipmentMap.get(guid));
        }
    }

    private boolean validShipmentGuids(Map<String, List<Integer>> shipmentGuidSendToBranch, UUID guid) {
        return shipmentGuidSendToBranch != null && shipmentGuidSendToBranch.containsKey(guid.toString()) && !CommonUtils.listIsNullOrEmpty(shipmentGuidSendToBranch.get(guid.toString()));
    }

    private void removePendingShipmentNotifications(EntityTransferV3ConsolidationDetails consolidationPayload, ConsolidationDetails consol, Integer tenant) {
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

    private void processConsoleNetworkTransfer(Integer tenant, ConsolidationDetails consol, EntityTransferV3ConsolidationDetails consolidationPayload, Long consolId) {
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

    private void sendOverarchingShipmentToNetworkTransfer(Integer tenant, EntityTransferV3ShipmentDetails entityTransferShipment, ShipmentDetails shipment) {
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


    @Transactional
    @Override
    public String importShipment (CommonRequestModel commonRequestModel) throws RunnerException {
        this.validateApprovalRoleForImport();
        ImportV3ShipmentRequest importShipmentRequest = (ImportV3ShipmentRequest) commonRequestModel.getData();

        // Update task status rejected
        if(Objects.equals(importShipmentRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            rejectionForReTransferNte(importShipmentRequest.getTaskId(), importShipmentRequest.getRejectRemarks(), SHIPMENT);
        }

        if (importShipmentRequest.getEntityData() == null) {
            throw new ValidationException("No Shipment payload present please check");
        }
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().documents(new ArrayList<>()).build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = importShipmentRequest.getEntityData();

        if(importShipmentRequest.getAssignedTo()!=null)
            entityTransferShipmentDetails.setAssignedTo(importShipmentRequest.getAssignedTo());
        MutableBoolean isCreateShip = new MutableBoolean(false);
        log.info("Import shipment request: {} with RequestId: {}", jsonHelper.convertToJson(entityTransferShipmentDetails), LoggerHelper.getRequestIdFromMDC());

        // Find old shipment with source guid
        List<ShipmentDetails> oldShipmentDetailsList = shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch());
        ShipmentDetails oldShipmentDetails = oldShipmentDetailsList!=null && !oldShipmentDetailsList.isEmpty()? oldShipmentDetailsList.get(0): null;

        // Import shipment implementation
        String department = commonUtils.getAutoPopulateDepartment(entityTransferShipmentDetails.getTransportMode(), entityTransferShipmentDetails.getDirection(), MdmConstants.SHIPMENT_MODULE);
        ShipmentDetailsResponse shipmentDetailsResponse =  this.createShipment(entityTransferShipmentDetails, copyDocumentsRequest, isCreateShip, department, oldShipmentDetailsList);
        log.info("Shipment got created successfully with RequestId: {}" , LoggerHelper.getRequestIdFromMDC());
        String shipmentId = shipmentDetailsResponse.getShipmentId();

        // Call Document Service api for copy docs
        String authToken = RequestAuthContext.getAuthToken();
        sendCopyDocumentRequest(copyDocumentsRequest, authToken);

        // Push data to dependant service
        pushImportShipmentDataToDependantService(shipmentDetailsResponse.getId(), isCreateShip.isTrue(), oldShipmentDetails);

        // Update task status approved
        updateNteStatus(importShipmentRequest, shipmentDetailsResponse);

        return shipmentId;
    }

    private void rejectionForReTransferNte(Long taskId, String rejectionRemarks, String entityType) {
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

    private void updateNteStatus(ImportV3ShipmentRequest importShipmentRequest, ShipmentDetailsResponse shipmentDetailsResponse) {
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
    }

    private void validateApprovalRoleForImport() {
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

    private void pushImportShipmentDataToDependantService(Long shipmentId, boolean isCreateShip, ShipmentDetails oldShipmentDetails) {
        try {
            Optional<ShipmentDetails> shipment = shipmentDao.findById(shipmentId);
            shipment.ifPresent(shipmentDetails -> shipmentService.triggerPushToDownStream(shipmentDetails, oldShipmentDetails, isCreateShip));
        } catch (Exception ex) {
            log.error("Error occurred while pushing import shipment data to dependent service : {}", ex.getMessage());
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> importConsolidation (CommonRequestModel commonRequestModel) throws RunnerException {
        this.validateApprovalRoleForImport();
        ImportV3ConsolidationRequest importConsolidationRequest = (ImportV3ConsolidationRequest) commonRequestModel.getData();
        // Update task status rejected
        if(Objects.equals(importConsolidationRequest.getOperation(), TaskStatus.REJECTED.getDescription())) {
            rejectionForReTransferNte(importConsolidationRequest.getTaskId(), importConsolidationRequest.getRejectRemarks(), CONSOLIDATION);
        }
        if (importConsolidationRequest.getEntityData() == null) {
            throw new ValidationException("No consolidation payload present please check");
        }
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = importConsolidationRequest.getEntityData();

        setAssignedToOfShipments(entityTransferConsolidationDetails, importConsolidationRequest);
        Map<UUID, Long> oldVsNewShipIds = new HashMap<>();

        // Import consolidation implementation
        ConsolidationDetailsResponse consolidationDetailsResponse = this.createConsolidation(entityTransferConsolidationDetails, oldVsNewShipIds);
        String consolidationNumber = Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getConsolidationNumber).orElse(null);

        // Update task status approved
        networkTransferService.updateStatusAndCreatedEntityId(importConsolidationRequest.getTaskId(), NetworkTransferStatus.ACCEPTED.name(), Optional.ofNullable(consolidationDetailsResponse).map(ConsolidationDetailsResponse::getId).orElse(null));
        var nte = networkTransferDao.findById(importConsolidationRequest.getTaskId());
        if(nte.isPresent() && !Objects.equals(nte.get().getSource(), NetworkTransferSource.EXTERNAL)) {
            updateNteStatus(nte.get(), consolidationDetailsResponse, oldVsNewShipIds);
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
            logIdNull(CONSOLIDATION_DETAILS_IS_NULL_FOR_ID_WITH_REQUEST_ID, consolId);
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

    private void setAssignedToOfShipments(EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails, ImportV3ConsolidationRequest importConsolidationRequest) {
        if (entityTransferConsolidationDetails.getShipmentsList()!=null
        && importConsolidationRequest.getShipmentNumberAssignedToMap()!=null) {
            Map<String, String> shipmentNumberAssignedToMap = importConsolidationRequest.getShipmentNumberAssignedToMap();
            for(EntityTransferV3ShipmentDetails shipmentDetails: entityTransferConsolidationDetails.getShipmentsList()){
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

    private ConsolidationDetailsResponse createConsolidation (EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails, Map<UUID, Long> oldVsNewShipIds) throws RunnerException {
        SyncingContext.setContext(false);
        List<ConsolidationDetails> oldConsolidationDetailsList = consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid());
        Map<UUID, List<UUID>> oldContVsOldShipGuidMap = entityTransferConsolidationDetails.getContainerVsShipmentGuid();
        Map<UUID, UUID> oldPackVsOldContGuidMap = entityTransferConsolidationDetails.getPackingVsContainerGuid();

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
            Set<Long> detachShipIds = oldConsolidationDetailsList.get(0).getShipmentsList().stream().map(BaseEntity::getId).collect(Collectors.toSet());
            if(!detachShipIds.isEmpty()) {
                ShipmentConsoleAttachDetachV3Request request = ShipmentConsoleAttachDetachV3Request.builder().consolidationId(oldConsolidationDetailsList.get(0).getId()).shipmentIds(detachShipIds).build();
                consolidationService.detachShipments(request);
            }
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
            Set<Long> shipmentIdSet = new HashSet<>(shipmentIds);
            if(!interBranchShipment.isEmpty()) {
                commonUtils.setInterBranchContextForHub();
                createShipmentPullRequest(interBranchShipment, consolidationDetailsResponse.getId());
                ShipmentConsoleAttachDetachV3Request request = ShipmentConsoleAttachDetachV3Request.builder().shipmentRequestedType(ShipmentRequestedType.APPROVE).consolidationId(consolidationDetailsResponse.getId()).shipmentIds(shipmentIdSet).isFromConsolidation(true).build();
                consolidationService.attachShipments(request);
            } else {
                ShipmentConsoleAttachDetachV3Request request = ShipmentConsoleAttachDetachV3Request.builder().consolidationId(consolidationDetailsResponse.getId()).shipmentIds(shipmentIdSet).isFromConsolidation(true).build();
                consolidationService.attachShipments(request);
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

    private ConsolidationDetailsResponse createOrUpdateConsolidation(EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails, List<ConsolidationDetails> oldConsolidationDetailsList, MutableBoolean isCreateConsole) throws RunnerException {
        ConsolidationEtV3Request consolidationDetailsRequest =  modelMapper.map(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
        ConsolidationDetailsResponse consolidationDetailsResponse;
        consolidationDetailsRequest.setDepartment(commonUtils.getAutoPopulateDepartment(consolidationDetailsRequest.getTransportMode(), consolidationDetailsRequest.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE));
        if(oldConsolidationDetailsList == null || oldConsolidationDetailsList.isEmpty()) {
            consolidationDetailsRequest.setGuid(null);
            consolidationDetailsRequest.setShipmentsList(null);
            consolidationDetailsRequest.setSourceGuid(entityTransferConsolidationDetails.getGuid());

            consolidationDetailsResponse = consolidationService.createConsolidationFromEntityTransfer(consolidationDetailsRequest);
            isCreateConsole.setTrue();
        } else {
            consolidationDetailsRequest = jsonHelper.convertValue(oldConsolidationDetailsList.get(0), ConsolidationEtV3Request.class);

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

    private void createOrUpdateShipment(EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails, Map<UUID, UUID> newVsOldPackingGuid, Map<UUID, Long> oldVsNewShipIds, List<UUID> shipmentGuids, List<Long> shipmentIds, List<Long> interBranchShipment, CopyDocumentsRequest copyDocumentsRequest, Map<Long, Boolean> isCreateShipMap) throws RunnerException {
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

                List<ShipmentDetails> oldShipmentDetailsList = shipmentDao.findShipmentBySourceGuidAndTenantId(ship.getGuid(), ship.getSendToBranch());

                // Create shipment
                ShipmentDetailsResponse shipmentDetailsResponse = createShipment(ship, copyDocumentsRequest, isCreateShip, department, oldShipmentDetailsList);
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


    private ShipmentDetailsResponse createShipment(EntityTransferV3ShipmentDetails entityTransferShipmentDetails, CopyDocumentsRequest copyDocumentsRequest, MutableBoolean isCreateShip, String department, List<ShipmentDetails> oldShipmentDetailsList) throws RunnerException {
        ShipmentEtV3Request shipmentRequest = modelMapper.map(entityTransferShipmentDetails, ShipmentEtV3Request.class);
        var tenantId = UserContext.getUser().getTenantId();
        shipmentRequest.setDepartment(department);
        if(entityTransferShipmentDetails.getSendToBranch() == null) {
            entityTransferShipmentDetails.setSendToBranch(tenantId);
        }
        // Set Inter Branch TenantId in TenantContext for inter branch Shipment
        if(Objects.equals(shipmentRequest.getTransportMode(), TRANSPORT_MODE_AIR) && !Objects.equals(shipmentRequest.getJobType(), SHIPMENT_TYPE_DRT) && !Objects.equals(entityTransferShipmentDetails.getSendToBranch(), tenantId)){
            commonUtils.setInterBranchContextForHub();
            TenantContext.setCurrentTenant(entityTransferShipmentDetails.getSendToBranch());
        }

        ShipmentDetailsResponse shipmentDetailsResponse = null;
        if(oldShipmentDetailsList == null || oldShipmentDetailsList.isEmpty()){
            shipmentRequest.setGuid(null);
            shipmentRequest.setShipmentCreatedOn(LocalDateTime.now());
            shipmentRequest.setSourceGuid(entityTransferShipmentDetails.getGuid());

            shipmentDetailsResponse = shipmentService.createShipmentFromEntityTransfer(shipmentRequest, true);
            isCreateShip.setTrue();
        } else {
            shipmentRequest = jsonHelper.convertValue(oldShipmentDetailsList.get(0), ShipmentEtV3Request.class);

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
        this.createImportEvent(entityTransferShipmentDetails.getSourceBranchTenantName(), shipmentDetailsResponse.getId(), EventConstants.TSHA, SHIPMENT);

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

    private void cleanAllListEntitiesForShipment(ShipmentEtV3Request shipmentRequest){
        if(shipmentRequest.getPackingList() != null) shipmentRequest.getPackingList().clear();
        if(shipmentRequest.getContainersList() != null) shipmentRequest.getContainersList().clear();
        if(shipmentRequest.getRoutingsList() != null) shipmentRequest.getRoutingsList().clear();
        if(shipmentRequest.getReferenceNumbersList() != null) shipmentRequest.getReferenceNumbersList().clear();
        if(shipmentRequest.getBookingCarriagesList() != null) shipmentRequest.getBookingCarriagesList().clear();
        if(shipmentRequest.getServicesList() != null) shipmentRequest.getServicesList().clear();
        if(shipmentRequest.getShipmentAddresses() != null) shipmentRequest.getShipmentAddresses().clear();
        if(shipmentRequest.getNotesList() != null) shipmentRequest.getNotesList().clear();
    }

    private void cleanAllListEntitiesForConsolidation(ConsolidationEtV3Request consolidationDetailsRequest){
        if(consolidationDetailsRequest.getPackingList() != null) consolidationDetailsRequest.getPackingList().clear();
        if(consolidationDetailsRequest.getContainersList() != null) consolidationDetailsRequest.getContainersList().clear();
        if(consolidationDetailsRequest.getRoutingsList() != null) consolidationDetailsRequest.getRoutingsList().clear();
        if(consolidationDetailsRequest.getReferenceNumbersList() != null) consolidationDetailsRequest.getReferenceNumbersList().clear();
        if(consolidationDetailsRequest.getConsolidationAddresses() != null) consolidationDetailsRequest.getConsolidationAddresses().clear();
        if(consolidationDetailsRequest.getShipmentsList() != null) consolidationDetailsRequest.getShipmentsList().clear();
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
            ListCommonRequest listCommonRequest = constructListCommonRequest(SHIPMENT_ID, shipmentIds, "IN");
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
            eventsRequest.setSource(MASTER_DATA_SOURCE_CARGOES_RUNNER);
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
            eventsRequest.setSource(MASTER_DATA_SOURCE_CARGOES_RUNNER);
            log.info("Import event request: {}", jsonHelper.convertToJson(eventsRequest));
            eventService.saveEvent(eventsRequest);
            log.info("Import {} event got created successfully with RequestId: {}", eventCode, LoggerHelper.getRequestIdFromMDC());
        }
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

    public List<String> getTenantName(List<Integer> tenantIds) {
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

    private EntityTransferV3ConsolidationDetails prepareConsolidationPayload(ConsolidationDetails consolidationDetails, SendConsolidationRequest sendConsolidationRequest) {
        List<Integer> tenantIds = new ArrayList<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.addAll(consolidationDetails.getShipmentsList().stream().map(ShipmentDetails::getTenantId).toList());
        var tenantMap = getTenantMap(tenantIds);
        EntityTransferV3ConsolidationDetails payload = jsonHelper.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);

        // Map container guid vs List<shipmentGuid>
        Map<UUID, List<UUID>> containerVsShipmentGuid = new HashMap<>();
        List<EntityTransferV3ShipmentDetails> transferShipmentDetails = new ArrayList<>();
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
        ConsolidationDetailsV3Response consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsV3Response.class);
        return consolidationService.fetchAllMasterDataByKey(consolidationDetailsResponse);
    }

    private EntityTransferV3ShipmentDetails prepareShipmentPayload(ShipmentDetails shipmentDetails) {
        EntityTransferV3ShipmentDetails payload = jsonHelper.convertValue(shipmentDetails, EntityTransferV3ShipmentDetails.class);
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
        if(DIRECTION_EXP.equalsIgnoreCase(direction)) {
            res = DIRECTION_IMP;
        }
        else if(DIRECTION_IMP.equalsIgnoreCase(direction)) {
            res = DIRECTION_EXP;
        }
        return res;
    }

    private Map<Integer, V1TenantResponse> getTenantMap(List<Integer> tenantId) {
        var v1Map = v1ServiceUtil.getTenantDetails(tenantId);
        return v1Map.keySet().stream().collect(
            Collectors.toMap(Function.identity(), k -> jsonHelper.convertValue(v1Map.get(k), V1TenantResponse.class))
        );
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

        var emailTemplatesRequests = getEmailTemplates(CONSOLIDATION_V3_IMPORT_EMAIL_TYPE);
        var emailTemplateModel = emailTemplatesRequests.stream().findFirst().orElse(new EmailTemplatesRequest());
        Boolean isInterBranchConsole = consolidationDetails.getInterBranchConsole();

        // No CC emails are used
        List<String> ccEmails = new ArrayList<>();
        // Current user (the sender) in CC for NTE
        UsersDto user = UserContext.getUser();
        if(user.Email!=null)
            ccEmails.add(user.Email);
        // Fetching role ids corresponding to console destination branches
        var shipDestinationBranchIds = getShipDestinationBranchIds(consolidationDetails, destinationBranches, shipmentGuidSendToBranch, isInterBranchConsole);

        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(shipDestinationBranchIds));

        for (Integer destinationBranch : destinationBranches) {
            List<String> emailList;
            emailList = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), destinationBranch);
            Boolean isReceivingBranch = consolidationDetails.getReceivingBranch()!=null && consolidationDetails.getReceivingBranch().intValue() == destinationBranch;
            var template = createConsolidationImportEmailBody(consolidationDetails, emailTemplateModel, branchIdVsTenantModelMap, isAutomaticTransfer, isReceivingBranch, isInterBranchConsole, destinationBranch);
            sendEmailNotification(template, emailList, ccEmails);
        }
    }

    private ArrayList<Integer> getShipDestinationBranchIds(ConsolidationDetails consolidationDetails, List<Integer> destinationBranches, Map<String, List<Integer>> shipmentGuidSendToBranch, Boolean isInterBranchConsole) {
        var shipDestinationBranchIds = new ArrayList<>(destinationBranches);
        if(shipmentGuidSendToBranch == null)
            shipmentGuidSendToBranch = new HashMap<>();
        for (var keySet : shipmentGuidSendToBranch.entrySet())
            shipDestinationBranchIds.addAll(keySet.getValue());
        shipDestinationBranchIds.add(TenantContext.getCurrentTenant());
        if (Boolean.TRUE.equals(isInterBranchConsole) && isNotEmpty(consolidationDetails.getShipmentsList())) {
            for (ShipmentDetails shipment : consolidationDetails.getShipmentsList()) {
                addIfNotPresent(shipDestinationBranchIds, shipment.getOriginBranch());
                addIfNotPresent(shipDestinationBranchIds, shipment.getReceivingBranch());

                if (isNotEmpty(shipment.getTriangulationPartnerList())) {
                    for (TriangulationPartner partner : shipment.getTriangulationPartnerList()) {
                        addIfNotPresent(shipDestinationBranchIds, partner.getTriangulationPartner());
                    }
                }
            }
        }
        return shipDestinationBranchIds;
    }

    private boolean isNotEmpty(Collection<?> collection) {
        return collection != null && !collection.isEmpty();
    }

    private void addIfNotPresent(List<Integer> list, Number value) {
        if (value != null) {
            int intValue = value.intValue();
            if (!list.contains(intValue)) {
                list.add(intValue);
            }
        }
    }

    public void sendShipmentEmailNotification(ShipmentDetails shipmentDetails, List<Integer> destinationBranches, Boolean isAutomaticTransfer) {
        var emailTemplatesRequests = getEmailTemplates(SHIPMENT_IMPORT_V3_EMAIL_TYPE);
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

        for(Integer tenantId: destinationBranches) {
            List<String> emailList;
            emailList = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), tenantId);
            sendEmailNotification(emailTemplateModel, emailList, ccEmails);
        }
    }

    public void createShipmentImportEmailBody(ShipmentDetails shipmentDetails, EmailTemplatesRequest template, Boolean isAutomaticTransfer) {
        UsersDto user = UserContext.getUser();

        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(List.of(TenantContext.getCurrentTenant())));
        // Subject
        String subject = (template.getSubject() == null) ?
                DEFAULT_SHIPMENT_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        subject = subject.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));

        // Body
        String body = (template.getBody() == null) ? DEFAULT_SHIPMENT_V3_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, Boolean.TRUE.equals(isAutomaticTransfer) ? "Automated Transfer" : StringUtility.convertToString(user.getDisplayName()));
        if(shipmentDetails.getHouseBill()!=null)
            body = body.replace(BL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getHouseBill()));
        else
            body = DEFAULT_SHIPMENT_V3_RECEIVED_BODY.replace("<p><strong>BL Number</strong>: {#BL_NUMBER}</p>  ", "");
        if(shipmentDetails.getMasterBill()!=null)
            body = body.replace(MBL_NUMBER_PLACEHOLDER, StringUtility.convertToString(shipmentDetails.getMasterBill()));
        else
            body = DEFAULT_SHIPMENT_V3_RECEIVED_BODY.replace("<p><strong>MBL Number</strong>: {#MBL_NUMBER}</p> ", "");
        body = body.replace(SENT_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(SHIPMENT_NUMBER_PLACEHOLDER, String.valueOf(shipmentDetails.getShipmentId()));
        body = setNamesInBody(shipmentDetails, body);
        template.setSubject(subject);
        template.setBody(body);
    }

    private String setNamesInBody(ShipmentDetails shipmentDetails, String body) {
        String clientName;
        String consignorName;
        String consigneeName;
        String fullName = "FullName";
        if(shipmentDetails.getClient()!=null && shipmentDetails.getClient().getOrgData()!=null && shipmentDetails.getClient().getOrgData().get(fullName)!=null) {
            clientName = shipmentDetails.getClient().getOrgData().get(fullName).toString();
            body = body.replace(CLIENT_NAME_PLACEHOLDER, clientName);
        }
        else
            body = DEFAULT_SHIPMENT_V3_RECEIVED_BODY.replace("<p><strong>Client:</strong>&nbsp;{#CLIENT_NAME}</p> ", "");
        if(shipmentDetails.getConsigner()!=null && shipmentDetails.getConsigner().getOrgData()!=null && shipmentDetails.getConsigner().getOrgData().get(fullName)!=null) {
            consignorName = shipmentDetails.getConsigner().getOrgData().get(fullName).toString();
            body = body.replace(CONSIGNOR_NAME_PLACEHOLDER, consignorName);
        }
        else
            body = DEFAULT_SHIPMENT_V3_RECEIVED_BODY.replace("<p><strong>Consignor:</strong>&nbsp;{#CONSIGNOR_NAME}</p> ", "");
        if(shipmentDetails.getConsignee()!=null && shipmentDetails.getConsignee().getOrgData()!=null && shipmentDetails.getConsignee().getOrgData().get(fullName)!=null) {
            consigneeName = shipmentDetails.getConsignee().getOrgData().get(fullName).toString();
            body = body.replace(CONSIGNEE_NAME_PLACEHOLDER, consigneeName);
        }
        else
            body = DEFAULT_SHIPMENT_V3_RECEIVED_BODY.replace("<p><strong>Consignee:</strong>&nbsp;{#CONSIGNEE_NAME}</p> ", "");
        return body;
    }

    public EmailTemplatesRequest createConsolidationImportEmailBody(ConsolidationDetails consolidationDetails, EmailTemplatesRequest template, Map<Integer, TenantModel> tenantMap, Boolean isAutomaticTransfer, Boolean isReceivingBranch, Boolean isInterBranchConsole, Integer destinationBranch) {
        UsersDto user = UserContext.getUser();

        Set<ShipmentDetails> shipmentDetailsList = (consolidationDetails.getShipmentsList() == null) ? new HashSet<>() : consolidationDetails.getShipmentsList();

        // Subject
        String subject = (template.getSubject() == null) ?
                DEFAULT_CONSOLIDATION_V3_RECEIVED_SUBJECT : template.getSubject();
        subject = subject.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(tenantMap.get(TenantContext.getCurrentTenant()).tenantName));
        subject = subject.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, StringUtility.convertToString(consolidationDetails.getConsolidationNumber()));
        subject = subject.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));

        // Body
        String body = (template.getBody() == null) ?
                DEFAULT_CONSOLIDATION_V3_RECEIVED_BODY : template.getBody();
        body = body.replace(SOURCE_BRANCH_PLACEHOLDER, StringUtility.convertToString(tenantMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(SENDER_USER_NAME_PLACEHOLDER, Boolean.TRUE.equals(isAutomaticTransfer) ? "Automated Transfer" : StringUtility.convertToString(user.getDisplayName()));
        body = body.replace(NUMBER_OF_SHIPMENTS_PLACEHOLDER, (consolidationDetails.getShipmentsList() == null) ? "0" : String.valueOf(consolidationDetails.getShipmentsList().size()));
        body = body.replace(MBL_NUMBER_PLACEHOLDER, TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) ? StringUtility.convertToString(consolidationDetails.getBol()) : StringUtility.convertToString(consolidationDetails.getMawb()));
        body = body.replace(TRANSFERRED_DATE_PLACEHOLDER, LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        body = body.replace(CONSOLIDATION_NUMBER_PLACEHOLDER, StringUtility.convertToString(consolidationDetails.getConsolidationNumber()));

        body = generateShipmentEmailBodyForConsole(shipmentDetailsList, body, tenantMap, isReceivingBranch, isInterBranchConsole, destinationBranch);
        return EmailTemplatesRequest.builder().body(body).subject(subject).build();

    }

    private String generateShipmentEmailBodyForConsole(Set<ShipmentDetails> shipmentDetailsList, String htmlTemplate, Map<Integer, TenantModel> tenantMap, Boolean isReceivingBranch, Boolean isInterBranchConsole, Integer destinationBranch) {
        Map<String, Object> tagDetails = new HashMap<>();
        // Step 1: Replace placeholder for shipment block
        htmlTemplate = htmlTemplate.replace("{#GROUPED_SHIPMENT_BODY}", GROUPED_SHIPMENT_BODY);

        // Step 2: Replace global tags like {#SOURCE_BRANCH}, {#MBL_NUMBER}, etc.
        String emailBody = replaceTagsValues(tagDetails, htmlTemplate);

        // Step 3: Extract the repeatable shipment block
        String repeatedBlock = extractRepeatBlock(emailBody, "{#SD_ShipmentDetails}", "{/SD_ShipmentDetails}");

        // Step 4: Generate populated blocks for each shipment
        StringBuilder shipmentDetailsBuilder = new StringBuilder();
        for (ShipmentDetails shipment : shipmentDetailsList) {
            String populatedBlock = getPopulatedBlock(shipment, repeatedBlock, tenantMap, isReceivingBranch, isInterBranchConsole, destinationBranch);

            shipmentDetailsBuilder.append(populatedBlock);
        }

        // Step 5: Replace block in final email
        return emailBody.replace("{#SD_ShipmentDetails}" + repeatedBlock + "{/SD_ShipmentDetails}",
                shipmentDetailsBuilder.toString());
    }

    private String getPopulatedBlock(ShipmentDetails shipmentDetails, String repeatedBlock, Map<Integer, TenantModel> tenantMap, Boolean isReceivingBranch, Boolean isInterBranchConsole, Integer destinationBranch) {
        String populatedBlock = repeatedBlock;

        Map<String, String> fieldMap = new LinkedHashMap<>();
        fieldMap.put("{SD_ShipmentNumber}", nullSafe(shipmentDetails.getShipmentId()));
        fieldMap.put("{SD_HAWB_HBL_Number}", nullSafe(shipmentDetails.getHouseBill()));
        fieldMap.put("{SD_ClientName}", "");
        fieldMap.put("{SD_ConsignorName}", "");
        fieldMap.put("{SD_ConsigneeName}", "");
        fieldMap.put("{SD_FromBranchName}", "");
        fieldMap.put("{SD_ToBranchName}", "");
        String clientName;
        String consignorName;
        String consigneeName;
        String fullName = "FullName";
        if(shipmentDetails.getClient()!=null && shipmentDetails.getClient().getOrgData()!=null && shipmentDetails.getClient().getOrgData().get(fullName)!=null) {
            clientName = shipmentDetails.getClient().getOrgData().get(fullName).toString();
            fieldMap.put("{SD_ClientName}", nullSafe(clientName));
        }
        if(shipmentDetails.getConsigner()!=null && shipmentDetails.getConsigner().getOrgData()!=null && shipmentDetails.getConsigner().getOrgData().get(fullName)!=null) {
            consignorName = shipmentDetails.getConsigner().getOrgData().get(fullName).toString();
            fieldMap.put("{SD_ConsignorName}", nullSafe(consignorName));
        }
        if(shipmentDetails.getConsignee()!=null && shipmentDetails.getConsignee().getOrgData()!=null && shipmentDetails.getConsignee().getOrgData().get(fullName)!=null) {
            consigneeName = shipmentDetails.getConsignee().getOrgData().get(fullName).toString();
            fieldMap.put("{SD_ConsigneeName}", nullSafe(consigneeName));
        }
        if(Boolean.TRUE.equals(isInterBranchConsole)){
            if(shipmentDetails.getOriginBranch()!=null) {
                String originBranchName = StringUtility.convertToString(tenantMap.get(shipmentDetails.getOriginBranch().intValue()).tenantName);
                fieldMap.put("{SD_FromBranchName}", nullSafe(originBranchName));
            }
            String toBranchName = "";
            if(Boolean.TRUE.equals(isReceivingBranch)){
                if(shipmentDetails.getReceivingBranch()!=null) {
                    String receivingBranchName = StringUtility.convertToString(tenantMap.get(shipmentDetails.getReceivingBranch().intValue()).tenantName);
                    toBranchName = nullSafe(receivingBranchName);
                }
            } else{
                String triangulationBranchName = StringUtility.convertToString(tenantMap.get(destinationBranch).tenantName);
                toBranchName = nullSafe(triangulationBranchName);
            }
            fieldMap.put("{SD_ToBranchName}", nullSafe(toBranchName));
        }
        populatedBlock = getPopulatedBlock(fieldMap, populatedBlock);
        return populatedBlock;
    }

    private String getPopulatedBlock(Map<String, String> fieldMap, String populatedBlock) {
        for (Map.Entry<String, String> entry : fieldMap.entrySet()) {
            String placeholder = entry.getKey();
            String value = entry.getValue();

            if (value.isEmpty()) {
                // Remove the full line (e.g., <strong>Consignor:</strong> {SD_ConsignorName}<br />)
                populatedBlock = populatedBlock.replaceAll("(?i).*<strong>[^<]*:</strong>\\s*" + Pattern.quote(placeholder) + "<br\\s*/?>\\s*", "");
            } else {
                populatedBlock = populatedBlock.replace(placeholder, value);
            }
        }
        return populatedBlock;
    }

    private String extractRepeatBlock(String body, String startTag, String endTag) {
        int start = body.indexOf(startTag) + startTag.length();
        int end = body.indexOf(endTag);
        if (start >= startTag.length() && end > start) {
            return body.substring(start, end);
        }
        return "";
    }

    private String nullSafe(String value) {
        return value != null ? value : "";
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
            List<String> importerEmailIds;
            importerEmailIds = getEmailsListByPermissionKeysAndTenantId(Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY), tenantId);
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
            String sourceBranchName = Objects.isNull(sourceTenantId) && tenantMap.containsKey(sourceTenantId) ? EMPTY_STRING : tenantMap.get(sourceTenantId).getTenantName();
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
        List<Object> field1List = new ArrayList<>(List.of(TYPE));
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
