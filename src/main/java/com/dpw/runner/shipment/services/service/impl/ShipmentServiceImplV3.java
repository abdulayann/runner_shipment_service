package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerUpdateFileEntitiesRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.AttachListShipmentMapper;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskApproveOrRejectRequest;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ConsoleShipmentData;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.*;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.projection.CustomerBookingProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.utils.v3.EventsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.formula.functions.T;
import org.jetbrains.annotations.Nullable;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.PADDING_10_PX;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.STYLE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.*;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@SuppressWarnings({"ALL", "java:S1172"})
@Service
@Slf4j
public class ShipmentServiceImplV3 implements IShipmentServiceV3 {

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;
    @Autowired
    ExecutorService executorService;
    @Autowired
    private IContainerV3Service containerV3Service;
    @Autowired
    private ContainerV3Util containerV3Util;
    @Autowired
    private CacheManager cacheManager;
    @Autowired
    private CustomKeyGenerator keyGenerator;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;

    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private INotificationDao notificationDao;
    private CommonUtils commonUtils;
    private IShipmentRepository shipmentRepository;
    private IShipmentDao shipmentDao;
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    private ShipmentMasterDataHelperV3 shipmentMasterDataHelper;
    private JsonHelper jsonHelper;
    private MasterDataUtils masterDataUtils;
    private IAuditLogService auditLogService;
    private ILogsHistoryService logsHistoryService;
    private IHblDao hblDao;
    private IDateTimeChangeLogService dateTimeChangeLogService;
    private IConsolidationDetailsDao consolidationDetailsDao;
    private IPartiesDao partiesDao;
    private IRoutingsDao routingsDao;
    private IPackingDao packingDao;
    private INotesDao notesDao;
    private DependentServiceHelper dependentServiceHelper;
    private IEventDao eventDao;
    private IEventsV3Service eventsV3Service;
    private IPackingService packingService;
    private IOrderManagementAdapter orderManagementAdapter;
    private V1ServiceUtil v1ServiceUtil;
    private IAwbDao awbDao;
    private IDocumentManagerService documentManagerService;
    private IHblService hblService;
    private IPackingV3Service packingV3Service;
    private IShipmentSync shipmentSync;
    private IConsolidationSync consolidationSync;
    private NetworkTransferV3Util networkTransferV3Util;
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    private IReferenceNumbersDao referenceNumbersDao;
    private ShipmentsV3Util shipmentsV3Util;
    private EventsV3Util eventsV3Util;
    private ShipmentValidationV3Util shipmentValidationV3Util;
    private final IDpsEventService dpsEventService;
    private final ModelMapper modelMapper;
    private final ConsolidationV3Service consolidationV3Service;
    private final MasterDataHelper masterDataHelper;
    private final IRoutingsV3Service routingsV3Service;
    private final INotificationService notificationService;
    private final IMDMServiceAdapter mdmServiceAdapter;
    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;
    @Autowired
    private KafkaProducer kafkaProducer;

    private static final Set<String> DIRECTION_EXM_CTS = new HashSet<>(Arrays.asList(DIRECTION_EXP, DIRECTION_CTS));

    public static final String TEMPLATE_NOT_FOUND_MESSAGE = "Template not found, please inform the region users manually";

    @Autowired
    public ShipmentServiceImplV3(
            IConsoleShipmentMappingDao consoleShipmentMappingDao,
            INotificationDao notificationDao,
            CommonUtils commonUtils,
            IShipmentRepository shipmentRepository,
            IShipmentDao shipmentDao,
            ShipmentMasterDataHelperV3 shipmentMasterDataHelper,
            JsonHelper jsonHelper,
            IHblDao hblDao,
            IPackingDao packingDao,
            MasterDataUtils masterDataUtils,
            IAuditLogService auditLogService,
            ILogsHistoryService logsHistoryService,
            IDateTimeChangeLogService dateTimeChangeLogService,
            IConsolidationDetailsDao consolidationDetailsDao,
            IPartiesDao partiesDao,
            IRoutingsDao routingsDao,
            INotesDao notesDao,
            IOrderManagementAdapter orderManagementAdapter,
            V1ServiceUtil v1ServiceUtil,
            DependentServiceHelper dependentServiceHelper,
            IEventDao eventDao,
            IEventsV3Service eventsV3Service,
            IAwbDao awbDao,
            IDocumentManagerService documentManagerService,
            IHblService hblService,
            IShipmentSync shipmentSync,
            IConsolidationSync consolidationSync,
            NetworkTransferV3Util networkTransferV3Util,
            ITruckDriverDetailsDao truckDriverDetailsDao,
            IReferenceNumbersDao referenceNumbersDao,
            ShipmentsV3Util shipmentsV3Util,
            EventsV3Util eventsV3Util,
            ShipmentValidationV3Util shipmentValidationV3Util,
            IShipmentsContainersMappingDao shipmentsContainersMappingDao,
            IDpsEventService dpsEventService, ModelMapper modelMapper,
            @Lazy ConsolidationV3Service consolidationV3Service,
            MasterDataHelper masterDataHelper, @Lazy IRoutingsV3Service routingsV3Service, IPackingService packingService,
            IPackingV3Service packingV3Service, INotificationService notificationService,
        IMDMServiceAdapter mdmServiceAdapter) {
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.notificationDao = notificationDao;
        this.commonUtils = commonUtils;
        this.shipmentRepository = shipmentRepository;
        this.shipmentDao = shipmentDao;
        this.shipmentMasterDataHelper = shipmentMasterDataHelper;
        this.jsonHelper = jsonHelper;
        this.hblDao = hblDao;
        this.packingDao = packingDao;
        this.masterDataUtils = masterDataUtils;
        this.auditLogService = auditLogService;
        this.logsHistoryService = logsHistoryService;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.partiesDao = partiesDao;
        this.routingsDao = routingsDao;
        this.notesDao = notesDao;
        this.orderManagementAdapter = orderManagementAdapter;
        this.v1ServiceUtil = v1ServiceUtil;
        this.dependentServiceHelper = dependentServiceHelper;
        this.eventDao = eventDao;
        this.eventsV3Service = eventsV3Service;
        this.awbDao = awbDao;
        this.documentManagerService = documentManagerService;
        this.hblService = hblService;
        this.routingsV3Service = routingsV3Service;
        this.packingService = packingService;
        this.shipmentSync = shipmentSync;
        this.consolidationSync = consolidationSync;
        this.networkTransferV3Util = networkTransferV3Util;
        this.truckDriverDetailsDao = truckDriverDetailsDao;
        this.referenceNumbersDao = referenceNumbersDao;
        this.dpsEventService = dpsEventService;
        this.modelMapper = modelMapper;
        this.shipmentsV3Util = shipmentsV3Util;
        this.eventsV3Util = eventsV3Util;
        this.shipmentValidationV3Util = shipmentValidationV3Util;
        this.shipmentsContainersMappingDao = shipmentsContainersMappingDao;
        this.consolidationV3Service = consolidationV3Service;
        this.masterDataHelper = masterDataHelper;
        this.packingV3Service = packingV3Service;
        this.notificationService = notificationService;
        this.mdmServiceAdapter = mdmServiceAdapter;
    }

    @Override
    public List<ShipmentDetailsProjection> findShipmentDetailsByAttachedContainerIds(List<Long> containerIds) {
        return shipmentDao.findShipmentDetailsByAttachedContainerIds(containerIds);
    }

    @Override
    public List<ShipmentDetails> getShipmentsFromId(List<Long> shipmentIds) {
        return shipmentDao.getShipmentNumberFromId(shipmentIds);
    }

    @Override
    public NotificationCount getPendingNotificationCount() {

        Integer tenantId = TenantContext.getCurrentTenant();
        Integer count = consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal(), tenantId);
        count += notificationDao.findAllPendingNotificationCount(SHIPMENT, tenantId);

        return NotificationCount.builder().count(count).build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        try {
            ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
            int totalPage = 0;
            long totalElements = 0;
            if (listCommonRequest == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
                throw new ValidationException("Include Columns field is mandatory");
            }
            Set<String> includeColumns = new HashSet<>(listCommonRequest.getIncludeColumns());
            CommonUtils.includeRequiredColumns(includeColumns);
            if (Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                        PageRequest.of(Math.max(0, listCommonRequest.getPageNo() - 1), listCommonRequest.getPageSize()));

                List<Long> shipmentIds = notificationDao.findEntityIdsByEntityType(SHIPMENT);

                Set<Long> uniqueShipmentIds = new HashSet<>(eligibleShipmentId.getContent());
                uniqueShipmentIds.addAll(shipmentIds);

                List<Long> combinedShipmentIds = new ArrayList<>(uniqueShipmentIds);

                andCriteria("id", combinedShipmentIds, "IN", listCommonRequest);

                totalElements = combinedShipmentIds.size();
                int pageSize = listCommonRequest.getPageSize();
                totalPage = (int) ((totalElements + pageSize - 1) / pageSize);
            }

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentConstants.TABLES_NAMES);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentRepository.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_V3_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());

            if (!Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
                totalPage = shipmentDetailsPage.getTotalPages();
                totalElements = shipmentDetailsPage.getTotalElements();
            }

            List<ShipmentListResponse> shipmentListResponses = new ArrayList<>();

            for (var curr : shipmentDetailsPage.getContent()) {
                ShipmentListResponse shipmentListResponse = (ShipmentListResponse) commonUtils.setIncludedFieldsToResponse(curr, includeColumns, new ShipmentListResponse());
                shipmentListResponses.add(shipmentListResponse);
            }
            List<IRunnerResponse> filteredList = convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData, shipmentListResponses, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()));

            return ResponseHelper.buildListSuccessResponse(
                    filteredList,
                    totalPage,
                    totalElements);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public void delete(CommonRequestModel commonRequestModel) {
        // Implement this method.
    }

    @Override
    public ShipmentRetrieveLiteResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData, String source) throws RunnerException, AuthenticationException {
        return retireveShipmentData(commonRequestModel, source);
    }

    public boolean isNotAllowedToViewShipment(List<TriangulationPartner> triangulationPartners,
                                              ShipmentDetails shipmentDetails, Long currentTenant,
                                              ConsolidationDetails consolidationDetails) {
        boolean isNotAllowed = true;
        if (consolidationDetails != null && Objects.equals(consolidationDetails.getReceivingBranch(), currentTenant))
            isNotAllowed = false;

        if (consolidationDetails != null && consolidationDetails.getTriangulationPartnerList() != null && consolidationDetails.getTriangulationPartnerList().stream().filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
            isNotAllowed = false;

        if (consolidationDetails != null && Objects.equals(consolidationDetails.getTriangulationPartner(), currentTenant))
            isNotAllowed = false;

        if (Objects.equals(shipmentDetails.getReceivingBranch(), currentTenant))
            isNotAllowed = false;

        if (triangulationPartners != null && triangulationPartners.stream().filter(Objects::nonNull)
                .anyMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
            isNotAllowed = false;

        if (Objects.equals(shipmentDetails.getTriangulationPartner(), currentTenant))
            isNotAllowed = false;

        return isNotAllowed;
    }

    private Optional<ShipmentDetails> retrieveForNte(CommonGetRequest request) throws RunnerException, AuthenticationException {
        Long id = request.getId();
        Optional<ShipmentDetails> shipmentDetails;
        if (id != null) {
            shipmentDetails = shipmentDao.findShipmentByIdWithQuery(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            shipmentDetails = shipmentDao.findShipmentByGuidWithQuery(guid);
        }
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        List<TriangulationPartner> triangulationPartners = shipmentDetails.get().getTriangulationPartnerList();
        Long currentTenant = TenantContext.getCurrentTenant().longValue();
        ConsolidationDetails consolidationDetails = null;
        if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.get().getConsolidationList())) {
            consolidationDetails = shipmentDetails.get().getConsolidationList().iterator().next();
        }
        if (isNotAllowedToViewShipment(triangulationPartners, shipmentDetails.get(), currentTenant, consolidationDetails)) {
            throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_SHIPMENT_FOR_NTE);
        }
        return shipmentDetails;
    }

    public ShipmentRetrieveLiteResponse retireveShipmentData(CommonRequestModel commonRequestModel, String source) throws RunnerException, AuthenticationException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        double start = System.currentTimeMillis();
        if (request.getId() == null && request.getGuid() == null) {
            log.error(ShipmentConstants.SHIPMENT_ID_GUID_NULL_FOR_RETRIEVE_NTE, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(ShipmentConstants.ID_GUID_NULL_ERROR);
        }
        Long id = request.getId();
        Optional<ShipmentDetails> shipmentDetails;
        if (Objects.equals(source, NETWORK_TRANSFER)) {
            shipmentDetails = retrieveForNte(request);
        } else {
            if (id != null) {
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
        }
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        double current = System.currentTimeMillis();
        log.info("Shipment details fetched successfully for Id {} with Request Id {} within: {}ms", id, LoggerHelper.getRequestIdFromMDC(), current - start);
        AtomicInteger pendingCount = new AtomicInteger(0);
        ShipmentDetails shipmentDetailsEntity = shipmentDetails.get();
        Long shipmentId = shipmentDetailsEntity.getId();
        UUID guid = shipmentDetailsEntity.getGuid();
        List<String> implications = new ArrayList<>();
        ShipmentRetrieveLiteResponse shipmentRetrieveLiteResponse = new ShipmentRetrieveLiteResponse();
        var pendingNotificationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setPendingCount(shipmentId, pendingCount)), executorService);
        var implicationListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setImplicationsResponse(guid, implications)), executorService);
        var containerTeuFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setContainerTeuCountResponse(shipmentRetrieveLiteResponse, shipmentDetailsEntity.getContainersList())), executorService);
        setColoadingStation(shipmentDetailsEntity);
        ShipmentRetrieveLiteResponse response = modelMapper.map(shipmentDetailsEntity, ShipmentRetrieveLiteResponse.class);
        log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
        CompletableFuture.allOf(pendingNotificationFuture, implicationListFuture, containerTeuFuture).join();
        if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
            response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
        response.setPendingActionCount((pendingCount.get() == 0) ? null : pendingCount.get());
        // set dps implications
        response.setImplicationList(implications);
        setConsoleInfo(shipmentId, response);
        setDgPackCountAndType(shipmentDetailsEntity, response);
        setMainCarriageFlag(shipmentDetailsEntity, response);
        response.setContainerCount(shipmentRetrieveLiteResponse.getContainerCount());
        response.setTeuCount(shipmentRetrieveLiteResponse.getTeuCount());
        return response;
    }

    private void setConsoleInfo(Long shipmentId, ShipmentRetrieveLiteResponse response) {
        try {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentId);
            if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
                Long consolidationId = consoleShipmentMappings.get(0).getConsolidationId();
                response.setConsolidationId(consolidationId);
                var console = consolidationV3Service.getConsolidationDetails(consolidationId, null);
                response.setConsolBookingNumber(console.get().getBookingNumber());
                response.setIsInterBranchConsoleAttached(Boolean.TRUE.equals(console.get().getInterBranchConsole()) && !Objects.equals(TenantContext.getCurrentTenant(), console.get().getTenantId()));
            }
        } catch (Exception e) {
            log.error("{} | Error in setConsoleInfo: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage(), e);
        }
    }

    private void setMainCarriageFlag(ShipmentDetails shipmentDetailsEntity, ShipmentRetrieveLiteResponse response) {
        List<Routings> routingsList = shipmentDetailsEntity.getRoutingsList();
        if (!CollectionUtils.isEmpty(routingsList)) {
            boolean isMainCarriagePresent = routingsList.stream()
                    .anyMatch(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE);
            response.setIsMainCarriageAvailable(isMainCarriagePresent);
        }
    }

    private static void setDgPackCountAndType(ShipmentDetails shipmentDetailsEntity, ShipmentRetrieveLiteResponse response) {
        List<Packing> packingList = shipmentDetailsEntity.getPackingList();
        if (!CollectionUtils.isEmpty(packingList)) {
            if (Constants.TRANSPORT_MODE_AIR.equals(response.getTransportMode())) {
                boolean isEmptyWeightPackAvailable = packingList.stream()
                        .anyMatch(packing -> packing.getWeight() == null);
                response.setIsEmptyWeightPackAvailable(isEmptyWeightPackAvailable);
            }
            response.setIsPacksAvailable(Boolean.TRUE);
        }
    }

    protected void setContainerTeuCountResponse(ShipmentRetrieveLiteResponse shipmentRetrieveLiteResponse, Set<Containers> containersList) {
        if (!CollectionUtils.isEmpty(containersList)) {
            setCounterCountAndTeuCount(shipmentRetrieveLiteResponse, containersList);
        }
    }

    private void setCounterCountAndTeuCount(ShipmentRetrieveLiteResponse response, Set<Containers> containersList) {
        long shipmentCont = containersList.stream()
                .mapToLong(Containers::getContainerCount)
                .sum();
        Map<String, Object> cacheMap = new HashMap<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();
        Double shipmentTeu = 0.0;
        processCacheAndContainerResponseList(containersList.stream().toList(), containerTypes, fieldNameKeyMap, cacheMap);
        for (Containers containers : containersList) {
            if (containers.getContainerCount() != null) {
                Object cache = getEntityTransferObjectCache(containers, cacheMap);
                EntityTransferContainerType object = (EntityTransferContainerType) cache;
                if (object != null && object.getTeu() != null) {
                    shipmentTeu = shipmentTeu + (containers.getContainerCount() * object.getTeu());
                }
            }
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        response.setTeuCount(new BigDecimal(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse)));
        response.setContainerCount(Long.valueOf(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse)));
    }

    protected Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
        Object cache = null;
        if (cacheMap.isEmpty()) {
            Optional<Cache> masterDataCacheOptional = Optional.ofNullable(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA));
            Cache.ValueWrapper resp = masterDataCacheOptional.map(cache1 -> cache1.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()))).orElse(null);
            if (!Objects.isNull(resp)) cache = resp.get();
        } else {
            cache = cacheMap.get(containers.getContainerCode());
        }
        return cache;
    }

    public void processCacheAndContainerResponseList(List<Containers> containers, Set<String> containerTypes, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(containers, ContainerResponse.class);
        if (!Objects.isNull(containerResponseList))
            containerResponseList.forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));
        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
    }

    private void setImplicationsResponse(UUID guid, List<String> implications) {
        implications.addAll(dpsEventService.getImplicationsForShipment(guid.toString()));
    }

    private void setPendingCount(Long shipmentId, AtomicInteger pendingCount) {
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(Arrays.asList(shipmentId), ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(shipmentId), SHIPMENT);
        int value = map.getOrDefault(shipmentId, 0) + notificationMap.getOrDefault(shipmentId, 0);
        pendingCount.set(value);
    }

    @Override
    @Transactional
    public ShipmentDetailsV3Response create(CommonRequestModel commonRequestModel) {
        ShipmentV3Request request = (ShipmentV3Request) commonRequestModel.getData();
        this.setColoadingStation(request);
        return this.createShipment(request, false, false);
    }

    private ShipmentDetailsV3Response createShipment(ShipmentV3Request request, boolean includeGuid, boolean isFromET) {
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (!Objects.equals(request.getTransportMode(), TRANSPORT_MODE_AIR)) {
            request.setSlac(null);
        }
        ShipmentDetails shipmentDetails = includeGuid ? jsonHelper.convertValue(request, ShipmentDetails.class) : jsonHelper.convertCreateValue(request, ShipmentDetails.class);

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            boolean syncConsole = false;

            ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
            beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails, includeGuid, consoleShipmentData);
            shipmentDetails.setConsolidationList(null);
            shipmentDetails.setContainersList(null);

            shipmentDetails = getShipment(shipmentDetails);

            consoleShipmentData.setCreate(true);
            consoleShipmentData.setSyncConsole(syncConsole);
            consoleShipmentData.setFromET(isFromET);
            afterSave(shipmentDetails, null, request, shipmentSettingsDetails, consoleShipmentData);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            // Trigger Kafka event for PushToDownStreamServices
            this.triggerPushToDownStream(shipmentDetails, null, true);
        } catch (Exception e) {
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(shipmentDetails, ShipmentDetailsV3Response.class);
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails.getShipmentId() == null) {
            shipmentDetails.setShipmentId(shipmentsV3Util.generateShipmentId(shipmentDetails));
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
    }

    @Override
    @Transactional
    public ShipmentDetailsV3Response completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentV3Request shipmentRequest = (ShipmentV3Request) commonRequestModel.getData();
        this.setColoadingStation(shipmentRequest);
        return completeUpdateShipment(shipmentRequest, false);
    }

    private ShipmentDetailsV3Response completeUpdateShipment(ShipmentV3Request shipmentRequest, boolean isFromET) throws RunnerException {
        long start = System.currentTimeMillis();
        log.info("{} | starts completeUpdateShipment....", LoggerHelper.getRequestIdFromMDC());
        long mid = System.currentTimeMillis();
        if (!Objects.equals(shipmentRequest.getTransportMode(), TRANSPORT_MODE_AIR)) {
            shipmentRequest.setSlac(null);
        }
        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        log.info("{} | completeUpdateShipment db query: retrieveByIdOrGuid.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
        if (oldEntity.isEmpty()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            mid = System.currentTimeMillis();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails entity = jsonHelper.convertValue(shipmentRequest, ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper request.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            entity.setId(oldEntity.get().getId());

            mid = System.currentTimeMillis();
            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper old entity.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            if (Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) && checkIfAlreadyPushRequested(oldEntity.get())) {
                throw new ValidationException(ErrorConstants.VALIDATE_JOB_TYPE_CHANGE);
            }
            mid = System.currentTimeMillis();
            boolean syncConsole = false;
            ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
            beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, false, consoleShipmentData);
            log.info("{} | completeUpdateShipment before save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            entity.setConsolidationList(null);
            entity.setContainersList(null);

            mid = System.currentTimeMillis();
            entity = shipmentDao.update(entity, false);
            log.info("{} | completeUpdateShipment Update.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            mid = System.currentTimeMillis();
            createAuditLog(entity, jsonHelper.convertToJson(oldConvertedShipment), DBOperationType.UPDATE.name());
            log.info("{} | completeUpdateShipment auditLog.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            mid = System.currentTimeMillis();
            consoleShipmentData.setCreate(false);
            consoleShipmentData.setSyncConsole(syncConsole);
            consoleShipmentData.setFromET(isFromET);
            afterSave(entity, oldConvertedShipment, shipmentRequest, shipmentSettingsDetails, consoleShipmentData);
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            // Trigger Kafka event for PushToDownStreamServices
            this.triggerPushToDownStream(entity, oldConvertedShipment, false);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return jsonHelper.convertValue(entity, ShipmentDetailsV3Response.class);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    private boolean checkIfAlreadyPushRequested(ShipmentDetails oldEntity) {
        Integer allMappingsCount = consoleShipmentMappingDao.countAllStateMappings(oldEntity.getId());
        return allMappingsCount > 0;
    }

    public void createLogHistoryForShipment(String entityPayload, Long id, UUID guid) {
        try {
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(id)
                    .entityType(Constants.SHIPMENT).entityGuid(guid).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Shipment: " + ex.getMessage());
        }
    }

    @Override
    public void createLogHistoryForShipment(ShipmentDetails shipmentDetails) {
        try {
            String entityPayload = jsonHelper.convertToJson(shipmentDetails);
            LogHistoryRequest build = LogHistoryRequest.builder()
                    .entityId(shipmentDetails.getId())
                    .entityType(SHIPMENT)
                    .entityGuid(shipmentDetails.getGuid())
                    .entityPayload(entityPayload)
                    .tenantId(shipmentDetails.getTenantId()).build();
            logsHistoryService.createLogHistory(build);
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory : " + ex.getMessage());
        }
    }

    private void setColoadingStation(ShipmentV3Request request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if (Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentV3Request request) throws RunnerException {
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Optional<ShipmentDetails> oldEntity;

        if (request.getId() != null) {
            long id = request.getId();
            oldEntity = shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        } else if (request.getGuid() != null) {
            UUID guid = request.getGuid();
            oldEntity = shipmentDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        } else {
            throw new RunnerException("Either Id or Guid is required");
        }
        return oldEntity;
    }

    @SuppressWarnings("java:S125")
    protected void beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean isImportFile, ConsoleShipmentData consoleShipmentData) throws RunnerException, JsonMappingException {
        shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, isCreate);

        /* Future to populate unloc code in shipment child entities*/
        CompletableFuture<Void> populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, oldEntity);

        shipmentsV3Util.processVoyageAndFlightNumber(shipmentDetails);

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        Set<ConsolidationDetails> consolidationDetails = oldEntity != null ? oldEntity.getConsolidationList() : new HashSet<>();
        shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, consolidationDetails);

        if (Boolean.TRUE.equals(shipmentRequest.getIsChargableEditable())) {
            shipmentDetails.setChargable(shipmentRequest.getChargable());
        }
        validateBeforeSave(shipmentDetails, oldEntity);

        processBranchesAndPartner(shipmentDetails);

        if (Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && DIRECTION_EXM_CTS.contains(shipmentDetails.getDirection())) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentDetails.getId());
            if (!hbls.isEmpty()) {
                hblDao.delete(hbls.get(0));
            }
            shipmentDetails.getAdditionalDetails().setDraftPrinted(false);
        }
        if (checkOriginalPrintedForJobTypeChange(shipmentDetails, oldEntity)) {
            throw new ValidationException("Consolidation type cannot be changed as the original BL has been generated for this shipment.");
        }
        updateAwbForDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails);

        var tenantSettings = Optional.ofNullable(commonUtils.getCurrentTenantSettings()).orElse(V1TenantSettingsResponse.builder().build());
        // If TransportModeConfig flag is ON, this block will check for the valid transport mode
        shipmentValidationV3Util.validTransportModeForTrasnportModeConfig(shipmentDetails, oldEntity, isCreate, isImportFile, tenantSettings);

        // Ignore events payload to avoid transaction issues bypassing shipmentDetailsDao.update(...);
        // Update happens in after save from request body
        shipmentDetails.setEventsList(null);

        populateUnlocCodeFuture.join();

        // Validation for fmcTlcField
        shipmentValidationV3Util.validationForFmcTlcFields(shipmentDetails);

        if(!setIsNullOrEmpty(consolidationDetails)) {
            ShipmentWtVolResponse shipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails.iterator().next());
            jsonHelper.updateValue(consoleShipmentData, shipmentWtVolResponse);
            consoleShipmentData.setConsolidationDetails(consolidationDetails.iterator().next());
        }
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, ConsoleShipmentData consoleShipmentData) throws RunnerException {
        boolean isCreate = consoleShipmentData.isCreate();
        boolean syncConsole = consoleShipmentData.isSyncConsole();
        boolean isFromET = consoleShipmentData.isFromET();
        log.info("shipment afterSave start.... ");
        List<Events> eventsList = oldEntity != null ? oldEntity.getEventsList() : new ArrayList<>();
        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        log.info("shipment afterSave request build.... ");

        storeMblAudit(shipmentDetails, oldEntity);
        log.info("shipment afterSave mblcheck.... ");

        Long id = shipmentDetails.getId();
        Integer previousStatus = !Objects.isNull(oldEntity) ? oldEntity.getStatus() : null;

        if (!isCreate) {
            // Update AWB
            updateAwb(shipmentDetails, oldEntity);
            // Update Container From Cargo
            updateContainerFromCargo(shipmentDetails);
            // update consolidation wt vol
            if(Objects.nonNull(consoleShipmentData.getConsolidationDetails()))
                consolidationV3Service.updateConsolidationCargoSummary(consoleShipmentData.getConsolidationDetails(), jsonHelper.convertValue(consoleShipmentData, ShipmentWtVolResponse.class));
        }
        log.info("shipment afterSave isCreate .... ");
        shipmentRequest.setId(id);
        dateTimeChangeLogService.createEntryFromShipment(jsonHelper.convertValue(shipmentRequest, ShipmentRequest.class), oldEntity);
        log.info("shipment afterSave dateTimeChangeLogService .... ");
        ConsolidationDetails consolidationDetails = updateLinkedShipmentData(shipmentDetails, oldEntity);
        log.info("shipment afterSave updateLinkedShipmentData.... ");
        if (!Objects.isNull(consolidationDetails)) {
            shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
            syncConsole = true;
        }
        processEventsInAfterSave(shipmentDetails, oldEntity, isCreate, shipmentSettingsDetails, eventsList, id, previousStatus);

        if (shipmentAddressList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, isCreate), id, Constants.SHIPMENT_ADDRESSES);
            shipmentDetails.setShipmentAddresses(updatedParties);
        }
        log.info("shipment afterSave partiesDao.updateEntityFromOtherEntity..... ");
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        log.info("shipment afterSave truckDriverDetailsDao.... ");
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isCreate), id);
            shipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        log.info("shipment afterSave referenceNumbersDao.updateEntityFromShipment..... ");

        if (!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }

        // Delete the shipment pending pull/push request tasks when the shipment got cancelled
        deletePendingStateAfterCancellation(shipmentDetails, oldEntity);
        log.info("shipment afterSave consoleShipmentMappingDao.deletePendingStateByShipmentId..... ");
        processSyncV1AndAsyncFunctions(shipmentDetails, oldEntity, shipmentSettingsDetails, syncConsole, consolidationDetails);
        log.info("shipment afterSave end..... ");
    }

    protected void processEventsInAfterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, List<Events> eventsList, Long id, Integer previousStatus) throws RunnerException {
        if (eventsList != null) {
            eventsList = setEventDetails(eventsList, shipmentDetails);
            eventsList = eventsV3Util.createOrUpdateEvents(shipmentDetails, oldEntity, eventsList, isCreate);
            if (eventsList != null) {
                commonUtils.updateEventWithMasterData(eventsList);
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                shipmentDetails.setEventsList(updatedEvents);
                eventsV3Service.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
            }
        }
        log.info("shipment afterSave eventDao.updateEntityFromOtherEntity.... ");

        // create Shipment event on the bases of auto create event flag
        if (isCreate && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate()))
            eventsV3Util.autoGenerateCreateEvent(shipmentDetails);
        log.info("shipment afterSave autoGenerateCreateEvent.... ");

        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails);
        log.info("shipment afterSave generateEvents.... ");
    }

    @SuppressWarnings({"java:S1066", "java:S2583"})
    protected void autoGenerateEvents(ShipmentDetails shipmentDetails) {
        Events response = null;
        if (shipmentDetails.getStatus() != null) {
            // LATER : remove this
            if (response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    protected List<Events> setEventDetails(List<Events> eventsList, ShipmentDetails shipmentDetails) {
        if (eventsList != null && !eventsList.isEmpty()) {
            for (Events events : eventsList) {
                events.setShipmentNumber(shipmentDetails.getShipmentId());
            }
        }
        return eventsList;
    }

    public void triggerPushToDownStream(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment, Boolean isCreate) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(shipmentDetails.getConsolidationList().iterator().next().getId());
        }

        boolean isAutoSell = false;
        if (oldShipment != null) {
            CarrierDetails oldCarrier = oldShipment.getCarrierDetails();
            CarrierDetails newCarrier = shipmentDetails.getCarrierDetails();

            String oldOrigin = oldCarrier != null ? oldCarrier.getOrigin() : null;
            String oldDestination = oldCarrier != null ? oldCarrier.getDestination() : null;
            String newOrigin = newCarrier != null ? newCarrier.getOrigin() : null;
            String newDestination = newCarrier != null ? newCarrier.getDestination() : null;

            if (!Objects.equals(oldOrigin, newOrigin) || !Objects.equals(oldDestination, newDestination)) {
                isAutoSell = true;
            }

        }
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(shipmentDetails.getId())
                .parentEntityName(SHIPMENT)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .isCreate(isCreate)
                        .isAutoSellRequired(isAutoSell)
                        .build())
                .build();
        if (!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
            PushToDownstreamEventDto.Triggers triggers = PushToDownstreamEventDto.Triggers.builder()
                    .entityId(consoleShipmentMappings.get(0).getConsolidationId())
                    .entityName(Constants.CONSOLIDATION)
                    .build();
            List<PushToDownstreamEventDto.Triggers> triggersList = consoleShipmentMappings.stream()
                    .filter(x -> !Objects.equals(x.getShipmentId(), shipmentDetails.getId()))
                    .map(x -> PushToDownstreamEventDto.Triggers.builder()
                            .entityId(x.getShipmentId())
                            .entityName(SHIPMENT)
                            .build())
                    .collect(Collectors.toList());
            triggersList.add(triggers);
            pushToDownstreamEventDto.setTriggers(triggersList);
        }
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, shipmentDetails.getId().toString());
    }

    private void processSyncV1AndAsyncFunctions(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, ConsolidationDetails consolidationDetails) {
        log.info("shipment afterSave syncShipment..... ");
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity)), executorService);
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails, oldEntity, false)), executorService);
    }

    protected void deletePendingStateAfterCancellation(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) && Objects.nonNull(oldEntity)
                && !Objects.equals(oldEntity.getStatus(), shipmentDetails.getStatus()) && Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            log.info("Request: {} | Deleting console_shipment_mapping due to shipment cancelled for shipment: {}", LoggerHelper.getRequestIdFromMDC(), shipmentDetails.getShipmentId());
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
        }
    }

    private void storeMblAudit(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (StringUtils.isNotBlank(shipmentDetails.getMasterBill())) {
            List<ConsolidationDetailsProjection> consolidations = consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill());

            consolidations.forEach(consolidation -> {
                try {
                    if (ObjectUtils.isEmpty(oldEntity) || ObjectUtils.notEqual(oldEntity.getMasterBill(), shipmentDetails.getMasterBill())) {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(MblDuplicatedLog.builder()
                                                .tenantId(consolidation.getTenantId())
                                                .consolidationNo(consolidation.getConsolidationNumber())
                                                .mblNumber(shipmentDetails.getMasterBill())
                                                .shipmentId(shipmentDetails.getShipmentId()).build())
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentDetails.getId())
                                        .entityType(MblDuplicatedLog.class.getSimpleName())
                                        .operation(DBOperationType.LOG.name()).build()
                        );
                    }
                } catch (Exception e) {
                    log.error("Unable to store mbl check audit for shipment id: " + shipmentDetails.getId());
                }
            });
        }
    }

    private List<Long> getShipmentIdsExceptCurrentShipment(Long consolidationId, ShipmentDetails shipment) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappings.stream().filter(c -> !Objects.equals(c.getShipmentId(), shipment.getId()))
                .map(ConsoleShipmentMapping::getShipmentId).toList();
    }

    protected ConsolidationDetails changeConsolidationDGValues(AtomicBoolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Set<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if (!setIsNullOrEmpty(consolidationList)) {
            return changeConsolidationDGValues(makeConsoleDG.get(), makeConsoleNonDG, consolidationList.iterator().next().getId(), shipment, null);
        }
        return null;
    }

    public ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Long consolidationId, ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if (makeConsoleDG) {
            consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
            return saveConsolidationDGValue(true, consolidationDetails);
        }
        if (makeConsoleNonDG.get()) {
            List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationId, shipment);
            makeConsoleNonDG.set(checkIfAllShipmentsAreNonDG(shipmentIdList));
            if (makeConsoleNonDG.get()) {
                consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
                return saveConsolidationDGValue(false, consolidationDetails);
            }
        }
        return null;
    }

    public ConsolidationDetails getConsolidationDetails(Long consolidationId, ConsolidationDetails consolidationDetails) {
        if (!Objects.isNull(consolidationDetails))
            return consolidationDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(consolidationId);
        if (optionalConsolidationDetails.isPresent())
            return optionalConsolidationDetails.get();
        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    public boolean checkIfAllShipmentsAreNonDG(List<Long> shipmentIdList) {
        if (!shipmentIdList.isEmpty()) {
            List<ShipmentDetails> shipmentDetails = shipmentDao.findByShipmentIdInAndContainsHazardous(shipmentIdList, true);
            if (!CollectionUtils.isEmpty(shipmentDetails))
                return false;
        }
        return true;
    }

    public ConsolidationDetails saveConsolidationDGValue(boolean dgFlag, ConsolidationDetails consolidationDetails) {
        if ((!Boolean.TRUE.equals(consolidationDetails.getHazardous()) && dgFlag)
                || (!dgFlag && Boolean.TRUE.equals(consolidationDetails.getHazardous()))) {
            consolidationDetails.setHazardous(dgFlag);
            consolidationDetails = consolidationDetailsDao.updateV3(consolidationDetails, true);
            return consolidationDetails;
        }
        return null;
    }

    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity) throws RunnerException {
        Set<ConsolidationDetails> consolidationList = oldEntity != null ? oldEntity.getConsolidationList() : new HashSet<>();
        ConsolidationDetails consolidationDetails = null;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (!setIsNullOrEmpty(consolidationList)) ? consolidationList.iterator().next() : null;
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId());
            if (optionalConsolidationDetails.isPresent())
                consolidationDetails = optionalConsolidationDetails.get();
            if (consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        AtomicBoolean makeConsoleDG = new AtomicBoolean(checkForDGShipmentAndAirDgFlag(shipment) || checkForOceanDGShipment(shipment));
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if (linkedConsol != null && isDiffPresentInOldNewShipment(shipment, oldEntity)) {
            consolidationDetails = processLinkedConsolidationDetails(shipment, oldEntity, consolidationList, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);
            consolidationDetails = consolidationDetailsDao.updateV3(consolidationDetails, makeConsoleNonDG.get() || makeConsoleDG.get());
            return consolidationDetails;
        } else // only execute when above logic execution not required (i.e. saving all shipments not required)
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList, shipment);
    }

    protected boolean isDiffPresentInOldNewShipment(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        return oldEntity == null || !Objects.equals(shipment.getMasterBill(), oldEntity.getMasterBill()) ||
                !Objects.equals(shipment.getDirection(), oldEntity.getDirection()) ||
                (shipment.getAdditionalDetails() != null && oldEntity.getAdditionalDetails() != null &&
                        (!Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), oldEntity.getAdditionalDetails().getExportBroker()) ||
                                !CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), oldEntity.getAdditionalDetails().getImportBroker()))) ||
                (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                        (!Objects.equals(shipment.getCarrierDetails().getVoyage(), oldEntity.getCarrierDetails().getVoyage()) ||
                                !Objects.equals(shipment.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()) ||
                                !Objects.equals(shipment.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
                                !Objects.equals(shipment.getCarrierDetails().getAircraftType(), oldEntity.getCarrierDetails().getAircraftType())
                        ));
    }

    protected ConsolidationDetails processLinkedConsolidationDetails(ShipmentDetails shipment, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationList, AtomicBoolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        ConsolidationDetails consolidationDetails = null;
        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(consolidationList.iterator().next().getId());
        if (optionalConsolidationDetails.isEmpty())
            return consolidationDetails;
        consolidationDetails = optionalConsolidationDetails.get();
        consolidationDetails.setBol(shipment.getMasterBill());
        if (consolidationDetails.getCarrierDetails() == null)
            consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
        consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
        consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
        consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
        consolidationDetails.setShipmentType(shipment.getDirection());

        if (makeConsoleDG.get())
            consolidationDetails.setHazardous(true);
        setSendindAndReceivingAgentForNonInterConsole(shipment, consolidationDetails);
        Boolean interBranchConsole = consolidationDetails.getInterBranchConsole();
        List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationList.iterator().next().getId(), shipment);
        if (!shipmentIdList.isEmpty()) {
            processShipmentIdList(shipment, shipmentIdList, interBranchConsole, makeConsoleNonDG, makeConsoleSciT1);
        }
        if (makeConsoleNonDG.get())
            consolidationDetails.setHazardous(false);
        if (makeConsoleSciT1.get() && checkConsoleSciUpdateT1(shipment, oldEntity))
            consolidationDetails.setSci(AwbConstants.T1);
        else if (Objects.equals(consolidationDetails.getSci(), AwbConstants.T1) && !makeConsoleSciT1.get() && oldEntity != null && !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
            consolidationDetails.setSci(null);
        return consolidationDetails;
    }

    protected boolean checkConsoleSciUpdateT1(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        if (shipment.getAdditionalDetails() == null) return false;
        if (Strings.isNullOrEmpty(shipment.getAdditionalDetails().getSci())) return false;
        if (!Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1)) return false;
        return oldEntity == null || !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci());
    }

    private void processShipmentIdList(ShipmentDetails shipment, List<Long> shipmentIdList, Boolean interBranchConsole, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(shipmentIdList.stream().collect(
                Collectors.toSet()));
        shipments = shipments.stream()
                .map(i -> {
                    i.setMasterBill(shipment.getMasterBill());
                    i.setDirection(shipment.getDirection());
                    if (shipment.getCarrierDetails() != null) {
                        i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                        i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                        i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                        i.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
                    }
                    if (!Boolean.TRUE.equals(interBranchConsole)) {
                        if (shipment.getAdditionalDetails() != null && isExportOrImportBrokerPresent(shipment)) {
                            addAdditionalDetailsForShipment(shipment, i);
                        } else if (shipment.getAdditionalDetails() == null && i.getAdditionalDetails() != null) {
                            i.getAdditionalDetails().setExportBroker(null);
                            i.getAdditionalDetails().setImportBroker(null);
                        }
                    }
                    if (makeConsoleNonDG.get() && Boolean.TRUE.equals(i.getContainsHazardous()))
                        makeConsoleNonDG.set(false);
                    if (Objects.equals(i.getAdditionalDetails().getSci(), AwbConstants.T1)) {
                        makeConsoleSciT1.set(true);
                    }
                    return i;
                }).toList();
        shipmentDao.saveAll(shipments);
    }

    @Override
    public List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException {
        return shipmentDao.saveAll(shipments);
    }

    protected boolean isExportOrImportBrokerPresent(ShipmentDetails shipment) {
        return CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getExportBroker()) || CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getImportBroker());
    }

    protected void addAdditionalDetailsForShipment(ShipmentDetails shipment, ShipmentDetails i) {
        if (i.getAdditionalDetails() == null) {
            i.setAdditionalDetails(new AdditionalDetails());
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), i.getAdditionalDetails().getExportBroker())) {
            i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
        }
        if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), i.getAdditionalDetails().getImportBroker())) {
            i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
        }
    }

    private void setSendindAndReceivingAgentForNonInterConsole(ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if (!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (shipment.getAdditionalDetails() != null) {
                if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), consolidationDetails.getSendingAgent())) {
                    consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
                }
                if (!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), consolidationDetails.getReceivingAgent())) {
                    consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
                }
            } else {
                consolidationDetails.setSendingAgent(null);
                consolidationDetails.setReceivingAgent(null);
            }
        }
    }

    private void updateAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) throws RunnerException {
        if (checkForAwbUpdate(shipmentDetails, oldEntity)) {
            awbDao.updatedAwbInformationEvent(shipmentDetails, oldEntity);
        }
    }

    private boolean checkForAwbUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if (!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
            return true;
        if (!Objects.equals(shipmentDetails.getSecurityStatus(), oldEntity.getSecurityStatus())) return true;
        return !Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldEntity.getAdditionalDetails().getEfreightStatus());
    }

    protected boolean checkOriginalPrintedForJobTypeChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (oldEntity == null)
            return false;
        if (!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA))
            return false;
        if (!DIRECTION_EXM_CTS.contains(shipmentDetails.getDirection()))
            return false;
        if (!Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal()))
            return false;
        return !Objects.equals(shipmentDetails.getJobType(), oldEntity.getJobType());
    }

    protected void updateAwbForDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        if (checkDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipmentDetails.getId());
            if (!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }
    }

    protected boolean checkDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails) {
        if (oldEntity == null)
            return false;
        if (!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()))
            return false;
        if (!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if (!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT))
            return false;
        return !Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(shipmentDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(shipmentDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    protected void processBranchesAndPartner(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getReceivingBranch() != null && shipmentDetails.getReceivingBranch() == 0) {
            shipmentDetails.setReceivingBranch(null);
        }
        if (ObjectUtils.isNotEmpty(shipmentDetails.getTriangulationPartnerList())
                && shipmentDetails.getTriangulationPartnerList().size() == 1) {
            TriangulationPartner triangulationPartner = shipmentDetails.getTriangulationPartnerList().get(0);
            if (triangulationPartner != null
                    && Long.valueOf(0).equals(triangulationPartner.getTriangulationPartner())) {
                shipmentDetails.setTriangulationPartnerList(null);
            }
        } else if (shipmentDetails.getTriangulationPartnerList() == null
                && shipmentDetails.getTriangulationPartner() != null
                && shipmentDetails.getTriangulationPartner() == 0) {
            shipmentDetails.setTriangulationPartner(null);
        }
        if (shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);
    }

    protected void validateBeforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null && shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
            throw new ValidationException("Consignor & Consignee parties can't be selected as same.");

        if (!isStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) {
            if (!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            } else if (!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }
        // Validate all shipment fields before creation or updation
        shipmentValidationV3Util.validateShipmentCreateOrUpdate(shipmentDetails, oldEntity);
    }

    private CompletableFuture<Void> getPopulateUnlocCodeFuture(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        CarrierDetails finalOldCarrierDetails = Optional.ofNullable(oldEntity).map(ShipmentDetails::getCarrierDetails).orElse(null);

        /* Set to extract the unlocations from entities whose unloc code needs to be saved */
        Set<String> unlocationsSet = Collections.synchronizedSet(new HashSet<>());
        Map<String, EntityTransferUnLocations> unLocationsMap = new ConcurrentHashMap<>();

        CompletableFuture.allOf(
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(shipmentDetails.getCarrierDetails(), finalOldCarrierDetails, unlocationsSet), executorService)
        ).join();

        return CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(unlocationsSet, unLocationsMap)), executorService)
                .thenCompose(v -> CompletableFuture.allOf(
                        CompletableFuture.runAsync(() -> commonUtils.updateCarrierUnLocData(shipmentDetails.getCarrierDetails(), unLocationsMap), executorService)
                ));
    }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if (!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if (checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkForOceanDGShipment(ShipmentDetails shipmentDetails) {
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if (checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return !Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation) {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(entity)
                            .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class) : null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operation).build()
            );
        } catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst, boolean getMasterData,
                                                             List<ShipmentListResponse> shipmentListResponses,
                                                             Set<String> includeColumns) {
        V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();
        List<IRunnerResponse> responseList = new ArrayList<>();
        Map<Long, ShipmentDetails> shipmentDetailsMap = lst.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));

        // Handle pending notifications
        handlePendingNotifications(lst, shipmentListResponses);

        // Get booking mappings
        Map<Long, Long> shipmentIdToBookingIdMap = getShipmentToBookingIdMap(shipmentListResponses);

        // Process each response
        shipmentListResponses.forEach(response -> {
            var ship = shipmentDetailsMap.get(response.getId());
            processShipmentResponse(response, ship, includeColumns, shipmentIdToBookingIdMap, tenantSettings);
            responseList.add(response);
        });

        shipmentMasterDataHelper.getMasterDataForList(lst, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    private void handlePendingNotifications(List<ShipmentDetails> lst, List<ShipmentListResponse> shipmentListResponses) {

        List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);

        shipmentListResponses.forEach(response -> {
            int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
            response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
        });
    }

    private Map<Long, Long> getShipmentToBookingIdMap(List<ShipmentListResponse> shipmentListResponses) {
        List<Long> shipmentIds = shipmentListResponses.stream().map(ShipmentListResponse::getId).distinct().toList();
        List<CustomerBookingProjection> bookingProjections = shipmentDao.findCustomerBookingProByShipmentIdIn(shipmentIds);
        return bookingProjections.stream()
                .collect(Collectors.toMap(CustomerBookingProjection::getShipmentId, CustomerBookingProjection::getId));
    }

    private void processShipmentResponse(ShipmentListResponse response, ShipmentDetails ship, Set<String> includeColumns,
                                         Map<Long, Long> shipmentIdToBookingIdMap, V1TenantSettingsResponse tenantSettings) {
        // Set booking ID
        Optional.ofNullable(shipmentIdToBookingIdMap.get(response.getId()))
                .ifPresent(response::setBookingId);

        // Handle conditional fields
        handleConditionalFields(response, ship, includeColumns);

        // Format weight and volume fields
        formatWeightAndVolumeFields(response, tenantSettings);
    }

    private void handleConditionalFields(ShipmentListResponse response, ShipmentDetails ship, Set<String> includeColumns) {
        if (includeColumns.contains(SHIPPER_REFERENCE)) {
            setShipperReferenceNumber(response, ship);
        }

        if (includeColumns.contains(SHIPMENT_STATUS_FIELDS) && ship.getStatus() != null && ship.getStatus() < ShipmentStatus.values().length) {
            response.setShipmentStatus(ShipmentStatus.values()[ship.getStatus()].toString());
        }

        if (includeColumns.contains(ORDERS_COUNT) && ObjectUtils.isNotEmpty(ship.getShipmentOrders())) {
            response.setOrdersCount(ship.getShipmentOrders().size());
        }
    }

    private void formatWeightAndVolumeFields(ShipmentListResponse response, V1TenantSettingsResponse tenantSettings) {
        if (ObjectUtils.isNotEmpty(response.getWeight())) {
            response.setWeightFormatted(IReport.convertToWeightNumberFormat(response.getWeight(), tenantSettings));
        }

        if (ObjectUtils.isNotEmpty(response.getVolume())) {
            response.setVolumeFormatted(IReport.convertToVolumeNumberFormat(response.getVolume(), tenantSettings));
        }

        if (ObjectUtils.isNotEmpty(response.getVolumetricWeight())) {
            response.setVolumetricWeightFormatted(IReport.convertToWeightNumberFormat(response.getVolumetricWeight(), tenantSettings));
        }

        if (ObjectUtils.isNotEmpty(response.getChargable())) {
            response.setChargableFormatted(IReport.convertToWeightNumberFormat(response.getChargable(), tenantSettings));
        }
    }

    private void setShipperReferenceNumber(ShipmentListResponse response, ShipmentDetails ship) {
        if (ship.getReferenceNumbersList() != null && !ship.getReferenceNumbersList().isEmpty()) {
            Optional<String> srnReferenceNumber = ship.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equalsIgnoreCase(SRN))
                    .findFirst()
                    .map(a -> a.getReferenceNumber());

            if (srnReferenceNumber.isPresent() && response.getPickupDetails() != null) {
                response.getPickupDetails().setShipperRef(srnReferenceNumber.get());
            }
        }
    }

    @Override
    public ShipmentPendingNotificationResponse getPendingNotificationData(CommonGetRequest request) {

        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(request.getId());
        if (!optionalShipmentDetails.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_FOR_GUID_MISSING_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        if (request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty()) {
            request.setIncludeColumns(List.of("transportMode", "containsHazardous", "guid", "receivingBranch", "triangulationPartnerList", "tenantId"));
        }
        Set<String> includeColumns = new HashSet<>(request.getIncludeColumns());
        CommonUtils.includeRequiredColumns(includeColumns);

        ShipmentPendingNotificationResponse shipmentResponse = (ShipmentPendingNotificationResponse) commonUtils.setIncludedFieldsToResponse(optionalShipmentDetails.get(), includeColumns, new ShipmentPendingNotificationResponse());
        shipmentMasterDataHelper.getMasterDataForEntity(shipmentResponse);
        return shipmentResponse;
    }

    @Override
    public Optional<ShipmentDetails> findById(Long shipmentId) {
        return shipmentDao.findById(shipmentId);
    }

    @Override
    public void updateCargoDetailsInShipment(ShipmentDetails shipmentDetails, CargoDetailsResponse cargoDetailsResponse) {
        Long shipmentId = shipmentDetails.getId();
        shipmentDao.updateCargoDetailsInShipment(shipmentId,
                cargoDetailsResponse.getNoOfPacks(),
                cargoDetailsResponse.getPacksUnit(),
                cargoDetailsResponse.getVolume(),
                cargoDetailsResponse.getVolumeUnit(),
                cargoDetailsResponse.getWeight(),
                cargoDetailsResponse.getWeightUnit(),
                cargoDetailsResponse.getVolumetricWeight(),
                cargoDetailsResponse.getVolumetricWeightUnit(),
                cargoDetailsResponse.getChargable(),
                cargoDetailsResponse.getChargeableUnit());
        shipmentDao.updateDgPacksDetailsInShipment(cargoDetailsResponse.getDgPacks(), cargoDetailsResponse.getDgPacksUnit(), shipmentId);
        shipmentDetails.setNoOfPacks(cargoDetailsResponse.getNoOfPacks());
        shipmentDetails.setPacksUnit(cargoDetailsResponse.getPacksUnit());
        shipmentDetails.setVolume(cargoDetailsResponse.getVolume());
        shipmentDetails.setVolumeUnit(cargoDetailsResponse.getVolumeUnit());
        shipmentDetails.setWeight(cargoDetailsResponse.getWeight());
        shipmentDetails.setWeightUnit(cargoDetailsResponse.getWeightUnit());
        shipmentDetails.setVolumetricWeight(cargoDetailsResponse.getVolumetricWeight());
        shipmentDetails.setVolumetricWeightUnit(cargoDetailsResponse.getVolumetricWeightUnit());
        shipmentDetails.setChargable(cargoDetailsResponse.getChargable());
        shipmentDetails.setChargeableUnit(cargoDetailsResponse.getChargeableUnit());
        shipmentDetails.setDgPacksCount(cargoDetailsResponse.getDgPacks());
        shipmentDetails.setDgPacksUnit(cargoDetailsResponse.getDgPacksUnit());
    }

    @Override
    public void updateShipmentDetailsFromPacks(Long shipmentId, DateBehaviorType dateType, LocalDateTime shipmentGateInDate, ShipmentPackStatus shipmentPackStatus) {
        shipmentDao.updateShipmentDetailsFromPacks(shipmentId, dateType, shipmentGateInDate, shipmentPackStatus);
    }

    @Override
    public String attachConsolidation(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException {
        return consolidationV3Service.attachShipments(shipmentAttachDetachRequest);
    }

    @Override
    public Map<String, Object> getAllMasterData(Long shipmentId, String xSource) {
        Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findById(shipmentId);
        if (!shipmentDetailsOptional.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentId);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
        long start = System.currentTimeMillis();
        List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ShipmentDetails.class, null), createFieldClassDto(AdditionalDetails.class, "additionalDetails.")));
        includeColumns.addAll(FieldUtils.getTenantIdAnnotationFields(List.of(createFieldClassDto(ShipmentDetails.class, null), createFieldClassDto(AdditionalDetails.class, "additionalDetails."))));
        includeColumns.addAll(ShipmentConstants.LIST_INCLUDE_COLUMNS_V3);
        ShipmentDetailsResponse shipmentDetailsResponse = (ShipmentDetailsResponse) commonUtils.setIncludedFieldsToResponse(shipmentDetails, includeColumns.stream().collect(Collectors.toSet()), new ShipmentDetailsResponse());
        log.info("Total time taken in setting shipment details response {}", (System.currentTimeMillis() - start));
        return fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
    }

    @Override
    public void updateShipmentFieldsAfterDetach(List<ShipmentDetails> detachedShipments) {
        for (ShipmentDetails detachedShipment : detachedShipments) {
            if (detachedShipment.getCarrierDetails() != null) {
                detachedShipment.getCarrierDetails().setEta(null);
                detachedShipment.getCarrierDetails().setEtd(null);
                detachedShipment.getCarrierDetails().setAta(null);
                detachedShipment.getCarrierDetails().setAtd(null);
                detachedShipment.getCarrierDetails().setShippingLine(null);
            }
            detachedShipment.setMasterBill(null);
            detachedShipment.setBookingNumber(null);
        }
    }

    @Override
    @Transactional
    public ShipmentSailingScheduleResponse updateSailingScheduleDataToShipment(ShipmentSailingScheduleRequest request) throws RunnerException {
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        Optional<RoutingsRequest> firstRouting = request.getRoutings().stream().findFirst();
        if (firstRouting.isEmpty()) {
            return new ShipmentSailingScheduleResponse();
        }
        Long shipmentId = firstRouting.get().getShipmentId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentId);
        if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
            throw new ValidationException("Consol is attached to shipment, sailing schedule update is not allowed");
        }
        List<Routings> routingsList = routingsV3Service.getRoutingsByShipmentId(shipmentId);
        if (!CollectionUtils.isEmpty(routingsList)) {
            routingsList.removeIf(routing -> routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE);
        }
        List<RoutingsRequest> finalShipmentRouteList = new ArrayList<>();
        finalShipmentRouteList.addAll(request.getRoutings());
        finalShipmentRouteList.addAll(jsonHelper.convertValueToList(routingsList, RoutingsRequest.class));

        bulkUpdateRoutingsRequest.setRoutings(finalShipmentRouteList);
        bulkUpdateRoutingsRequest.setEntityId(shipmentId);
        routingsV3Service.updateBulk(bulkUpdateRoutingsRequest, SHIPMENT);
        //update shipment fields
        Optional<ShipmentDetails> shipmentDetailsEntity = shipmentDao.findById(shipmentId);
        if (shipmentDetailsEntity.isEmpty())
            return new ShipmentSailingScheduleResponse();
        ShipmentDetails shipmentDetails = shipmentDetailsEntity.get();
        updateCutoffDetailsToShipment(request, shipmentDetails);
        String carrierNameFromMasterData = masterDataUtils.getCarrierNameFromMasterDataUsingScacCodeFromIntraa(request.getScacCode());
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();
        carrierDetails.setShippingLine(carrierNameFromMasterData);
        carrierDetailsDao.update(carrierDetails);
        return new ShipmentSailingScheduleResponse();
    }

    @Override
    public void updateCutoffDetailsToShipment(ShipmentSailingScheduleRequest request, ShipmentDetails shipmentDetails) {
        if (TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode())) {
            shipmentDao.updateSailingScheduleRelatedInfo(request, shipmentDetails.getId());

        } else if (TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode())) {
            shipmentDao.updateSailingScheduleRelatedInfoForAir(request, shipmentDetails.getId());
        }
    }

    public Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllMasterDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllUnlocationDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCarrierDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCurrencyDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllWarehouseDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllActivityDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllSalesAgentInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllVesselDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var organizationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllOrganizationDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture, vesselsFuture, organizationFuture).join();

        return masterDataResponse;
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    @Override
    public ShipmentPacksAssignContainerTrayDto getShipmentAndPacksForConsolidationAssignContainerTray(Long containerId, Long consolidationId) {
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();

        ListCommonRequest listCommonRequest = constructListCommonRequest(CONSOLIDATION_ID, consolidationId, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentService.tableNames);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetails.getContent().stream().collect(Collectors.toMap(e -> e.getId(), Function.identity()));
        Set<Long> containerIds = new HashSet<>();

        response.setShipmentsList(jsonHelper.convertValueToList(shipmentDetails.getContent(), ShipmentPacksAssignContainerTrayDto.Shipments.class));
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            shipments.setPacksList(jsonHelper.convertValueToList(
                    shipmentDetailsMap.containsKey(shipments.getId()) ? shipmentDetailsMap.get(shipments.getId()).getPackingList().stream().toList() : new ArrayList<>(),
                    ShipmentPacksAssignContainerTrayDto.Shipments.Packages.class));
            shipments.getPacksList().stream()
                    .map(ShipmentPacksAssignContainerTrayDto.Shipments.Packages::getContainerId)
                    .filter(Objects::nonNull)
                    .forEach(containerIds::add);
        }
        setContainerNumberAndMasterData(response, containerIds);

        List<ShipmentsContainersMapping> shipmentsContainersMappingsList = shipmentsContainersMappingDao.findByContainerId(containerId);
        List<Long> assignedShipmentsList = shipmentsContainersMappingsList.stream().map(e -> e.getShipmentId()).toList();
        response.setIsFCLShipmentAssigned(false);
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            if (assignedShipmentsList.contains(shipments.getId())) {
                shipments.setSelectedContainerAssigned(true);
                if (CARGO_TYPE_FCL.equals(shipments.getShipmentType()) || CARGO_TYPE_FTL.equalsIgnoreCase(shipments.getShipmentType())) {
                    response.setIsFCLShipmentAssigned(true);
                    response.setAssignedFCLShipment(shipments.getId());
                }
            } else {
                shipments.setSelectedContainerAssigned(false);
            }
        }
        return response;
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addCommodityMasterData(ShipmentPacksAssignContainerTrayDto response) {
        try {
            Set<String> commodityTypes = new HashSet<>();
            for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
                for (ShipmentPacksAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                    if (!isStringNullOrEmpty(packages.getCommodity())) {
                        commodityTypes.add(packages.getCommodity());
                    }
                }
            }

            Map<String, EntityTransferCommodityType> commodityMasterData = new HashMap<>();
            if (!setIsNullOrEmpty(commodityTypes)) {
                commodityMasterData = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
                setCommodityMasterData(commodityMasterData, response);
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(commodityMasterData));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addCommodityMasterData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public void setCommodityMasterData(Map<String, EntityTransferCommodityType> commodityMasterData, ShipmentPacksAssignContainerTrayDto response) {
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            for (ShipmentPacksAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                if (!isStringNullOrEmpty(packages.getCommodity()) && commodityMasterData.containsKey(packages.getCommodity())) {
                    packages.setCommodityCode(packages.getCommodity());
                    packages.setCommodity(commodityMasterData.get(packages.getCommodityCode()).getDescription());
                }
            }
        }
    }


    public void setContainerNumberAndMasterData(ShipmentPacksAssignContainerTrayDto response, Set<Long> containerIds) {
        Map<Long, ContainerInfoProjection> containerIdNumberMap = packingV3Service.getContainerIdNumberMap(containerIds);
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            if (!listIsNullOrEmpty(shipments.getPacksList())) {
                for (ShipmentPacksAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                    if (Objects.nonNull(packages.getContainerId()) && containerIdNumberMap.containsKey(packages.getContainerId())) {
                        packages.setContainerNumber(containerIdNumberMap.get(packages.getContainerId()).getContainerNumber());
                        packages.setContainerCode(containerIdNumberMap.get(packages.getContainerId()).getContainerCode());
                    }
                }
            }
        }
        addCommodityMasterData(response);
    }

    @Override
    public ShipmentPacksUnAssignContainerTrayDto getShipmentAndPacksForConsolidationUnAssignContainerTray(Long containerId) {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(containerId);
        if (listIsNullOrEmpty(shipmentsContainersMappings))
            return response;
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentsContainersMappings.stream().map(e -> e.getShipmentId()).collect(Collectors.toSet()));
        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(e -> e.getId(), Function.identity()));
        response.setShipmentsList(jsonHelper.convertValueToList(shipmentDetails, ShipmentPacksUnAssignContainerTrayDto.Shipments.class));
        Set<Long> containerIds = new HashSet<>();
        for (ShipmentPacksUnAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            shipments.setPacksList(jsonHelper.convertValueToList(
                    shipmentDetailsMap.get(shipments.getId()).getPackingList().stream().filter(e -> containerId.equals(e.getContainerId())).toList(),
                    ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.class));
            shipments.getPacksList().stream()
                    .map(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages::getContainerId)
                    .filter(Objects::nonNull)
                    .forEach(containerIds::add);
        }
        setContainerNumberAndMasterData(response, containerIds);
        return response;
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addCommodityMasterData(ShipmentPacksUnAssignContainerTrayDto response) {
        try {
            Set<String> commodityTypes = new HashSet<>();
            for (ShipmentPacksUnAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
                for (ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                    if (!isStringNullOrEmpty(packages.getCommodity())) {
                        commodityTypes.add(packages.getCommodity());
                    }
                }
            }

            Map<String, EntityTransferCommodityType> commodityMasterData = new HashMap<>();
            if (!setIsNullOrEmpty(commodityTypes)) {
                commodityMasterData = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
                setCommodityMasterData(commodityMasterData, response);
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(commodityMasterData));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addCommodityMasterData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public void setCommodityMasterData(Map<String, EntityTransferCommodityType> commodityMasterData, ShipmentPacksUnAssignContainerTrayDto response) {
        for (ShipmentPacksUnAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            for (ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                if (!isStringNullOrEmpty(packages.getCommodity()) && commodityMasterData.containsKey(packages.getCommodity())) {
                    packages.setCommodityCode(packages.getCommodity());
                    packages.setCommodity(commodityMasterData.get(packages.getCommodityCode()).getDescription());
                }
            }
        }
    }


    public void setContainerNumberAndMasterData(ShipmentPacksUnAssignContainerTrayDto response, Set<Long> containerIds) {
        Map<Long, ContainerInfoProjection> containerIdNumberMap = packingV3Service.getContainerIdNumberMap(containerIds);
        for (ShipmentPacksUnAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            if (!listIsNullOrEmpty(shipments.getPacksList())) {
                for (ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages packages : shipments.getPacksList()) {
                    if (Objects.nonNull(packages.getContainerId()) && containerIdNumberMap.containsKey(packages.getContainerId())) {
                        packages.setContainerNumber(containerIdNumberMap.get(packages.getContainerId()).getContainerNumber());
                        packages.setContainerCode(containerIdNumberMap.get(packages.getContainerId()).getContainerCode());
                    }
                }
            }
        }
        addCommodityMasterData(response);
    }

    @Override
    public Long assignFirstBookingContainerToShipmentCargo(List<Containers> expandedContainers, CustomerBookingV3Request customerBookingV3Request) throws RunnerException {
        Long containerId = null;
        for (int i = 0; i < expandedContainers.size(); i++) {
            Containers containers = expandedContainers.get(i);
            if (i == 0) {
                containerV3Service.addShipmentCargoToContainerInCreateFromBooking(containers, customerBookingV3Request);
                containerV3Util.setContainerNetWeight(containers);
                containerId = containers.getId();
            } else {
                containerV3Util.resetContainerDataForRecalculation(containers);
            }
        }
        return containerId;
    }

    @Override
    public List<ShipmentDetails> findByIdIn(List<Long> shipmentIds) {
        return shipmentDao.findByIdIn(shipmentIds);
    }

    @Override
    public ShipmentDetailsV3Response createShipmentInV3(CustomerBookingV3Request customerBookingRequest) throws RunnerException {
        Set<ConsolidationDetailsRequest> consolidationDetails = new HashSet<>();
        Set<ContainerRequest> containerList = new HashSet<>();
        Long containerAssignedToShipmentCargo = null;
        if (isConsoleCreationNeededV3(customerBookingRequest)) {
            ConsolidationDetailsV3Request consolidationDetailsV3Request = ConsolidationDetailsV3Request.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                            .destination(customerBookingRequest.getCarrierDetails().getDestination())
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .eta(customerBookingRequest.getCarrierDetails().getEta())
                            .etd(customerBookingRequest.getCarrierDetails().getEtd())
                            .ata(customerBookingRequest.getCarrierDetails().getAta())
                            .atd(customerBookingRequest.getCarrierDetails().getAtd())
                            .build()).
                    consolidationType("STD").
                    transportMode(customerBookingRequest.getTransportType()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    shipmentType(customerBookingRequest.getDirection()).
                    referenceNumber(customerBookingRequest.getBookingNumber()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                    deliveryMode(customerBookingRequest.getServiceMode()).
                    partner(customerBookingRequest.getPartner()).
                    coLoadBookingReference(customerBookingRequest.getPartnerBkgNumber()).
                    coLoadMBL(customerBookingRequest.getPartnerBLOrAWBNumber()).
                    carrierBookingRef(customerBookingRequest.getCarrierBookingNumber()).
                    build();
            // Set Department in case single department is available
            consolidationDetailsV3Request.setDepartment(commonUtils.getAutoPopulateDepartment(
                    consolidationDetailsV3Request.getTransportMode(), consolidationDetailsV3Request.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE
            ));

            //
            Pair<ConsolidationDetails, Long> createConsoleResponse = consolidationV3Service.createConsolidationForBooking(CommonRequestModel.buildRequest(consolidationDetailsV3Request), customerBookingRequest);
            ConsolidationDetails consolDetailsResponse = createConsoleResponse.getLeft();
            containerAssignedToShipmentCargo = createConsoleResponse.getRight();

            if (consolDetailsResponse != null) {
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList() != null ? new HashSet<>(consolRequest.getContainersList()) : null;
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        ShipmentV3Request shipmentRequest = getShipmentRequestFromBookingV3(customerBookingRequest, consolidationDetails);
        // Set Department in case single department is available
        shipmentRequest.setDepartment(commonUtils.getAutoPopulateDepartment(
                shipmentRequest.getTransportMode(), shipmentRequest.getDirection(), MdmConstants.SHIPMENT_MODULE
        ));
        shipmentRequest.setContainerAssignedToShipmentCargo(containerAssignedToShipmentCargo);
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = calculateShipmentWV(jsonHelper.convertValue(shipmentRequest, AutoUpdateWtVolRequest.class));
        shipmentRequest.setNoOfPacks(getIntFromString(autoUpdateWtVolResponse.getNoOfPacks()));
        shipmentRequest.setPacksUnit(autoUpdateWtVolResponse.getPacksUnit());
        shipmentRequest.setWeight(autoUpdateWtVolResponse.getWeight());
        shipmentRequest.setWeightUnit(autoUpdateWtVolResponse.getWeightUnit());
        shipmentRequest.setVolume(autoUpdateWtVolResponse.getVolume());
        shipmentRequest.setVolumeUnit(autoUpdateWtVolResponse.getVolumeUnit());
        shipmentRequest.setChargable(
                autoUpdateWtVolResponse.getChargable() != null
                        ? autoUpdateWtVolResponse.getChargable().setScale(10, RoundingMode.HALF_UP).stripTrailingZeros()
                        : null
        );
        shipmentRequest.setChargeableUnit(autoUpdateWtVolResponse.getChargeableUnit());
        shipmentRequest.setVolumetricWeight(autoUpdateWtVolResponse.getVolumetricWeight());
        shipmentRequest.setVolumetricWeightUnit(autoUpdateWtVolResponse.getVolumetricWeightUnit());
        shipmentRequest.setNetWeight(autoUpdateWtVolResponse.getNetWeight());
        shipmentRequest.setNetWeightUnit(autoUpdateWtVolResponse.getNetWeightUnit());
        shipmentRequest.setInnerPacks(autoUpdateWtVolResponse.getInnerPacks());
        shipmentRequest.setInnerPackUnit(autoUpdateWtVolResponse.getInnerPackUnit());
        shipmentRequest.setOrderManagementId(customerBookingRequest.getOrderManagementId());
        shipmentRequest.setOrderManagementNumber(customerBookingRequest.getOrderManagementNumber());

        if (customerBookingRequest.getOrderManagementId() != null) {
            ShipmentDetails shipmentDetails = null;
            shipmentDetails = orderManagementAdapter.getOrderByGuid(customerBookingRequest.getOrderManagementId());

            if (shipmentDetails != null) {
                processShipmentRequestFromDetails(shipmentRequest, shipmentDetails);
            }

        }

        shipmentRequest.setContainsHazardous(customerBookingRequest.getIsDg());
        shipmentRequest.setCustomerBookingGuid(customerBookingRequest.getGuid());
        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest), customerBookingRequest, containerList);
    }

    public ShipmentDetailsV3Response createFromBooking(CommonRequestModel commonRequestModel, CustomerBookingV3Request customerBookingV3Request, Set<ContainerRequest> containerList) {
        ShipmentV3Request request = (ShipmentV3Request) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            /*  Populate unloc code for entities */
            var populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, null);

            if (containerList != null && !containerList.isEmpty()) {
                shipmentDetails.setContainersList(new HashSet<>(jsonHelper.convertValueToList(containerList.stream().toList(), Containers.class)));
            }

            populateUnlocCodeFuture.join();

            if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
                if (!commonUtils.checkIfPartyExists(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                    shipmentDetails.getAdditionalDetails().setImportBrokerCountry(commonUtils.getCountryFromUnLocCode(shipmentDetails.getCarrierDetails().getDestinationLocCode()));
                }
                if (!commonUtils.checkIfPartyExists(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                    shipmentDetails.getAdditionalDetails().setExportBrokerCountry(commonUtils.getCountryFromUnLocCode(shipmentDetails.getCarrierDetails().getOriginLocCode()));
                }
            }
            populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails);
            shipmentDetails = getShipment(shipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if (shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails);
            generateAfterSaveEvents(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = getAndSetPackings(customerBookingV3Request, shipmentId, shipmentDetails);
            List<RoutingsRequest> routingsRequest = customerBookingV3Request.getRoutingList();

            if (ObjectUtils.isNotEmpty(routingsRequest)) {
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(shipmentDetails.getRoutingsList(), shipmentId));
            }

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (ObjectUtils.isNotEmpty(referenceNumbersRequest))
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            checkContainerAssignedForHbl(shipmentDetails, updatedPackings);

            List<NotesRequest> notesRequest = getNotesRequests(request, shipmentId);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, true, false, null);

            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        ShipmentDetailsV3Response shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsV3Response.class);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromBookingToShipment(shipmentDetailsResponse.getGuid().toString(), shipmentDetailsResponse.getCustomerBookingGuid().toString())), executorService);
        return shipmentDetailsResponse;
    }

    private void generateAfterSaveEvents(ShipmentDetails shipmentDetails) {
        if (Objects.nonNull(shipmentDetails.getAdditionalDetails().getPickupDate())) {
            createAutomatedEvents(shipmentDetails, EventConstants.CACO, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
        }
        if (Objects.nonNull(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate())) {
            createAutomatedEvents(shipmentDetails, EventConstants.CADE, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
        }
        if (Objects.nonNull(shipmentDetails.getBrokerageAtOriginDate())) {
            createAutomatedEvents(shipmentDetails, EventConstants.ECCC, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
        }
        if (Objects.nonNull(shipmentDetails.getBookingNumber())) {
            createAutomatedEvents(shipmentDetails, EventConstants.BOCO, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
        }
    }

    private List<PackingV3Request> getPackingListRequest(CustomerBookingV3Request customerBookingRequest) {
        return customerBookingRequest.getPackingList() != null ? customerBookingRequest.getPackingList().stream().map(obj -> {
            setHeightWidthUnit(obj);
            if (obj.getWeight() != null)
                obj.setWeight(obj.getWeight().multiply(new BigDecimal(obj.getPacks())));
            if (obj.getVolume() != null)
                obj.setVolume(obj.getVolume().multiply(new BigDecimal(obj.getPacks())));
            if (TRANSPORT_MODE_AIR.equalsIgnoreCase(customerBookingRequest.getTransportType())) {
                calculateWeightVolumeForPacks(obj);
            }

            return obj;
        }).collect(Collectors.toList()) : null;
    }

    private void setHeightWidthUnit(PackingV3Request obj) {
        if (!StringUtility.isEmpty(obj.getLengthUnit())) {
            obj.setWidthUnit(obj.getLengthUnit());
            obj.setHeightUnit(obj.getLengthUnit());
        }
    }

    private void calculateWeightVolumeForPacks(PackingV3Request obj) {
        try {
            // Convert Weight to KGs
            if (Objects.nonNull(obj.getWeight())) {
                obj.setWeight(new BigDecimal(convertUnit(MASS, obj.getWeight(), obj.getWeightUnit(), WEIGHT_UNIT_KG).toString()));
                obj.setWeightUnit(Constants.WEIGHT_UNIT_KG);
            }

            // Convert Volume to M3
            if (Objects.nonNull(obj.getVolume())) {
                obj.setVolume(new BigDecimal(convertUnit(VOLUME, obj.getVolume(), obj.getVolumeUnit(), VOLUME_UNIT_M3).toString()));
                obj.setVolumeUnit(Constants.VOLUME_UNIT_M3);

                double factor = Constants.AIR_FACTOR_FOR_VOL_WT;
                BigDecimal wvInKG = obj.getVolume().multiply(BigDecimal.valueOf(factor));
                obj.setVolumeWeight(wvInKG);
                obj.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KG);
            }

            // Calculate chargeable
            if (Objects.nonNull(obj.getWeight()) && Objects.nonNull(obj.getVolumeWeight())) {
                obj.setChargeable(obj.getVolumeWeight().max(obj.getWeight()));
                obj.setChargeableUnit(WEIGHT_UNIT_KG);
            }
        } catch (Exception e) {
            log.error("Error while unit conversion for AIR transport mode in shipment packs from booking", e);
        }
    }

    public boolean isConsoleCreationNeededV3(CustomerBookingV3Request customerBookingRequest) {
        return (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL)) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_ROA) &&
                        (Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FTL) || Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL))) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_RAI) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL));
    }

    @Override
    public List<RoutingsRequest> getCustomerBookingRequestRoutingList(CarrierDetailRequest carrierDetailRequest, String transportMode) {

        if (ObjectUtils.isEmpty(carrierDetailRequest) || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            return new ArrayList<>();
        }

        // Get carrier details from the customer booking request
        CarrierDetailRequest carrierDetails = Optional.ofNullable(carrierDetailRequest)
                .orElse(new CarrierDetailRequest());

        List<Routings> routingsList = routingsDao.generateDefaultRouting(jsonHelper.convertValue(carrierDetails, CarrierDetails.class), transportMode);

        return commonUtils.convertToList(routingsList, RoutingsRequest.class);

    }

    private ShipmentV3Request getShipmentRequestFromBookingV3(CustomerBookingV3Request customerBookingRequest, Set<ConsolidationDetailsRequest> consolidationDetails) {
        return ShipmentV3Request.builder().
                carrierDetails(CarrierDetailRequest.builder()
                        .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                        .destination(customerBookingRequest.getCarrierDetails().getDestination())
                        .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                        .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                        .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                        .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                        .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                        .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                        .carrierCountry(customerBookingRequest.getCarrierDetails().getCarrierCountry())
                        .minTransitHours(customerBookingRequest.getCarrierDetails().getMinTransitHours())
                        .maxTransitHours(customerBookingRequest.getCarrierDetails().getMaxTransitHours())
                        .carrierAddedFromNpm(customerBookingRequest.getCarrierDetails().getCarrierAddedFromNpm())
                        .build()
                ).
                additionalDetails(AdditionalDetailV3Request.builder().
                        pickupDate(customerBookingRequest.getPickupAtOriginDate()).
                        cargoDeliveredDate(customerBookingRequest.getDeliveryAtDestinationDate()).
                        build()).
                contractId(customerBookingRequest.getContractId()).
                parentContractId(customerBookingRequest.getParentContractId()).
                contractType(customerBookingRequest.getContractStatus()).
                noOfPacks(customerBookingRequest.getQuantity()).
                packsUnit(customerBookingRequest.getQuantityUnit()).
                weight(customerBookingRequest.getGrossWeight()).
                weightUnit(customerBookingRequest.getGrossWeightUnit()).
                volume(customerBookingRequest.getVolume()).
                volumeUnit(customerBookingRequest.getVolumeUnit()).
                volumetricWeight(customerBookingRequest.getWeightVolume()).
                volumetricWeightUnit(customerBookingRequest.getWeightVolumeUnit()).
                bookingNumber(customerBookingRequest.getCarrierBookingNumber()).
                bookingCreatedDate(customerBookingRequest.getBookingDate()).
                shipmentCreatedOn(LocalDateTime.now()).
                client(createPartiesRequest(customerBookingRequest.getCustomer(), customerBookingRequest.getClientCountry())).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee(), customerBookingRequest.getConsigneeCountry())).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor(), customerBookingRequest.getConsignorCountry())).
                additionalDetails(AdditionalDetailV3Request.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty(), customerBookingRequest.getNotifyPartyCountry())).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                transportMode(customerBookingRequest.getTransportType()).
                direction(customerBookingRequest.getDirection()).
                jobType("STD").
                incoterms(customerBookingRequest.getIncoTerms()).
                serviceType(customerBookingRequest.getServiceMode()).
                status(4).
                fmcTlcId(customerBookingRequest.getFmcTlcId()).
                clientCountry(customerBookingRequest.getClientCountry()).
                consignorCountry(customerBookingRequest.getConsignorCountry()).
                consigneeCountry(customerBookingRequest.getConsigneeCountry()).
                notifyPartyCountry(customerBookingRequest.getNotifyPartyCountry()).
                salesBranch(customerBookingRequest.getSalesBranch()).
                primarySalesAgentEmail(customerBookingRequest.getPrimarySalesAgentEmail()).
                secondarySalesAgentEmail(customerBookingRequest.getSecondarySalesAgentEmail()).
                //TODO: check with shipment team
                //containersList(consolidationDetails != null && !consolidationDetails.isEmpty() ? containerList : null).
                //packingList(getPackingListRequestV3(customerBookingRequest)).
                //fileRepoList(customerBookingRequest.getFileRepoList()).
                //routingsList(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled()) && Boolean.TRUE.equals(isRouteMasterEnabled) ? null : customerBookingRequestRoutingList).
                //consolidationList(isConsoleCreationNeededV3(customerBookingRequest) ? consolidationDetails : null).
                        referenceNumbersList(createReferenceNumbersList(customerBookingRequest.getReferenceNumbersList())).
                //notesList(createNotes(notes)).
                        sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                consolRef(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getReferenceNumber() : "").
                masterBill(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getBol() : null).
                freightLocalCurrency(UserContext.getUser().CompanyCurrency).
                currentPartyForQuote(customerBookingRequest.getCurrentPartyForQuote()).
                autoUpdateWtVol(true).
                isReefer(customerBookingRequest.getIsReefer()).
                incotermsLocation(customerBookingRequest.getIncotermsLocation()).
                cargoReadinessDate(customerBookingRequest.getCargoReadinessDate()).
                controlled(customerBookingRequest.getControlled()).
                controlledReferenceNumber(customerBookingRequest.getControlledReferenceNumber()).
                partner(customerBookingRequest.getPartner()).
                bookingAgent(customerBookingRequest.getBookingAgent()).
                coLoadBkgNumber(customerBookingRequest.getPartnerBkgNumber()).
                pickupAtOriginType(customerBookingRequest.getPickupAtOriginType()).
                deliveryAtDestinationType(customerBookingRequest.getDeliveryAtDestinationType()).
                brokerageAtOriginType(customerBookingRequest.getBrokerageAtOriginType()).
                brokerageAtDestinationType(customerBookingRequest.getBrokerageAtDestinationType()).
                brokerageAtOrigin(customerBookingRequest.getBrokerageAtOrigin()).
                brokerageAtDestination(customerBookingRequest.getBrokerageAtDestination()).
                brokerageAtOriginDate(customerBookingRequest.getBrokerageAtOriginDate()).
                brokerageAtDestinationDate(customerBookingRequest.getBrokerageAtDestinationDate()).
                terminalCutoff(customerBookingRequest.getTerminalCutoff()).
                verifiedGrossMassCutoff(customerBookingRequest.getVerifiedGrossMassCutoff()).
                shippingInstructionCutoff(customerBookingRequest.getShippingInstructionCutoff()).
                dgCutoff(customerBookingRequest.getDgCutoff()).
                reeferCutoff(customerBookingRequest.getReeferCutoff()).
                earliestEmptyEquipmentPickUp(customerBookingRequest.getEarliestEmptyEquipmentPickUp()).
                latestFullEquipmentDeliveredToCarrier(customerBookingRequest.getLatestFullEquipmentDeliveredToCarrier()).
                earliestDropOffFullEquipmentToCarrier(customerBookingRequest.getEarliestDropOffFullEquipmentToCarrier()).
                latestArrivalTime(customerBookingRequest.getLatestArrivalTime()).
                build();
    }

    private AutoUpdateWtVolResponse calculateShipmentWV(AutoUpdateWtVolRequest request) throws RunnerException {
        AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
        List<Packing> packingList = new ArrayList<>();
        if (request.getPackingList() != null)
            packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Containers> containersList = new ArrayList<>();
        if (request.getContainersList() != null)
            containersList = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
        calculatePacksAndPacksUnit(packingList, response);
        response = calculateWeightAndVolumeUnit(request, packingList, response);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isPacksPresent = packingList != null && !packingList.isEmpty();
        if (!isPacksPresent)
            response = updateShipmentDetails(response, containersList);
        calculateVW(request, response, true);
        if (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer().booleanValue()
                || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) || isPacksPresent) {
            ShipmentMeasurementDetailsDto dto = new ShipmentMeasurementDetailsDto();
            response.setPackSummary(packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), dto));
            updateResponseFromDto(request, response, dto, shipmentSettingsDetails);
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if (Boolean.TRUE.equals(v1TenantSettingsResponse.getP100Branch()) && Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)) {
            calculatePacksAndPacksUnitFromContainer(response, containersList);
        }
        return response;
    }

    private PartiesRequest createPartiesRequest(PartiesRequest party, String countryCode) {
        if (party == null)
            return null;
        return PartiesRequest.builder()
                .addressCode(party.getAddressCode())
                .addressData(party.getAddressData())
                .orgCode(party.getOrgCode())
                .orgData(party.getOrgData())
                .orgId(party.getOrgId())
                .addressId(party.getAddressId())
                .countryCode(countryCode)
                .build();
    }

    private void processShipmentRequestFromDetails(ShipmentV3Request shipmentRequest, ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getGoodsDescription() != null)
            shipmentRequest.setGoodsDescription(shipmentDetails.getGoodsDescription());

        if (shipmentDetails.getReferenceNumbersList() != null) {
            List<ReferenceNumbersRequest> referenceNumbersList = jsonHelper.convertValue(shipmentDetails.getReferenceNumbersList(), new TypeReference<List<ReferenceNumbersRequest>>() {
            });
            shipmentRequest.setReferenceNumbersList(referenceNumbersList);
        }

        if (shipmentDetails.getAdditionalDetails() != null) {
            if (shipmentDetails.getAdditionalDetails().getImportBroker() != null) {
                PartiesRequest importBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getImportBroker(), PartiesRequest.class);
                shipmentRequest.getAdditionalDetails().setImportBroker(importBroker);
            }

            if (shipmentDetails.getAdditionalDetails().getExportBroker() != null) {
                PartiesRequest exportBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getExportBroker(), PartiesRequest.class);
                shipmentRequest.getAdditionalDetails().setExportBroker(exportBroker);
            }
        }
    }

    public void populateOriginDestinationAgentDetailsForBookingShipment(ShipmentDetails shipmentDetails) {
        ConsolidationDetails consolidationDetails = getConsolidationDetails(shipmentDetails);
        if (consolidationDetails != null && !Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            boolean consolUpdated = false;
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);
            } else if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                consolidationDetails.setSendingAgent(shipmentDetails.getAdditionalDetails().getExportBroker());
                consolUpdated = true;
            }

            if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
                setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);
            } else if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                consolidationDetails.setReceivingAgent(shipmentDetails.getAdditionalDetails().getImportBroker());
                consolUpdated = true;
            }

            if (consolUpdated) {
                consolidationDetailsDao.updateV3(consolidationDetails);
            }
        } else if (consolidationDetails != null && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            setOriginBranchFromExportBroker(shipmentDetails);
        }
        if (consolidationDetails == null) {
            populateImportExportBrokerForShipment(shipmentDetails);
        }
    }

    private ConsolidationDetails getConsolidationDetails(ShipmentDetails shipmentDetails) {
        ConsolidationDetails consolidationDetails = null;
        if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }
        return consolidationDetails;
    }

    private void setOriginBranchFromExportBroker(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getExportBroker() != null && shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData() != null && shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData().get("TenantId") != null)
            shipmentDetails.setOriginBranch(Long.valueOf(shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData().get("TenantId").toString()));
    }

    private void setExportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if (shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getSendingAgent(), shipmentDetails.getAdditionalDetails().getExportBroker())) {
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        }
    }

    private void setImportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if (shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getReceivingAgent(), shipmentDetails.getAdditionalDetails().getImportBroker())) {
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        }
    }

    private void populateImportExportBrokerForShipment(ShipmentDetails shipmentDetails) {
        if (Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()) &&
                (shipmentDetails.getAdditionalDetails() == null || !CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker()))) {
            if (shipmentDetails.getAdditionalDetails() == null) {
                shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            }
            shipmentDetails.getAdditionalDetails().setExportBroker(v1ServiceUtil.getDefaultAgentOrgParty(null));
            if (shipmentDetails.getTransportMode() != null && Objects.equals(shipmentDetails.getTransportMode(), TRANSPORT_MODE_AIR))
                setOriginBranchFromExportBroker(shipmentDetails);
        } else if (Constants.DIRECTION_IMP.equals(shipmentDetails.getDirection()) &&
                (shipmentDetails.getAdditionalDetails() == null || !CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker()))) {
            if (shipmentDetails.getAdditionalDetails() == null) {
                shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            }
            shipmentDetails.getAdditionalDetails().setImportBroker(v1ServiceUtil.getDefaultAgentOrgParty(null));
        }
    }

    private void autoGenerateCreateEvent(ShipmentDetails shipmentDetails) {
        Events response = null;
        response = createAutomatedEvents(shipmentDetails, EventConstants.SHCR, commonUtils.getUserZoneTime(LocalDateTime.now()), null);

        if (shipmentDetails.getEventsList() == null) {
            shipmentDetails.setEventsList(new ArrayList<>());
        }
        shipmentDetails.getEventsList().add(response);
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
                                         LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = initializeAutomatedEvents(shipmentDetails, eventCode, actualDateTime, estimatedDateTime);
        commonUtils.updateEventWithMasterData(List.of(events));
        // Persist the event
        eventDao.save(events);
        return events;
    }

    private Events initializeAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
                                             LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(actualDateTime);
        events.setEstimated(estimatedDateTime);
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setShipmentNumber(shipmentDetails.getShipmentId());
        events.setDirection(shipmentDetails.getDirection());
        // Attach to console as well
        eventDao.updateFieldsForShipmentGeneratedEvents(List.of(events), shipmentDetails);

        return events;
    }

    private List<Packing> getAndSetPackings(CustomerBookingV3Request request, Long shipmentId, ShipmentDetails shipmentDetails) {
        List<Packing> updatedPackings = new ArrayList<>();
        //TODO: check with shipment team
        if (request.getPackingList() != null) {
            updatedPackings = packingDao.saveEntityFromShipment(jsonHelper.convertValueToList(request.getPackingList(), Packing.class), shipmentId);
            shipmentDetails.setPackingList(updatedPackings);
        }
        return updatedPackings;
    }

    private void checkContainerAssignedForHbl(ShipmentDetails shipmentDetails, List<Packing> updatedPackings) {
        if (shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty()) {
            hblService.checkAllContainerAssigned(shipmentDetails, shipmentDetails.getContainersList(), updatedPackings);
        }
    }

    private List<NotesRequest> getNotesRequests(ShipmentV3Request request, Long shipmentId) {
        List<NotesRequest> notesRequest = null;
        //request.getNotesList();
        if (notesRequest != null) {
            for (NotesRequest req : notesRequest) {
                req.setEntityId(shipmentId);
            }
        }
        if (notesRequest != null) {
            for (NotesRequest req : notesRequest) {
                notesDao.save(jsonHelper.convertValue(req, Notes.class));
            }
        }
        return notesRequest;
    }

    public DocumentManagerResponse<T> addFilesFromBookingToShipment(String shipmentGuid, String bookingGuid) {
        try {
            List<DocumentManagerUpdateFileEntitiesRequest.UpdateFileRequest> updateFileRequests = new ArrayList<>();
            updateFileRequests.add(DocumentManagerUpdateFileEntitiesRequest.UpdateFileRequest.builder()
                    .source(DocumentManagerUpdateFileEntitiesRequest.EntityData.builder()
                            .entityKey(bookingGuid)
                            .entityType(BOOKINGS_WITH_SQ_BRACKETS)
                            .build())
                    .entitiesToAttach(List.of(DocumentManagerUpdateFileEntitiesRequest.EntityData.builder()
                            .entityKey(shipmentGuid)
                            .entityType(SHIPMENTS_WITH_SQ_BRACKETS)
                            .build()))
                    .build());

            return documentManagerService.updateFileEntities(DocumentManagerUpdateFileEntitiesRequest.builder()
                    .filesToUpdate(updateFileRequests)
                    .build());
        } catch (Exception ex) {
            log.error("CR-ID {} || Error in addFilesFromBookingToShipment: {} with Shipment Guid as: {}",
                    LoggerHelper.getRequestIdFromMDC(), ex.getLocalizedMessage(), shipmentGuid);
        }
        return null;
    }

    private List<ReferenceNumbersRequest> createReferenceNumbersList(List<ReferenceNumbersRequest> referenceNumbers) {
        if (referenceNumbers == null) return null;
        return referenceNumbers.stream().filter(Objects::nonNull).map(refNumber ->
                ReferenceNumbersRequest.builder()
                        .consolidationId(refNumber.getConsolidationId())
                        .countryOfIssue(refNumber.getCountryOfIssue())
                        .type(refNumber.getType())
                        .referenceNumber(refNumber.getReferenceNumber())
                        .shipmentId(refNumber.getShipmentId())
                        .isPortalEnable(refNumber.getIsPortalEnable())
                        .build()).toList();
    }

    private List<NotesRequest> createNotes(List<Notes> notes) {
        if (notes == null) return null;
        return notes.stream().filter(Objects::nonNull).map(note ->
                NotesRequest.builder()
                        .assignedTo(note.getAssignedTo())
                        .label(note.getLabel())
                        .text(note.getText())
                        .insertUserDisplayName(note.getCreatedBy())
                        .isPublic(note.getIsPublic())
                        .insertDate(note.getCreatedAt())
                        .entityType(Constants.CUSTOMER_BOOKING)
                        .build()).toList();
    }

    private <T> T calculatePacksAndPacksUnit(List<Packing> packings, T response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        String packingUnit = null;
        if (packings != null && !packings.isEmpty()) {
            for (Packing packing : packings) {
                if (!isStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                if (tempPackingUnit == null) {
                    tempPackingUnit = packing.getPacksType();
                    packingUnit = packing.getPacksType();
                } else {
                    if (isMPKUnitCase(packing, tempPackingUnit)) {
                        packingUnit = Constants.MPK;
                    }
                }
            }
        }
        getResponseForPacks(response, totalPacks, packingUnit);
        return response;
    }

    private boolean isMPKUnitCase(Packing packing, String tempPackingUnit) {
        return !isStringNullOrEmpty(packing.getPacksType()) && tempPackingUnit.equals(packing.getPacksType());
    }

    private <T> void getResponseForPacks(T response, Integer totalPacks, String packingUnit) {
        if (response instanceof AutoUpdateWtVolResponse autoUpdateWtVolResponse) {
            autoUpdateWtVolResponse.setNoOfPacks(totalPacks.toString());
            autoUpdateWtVolResponse.setPacksUnit(packingUnit);
        } else if (response instanceof MeasurementBasisResponse measurementBasisResponseas) {
            measurementBasisResponseas.setPackCount(totalPacks);
        }
    }

    protected AutoUpdateWtVolResponse calculateWeightAndVolumeUnit(AutoUpdateWtVolRequest request, List<Packing> packings, AutoUpdateWtVolResponse response) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if (isStringNullOrEmpty(request.getWeightUnit()))
            response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        if (isStringNullOrEmpty(request.getVolumeUnit()))
            response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        if (packings != null && !packings.isEmpty()) {
            for (Packing packing : packings) {
                if (packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if (packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
            }
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(request, response, false);
        }
        return response;
    }

    protected AutoUpdateWtVolResponse calculateVW(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, boolean recalculateVwObInKgAndM3) throws RunnerException {
        if (isStringNullOrEmpty(request.getTransportMode()))
            return response;
        if (!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationV3Service.calculateVolumeWeight(request.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if (request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                response.setChargable(BigDecimal.valueOf(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if (checkConditionForSEAorROAD(request)){
                double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                response.setChargable(BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3)));
                response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                if (recalculateVwObInKgAndM3)
                    vwOb = consolidationV3Service.calculateVolumeWeight(request.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
            }

            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private boolean checkConditionForSEAorROAD(AutoUpdateWtVolRequest request) {
        String transportMode = request.getTransportMode();
        String cargoType = request.getShipmentType();

        if (isStringNullOrEmpty(cargoType)) return false;

        boolean isSeaLCL = commonUtils.isSeaLCL(transportMode, cargoType);
        boolean isRoadLCLorLTL = commonUtils.isRoadLCLorLTL(transportMode, cargoType);

        return isSeaLCL || isRoadLCLorLTL;
    }

    protected AutoUpdateWtVolResponse updateShipmentDetails(AutoUpdateWtVolResponse response, List<Containers> containersList) throws RunnerException { // to account for updateShipmentDetails flag in v1 container summary
        double totalWeight = 0;
        double packageCount = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        int totalPacks = 0;
        String packsUnit = "";
        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        toWeightUnit = getToWeightUnit(shipmentSettingsDetails, toWeightUnit);
        toVolumeUnit = getToVolumeUnit(shipmentSettingsDetails, toVolumeUnit);
        if (containersList != null) {
            for (Containers containers : containersList) {
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                if (!isStringNullOrEmpty(containers.getPacks()))
                    packageCount = packageCount + Long.parseLong(containers.getPacks());
                totalVolume = totalVolume + volume;
                if (containers.getContainerCount() != null)
                    totalContainerCount = totalContainerCount + containers.getContainerCount();
                if (!isStringNullOrEmpty(containers.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(containers.getPacks());
            }
        }
        if (!containersList.isEmpty()) {
            packsUnit = setPacksUnit(containersList);
        }
        response.setWeight(BigDecimal.valueOf(totalWeight));
        response.setVolume(BigDecimal.valueOf(totalVolume));
        response.setWeightUnit(toWeightUnit);
        response.setVolumeUnit(toVolumeUnit);
        response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
        response.setPacksUnit(packsUnit);
        response.setNetWeight(BigDecimal.valueOf(tareWeight));
        response.setNetWeightUnit(toWeightUnit);
        return response;
    }

    private String getToWeightUnit(ShipmentSettingsDetails shipmentSettingsDetails, String toWeightUnit) {
        if (!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        return toWeightUnit;
    }

    private String getToVolumeUnit(ShipmentSettingsDetails shipmentSettingsDetails, String toVolumeUnit) {
        if (!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        return toVolumeUnit;
    }

    private String setPacksUnit(List<Containers> containersList) {
        String firstPacksType = containersList.get(0).getPacksType();
        boolean isSame = containersList.stream()
                .map(Containers::getPacksType)
                .allMatch(packsType -> packsType == null || packsType.equals(firstPacksType));

        if (isSame) {
            return firstPacksType;
        } else {
            return Constants.MPK;
        }
    }

    private void updateResponseFromDto(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, ShipmentMeasurementDetailsDto dto, ShipmentSettingsDetails shipmentSettingsDetails) {
        String transportMode = request.getTransportMode();
        String cargoType = request.getShipmentType();

        boolean isSeaLCL = commonUtils.isSeaLCL(transportMode, cargoType);
        boolean isAir = TRANSPORT_MODE_AIR.equals(transportMode);
        boolean isRoadLCLorLTL = commonUtils.isRoadLCLorLTL(transportMode, cargoType);

        if (isSeaLCL || isAir || isRoadLCLorLTL) {
            response.setInnerPacks(dto.getInnerPacks());
            response.setInnerPackUnit(dto.getInnerPackUnit());
        }
        if (shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer()
                && request.getPackingList() != null && !request.getPackingList().isEmpty()) {
            response.setWeight(dto.getWeight());
            response.setWeightUnit(dto.getWeightUnit());
            response.setVolume(dto.getVolume());
            response.setVolumeUnit(dto.getVolumeUnit());
            response.setNetWeight(dto.getNetWeight());
            response.setNetWeightUnit(dto.getNetWeightUnit());
            response.setNoOfPacks(dto.getNoOfPacks());
            response.setPacksUnit(dto.getPacksUnit());
        } else if (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer()) {
            response.setNoOfPacks(dto.getNoOfPacks());
            response.setPacksUnit(dto.getPacksUnit());
        }
    }

    private void calculatePacksAndPacksUnitFromContainer(AutoUpdateWtVolResponse response, List<Containers> containersList) {
        if (containersList != null && !containersList.isEmpty()) {
            String packsUnit = "";
            long packageCount = 0;
            long totalPacks = 0;
            for (Containers container : containersList) {
                if (!isStringNullOrEmpty(container.getPacks())) {
                    packageCount = packageCount + Integer.parseInt(container.getPacks());
                    totalPacks = totalPacks + Integer.parseInt(container.getPacks());
                }
            }
            packsUnit = setPacksUnit(containersList);
            response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
            response.setPacksUnit(packsUnit);
        }
    }

    private void updateContainerFromCargo(ShipmentDetails shipmentDetails) throws RunnerException {
        if (!TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) || Objects.isNull(shipmentDetails.getContainerAssignedToShipmentCargo()))
            return;
        containerV3Service.updateAttachedContainersData(List.of(shipmentDetails.getContainerAssignedToShipmentCargo()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, String consoleGuid, boolean isAttached, boolean getMasterData,
            boolean fromNte) throws AuthenticationException {
        if (consoleId == null && consoleGuid == null) {
            throw new ValidationException("Required parameters missing: consoleId and consoleGuid");
        }

        Optional<ConsolidationDetails> consolidationDetails = getOptionalConsolidationDetails(consoleId, consoleGuid, fromNte);

        if (consolidationDetails.isEmpty()) {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        if (consoleId == null) {
            consoleId = consolidationDetails.get().getId();
        }

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
        }
        if (request.getFilterCriteria().isEmpty()) {
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        Map<Long, ConsoleShipmentMapping> requestedTypeMap = new HashMap<>();
        // InterBranch Logic
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
            if (!isAttached) {
                var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdAll(consoleId);
                if (consoleShipMappingList == null || consoleShipMappingList.isEmpty()) {
                    return ResponseHelper.buildListSuccessResponse(new ArrayList<>(), 1, 0);
                }
                requestedTypeMap = consoleShipMappingList.stream()
                        .collect(Collectors.toMap(ConsoleShipmentMapping::getShipmentId, Function.identity(), (existingValue, newValue) -> existingValue));
                List<Long> shipIds = consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
                CommonUtils.andCriteria("id", shipIds, "IN", request);
            } else {
                CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
            }
        } else {
            CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
        }
        var response = listShipment(CommonRequestModel.buildRequest(request), getMasterData);
        processResponseList(response, requestedTypeMap);
        if (fromNte) {
            TenantContext.removeTenant();
        }
        return response;
    }

    @Override
    public String sendOceanDGApprovalEmail(OceanDGApprovalRequest dgApprovalRequest) throws RunnerException {
        if (Objects.isNull(dgApprovalRequest)) {
            log.error("Invalid request for sendEmailForDGApprove");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }

        Long shipId = dgApprovalRequest.getShipmentId();
        String remarks = dgApprovalRequest.getRemarks();

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipId)
            .orElseThrow(() -> new DataRetrievalFailureException("Shipment details not found for ID: " + shipId));

        if(Constants.IMP.equals(shipmentDetails.getDirection())) {
            return "DG approval not required for Import Shipment";
        }

        boolean isOceanDgUser = UserContext.isOceanDgUser();
        OceanDGStatus dgStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus updatedDgStatus = determineDgStatusAfterApproval(dgStatus, isOceanDgUser, shipmentDetails);
        DBOperationType operationType = determineOperationType(dgStatus, isOceanDgUser);

        boolean isShipmentdg = isOceanDG(shipmentDetails);
        log.info("DG Approval Processing: requestId={}, shipmentId={}, isOceanDgUser={}, currentStatus={}, updatedStatus={}, operationType={}, isShipmentdg={}",
            LoggerHelper.getRequestIdFromMDC(), shipId, isOceanDgUser, dgStatus, updatedDgStatus, operationType, isShipmentdg);

        String warning = null;
        if(!isShipmentdg){
            warning = "Shipment does not have any DG container or package, no need of any dg approval";
            updatedDgStatus = null;
            operationType = DG_REQUEST;
        }

        if((dgStatus == OCEAN_DG_ACCEPTED || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus== OCEAN_DG_COMMERCIAL_REJECTED) && !checkForClass1(shipmentDetails) && warning == null){
            warning = "Shipment does not have any class1 DG container or package, no need of commercial dg approval";
            updatedDgStatus = OCEAN_DG_ACCEPTED;
            operationType = DG_APPROVE;
        }

        if(dgStatus == OCEAN_DG_COMMERCIAL_ACCEPTED){
            warning = "Shipment is already in commercial approved state";
            updatedDgStatus = OCEAN_DG_COMMERCIAL_ACCEPTED;
            operationType = COMMERCIAL_APPROVE;
        }

        if ((!isOceanDgUser || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus== OCEAN_DG_COMMERCIAL_REJECTED) && warning == null) {
            sendEmailForDGApproval(shipmentDetails, remarks);
        }

        try {
            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                    .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                    .newData(OceanDGRequestLog.builder()
                        .time(LocalDateTime.now())
                        .userName(UserContext.getUser().getUsername())
                        .build())
                    .prevData(null)
                    .parent(ShipmentDetails.class.getSimpleName())
                    .parentId(shipmentDetails.getId())
                    .entityType(OceanDGRequestLog.class.getSimpleName())
                    .operation(operationType.name()).build()
            );

        }catch (Exception ex){
            log.error("Audit failed for shipmentId: {} and operation: {}. Error: {}", shipmentDetails.getId(), operationType, ex.getMessage(), ex);
        }

        shipmentDetails.setOceanDGStatus(updatedDgStatus);
        shipmentDao.save(shipmentDetails, false);

        return warning;
    }

    @Override
    public String dgApprovalResponse(OceanDGRequestV3 request) throws RunnerException {
        if (Objects.isNull(request)) {
            log.error("Invalid request for sendEmailForDGApprove");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }

        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId())
            .orElseThrow(() -> new DataRetrievalFailureException("Shipment details not found for ID: " + request.getShipmentId()));

        if(Constants.IMP.equals(shipmentDetails.getDirection())) {
            return "DG approval not required for Import Shipment";
        }

        OceanDGStatus oldDgStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus updatedDgStatus = getDgStatusAfterApprovalResponse(oldDgStatus, request.getStatus());

        if(updatedDgStatus == null){
            throw new RunnerException(String.format("Ocean DG status value %s is invalid", oldDgStatus));
        }

        if(CollectionUtils.isEmpty(request.getTaskGuids())){
            fetchDgUserTask(request);
        }

        String warning = sendEmailResponseToDGRequester(request, shipmentDetails, updatedDgStatus);
        DBOperationType operationType = determineOperationTypeAfterApproval(oldDgStatus, request);

        closeOceanDgTask(request);
        try {
            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                    .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                    .newData(OceanDGRequestLog.builder()
                        .time(LocalDateTime.now())
                        .userName(UserContext.getUser().DisplayName)
                        .build())
                    .prevData(null)
                    .parent(ShipmentDetails.class.getSimpleName())
                    .parentId(shipmentDetails.getId())
                    .entityType(OceanDGRequestLog.class.getSimpleName())
                    .operation(operationType.name()).build()
            );
        } catch (Exception ex){
            log.error("Audit failed for shipmentId: {} and operation: {}. Error: {}", shipmentDetails.getId(), operationType, ex.getMessage(), ex);
        }

        if(updatedDgStatus == OceanDGStatus.OCEAN_DG_ACCEPTED && checkForClass1(shipmentDetails)){
            updatedDgStatus = OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
        }
        shipmentDetails.setOceanDGStatus(updatedDgStatus);

        shipmentDao.save(shipmentDetails, false);

        return warning;
    }

    private void fetchDgUserTask(OceanDGRequestV3 request) throws RunnerException {
        CommonV1ListRequest commonV1ListRequest = createCriteriaTaskListRequest(request.getShipmentId().toString(), SHIPMENTS_WITH_SQ_BRACKETS);
        log.info("V1 task list request: {}" , jsonHelper.convertToJson(commonV1ListRequest));

        List<Map<String, Object>> mapList;
        try {
            mapList = mdmServiceAdapter.getTaskList(request.getShipmentGuid(), SHIPMENT, PENDING_ACTION_TASK, DG_OCEAN_APPROVAL);
        }
        catch (Exception ex) {
            log.error("Failed to fetch pending tasks from MDM with RequestId - {} : {}: ", LoggerHelper.getRequestIdFromMDC(), ex);
            throw new RunnerException(ex.getMessage());
        }


        if(mapList.isEmpty()) return;

        if(mapList.size() > 1){
            log.error("More than one task in Pending State of oceanDG exist for shipment : " + request.getShipmentId());
        }

        List<String> taskGuids = new ArrayList<>();
        for(Map<String, Object> map : mapList){
            taskGuids.add(map.get("uuid").toString());
        }
        request.setTaskGuids(taskGuids);
        request.setUserEmail(mapList.get(0).get("userEmail").toString());
    }

    private void closeOceanDgTask(OceanDGRequestV3 request){
        MdmTaskApproveOrRejectRequest taskUpdateRequest = MdmTaskApproveOrRejectRequest.builder()
            .status(request.getStatus().getName())
            .approvedOrRejectedBy(UserContext.getUser().getUsername())
            .build();

        if(TaskStatus.APPROVED.equals(request.getStatus())){
            taskUpdateRequest.setApprovalComments(request.getRemarks());
        }else{
            taskUpdateRequest.setRejectedComments(request.getRemarks());
        }

        try {
            for(String taskGuid : request.getTaskGuids()){
                taskUpdateRequest.setTaskUuid(taskGuid);
                mdmServiceAdapter.approveOrRejectTask(taskUpdateRequest);
            }
        }
        catch (Exception ex) {
            log.error("task approval or rejection is failed for requestId from MDM: {} : {} " ,LoggerHelper.getRequestIdFromMDC() , request.getShipmentId());
        }
    }

    private DBOperationType determineOperationTypeAfterApproval(OceanDGStatus dgStatus, OceanDGRequestV3 request){
        DBOperationType operationType = DG_REQUEST;
        if(dgStatus == OCEAN_DG_REQUESTED){
            if(request.getStatus() == TaskStatus.APPROVED){
                operationType = DG_APPROVE;
            }else{
                operationType = DBOperationType.DG_REJECT;
            }
        }else if(dgStatus == OCEAN_DG_COMMERCIAL_REQUESTED){
            if(request.getStatus() == TaskStatus.REJECTED){
                operationType = COMMERCIAL_APPROVE;
            }else{
                operationType = DBOperationType.COMMERCIAL_REJECT;
            }
        }
        return operationType;
    }

    private String sendEmailResponseToDGRequester(OceanDGRequestV3 request, ShipmentDetails shipmentDetails, OceanDGStatus newStatus) throws RunnerException {

        String warningMessage = null;
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplates = new EnumMap<>(OceanDGStatus.class);
        CompletableFuture<Void> emailTemplateFuture = CompletableFuture.runAsync(
            masterDataUtils.withMdc(() -> commonUtils.getDGEmailTemplate(emailTemplates)),
            executorService
        );
        emailTemplateFuture.join();

        try {
            EmailTemplatesRequest template = emailTemplates.get(newStatus);
            if(template == null){
                warningMessage = "No template is present for status: " + newStatus;
                return warningMessage;
            }

            commonUtils.sendEmailResponseToDGRequesterV3(template, request, shipmentDetails);
        } catch (Exception e) {
            log.error(ERROR_WHILE_SENDING_EMAIL, e.getMessage());
            warningMessage = ERROR_WHILE_SENDING_EMAIL + e.getMessage();
        }
        return warningMessage;
    }

    private CommonV1ListRequest createCriteriaTaskListRequest(Object value1, Object value2) {
        List<Object> criteria1 = new ArrayList<>(List.of(List.of("entityUuid"), "=", value1));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of("EntityType"), "=", value2));
        List<Object> criteria3 = new ArrayList<>(List.of(List.of("TaskType"), "=", 22));
        List<Object> criteria4 = new ArrayList<>(List.of(List.of("Status"), "=", 0));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(List.of(criteria1, "and", criteria2), "and", criteria3), "and" , criteria4)).build();
    }

    private OceanDGStatus getDgStatusAfterApprovalResponse(OceanDGStatus currentStatus, TaskStatus approvalStatus) {
        if (currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED ?
                OCEAN_DG_COMMERCIAL_ACCEPTED :
                OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
        } else if (currentStatus == OCEAN_DG_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED  ?
                OceanDGStatus.OCEAN_DG_ACCEPTED :
                OceanDGStatus.OCEAN_DG_REJECTED;
        }
        return null;
    }

    private OceanDGStatus determineDgStatusAfterApproval(OceanDGStatus dgStatus, boolean isOceanDgUser, ShipmentDetails shipmentDetails) {
        boolean isClass1 = checkForClass1(shipmentDetails);
        if(dgStatus == OCEAN_DG_ACCEPTED && !isClass1){
            return dgStatus;
        }
        if ((dgStatus == OCEAN_DG_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_REJECTED) && isOceanDgUser) {
            return isClass1 ? OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED : OCEAN_DG_ACCEPTED;
        } else {
            return (dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_COMMERCIAL_REJECTED) ? OCEAN_DG_COMMERCIAL_REQUESTED : OCEAN_DG_REQUESTED;
        }
    }

    private DBOperationType determineOperationType(OceanDGStatus dgStatus, boolean isOceanDgUser) {
        if(dgStatus == OCEAN_DG_REQUESTED && isOceanDgUser) return DG_APPROVE;
        return dgStatus == OCEAN_DG_REQUESTED
            ? DBOperationType.DG_REQUEST
            : COMMERCIAL_REQUEST;
    }

    private void sendEmailForDGApproval(ShipmentDetails shipmentDetails, String remarks)
        throws RunnerException {
        OceanDGStatus oceanDGStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus templateStatus = emailTemplateForDGApproval(oceanDGStatus);

        if(templateStatus == null){
            throw new RunnerException( String.format("User cannot send email in %s DGStatus", oceanDGStatus));
        }
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        VesselsResponse vesselsResponse = new VesselsResponse();
        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getDGEmailTemplate(emailTemplatesRequestMap)), executorService);
        var vesselResponseFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getVesselsData(shipmentDetails.getCarrierDetails(), vesselsResponse)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, vesselResponseFuture).join();
        Integer roleId = commonUtils.getRoleId(templateStatus);
        List<String> toUserEmails = commonUtils.getUserEmailsByRoleId(roleId);
        TaskCreateResponse taskCreateResponse =  commonUtils.createTaskMDM(shipmentDetails, roleId);

        try {
            sendEmailForDGApproval(emailTemplatesRequestMap, toUserEmails, vesselsResponse, templateStatus, shipmentDetails, remarks,
                taskCreateResponse);
        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }
    }

    private boolean checkForClass1(ShipmentDetails shipmentDetails) {
        if(shipmentDetails == null) return false;
        return shipmentDetails.getContainersList() != null &&
            shipmentDetails.getContainersList().stream()
                .filter(Objects::nonNull)
                .anyMatch(containers -> Boolean.TRUE.equals(containers.getHazardous()) &&
                    Optional.ofNullable(containers.getDgClass())
                        .map(dgClass -> dgClass.startsWith("1"))
                        .orElse(false))
            || shipmentDetails.getPackingList() != null &&
            shipmentDetails.getPackingList().stream()
                .filter(Objects::nonNull)
                .anyMatch(packing -> Boolean.TRUE.equals(packing.getHazardous()) &&
                    Optional.ofNullable(packing.getDGClass())
                        .map(dgClass -> dgClass.startsWith("1"))
                        .orElse(false));

    }

    private OceanDGStatus emailTemplateForDGApproval(OceanDGStatus currentStatus) {
        if (currentStatus == null || currentStatus == OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED || currentStatus == OceanDGStatus.OCEAN_DG_REJECTED ) {
            return OCEAN_DG_REQUESTED;
        } else if (currentStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED || currentStatus == OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED) {
            return OCEAN_DG_COMMERCIAL_REQUESTED;
        } else {
            return null;
        }
    }

    private void sendEmailForDGApproval(Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap, List<String> toEmailIds,
        VesselsResponse vesselsResponse, OceanDGStatus templateStatus, ShipmentDetails shipmentDetails, String remarks,
        TaskCreateResponse taskCreateResponse) throws RunnerException {
        EmailTemplatesRequest emailTemplate = Optional.ofNullable(emailTemplatesRequestMap.get(templateStatus))
            .orElseThrow(() -> new RunnerException("template is not present for : " + templateStatus));

        if (CollectionUtils.isEmpty(toEmailIds)) {
            throw new RunnerException("There are no DG certified users for your branch. Please contact admin");
        }

        Map<String, Object> dictionary = new HashMap<>();
        populateDGDictionary(templateStatus, dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);

        emailTemplate.setBody(generateEmailBody(dictionary, shipmentDetails, emailTemplate.getBody()));
        notificationService.sendEmail(emailTemplate.getBody(), emailTemplate.getSubject(), new ArrayList<>(toEmailIds), new ArrayList<>());
    }

    private void populateDGDictionary(OceanDGStatus templateStatus, Map<String, Object> dictionary,
        ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks, TaskCreateResponse taskCreateResponse) {

        if (templateStatus == OCEAN_DG_REQUESTED) {
            commonUtils.populateDictionaryForOceanDGApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        } else if (templateStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            commonUtils.populateDictionaryForOceanDGCommercialApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        }
    }

    private String generateEmailBody(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, String htmlTemplate) {
        String tableTemplate = extractTableTemplate(htmlTemplate);

        String populatedTable = populateTableWithData(tableTemplate, shipmentDetails);

        String emailBody = htmlTemplate.replace(tableTemplate, populatedTable);
        emailBody = commonUtils.replaceTagsFromData(dictionary, emailBody);
        return emailBody;
    }

    private String extractTableTemplate(String htmlTemplate) {
        int tableStartIndex = htmlTemplate.indexOf("<table");
        int tableEndIndex = htmlTemplate.indexOf("</table>") + "</table>".length();

        if (tableStartIndex != -1 && tableEndIndex > tableStartIndex) {
            return htmlTemplate.substring(tableStartIndex, tableEndIndex);
        }

        return "";
    }

    private String populateTableWithData(String tableTemplate, ShipmentDetails shipmentDetails) {
        if(tableTemplate.isEmpty()) return tableTemplate;

        Document document = Jsoup.parse(tableTemplate);
        Element table = document.select("table").first();

        assert table != null;
        Element rowTemplate = table.select("tbody tr").get(1);

        rowTemplate.remove();
        Map<Long, String> containerIdNumberMap = Optional.ofNullable(shipmentDetails)
            .map(ShipmentDetails::getContainersList)
            .orElse(Collections.emptySet())
            .stream()
            .filter(container -> container.getId() != null && container.getContainerNumber() != null)
            .collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));


        processTableWithPackingList(shipmentDetails, rowTemplate, containerIdNumberMap, table);

        processTableWithContainerList(shipmentDetails, rowTemplate, table);

        return table.outerHtml();
    }

    private void processTableWithPackingList(ShipmentDetails shipmentDetails, Element rowTemplate, Map<Long, String> containerIdNumberMap, Element table) {
        for (Packing packing : shipmentDetails.getPackingList()) {
            if(!Boolean.TRUE.equals(packing.getHazardous())) continue;

            Element newRow = rowTemplate.clone();
            newRow.select("td").get(0).text(
                getValueOrDefault(packing.getPacks(), "") + " " + getValueOrDefault(packing.getPacksType(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(1).text(
                containerIdNumberMap.get(packing.getContainerId()) != null
                    ? String.valueOf(containerIdNumberMap.get(packing.getContainerId()))
                    : ""
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(2).text(
                getValueOrDefault(packing.getDGClass(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(3).text(
                getValueOrDefault(packing.getUnNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(4).text(
                getValueOrDefault(packing.getProperShippingName(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(5).text(
                getValueOrDefault(packing.getPackingGroup(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(6).text(

                (packing.getMinimumFlashPoint() != null ? packing.getMinimumFlashPoint() : "") +
                    (packing.getMinimumFlashPointUnit() != null ? packing.getMinimumFlashPointUnit() : "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(7).text(
                Boolean.TRUE.equals(packing.getMarinePollutant()) ? "Yes" : "No"
            ).attr(STYLE, PADDING_10_PX);


            table.select("tbody").first().appendChild(newRow);
        }
    }

    private void processTableWithContainerList(ShipmentDetails shipmentDetails, Element rowTemplate, Element table) {
        for (Containers containers : shipmentDetails.getContainersList()) {
            if(!Boolean.TRUE.equals(containers.getHazardous())) continue;

            Element newRow = rowTemplate.clone();
            newRow.select("td").get(0).text(
                getValueOrDefault(containers.getPacks(), "") + " " + getValueOrDefault(containers.getPacksType(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(1).text(
                getValueOrDefault(containers.getContainerNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(2).text(
                getValueOrDefault(containers.getDgClass(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(3).text(
                getValueOrDefault(containers.getUnNumber(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(4).text(
                getValueOrDefault(containers.getProperShippingName(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(5).text(
                getValueOrDefault(containers.getPackingGroup(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(6).text(
                getValueOrDefault(containers.getMinimumFlashPoint(), "") +
                    getValueOrDefault(containers.getMinimumFlashPointUnit(), "")
            ).attr(STYLE, PADDING_10_PX);

            newRow.select("td").get(7).text(
                Boolean.TRUE.equals(containers.getMarinePollutant()) ? "Yes" : "No"
            ).attr(STYLE, PADDING_10_PX);


            table.select("tbody").first().appendChild(newRow);
        }
    }

    public static <T> T getValueOrDefault(T value, T defaultValue) {
        return value != null ? value : defaultValue;
    }

    private Optional<ConsolidationDetails> getOptionalConsolidationDetails(Long consoleId, String consoleGuid, boolean fromNte) throws AuthenticationException {
        Optional<ConsolidationDetails> consolidationDetails;
        if (consoleId != null) {
            if (fromNte) {
                consolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(consoleId);
                if (consolidationDetails.isPresent()) {
                    this.isValidNte(consolidationDetails.get());
                    TenantContext.setCurrentTenant(consolidationDetails.get().getTenantId());
                }
            } else {
                consolidationDetails = consolidationDetailsDao.findById(consoleId);
            }
        } else {
            UUID guid = UUID.fromString(consoleGuid);
            consolidationDetails = consolidationDetailsDao.findByGuid(guid);
        }
        return consolidationDetails;
    }

    private boolean isOceanDG(ShipmentDetails shipmentDetails) {
        if (shipmentDetails == null) return false;

        boolean containerHasDGClass = shipmentDetails.getContainersList() != null &&
            shipmentDetails.getContainersList().stream()
                .filter(Objects::nonNull)
                .anyMatch(container -> Boolean.TRUE.equals(container.getHazardous()) && container.getDgClass() != null);


        boolean packingHasDGClass = shipmentDetails.getPackingList() != null &&
            shipmentDetails.getPackingList().stream()
                .filter(Objects::nonNull)
                .anyMatch(packing -> Boolean.TRUE.equals(packing.getHazardous()) && packing.getDGClass() != null);


        return containerHasDGClass || packingHasDGClass;
    }

    private void processResponseList(ResponseEntity<IRunnerResponse> response, Map<Long, ConsoleShipmentMapping> requestedTypeMap) {
        if (response.getBody() instanceof RunnerListResponse<?> responseList) {
            for (var resp : responseList.getData()) {
                if (resp instanceof ShipmentListResponse shipmentListResponse
                        && requestedTypeMap.containsKey(shipmentListResponse.getId())
                        && !Objects.isNull(requestedTypeMap.get(shipmentListResponse.getId()).getRequestedType())) {
                    shipmentListResponse.setRequestedType(requestedTypeMap.get(shipmentListResponse.getId()).getRequestedType().getDescription());
                    shipmentListResponse.setRequestedBy(requestedTypeMap.get(shipmentListResponse.getId()).getCreatedBy());
                    shipmentListResponse.setRequestedOn(requestedTypeMap.get(shipmentListResponse.getId()).getCreatedAt());
                }
            }
        }
    }

    private boolean isValidNte(ConsolidationDetails consolidationDetails) throws AuthenticationException {
        List<TriangulationPartner> triangulationPartners = consolidationDetails.getTriangulationPartnerList();
        Long currentTenant = TenantContext.getCurrentTenant().longValue();
        if (Objects.equals(currentTenant, consolidationDetails.getTenantId())) {
            return false;
        }
        if (
                (triangulationPartners == null
                        && !Objects.equals(consolidationDetails.getTriangulationPartner(), TenantContext.getCurrentTenant().longValue())
                        && !Objects.equals(consolidationDetails.getReceivingBranch(), TenantContext.getCurrentTenant().longValue()))
                        ||
                        ((triangulationPartners == null || triangulationPartners.stream().filter(Objects::nonNull)
                                .noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
                                && !Objects.equals(consolidationDetails.getReceivingBranch(), currentTenant))
        ) {
            throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE);
        }
        return true;
    }


    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> aibAction(AibActionShipment request) throws RunnerException {
        Set<ShipmentRequestedType> requestSet = new HashSet<>();

        commonUtils.setInterBranchContextForColoadStation();

        if (request.getConsoleIdsList() == null || request.getConsoleIdsList().isEmpty()) {
            throw new InvalidDataAccessApiUsageException("Console Ids list should not be empty!!!");
        }

        if (ShipmentRequestedType.APPROVE.equals(request.getShipmentRequestedType())) {
            try {
                consolidationV3Service.attachShipments(
                        ShipmentConsoleAttachDetachV3Request.builder()
                                .shipmentRequestedType(request.getShipmentRequestedType())
                                .consolidationId(request.getConsoleIdsList().get(0))
                                .shipmentIds(Set.of(request.getShipmentId())).build());
            } catch (RunnerException e) {
                log.error("{} | Error while attaching shipments in AIB Action: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage(), e);
                throw new ValidationException(e.getMessage());
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, request.getShipmentId(), EQ);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

            consoleShipmentMappingDao.deletePendingStateByShipmentId(request.getShipmentId());
            // one shipment and one console, shipment pull accepted
            // one shipment and multiple console, shipment pull and push rejected
            sendEmailsForPullRequestAccept(request.getConsoleIdsList().get(0), request.getShipmentId(), requestSet, consoleShipmentMappingsForEmails);
        } else if (ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(request.getShipmentRequestedType())) {
            // fetching from console shipment mapping
            ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, request.getShipmentId(), EQ, null);
            listCommonRequest = andCriteria(CONSOLIDATION_ID, request.getConsoleIdsList(), Constants.IN, listCommonRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
            request.getConsoleIdsList().stream().forEach(consoleId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(consoleId, request.getShipmentId()));
            // one shipment and multiple console, shipment pull rejected
            if (ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType())) {
                sendEmailForPullRequestReject(request.getShipmentId(), request.getConsoleIdsList(), requestSet, request.getRejectRemarks(), consoleShipmentMappings);
            }
            if (ShipmentRequestedType.WITHDRAW.equals(request.getShipmentRequestedType())) {
                sendEmailForPushRequestWithdrawl(request.getShipmentId(), request.getConsoleIdsList(), requestSet, request.getRejectRemarks());
            }
        }

        String warning = getWarningMsg(requestSet);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    public void sendEmailsForPullRequestAccept(Long consoleId, Long shipmentId, Set<ShipmentRequestedType> requestedTypes, List<ConsoleShipmentMapping> consoleShipMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplateMap = new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> locationsMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        List<ConsolidationDetails> otherConsolidationdetails = new ArrayList<>();

        // fetching data from db
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consoleId);
        String requestedUsername = fetchShipmentsAndConsolidationsForPullRequestEmails(tenantIds, usernamesList, consoleId, shipmentId, shipmentDetails, consolidationDetails, consoleShipMappings, otherConsolidationdetails);

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplateMap)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), locationsMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, tenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PULL_ACCEPTED, null, emailTemplateMap, requestedTypes, locationsMap, carrierMap, usernameEmailsMap, tenantSettingsMap, requestedUsername, null);
        } catch (Exception e) {
            log.error(ERROR_WHILE_SENDING_EMAIL);
        }
        if (!otherConsolidationdetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationdetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            consoleShipMappings.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if (finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId())) {
                        if (consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplateMap, requestedTypes, locationsMap, carrierMap, usernameEmailsMap, tenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
                        else
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplateMap, requestedTypes, locationsMap, carrierMap, usernameEmailsMap, tenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }

    }

    public String fetchShipmentsAndConsolidationsForPullRequestEmails(Set<Integer> tenantIds, Set<String> userNames, Long consoleId, Long shipmentId,
                                                                       ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                                       List<ConsoleShipmentMapping> consoleShipMappings, List<ConsolidationDetails> otherConsoles) {
        // fetching shipment and console
        tenantIds.add(consolidationDetails.getTenantId());
        userNames.add(shipmentDetails.getCreatedBy());
        userNames.add(shipmentDetails.getAssignedTo());
        userNames.add(consolidationDetails.getCreatedBy());

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        String requestedUsername = null;
        for (ConsoleShipmentMapping consoleShipMapping : consoleShipMappings) {
            if (!Boolean.TRUE.equals(consoleShipMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipMapping.getConsolidationId());
            } else if (Objects.equals(consoleShipMapping.getShipmentId(), shipmentId) && Objects.equals(consoleShipMapping.getConsolidationId(), consoleId)) {
                requestedUsername = consoleShipMapping.getCreatedBy();
            }
            userNames.add(consoleShipMapping.getCreatedBy());
        }
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, Constants.IN);
        Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
        for (ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
            userNames.add(consolidationDetails1.getCreatedBy());
            tenantIds.add(consolidationDetails1.getTenantId());
            otherConsoles.add(consolidationDetails1);
        }
        return requestedUsername;
    }

    public void sendEmailForPullRequestReject(Long shipmentId, List<Long> consoleIds, Set<ShipmentRequestedType> requestedTypes,
                                              String rejectRemarks, List<ConsoleShipmentMapping> consoleShipMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap = new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> userNames = new HashSet<>();

        // fetching shipment and consolidations
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, consoleIds, Constants.IN);
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        for (ConsolidationDetails consolidationDetails : consolidationDetailsPage.getContent()) {
            consolidationDetailsMap.put(consolidationDetails.getId(), consolidationDetails);
            tenantIds.add(consolidationDetails.getTenantId());
            userNames.add(consolidationDetails.getCreatedBy());
        }

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        userNames.add(shipmentDetails.getCreatedBy());
        userNames.add(shipmentDetails.getAssignedTo());

        Map<Long, String> consoleRequestUserMap = new HashMap<>();
        for (ConsoleShipmentMapping consoleShipmentMapping : consoleShipMappings) {
            consoleRequestUserMap.put(consoleShipmentMapping.getConsolidationId(), consoleShipmentMapping.getCreatedBy());
            userNames.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, tenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(userNames, userEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for (Long consoleId : consoleIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetailsMap.get(consoleId), SHIPMENT_PULL_REJECTED, rejectRemarks, emailTemplatesMap, requestedTypes, null, null, userEmailsMap, tenantSettingsMap, consoleRequestUserMap.get(consoleId), null);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    private void sendEmailForPushRequestWithdrawl(Long shipmentId, List<Long> consolidationIds,
                                                  Set<ShipmentRequestedType> requestedTypes, String withdrawRemarks) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap = new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> userNames = new HashSet<>();
        Map<Integer, TenantModel> tenantsMap = new HashMap<>();

        List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(consolidationIds));
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
        Map<Long, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        if (!listIsNullOrEmpty(consolidationDetails)) {
            for (ConsolidationDetails consolidationDetails1 : consolidationDetails) {
                consolidationDetailsMap.put(consolidationDetails1.getId(), consolidationDetails1);
                tenantIds.add(consolidationDetails1.getTenantId());
                userNames.add(consolidationDetails1.getCreatedBy());
            }
        }
        userNames.add(shipmentDetails.get().getCreatedBy());
        userNames.add(shipmentDetails.get().getAssignedTo());
        tenantIds.add(shipmentDetails.get().getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(userNames, userEmailsMap)), executorService);
        var tenantsDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettingsAndTenantsData(tenantIds, tenantSettingsMap, tenantsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, tenantsDataFuture, userEmailsFuture).join();
        for (ConsolidationDetails consolidationDetails1 : consolidationDetails) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails.get(), consolidationDetails1, SHIPMENT_PUSH_WITHDRAW, withdrawRemarks, emailTemplatesMap, requestedTypes, null, null, userEmailsMap, tenantSettingsMap, null, tenantsMap);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    private String getWarningMsg(Set<ShipmentRequestedType> shipmentRequestedTypes) {
        return !shipmentRequestedTypes.isEmpty() ? TEMPLATE_NOT_FOUND_MESSAGE : null;
    }

    // This method will process AIB Push request
    @Override
    public ResponseEntity<IRunnerResponse> aibPushRequest(Long shipId, Long consoleId, String remarks) throws RunnerException {
        commonUtils.setInterBranchContextForColoadStation();
        List<ConsoleShipmentMapping> consoleShipMappings = consoleShipmentMappingDao.findByShipmentIdAll(shipId);
        List<ConsoleShipmentMapping> pullRequests = new ArrayList<>();
        List<ConsoleShipmentMapping> pushRequests = new ArrayList<>();
        for (var consoleShip : consoleShipMappings) {
            ResponseEntity<IRunnerResponse> buildFailedResponse = checkAlreadyExistingConsole(consoleId, consoleShip);
            if (buildFailedResponse != null) return buildFailedResponse;
            updatePullRequests(consoleShip, pullRequests, pushRequests);
        }
        awbDao.validateAirMessaging(consoleId);
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).orElseThrow(() -> new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consoleId).orElseThrow(() -> new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        if (consolidationV3Service.checkForAirDGFlag(consolidationDetails)) {
            if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                return ResponseHelper.buildFailedResponse(String.format(AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION, consolidationDetails.getConsolidationNumber()));
            }
            if (Boolean.TRUE.equals(consolidationDetails.getHazardous())) {
                return ResponseHelper.buildFailedResponse(String.format(AIR_DG_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_SHIPMENT, shipmentDetails.getShipmentId()));
            }
        }
        ConsoleShipmentMapping entity = ConsoleShipmentMapping.builder()
                .shipmentId(shipId)
                .consolidationId(consoleId)
                .isAttachmentDone(false)
                .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                .build();

        boolean isImportShipment = false;
        if (Constants.DIRECTION_IMP.equalsIgnoreCase(shipmentDetails.getDirection())) {
            isImportShipment = true;

            consolidationV3Service.attachShipments(
                    ShipmentConsoleAttachDetachV3Request.builder()
                            .shipmentRequestedType(ShipmentRequestedType.APPROVE)
                            .consolidationId(consoleId)
                            .shipmentIds(Set.of(shipId)).build());

            var emailTemplatesRequests = commonUtils.getEmailTemplates(IMPORT_SHIPMENT_PUSH_ATTACHMENT_EMAIL);
            if (Objects.isNull(emailTemplatesRequests) || emailTemplatesRequests.isEmpty())
                return ResponseHelper.buildSuccessResponseWithWarning(TEMPLATE_NOT_FOUND_MESSAGE);
            sendImportShipmentPushAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequests);
        }
        if (!isImportShipment) {
            Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
            consoleShipmentMappingDao.save(entity);
            sendEmailForNonImportShipment(shipId, consoleId, remarks, pullRequests, shipmentRequestedTypes, pushRequests);
            String warning = getWarningMsg(shipmentRequestedTypes);
            return ResponseHelper.buildSuccessResponseWithWarning(warning);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Nullable
    private static ResponseEntity<IRunnerResponse> checkAlreadyExistingConsole(Long consoleId, ConsoleShipmentMapping consoleShip) {
        if (!Objects.equals(consoleShip.getConsolidationId(), consoleId) && Boolean.TRUE.equals(consoleShip.getIsAttachmentDone())) {
            return ResponseHelper.buildFailedResponse("These is already consolidation exist in shipment. Please detach and update shipment first.");
        }
        if (Objects.equals(consoleShip.getConsolidationId(), consoleId)) {
            return ResponseHelper.buildSuccessResponse();
        }
        return null;
    }

    private void updatePullRequests(ConsoleShipmentMapping consoleShip, List<ConsoleShipmentMapping> pullRequests, List<ConsoleShipmentMapping> pushRequests) {
        if (ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.equals(consoleShip.getRequestedType()))
            pullRequests.add(jsonHelper.convertValue(consoleShip, ConsoleShipmentMapping.class));
        if (ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.equals(consoleShip.getRequestedType()))
            pushRequests.add(jsonHelper.convertValue(consoleShip, ConsoleShipmentMapping.class));
    }

    private void sendEmailForNonImportShipment(Long shipId, Long consoleId, String remarks, List<ConsoleShipmentMapping> pullRequests, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> pushRequests) {
        if (!pullRequests.isEmpty()) {
            pullRequests.forEach(e -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(e.getConsolidationId(), e.getShipmentId()));
            sendEmailForPullRequestReject(shipId, pullRequests.stream().map(e -> e.getConsolidationId()).toList(), shipmentRequestedTypes,
                    remarks + "<br>Target Shipment has been requested to attach with an another consolidation already. ",
                    pullRequests);
        }
        if (!pushRequests.isEmpty()) {
            pushRequests.forEach(e -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(e.getConsolidationId(), e.getShipmentId()));
            sendEmailForPushRequestWithdrawl(shipId, List.of(pushRequests.get(0).getConsolidationId()), shipmentRequestedTypes, remarks);
        }
        sendEmailForPushRequested(shipId, consoleId, shipmentRequestedTypes);
    }

    public void sendEmailForPushRequested(Long shipmentId, Long consoleId, Set<ShipmentRequestedType> requestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap = new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> locationsMap = new HashMap<>();
        Map<String, CarrierMasterData> carriersMap = new HashMap<>();
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> userNames = new HashSet<>();

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        setColoadingStation(shipmentDetails);
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consoleId).get();

        userNames.add(shipmentDetails.getCreatedBy());
        userNames.add(shipmentDetails.getAssignedTo());
        userNames.add(consolidationDetails.getCreatedBy());
        tenantIds.add(consolidationDetails.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carriersMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), locationsMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, tenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(userNames, userEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PUSH_REQUESTED, null, emailTemplatesMap, requestedTypes, locationsMap, carriersMap, userEmailsMap, tenantSettingsMap, null, null);
        } catch (Exception e) {
            log.error("Error while sending email");
        }
    }

    private void setColoadingStation(ShipmentDetails request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if (Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(request.getDirection(), Constants.DIRECTION_EXP)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    private ResponseEntity<IRunnerResponse> sendImportShipmentPushAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                                                  List<EmailTemplatesRequest> emailTemplates) {
        var emailTemplateModel = emailTemplates.stream().findFirst().orElse(new EmailTemplatesRequest());

        List<String> toEmailList = new ArrayList<>();
        List<String> ccEmailsList = new ArrayList<>();
        if (consolidationDetails.getCreatedBy() != null)
            toEmailList.add(consolidationDetails.getCreatedBy());

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(shipmentDetails.getTenantId());
        tenantIds.add(consolidationDetails.getTenantId());

        Map<String, Object> dictionary = new HashMap<>();
        Map<String, UnlocationsResponse> locationsMap = new HashMap<>();
        Map<String, CarrierMasterData> carriersMap = new HashMap<>();

        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carriersMap)), executorService);
        var locationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), locationsMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);

        CompletableFuture.allOf(carrierFuture, locationsFuture, toAndCcEmailIdsFuture).join();

        commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, shipmentDetails.getTenantId(), true);
        ccEmailsList.addAll(new ArrayList<>(toEmailIds));
        ccEmailsList.addAll(new ArrayList<>(ccEmailIds));
        if (consolidationDetails.getCreatedBy() == null) {
            toEmailIds.clear();
            ccEmailIds.clear();
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, consolidationDetails.getTenantId(), false);
            toEmailList.addAll(new ArrayList<>(toEmailIds));
        }

        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carriersMap, locationsMap);
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, toEmailList, ccEmailsList);
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel) {
        AttachListShipmentRequest request = (AttachListShipmentRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (!consolidationDetails.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getConsolidationId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettings = commonUtils.getShipmentSettingFromContext();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        log.info("{} | attachListShipment | Final Criteria: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listRequest));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, ShipmentService.tableNames);
        Specification<ShipmentDetails> spec = tuple.getLeft();
        if (shipmentSettings.getIsShipmentLevelContainer() != null && shipmentSettings.getIsShipmentLevelContainer())
            spec = spec.and(ShipmentService.notInConsoleMappingTable());
        else
            spec = spec.and(ShipmentService.notInConsoleMappingTable()).and(ShipmentService.notInContainerMappingTable());
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(spec, tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsPage.getContent(), true, request),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails console) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        boolean setShipmentTypefilter = false;
        boolean isFcl = true;
        boolean isLcl = true;
        Set<ShipmentDetails> shipmentDetailsList = console.getShipmentsList();
        if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && shipmentDetailsList != null && !shipmentDetailsList.isEmpty()) {
            setShipmentTypefilter = true;
            for (var ship : shipmentDetailsList) {
                isFcl = Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL);
                isLcl = Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL);
            }
        }
        List<ConsoleShipmentMapping> consoleShipMappings = consoleShipmentMappingDao.findByConsolidationIdAll(request.getConsolidationId());
        List<Long> excludeShipments = consoleShipMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();

        if (request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()) {
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE, console.getTransportMode(), EQ, request);
        if (excludeShipments != null && !excludeShipments.isEmpty())
            defaultRequest = CommonUtils.andCriteria(ID, excludeShipments, "NOTIN", defaultRequest);

        addDirectionCriteria(console, defaultRequest);
        addShipmentTypeCriteria(console, setShipmentTypefilter, defaultRequest, isFcl, isLcl);
        CommonUtils.andCriteria(Constants.STATUS, 2, "!=", defaultRequest);
        CommonUtils.andCriteria(Constants.STATUS, 3, "!=", defaultRequest);
        if (checkForNonDGConsoleAndAirDgFlagAndNonDGUser(console))
            CommonUtils.andCriteria(CONTAINS_HAZARDOUS, false, EQ, defaultRequest);
        List<FilterCriteria> criterias = defaultRequest.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(Constants.TRANSPORT_MODE).operator("!=").value(Constants.TRANSPORT_MODE_AIR).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        List<FilterCriteria> innerFilers1 = new ArrayList<>();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator("!=").value(Constants.SHIPMENT_TYPE_DRT).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
        CarrierDetails consolidationCarrierDetails = console.getCarrierDetails();
        addCriteriaForAir(console, tenantSettings, consolidationCarrierDetails, defaultRequest);

        defaultRequest = processEtaMatchRequest(request, console, tenantSettings, consolidationCarrierDetails, defaultRequest, innerFilters);
        defaultRequest = processEtdMatchRequest(request, console, tenantSettings, consolidationCarrierDetails, defaultRequest, innerFilters);
        processScheduleMatchRequest(request, console, consolidationCarrierDetails, innerFilters);
        return defaultRequest;
    }

    private void addDirectionCriteria(ConsolidationDetails consolidationDetails, ListCommonRequest defaultRequest) {
        if (!Objects.isNull(consolidationDetails.getShipmentType()))
            CommonUtils.andCriteria(Constants.DIRECTION, consolidationDetails.getShipmentType(), Constants.EQ, defaultRequest);
        else
            CommonUtils.andCriteria(Constants.DIRECTION, "", Constants.IS_NULL, defaultRequest);
    }

    private void addShipmentTypeCriteria(ConsolidationDetails console, boolean setShipmentTypefilter, ListCommonRequest defaultRequest, boolean isFcl, boolean isLcl) {
        if (setShipmentTypefilter) {
            if (isFcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, Constants.CARGO_TYPE_FCL, Constants.EQ, defaultRequest);
            else if (isLcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, Constants.SHIPMENT_TYPE_LCL, Constants.EQ, defaultRequest);
        }
        if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.isNull(console.getContainerCategory())) {
            CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, console.getContainerCategory(), Constants.EQ, defaultRequest);
        }
    }

    private boolean checkForNonDGConsoleAndAirDgFlagAndNonDGUser(ConsolidationDetails console) {
        if (!consolidationV3Service.checkForAirDGFlag(console))
            return false;
        if (Boolean.TRUE.equals(console.getHazardous()))
            return false;
        return !isAirDgUser();
    }

    private boolean isAirDgUser() {
        return UserContext.isAirDgUser();
    }

    private void addCriteriaForAir(ConsolidationDetails console, V1TenantSettingsResponse tenantSettings, CarrierDetails consoleCarrier, ListCommonRequest defaultRequest) {
        if (!Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                || Boolean.FALSE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            if (!Objects.isNull(consoleCarrier.getOriginPort()))
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, consoleCarrier.getOriginPort(), Constants.EQ, defaultRequest);
            else
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, "", Constants.IS_NULL, defaultRequest);
            if (!Objects.isNull(consoleCarrier.getDestinationPort()))
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, consoleCarrier.getDestinationPort(), Constants.EQ, defaultRequest);
            else
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, "", Constants.IS_NULL, defaultRequest);
        }
    }

    private ListCommonRequest processEtaMatchRequest(AttachListShipmentRequest request, ConsolidationDetails console, V1TenantSettingsResponse tenantSettings,
                                                     CarrierDetails consoleCarrier, ListCommonRequest defaultRequest, List<FilterCriteria> innerFilters) {
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        FilterCriteria filterCriteria;
        if (Boolean.TRUE.equals(request.getEtaMatch())) {
            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(console.getShipmentType(), Constants.DIRECTION_EXP)
                    && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consoleCarrier.getEta())) {
                    LocalDateTime eta = consoleCarrier.getEta();
                    var thresholdETAFrom = eta.plusDays(-1);
                    var thresholdETATo = eta.plusDays(1);

                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETAFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETATo, "<=", defaultRequest);
                }
            } else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consoleCarrier.getEta()))
                    criteria = Criteria.builder().fieldName("eta").operator(Constants.EQ).value(consoleCarrier.getEta()).build();
                else
                    criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    private ListCommonRequest processEtdMatchRequest(AttachListShipmentRequest request, ConsolidationDetails console,
                                                     V1TenantSettingsResponse tenantSettings, CarrierDetails consoleCarrier,
                                                     ListCommonRequest defaultRequest, List<FilterCriteria> innerFilters) {
        FilterCriteria filterCriteria;
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        if (Boolean.TRUE.equals(request.getEtdMatch())) {

            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                    && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consoleCarrier.getEtd())) {
                    LocalDateTime etd = consoleCarrier.getEtd();
                    var thresholdETDFrom = etd.plusDays(-1);
                    var thresholdETDTo = etd.plusDays(1);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDTo, "<=", defaultRequest);
                }
            } else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consoleCarrier.getEtd()))
                    criteria = Criteria.builder().fieldName("etd").operator(Constants.EQ).value(consoleCarrier.getEtd()).build();
                else
                    criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    private List<IRunnerResponse> convertEntityListToDtoListForAttachListShipment(List<ShipmentDetails> shipmentDetails, boolean getMasterData, ListCommonRequest request) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<Long> shipmentIdList = shipmentDetails.stream().map(ShipmentDetails::getId).toList();
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);
        List<AttachListShipmentResponse> attachListShipmentResponse = AttachListShipmentMapper.INSTANCE.toAttachListShipmentResponse(shipmentDetails);
        attachListShipmentResponse.forEach(response -> {
            if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
            int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
            response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            responseList.add(response);
        });
        shipmentMasterDataHelper.getMasterDataForList(shipmentDetails, responseList, getMasterData, true, request.getIncludeColumns().stream().collect(Collectors.toSet()));

        return responseList;
    }

    private void processScheduleMatchRequest(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails, CarrierDetails consolidationCarrierDetails, List<FilterCriteria> innerFilters) {
        if(Boolean.TRUE.equals(request.getScheduleMatch())){
            if(Objects.equals(consolidationDetails.getTransportMode(),Constants.TRANSPORT_MODE_AIR)){
                processAirScheduledMatchRequest(consolidationCarrierDetails, innerFilters);
            }
            else if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA)){
                processSeaScheduledMatchRequest(consolidationCarrierDetails, innerFilters);
            }
        }
    }

    private void processAirScheduledMatchRequest(CarrierDetails consoleCarrier, List<FilterCriteria> innerFilters) {
        List<FilterCriteria> innerFilers1;
        Criteria criteria;
        FilterCriteria filterCriteria;
        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consoleCarrier.getFlightNumber()))
            criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.EQ).value(consoleCarrier.getFlightNumber()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consoleCarrier.getShippingLine()))
            criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.EQ).value(consoleCarrier.getShippingLine()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
    }
    private void processSeaScheduledMatchRequest(CarrierDetails consoleCarrier, List<FilterCriteria> innerFilters) {
        Criteria criteria;
        List<FilterCriteria> innerFilers1;
        FilterCriteria filterCriteria;
        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consoleCarrier.getVessel()))
            criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.EQ).value(consoleCarrier.getVessel()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        innerFilers1 = new ArrayList<>();
        if(!Objects.isNull(consoleCarrier.getVoyage()))
            criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.EQ).value(consoleCarrier.getVoyage()).build();
        else
            criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);
    }

    /**
     * This method will provide Interbranch Pending Notifications - Includes both Pull & Push requests for shipment
     * @param commonRequestModel
     * @return
     */

    @Override
    public ResponseEntity<IRunnerResponse> aibPendingNotification(CommonRequestModel commonRequestModel) {
        AibNotificationRequest request = (AibNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingShipmentActionsResponse> response = new PendingNotificationResponse<>();
        if(Objects.isNull(request.getId())) {
            log.info("Received empty request for pending notification in shipments", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(response);
        }
        var notificationMap = getNotificationMap(request);
        response.setNotificationMap(notificationMap);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private Map<Long, List<PendingShipmentActionsResponse>> getNotificationMap(AibNotificationRequest request) {
        // Get data of all consolidation pulling this shipment that are not yet attached
        Map<Long, List<PendingShipmentActionsResponse>> notificationResultMap = new HashMap<>();

        if(commonUtils.getCurrentTenantSettings() == null || !Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled())) {
            return notificationResultMap;
        }

        try {
            ListCommonRequest listRequest = constructListCommonRequest("shipmentId", request.getId(), Constants.EQ);
            listRequest = andCriteria(Constants.IS_ATTACHMENT_DONE, false, Constants.EQ, listRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> consoleShipMappingPair = fetchData(listRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> mappingPage = consoleShipmentMappingDao.findAll(consoleShipMappingPair.getLeft(), consoleShipMappingPair.getRight());

            List<Long> consolidationIds = mappingPage.getContent().stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            final var consoleShipmentsMap = mappingPage.getContent().stream().collect(Collectors.toMap(
                    ConsoleShipmentMapping::getConsolidationId, Function.identity(), (oldVal, newVal) -> oldVal)
            );

            commonUtils.setInterBranchContextForColoadStation();

            listRequest = constructListCommonRequest(Constants.ID, consolidationIds, Constants.IN);
            listRequest.setContainsText(request.getContainsText());
            listRequest.setSortRequest(request.getSortRequest());
            Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listRequest, ConsolidationDetails.class, ConsolidationService.tableNames);
            Page<ConsolidationDetails> consolPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());

            Set tenantIds = new HashSet<String>();
            Set locations =  new HashSet<String>();
            final CarrierDetails nullCarrierDetails = new CarrierDetails();
            consolPage.getContent().stream().forEach(i -> {
                tenantIds.add(StringUtility.convertToString(i.getTenantId()));
                var carrierDetails = Optional.ofNullable(i.getCarrierDetails()).orElse(nullCarrierDetails);
                locations.add(carrierDetails.getOriginPort());
                locations.add(carrierDetails.getDestinationPort());
            });
            Map<String, TenantModel> v1TenantData = masterDataUtils.fetchInTenantsList(tenantIds);
            Map<String, EntityTransferUnLocations> v1LocationData = masterDataUtils.fetchInBulkUnlocations(locations, EntityTransferConstants.LOCATION_SERVICE_GUID);

            masterDataUtils.pushToCache(v1TenantData, CacheConstants.TENANTS, tenantIds, new TenantModel(), null);
            masterDataUtils.pushToCache(v1LocationData, CacheConstants.UNLOCATIONS, locations, new EntityTransferUnLocations(), null);

            // console id vs list of ship ids
            Map<Long, List<Long>> consoleVsShipIdMap = new HashMap<>();

            // generate mapping for shipment id vs list of pulling consol(s)
            for(var mapping : mappingPage.getContent()) {
                if(!notificationResultMap.containsKey(mapping.getShipmentId())) {
                    notificationResultMap.put(mapping.getShipmentId(), new ArrayList<>());
                }
                if(!consoleVsShipIdMap.containsKey(mapping.getConsolidationId())) {
                    consoleVsShipIdMap.put(mapping.getConsolidationId(), new ArrayList<>());
                }
                consoleVsShipIdMap.get(mapping.getConsolidationId()).add(mapping.getShipmentId());
            }

            consolPage.getContent().stream().forEach(i -> {
                var res = mapToNotification(i, consoleShipmentsMap, v1TenantData, v1LocationData);
                consoleVsShipIdMap.get(i.getId()).forEach(shipId -> notificationResultMap.get(shipId).add(res));
            });

        }
        catch(Exception e) {
            log.error("Error while generating notification map for input Shipment", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
        }

        return notificationResultMap;
    }

    private PendingShipmentActionsResponse mapToNotification(ConsolidationDetails consol, Map<Long, ConsoleShipmentMapping> consoleShipMap,
                                                             Map<String, TenantModel> tenantMap, Map<String, EntityTransferUnLocations> locationsMap) {
        var carrierDetails = Optional.ofNullable(consol.getCarrierDetails()).orElse(new CarrierDetails());
        var tenantData = Optional.ofNullable(tenantMap.get(StringUtility.convertToString(consol.getTenantId()))).orElse(new TenantModel());
        return PendingShipmentActionsResponse.builder()
                .consolId(consol.getId())
                .consolidationNumber(consol.getReferenceNumber())
                .masterBill(consol.getMawb())
                .ata(carrierDetails.getAta())
                .atd(carrierDetails.getAtd())
                .eta(carrierDetails.getEta())
                .etd(carrierDetails.getEtd())
                .pol(Optional.ofNullable(locationsMap.get(carrierDetails.getOriginPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getOriginPort()))
                .pod(Optional.ofNullable(locationsMap.get(carrierDetails.getDestinationPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getDestinationPort()))
                .lat(consol.getLatDate())
                .branch(tenantData.getCode() + " - " + tenantData.getTenantName())
                .branchDisplayName(tenantData.displayName)
                .hazardous(consol.getHazardous())
                .requestedBy(consoleShipMap.get(consol.getId()).getCreatedBy())
                .requestedOn(consoleShipMap.get(consol.getId()).getCreatedAt())
                .requestedType(consoleShipMap.get(consol.getId()).getRequestedType())
                .build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().id(shipmentDetails.get().getId()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ShipmentDetailsResponse completeUpdateShipmentFromEntityTransfer(ShipmentEtV3Request shipmentRequest) throws RunnerException {
        long start = System.currentTimeMillis();
        log.info("{} | starts completeUpdateShipment....", LoggerHelper.getRequestIdFromMDC());
        long mid = System.currentTimeMillis();
        ShipmentV3Request request = jsonHelper.convertValue(shipmentRequest, ShipmentV3Request.class);
        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(request);
        log.info("{} | completeUpdateShipment db query: retrieveByIdOrGuid.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

        try {
            mid = System.currentTimeMillis();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper request.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            entity.setId(oldEntity.get().getId());
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);

            mid = System.currentTimeMillis();
            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper old entity.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            if(Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) &&  checkIfAlreadyPushRequested(oldEntity.get())) {
                throw new ValidationException(ErrorConstants.VALIDATE_JOB_TYPE_CHANGE);
            }
            mid = System.currentTimeMillis();

            log.info("{} | completeUpdateShipment before save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            mid = System.currentTimeMillis();
            entity = shipmentDao.update(entity, false);
            log.info("{} | completeUpdateShipment Update.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            mid = System.currentTimeMillis();
            createAuditLog(entity, jsonHelper.convertToJson(oldConvertedShipment), DBOperationType.UPDATE.name());
            log.info("{} | completeUpdateShipment auditLog.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            mid = System.currentTimeMillis();
            shipmentsV3Util.afterSaveforEt(entity, oldConvertedShipment, false, shipmentRequest, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached, false);
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            ShipmentDetails finalEntity1 = entity;
            String entityPayload = jsonHelper.convertToJson(finalEntity1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalEntity1.getId(), finalEntity1.getGuid())), executorServiceMasterData);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return shipmentDetailsMapper.map(entity);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }

    }

    public ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentEtV3Request request, boolean includeGuid) {

        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(new HashSet<>(jsonHelper.convertValueToList(request.getConsolidationList().stream().toList(), ConsolidationDetails.class)));

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();

            if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            {
                for (Containers container: shipmentDetails.getContainersList()) {
                    addAuditLogContainers(container, null, shipmentId, DBOperationType.CREATE.name());
                }
            }

            shipmentsV3Util.afterSaveforEt(shipmentDetails, null, true, request, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached, includeGuid);

            // audit logs
            createAuditLog(shipmentDetails, null, DBOperationType.CREATE.name());

            ShipmentDetails finalShipmentDetails1 = shipmentDetails;
            String entityPayload = jsonHelper.convertToJson(finalShipmentDetails1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalShipmentDetails1.getId(), finalShipmentDetails1.getGuid())), executorService);
        } catch (Exception e) {
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
    }

    private void addAuditLogContainers(Containers container, Containers prev, Long shipmentId, String operationName) throws RunnerException {
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(container)
                            .prevData(prev)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentId)
                            .operation(operationName).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
            log.error(e.getMessage());
        }
    }

}
