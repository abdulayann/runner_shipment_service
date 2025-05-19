package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerUpdateFileEntitiesRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.MeasurementBasisResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.AdditionalDetailV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.MblDuplicatedLog;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.utils.v3.EventsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.formula.functions.T;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKINGS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MASS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ORDERS_COUNT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_STATUS_FIELDS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPPER_REFERENCE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME_UNIT_M3;
import static com.dpw.runner.shipment.services.commons.constants.Constants.WEIGHT_UNIT_KG;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.getIntFromString;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.roundOffAirShipment;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@SuppressWarnings("ALL")
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
    private BookingIntegrationsUtility bookingIntegrationsUtility;
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
    @Autowired
    private KafkaProducer kafkaProducer;


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
            @Lazy BookingIntegrationsUtility bookingIntegrationsUtility,
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
            MasterDataHelper masterDataHelper, @Lazy IRoutingsV3Service routingsV3Service, IPackingService packingService) {
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
        this.bookingIntegrationsUtility = bookingIntegrationsUtility;
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
            List<IRunnerResponse> filteredList = convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData, shipmentListResponses, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()), listCommonRequest.getNotificationFlag());

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
        var pendingNotificationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setPendingCount(shipmentId, pendingCount)), executorService);
        var implicationListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> setImplicationsResponse(guid, implications)), executorService);

        ShipmentRetrieveLiteResponse response = modelMapper.map(shipmentDetailsEntity, ShipmentRetrieveLiteResponse.class);
        log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
        CompletableFuture.allOf(pendingNotificationFuture, implicationListFuture).join();
        if (response.getStatus() != null && response.getStatus() < ShipmentStatus.values().length)
            response.setShipmentStatus(ShipmentStatus.values()[response.getStatus()].toString());
        response.setPendingActionCount((pendingCount.get() == 0) ? null : pendingCount.get());
        // set dps implications
        response.setImplicationList(implications);
        //add isPacksAvailable flag
        if (!CollectionUtils.isEmpty(shipmentDetailsEntity.getPackingList())) {
            response.setIsPacksAvailable(Boolean.TRUE);
        }
        Set<Containers> containersList = shipmentDetailsEntity.getContainersList();
        if (!CollectionUtils.isEmpty(containersList)) {
            setCounterCountAndTeuCount(response, containersList);
        }
        return response;
    }

    private void setCounterCountAndTeuCount(ShipmentRetrieveLiteResponse response, Set<Containers> containersList) {
        long shipmentCont = containersList.stream()
                .mapToLong(Containers::getContainerCount)
                .sum();
        Map<String, Object> cacheMap = new HashMap<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();
        Double consoleTeu = 0.0;
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

    private Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
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

        ShipmentDetails shipmentDetails = includeGuid ? jsonHelper.convertValue(request, ShipmentDetails.class) : jsonHelper.convertCreateValue(request, ShipmentDetails.class);

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            boolean syncConsole = false;

            beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails, includeGuid);
            shipmentDetails.setConsolidationList(null);
            shipmentDetails.setContainersList(null);

            shipmentDetails = getShipment(shipmentDetails);

            afterSave(shipmentDetails, null, true, request, shipmentSettingsDetails, syncConsole, isFromET);

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

//            ShipmentDetails finalShipmentDetails1 = shipmentDetails;
//            String entityPayload = jsonHelper.convertToJson(finalShipmentDetails1);
//            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalShipmentDetails1.getId(), finalShipmentDetails1.getGuid())), executorService);
            // Trigger Kafka event for PushToDownStreamServices
            this.triggerPushToDownStream(shipmentDetails, true);
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
            beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, false);
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
            afterSave(entity, oldConvertedShipment, false, shipmentRequest, shipmentSettingsDetails, syncConsole, isFromET);
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
//            ShipmentDetails finalEntity1 = entity;
//            String entityPayload = jsonHelper.convertToJson(finalEntity1);
//            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalEntity1.getId(), finalEntity1.getGuid())), executorServiceMasterData);
            // Trigger Kafka event for PushToDownStreamServices
            this.triggerPushToDownStream(entity, false);
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
    protected void beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean isImportFile) throws RunnerException {
        shipmentValidationV3Util.validateStaleShipmentUpdateError(shipmentDetails, isCreate);

        /* Future to populate unloc code in shipment child entities*/
        CompletableFuture<Void> populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, oldEntity);

        processVoyageAndFlightNumber(shipmentDetails);

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        Set<ConsolidationDetails> consolidationDetails = oldEntity != null ? oldEntity.getConsolidationList() : new HashSet<>();
        shipmentValidationV3Util.processDGValidations(shipmentDetails, oldEntity, consolidationDetails);

        if (Boolean.TRUE.equals(shipmentRequest.getIsChargableEditable())) {
            shipmentDetails.setChargable(shipmentRequest.getChargable());
        }
        validateBeforeSave(shipmentDetails);

        processBranchesAndPartner(shipmentDetails);

        if (Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
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
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, boolean isFromET) throws RunnerException {
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

//        pushShipmentDataToDependentService(shipmentDetails, oldEntity, isCreate, shipmentRequest, isFromET);

        if (!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }

        // Delete the shipment pending pull/push request tasks when the shipment got cancelled
        deletePendingStateAfterCancellation(shipmentDetails, oldEntity);
        log.info("shipment afterSave consoleShipmentMappingDao.deletePendingStateByShipmentId..... ");
        processSyncV1AndAsyncFunctions(shipmentDetails, oldEntity, shipmentSettingsDetails, syncConsole, consolidationDetails);
        log.info("shipment afterSave end..... ");
    }

    private void triggerPushToDownStream(ShipmentDetails shipmentDetails, Boolean isCreate){
        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(shipmentDetails.getConsolidationList().iterator().next().getId());
        }

        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(shipmentDetails.getId())
                .parentEntityName(SHIPMENT)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .isCreate(isCreate)
                        .build())
                .build();
        if(!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
            PushToDownstreamEventDto.Triggers triggers = PushToDownstreamEventDto.Triggers.builder()
                    .entityId(consoleShipmentMappings.get(0).getConsolidationId())
                    .entityName(Constants.CONSOLIDATION)
                    .build();
            List<PushToDownstreamEventDto.Triggers> triggersList = consoleShipmentMappings.stream().filter(x-> !Objects.equals(x.getShipmentId(), shipmentDetails.getId())).map(x -> {
                return PushToDownstreamEventDto.Triggers.builder()
                        .entityId(x.getShipmentId())
                        .entityName(SHIPMENT)
                        .build();
            }).toList();
            triggersList.add(triggers);
            pushToDownstreamEventDto.setTriggers(triggersList);
        }
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, shipmentDetails.getId().toString());
    }

    private void processSyncV1AndAsyncFunctions(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, ConsolidationDetails consolidationDetails) {
        // Syncing shipment to V1
        syncShipment(shipmentDetails, consolidationDetails, syncConsole);
        log.info("shipment afterSave syncShipment..... ");
//        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
//            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity)), executorService);
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails, oldEntity, false)), executorService);
    }

    private void syncShipment(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, boolean syncConsole) {
        String transactionId = shipmentDetails.getGuid().toString();
        try {
            shipmentSync.sync(shipmentDetails, null, null, transactionId, false);
        } catch (Exception e) {
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        if (syncConsole && consolidationDetails != null) {
            try {
                consolidationSync.sync(consolidationDetails, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
    }

    @Override
    public void syncShipmentsList(List<ShipmentDetails> shipments, String transactionId) {
        for (ShipmentDetails shipmentDetails : shipments) {
            try {
                shipmentSync.sync(shipmentDetails, null, null, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }
    }

    protected void deletePendingStateAfterCancellation(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) && Objects.nonNull(oldEntity)
                && !Objects.equals(oldEntity.getStatus(), shipmentDetails.getStatus()) && Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            log.info("Request: {} | Deleting console_shipment_mapping due to shipment cancelled for shipment: {}", LoggerHelper.getRequestIdFromMDC(), shipmentDetails.getShipmentId());
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
        }
    }

    private void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentV3Request shipmentRequest, boolean isFromET) {
        if (!isFromET) {
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, isCreate, Boolean.TRUE.equals(shipmentRequest.getIsAutoSellRequired()), Optional.ofNullable(oldEntity).map(ShipmentDetails::getContainersList).orElse(null));
            log.info("shipment afterSave pushShipmentDataToDependentService..... ");
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

    protected ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Set<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if (!setIsNullOrEmpty(consolidationList)) {
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList.iterator().next().getId(), shipment, null);
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
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, dgFlag);
            return consolidationDetails;
        }
        return null;
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

    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity) throws RunnerException {
        Set<ConsolidationDetails> consolidationList = oldEntity != null ? oldEntity.getConsolidationList() : new HashSet<>();
        ConsolidationDetails consolidationDetails;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (!setIsNullOrEmpty(consolidationList)) ? consolidationList.iterator().next() : null;
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if (consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        boolean makeConsoleDG = checkForDGShipmentAndAirDgFlag(shipment) || checkForOceanDGShipment(shipment);
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if (linkedConsol != null && isDiffPresentInOldNewShipment(shipment, oldEntity)) {
            consolidationDetails = processLinkedConsolidationDetails(shipment, oldEntity, consolidationList, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipment.getContainsHazardous()));
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

    protected ConsolidationDetails processLinkedConsolidationDetails(ShipmentDetails shipment, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationList, boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        consolidationDetails = consolidationDetailsDao.findById(consolidationList.iterator().next().getId()).get();
        consolidationDetails.setBol(shipment.getMasterBill());
        if (consolidationDetails.getCarrierDetails() == null)
            consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
        consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
        consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
        consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
        consolidationDetails.setShipmentType(shipment.getDirection());

        if (makeConsoleDG)
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
        if (!Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP))
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

    protected void validateBeforeSave(ShipmentDetails shipmentDetails) {
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

    private void processVoyageAndFlightNumber(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getCarrierDetails() != null) {
            if (shipmentDetails.getTransportMode() != null && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setVoyage(null);
            } else {
                shipmentDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
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

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst, boolean getMasterData, List<ShipmentListResponse> shipmentListResponses, Set<String> includeColumns, Boolean notificationFlag) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        Map<Long, ShipmentDetails> shipmentDetailsMap = lst.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
        // Pending Notification Count
        if (Boolean.TRUE.equals(notificationFlag)) {
            List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
            var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
            var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);
            shipmentListResponses.forEach(response -> {
                int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
                response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            });
        }
        shipmentListResponses.forEach(response -> {
            var ship = shipmentDetailsMap.get(response.getId());
            if (includeColumns.contains(SHIPPER_REFERENCE))
                setShipperReferenceNumber(response, ship);
            if (includeColumns.contains(SHIPMENT_STATUS_FIELDS) && ship.getStatus() != null && ship.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[ship.getStatus()].toString());
            if (includeColumns.contains(ORDERS_COUNT) && ObjectUtils.isNotEmpty(ship.getShipmentOrders()))
                response.setOrdersCount(ship.getShipmentOrders().size());
            responseList.add(response);
        });
        shipmentMasterDataHelper.getMasterDataForList(lst, responseList, getMasterData, true, includeColumns);
        return responseList;
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
    public void updateCargoDetailsInShipment(Long shipmentId, CargoDetailsResponse cargoDetailsResponse) {
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
        includeColumns.addAll(FieldUtils.getTenantIdAnnotationFields(List.of(createFieldClassDto(ShipmentDetails.class, null))));
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
    public ShipmentSailingScheduleResponse updateSailingScheduleDataToShipment(ShipmentSailingScheduleRequest request) throws RunnerException {
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(request.getRoutings());
        Long shipmentId = request.getRoutings().stream().findFirst().get().getShipmentId();
        bulkUpdateRoutingsRequest.setEntityId(shipmentId);
        routingsV3Service.updateBulk(bulkUpdateRoutingsRequest, SHIPMENT);
        //update shipment fields
        //Long shipmentId = request.getRoutings().stream().findFirst().get().getShipmentId();
        Optional<ShipmentDetails> shipmentDetailsEntity = shipmentDao.findById(shipmentId);
        ShipmentDetails shipmentDetails = shipmentDetailsEntity.get();
        updateCutoffDetailsToShipment(request, shipmentDetails);
        shipmentDetails.getCarrierDetails().setShippingLine(request.getCarrier());
        shipmentDao.update(shipmentDetails, false);
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
        ListCommonRequest listCommonRequest = constructListCommonRequest(CONSOLIDATION_ID, consolidationId, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentService.tableNames);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetails.getContent().stream().collect(Collectors.toMap(e -> e.getId(), Function.identity()));
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();
        response.setShipmentsList(jsonHelper.convertValueToList(shipmentDetails.getContent(), ShipmentPacksAssignContainerTrayDto.Shipments.class));
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            shipments.setPacksList(jsonHelper.convertValueToList(
                    shipmentDetailsMap.containsKey(shipments.getId()) ? shipmentDetailsMap.get(shipments.getId()).getPackingList().stream().toList() : new ArrayList<>(),
                    ShipmentPacksAssignContainerTrayDto.Shipments.Packages.class));
        }
        List<ShipmentsContainersMapping> shipmentsContainersMappingsList = shipmentsContainersMappingDao.findByContainerId(containerId);
        List<Long> assignedShipmentsList = shipmentsContainersMappingsList.stream().map(e -> e.getShipmentId()).toList();
        response.setIsFCLShipmentAssigned(false);
        for (ShipmentPacksAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            if (assignedShipmentsList.contains(shipments.getId())) {
                shipments.setSelectedContainerAssigned(true);
                if (CARGO_TYPE_FCL.equals(shipments.getShipmentType())) {
                    response.setIsFCLShipmentAssigned(true);
                    response.setAssignedFCLShipment(shipments.getId());
                }
            } else {
                shipments.setSelectedContainerAssigned(false);
            }
        }
        return response;
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
        for (ShipmentPacksUnAssignContainerTrayDto.Shipments shipments : response.getShipmentsList()) {
            shipments.setPacksList(jsonHelper.convertValueToList(
                    shipmentDetailsMap.get(shipments.getId()).getPackingList().stream().filter(e -> containerId.equals(e.getContainerId())).toList(),
                    ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.class));
        }
        return response;
    }

//    private Long assignFirstBookingContainerToShipmentCargo(List<Containers> expandedContainers, ShipmentDetails shipmentDetails) throws RunnerException {
//        Long containerId = null;
//        for(int i=0;i<expandedContainers.size();i++) {
//            Containers containers = expandedContainers.get(i);
//            containers.setAssigned(true);
//            if(i == 0) {
//                containerV3Service.addShipmentCargoToContainer(containers, shipmentDetails);
//                containerV3Util.setContainerNetWeight(containers);
//                containerId = containers.getId();
//            } else {
//                containerV3Util.resetContainerDataForRecalculation(containers);
//            }
//        }
//        return containerId;
//    }

    @Override
    public ShipmentDetailsV3Response createShipmentInV3(CustomerBookingV3Request customerBookingRequest) throws RunnerException {
        Set<ConsolidationDetailsRequest> consolidationDetails = new HashSet<>();
        Set<ContainerRequest> containerList = new HashSet<>();
        if (isConsoleCreationNeededV3(customerBookingRequest)) {
            ConsolidationDetailsV3Request consolidationDetailsV3Request = ConsolidationDetailsV3Request.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .eta(customerBookingRequest.getCarrierDetails().getEta())
                            .etd(customerBookingRequest.getCarrierDetails().getEtd())
                            .ata(customerBookingRequest.getCarrierDetails().getAta())
                            .atd(customerBookingRequest.getCarrierDetails().getAtd())
                            .build()).
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

            ConsolidationDetailsResponse consolDetailsResponse = consolidationV3Service.createConsolidationForBooking(CommonRequestModel.buildRequest(consolidationDetailsV3Request), customerBookingRequest);

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
        //todo: remove this: aditya
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
            //TODO: pushing to dependent services
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, true, false, null);
            setShipmentFromBooking(shipmentDetails, notesRequest);

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

    //todo: add new fields to shipment
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
        ConsolidationDetails consolidationDetails = null;
        if (!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())) {
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }
        if (consolidationDetails != null && !Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            boolean consolUpdated = false;
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);
                setOriginBranchFromExportBroker(shipmentDetails);
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
                consolidationDetailsDao.save(consolidationDetails, false);
            }
        }
        if (consolidationDetails == null) {
            populateImportExportBrokerForShipment(shipmentDetails);
        }
    }

    private void setOriginBranchFromExportBroker(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getExportBroker() != null)
            shipmentDetails.setOriginBranch(Long.valueOf(shipmentDetails.getAdditionalDetails().getExportBroker().getTenantId()));
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

    public void setShipmentFromBooking(ShipmentDetails shipmentDetails, List<NotesRequest> notesRequest) {
        try {
            shipmentDetails.setNotesList(null);
            shipmentSync.syncFromBooking(shipmentDetails, null, notesRequest);
        } catch (Exception e) {
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
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
            if (request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !isStringNullOrEmpty(request.getShipmentType()) && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
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
        if ((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) &&
                Objects.equals(request.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) ||
                Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
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
        if(!TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) || Objects.isNull(shipmentDetails.getContainerAssignedToShipmentCargo()))
            return;
        containerV3Service.updateAttachedContainersData(List.of(shipmentDetails.getContainerAssignedToShipmentCargo()));
    }
}
