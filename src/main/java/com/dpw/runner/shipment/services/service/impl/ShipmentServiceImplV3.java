package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AUTO_REJECTION_REMARK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKINGS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FTL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CLIENT_PARTY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSIGNEE_PARTY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSIGNOR_PARTY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_OCEAN_APPROVAL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_CTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.EQ;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ERROR_MESSAGE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ERROR_WHILE_SENDING_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.IMPORT_SHIPMENT_PUSH_ATTACHMENT_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ORDERS_COUNT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.PENDING_ACTION_TASK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_WITH_SQ_BRACKETS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_STATUS_FIELDS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_DRT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPPER_REFERENCE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.PADDING_10_PX;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.STYLE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.COMMERCIAL_APPROVE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.COMMERCIAL_REQUEST;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.DG_APPROVE;
import static com.dpw.runner.shipment.services.commons.enums.DBOperationType.DG_REQUEST;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_WITHDRAW;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.roundOffAirShipment;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AibActionShipment;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
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
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.AttachListShipmentMapper;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskApproveOrRejectRequest;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ConsoleShipmentData;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.AdditionalDetailV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
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
import com.dpw.runner.shipment.services.entity.OceanDGRequestLog;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
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
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.EventsV3Util;
import com.dpw.runner.shipment.services.utils.v3.NpmContractV3Util;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
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
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
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
    @Lazy
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
    private INetworkTransferDao networkTransferDao;
    private IDateTimeChangeLogService dateTimeChangeLogService;
    private IConsolidationDetailsDao consolidationDetailsDao;
    private IPartiesDao partiesDao;
    private IRoutingsDao routingsV3Dao;
    private IPackingDao packingDao;
    private IContainerDao containerDao;
    private DependentServiceHelper dependentServiceHelper;
    private IEventDao eventDao;
    private IEventsV3Service eventsV3Service;
    private IOrderManagementAdapter orderManagementAdapter;
    private V1ServiceUtil v1ServiceUtil;
    private IAwbDao awbDao;
    private IDocumentManagerService documentManagerService;
    private IHblService hblService;
    private IPackingV3Service packingV3Service;
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
    private INPMServiceAdapter npmServiceAdapter;
    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;
    @Autowired
    private KafkaProducer kafkaProducer;
    @Autowired
    private NpmContractV3Util npmContractV3Util;
    @Autowired
    private PackingV3Util packingV3Util;

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
            IContainerDao containerDao,
            IOrderManagementAdapter orderManagementAdapter,
            V1ServiceUtil v1ServiceUtil,
            DependentServiceHelper dependentServiceHelper,
            IEventDao eventDao,
            IEventsV3Service eventsV3Service,
            IAwbDao awbDao,
            IDocumentManagerService documentManagerService,
            IHblService hblService,
            NetworkTransferV3Util networkTransferV3Util,
            ITruckDriverDetailsDao truckDriverDetailsDao,
            IReferenceNumbersDao referenceNumbersDao,
            ShipmentsV3Util shipmentsV3Util,
            EventsV3Util eventsV3Util,
            ShipmentValidationV3Util shipmentValidationV3Util,
            IShipmentsContainersMappingDao shipmentsContainersMappingDao,
            IDpsEventService dpsEventService, ModelMapper modelMapper,
            @Lazy ConsolidationV3Service consolidationV3Service,
            MasterDataHelper masterDataHelper, @Lazy IRoutingsV3Service routingsV3Service,
            IPackingV3Service packingV3Service, INPMServiceAdapter npmServiceAdapater,
            INetworkTransferDao networkTransferDao,
            INotificationService notificationService, IMDMServiceAdapter mdmServiceAdapter) {
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.notificationDao = notificationDao;
        this.commonUtils = commonUtils;
        this.shipmentRepository = shipmentRepository;
        this.shipmentDao = shipmentDao;
        this.shipmentMasterDataHelper = shipmentMasterDataHelper;
        this.jsonHelper = jsonHelper;
        this.hblDao = hblDao;
        this.packingDao = packingDao;
        this.containerDao = containerDao;
        this.masterDataUtils = masterDataUtils;
        this.auditLogService = auditLogService;
        this.logsHistoryService = logsHistoryService;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.partiesDao = partiesDao;
        this.routingsV3Dao = routingsDao;
        this.orderManagementAdapter = orderManagementAdapter;
        this.v1ServiceUtil = v1ServiceUtil;
        this.dependentServiceHelper = dependentServiceHelper;
        this.eventDao = eventDao;
        this.eventsV3Service = eventsV3Service;
        this.awbDao = awbDao;
        this.documentManagerService = documentManagerService;
        this.hblService = hblService;
        this.routingsV3Service = routingsV3Service;
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
        this.npmServiceAdapter = npmServiceAdapater;
        this.networkTransferDao = networkTransferDao;
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

    public Optional<ShipmentDetails> retrieveForNte(CommonGetRequest request) throws RunnerException, AuthenticationException {
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
        setConsoleAndNteInfo(shipmentId, response);
        setDgPackCountAndType(shipmentDetailsEntity, response);
        setMainCarriageFlag(shipmentDetailsEntity, response);
        response.setContainerCount(shipmentRetrieveLiteResponse.getContainerCount());
        response.setTeuCount(shipmentRetrieveLiteResponse.getTeuCount());
        return response;
    }

    private void setConsoleAndNteInfo(Long shipmentId, ShipmentRetrieveLiteResponse response) {
        try {
            Optional<ConsolidationDetails> console = Optional.empty();
            Boolean isInterBranchShip = false;
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentId);
            if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
                Long consolidationId = consoleShipmentMappings.get(0).getConsolidationId();
                response.setConsolidationId(consolidationId);
                console = consolidationV3Service.getConsolidationDetails(consolidationId, null);
                response.setConsolBookingNumber(console.get().getBookingNumber());
                isInterBranchShip = Boolean.TRUE.equals(console.get().getInterBranchConsole());
                response.setIsInterBranchConsoleAttached(isInterBranchShip && !Objects.equals(TenantContext.getCurrentTenant(), console.get().getTenantId()));

            }
            if (Objects.equals(response.getTransportMode(), TRANSPORT_MODE_AIR) &&
                    (Objects.equals(response.getJobType(), SHIPMENT_TYPE_DRT) || isInterBranchShip) && response.getReceivingBranch() != null) {
                String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(response.getId(), SHIPMENT, response.getReceivingBranch().intValue());
                response.setTransferStatus(transferStatus);
            } else if (console.get() != null && console.get().getReceivingBranch() != null) {
                String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(console.get().getId(), CONSOLIDATION, console.get().getReceivingBranch().intValue());
                response.setTransferStatus(transferStatus);
            }
        } catch (Exception e) {
            log.error("{} | Error in setConsoleInfo: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage(), e);
        }
    }

    private void setMainCarriageFlag(ShipmentDetails shipmentDetailsEntity, ShipmentRetrieveLiteResponse response) {
        List<Routings> routingsList = shipmentDetailsEntity.getRoutingsList();
        if (!CollectionUtils.isEmpty(routingsList)) {
            List<Routings> mainCarriageRoutings = routingsList.stream().filter(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE).toList();
            if (!CollectionUtils.isEmpty(mainCarriageRoutings)) {
                response.setIsMainCarriageAvailable(true);
                Optional<Routings> routings = mainCarriageRoutings.stream().filter(r -> Boolean.TRUE.equals(r.getIsSelectedForDocument())).findFirst();
                Routings route = mainCarriageRoutings.get(0);
                if (routings.isPresent()) {
                    route = routings.get();
                }
                if ((Constants.TRANSPORT_MODE_SEA.equals(route.getMode()) && StringUtility.isNotEmpty(route.getVesselName()) && StringUtility.isNotEmpty(route.getVoyage()))
                        || (TRANSPORT_MODE_AIR.equals(route.getMode()) && StringUtility.isNotEmpty(route.getCarrier()) && StringUtility.isNotEmpty(route.getFlightNumber()))) {
                    response.setIsVesselVoyageOrCarrierFlightNumberAvailable(true);
                }
            }

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
        var count = consoleShipmentMappingDao.countAllStateMappings(shipmentId);
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(shipmentId), SHIPMENT);
        int value = (Objects.nonNull(count) ? count : 0) + notificationMap.getOrDefault(shipmentId, 0);
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

            ListContractResponse npmContractResponse = null;
            Boolean hasDestinationContract = shipmentDetails.getDestinationContractId() != null && !shipmentDetails.getDestinationContractId().isEmpty();
            if (shipmentDetails.getContractId() != null || shipmentDetails.getDestinationContractId() != null) {
                npmContractResponse = getNpmContract(shipmentDetails);
                populateShipmentDetailsFromContract(npmContractResponse, shipmentDetails, hasDestinationContract);
            }


            ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
            beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails, includeGuid, consoleShipmentData);
            shipmentDetails.setConsolidationList(null);
            shipmentDetails.setContainersList(null);

            shipmentDetails = getShipment(shipmentDetails);

            consoleShipmentData.setCreate(true);
            consoleShipmentData.setSyncConsole(syncConsole);
            consoleShipmentData.setFromET(isFromET);
            afterSave(shipmentDetails, null, request, shipmentSettingsDetails, consoleShipmentData);

            if (npmContractResponse != null && !CollectionUtils.isEmpty(npmContractResponse.getContracts())) {
                updatePackingAndContainerFromContract(npmContractResponse.getContracts().get(0), shipmentDetails, hasDestinationContract);
            }

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
            log.error(Constants.ERROR_MESSAGE + e.getStackTrace());
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
            ListContractResponse npmContractResponse = null;
            Boolean hasDestinationContract = entity.getDestinationContractId() != null && !entity.getDestinationContractId().isEmpty();
            if (Boolean.TRUE.equals(isContractUpdated(entity, oldConvertedShipment))) {
                npmContractResponse = getNpmContract(entity);
                populateShipmentDetailsFromContract(npmContractResponse, entity, hasDestinationContract);
            }

            log.info("{} | completeUpdateShipment before save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            entity.setConsolidationList(null);
            entity.setContainersList(null);
            setShipmentCargoFields(entity, oldEntity.get());

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
            if (npmContractResponse != null && !CollectionUtils.isEmpty(npmContractResponse.getContracts())) {
                updatePackingAndContainerFromContract(npmContractResponse.getContracts().get(0), entity, entity.getContractId() != null);
            }
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            // Trigger Kafka event for PushToDownStreamServices
            this.triggerPushToDownStream(entity, oldConvertedShipment, false);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return jsonHelper.convertValue(entity, ShipmentDetailsV3Response.class);

        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(ERROR_MESSAGE + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    private Boolean isContractUpdated(ShipmentDetails shipmentDetails, ShipmentDetails oldShipmentDetails) {
        String oldContractId = oldShipmentDetails != null ? oldShipmentDetails.getContractId() : null;
        String newContractId = shipmentDetails != null ? shipmentDetails.getContractId() : null;

        String oldDestinationContractId = oldShipmentDetails != null ? oldShipmentDetails.getDestinationContractId() : null;
        String newDestinationContractId = shipmentDetails != null ? shipmentDetails.getDestinationContractId() : null;

        boolean isContractIdChanged = (oldContractId == null && newContractId != null) || (oldContractId != null && !oldContractId.equals(newContractId));
        boolean isDestinationContractIdChanged = (oldDestinationContractId == null && newDestinationContractId != null) || (oldDestinationContractId != null && !oldDestinationContractId.equals(newDestinationContractId));
        return (isContractIdChanged || isDestinationContractIdChanged);
    }

    private void updatePackingAndContainerFromContract(ListContractResponse.ContractResponse contractResponse, ShipmentDetails shipmentDetails, Boolean isDestinationQuote) throws RunnerException {
        if (Boolean.TRUE.equals(isDestinationQuote)) {
            validateDestinationQuoteIntegrity(contractResponse, shipmentDetails);
        }
        List<ListContractResponse.ContractUsage> contractUsages = Optional.ofNullable(contractResponse.getContract_usage()).orElse(List.of());
        String transportMode = shipmentDetails.getTransportMode();
        String shipmentType = shipmentDetails.getShipmentType();
        if ((TRANSPORT_MODE_SEA.equals(transportMode) && SHIPMENT_TYPE_LCL.equals(shipmentType)) || TRANSPORT_MODE_AIR.equals(transportMode)) {
            handlePackingUpdate(contractUsages, shipmentDetails);
        } else {
            BulkContainerResponse bulkContainerResponse = handleContainerUpdate(contractUsages, shipmentDetails);
            List<Containers> containersList = new ArrayList<>(jsonHelper.convertValueToSet(bulkContainerResponse.getContainerResponseList(), Containers.class));
            if (CollectionUtils.isEmpty(shipmentDetails.getConsolidationList())) {
                ConsolidationDetails consolidationDetailsV3 = createConsolidationInV3(shipmentDetails, containersList);
                if (!Objects.isNull(consolidationDetailsV3)) {
                    consolidationV3Service.attachShipments(ShipmentConsoleAttachDetachV3Request.builder().consolidationId(consolidationDetailsV3.getId()).shipmentIds(Collections.singleton(shipmentDetails.getId())).build());
                }
            } else {
                ConsolidationDetails consolidationDetails = shipmentDetails.getConsolidationList().stream().iterator().next();
                containerV3Service.deleteBulk(jsonHelper.convertValueToList(consolidationDetails.getContainersList(), ContainerV3Request.class), SHIPMENT);
                setContainersInV3Console(containersList, consolidationDetails.getId(), consolidationDetails);
                consolidationDetailsDao.save(consolidationDetails, false);
            }
        }
    }

    public ConsolidationDetails createConsolidationInV3(ShipmentDetails shipmentDetailsV3, List<Containers> containersV3) throws RunnerException {
        ShipmentSettingsDetails shipmentSettingsV3 = commonUtils.getShipmentSettingFromContext();
        if (Boolean.TRUE.equals(shipmentSettingsV3.getShipConsolidationContainerEnabled())) {
            ConsolidationDetails consolidationDetailsV3 = new ConsolidationDetails();
            consolidationDetailsV3.setConsolidationType(shipmentDetailsV3.getJobType());
            consolidationDetailsV3.setTransportMode(shipmentDetailsV3.getTransportMode());
            validateCreateV3Consolidations(shipmentDetailsV3, shipmentSettingsV3);
            consolidationDetailsV3.setCarrierDetails(jsonHelper.convertValue(shipmentDetailsV3.getCarrierDetails(), CarrierDetails.class));
            consolidationDetailsV3.getCarrierDetails().setId(null);
            consolidationDetailsV3.getCarrierDetails().setGuid(null);
            consolidationDetailsV3.setOpenForAttachment(true);
            if (shipmentSettingsV3.getShipmentLite() != null && shipmentSettingsV3.getShipmentLite() && shipmentDetailsV3.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetailsV3.getDirection().equals(Constants.DIRECTION_EXP)) {
                consolidationDetailsV3.setPayment(shipmentDetailsV3.getPaymentTerms());
            }
            if (consolidationDetailsV3.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || consolidationDetailsV3.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetailsV3.getCarrierDetails().setOrigin(consolidationDetailsV3.getCarrierDetails().getOriginPort());
                consolidationDetailsV3.getCarrierDetails().setOriginLocCode(consolidationDetailsV3.getCarrierDetails().getOriginPortLocCode());
                consolidationDetailsV3.getCarrierDetails().setDestination(consolidationDetailsV3.getCarrierDetails().getDestinationPort());
                consolidationDetailsV3.getCarrierDetails().setDestinationLocCode(consolidationDetailsV3.getCarrierDetails().getDestinationPortLocCode());
            }
            consolidationDetailsV3.setShipmentType(shipmentDetailsV3.getDirection());
            consolidationDetailsV3.setContainerCategory(shipmentDetailsV3.getShipmentType());
            consolidationDetailsV3.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetailsV3.setIsSendingAgentFreeTextAddress(false);
            consolidationDetailsV3.setIsInland(false);
            consolidationDetailsV3.setCarrierBookingRef(shipmentDetailsV3.getBookingNumber());
            consolidationDetailsV3.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            consolidationDetailsV3.setDepartment(commonUtils.getAutoPopulateDepartment(
                    shipmentDetailsV3.getTransportMode(), shipmentDetailsV3.getDirection(), MdmConstants.CONSOLIDATION_MODULE
            ));
            if (StringUtility.isNotEmpty(shipmentDetailsV3.getMasterBill())) {
                consolidationDetailsV3.setBol(shipmentDetailsV3.getMasterBill());
            }
            if (Objects.equals(TRANSPORT_MODE_SEA, shipmentDetailsV3.getTransportMode()) || Objects.equals(TRANSPORT_MODE_AIR, shipmentDetailsV3.getTransportMode()))
                consolidationDetailsV3.setHazardous(shipmentDetailsV3.getContainsHazardous());
            consolidationV3Service.generateConsolidationNumber(consolidationDetailsV3);
            addAgentDetailsForV3Console(shipmentDetailsV3, consolidationDetailsV3);
            List<Routings> createRoutesV3 = getV3RoutingsList(shipmentDetailsV3, consolidationDetailsV3);
            consolidationDetailsV3 = consolidationDetailsDao.save(consolidationDetailsV3, false, Boolean.TRUE.equals(shipmentDetailsV3.getContainsHazardous()));
            if (!CommonUtils.listIsNullOrEmpty(createRoutesV3)) {
                routingsV3Dao.saveEntityFromConsole(createRoutesV3, consolidationDetailsV3.getId());
            }
            Long id = consolidationDetailsV3.getId();
            setContainersInV3Console(containersV3, id, consolidationDetailsV3);
            createAutoV3EventCreate(shipmentSettingsV3, consolidationDetailsV3);
            consolidationV3Service.pushShipmentDataToDependentService(consolidationDetailsV3, true, null);
            return consolidationDetailsV3;
        }
        return null;
    }

    private void validateCreateV3Consolidations(ShipmentDetails shipmentDetailsV3, ShipmentSettingsDetails shipmentSettingsV3) {
        if ((shipmentSettingsV3.getConsolidationLite() == null || !shipmentSettingsV3.getConsolidationLite())
                && !Objects.equals(shipmentDetailsV3.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                && (StringUtility.isEmpty(shipmentDetailsV3.getCarrierDetails().getOriginPort()) || StringUtility.isEmpty(shipmentDetailsV3.getCarrierDetails().getDestinationPort()))) {
            throw new ValidationException("Not able to create consolidation, before adding 'New Containers', please provide ‘Origin’ and ‘Destination’ values.");
        }
        if (StringUtility.isNotEmpty(shipmentDetailsV3.getCarrierDetails().getOriginPort()) && Objects.equals(shipmentDetailsV3.getCarrierDetails().getOriginPort(), shipmentDetailsV3.getCarrierDetails().getDestinationPort())) {
            throw new ValidationException("‘Origin’ and ‘Destination’ can't be same.");
        }
    }

    private void addAgentDetailsForV3Console(ShipmentDetails shipmentDetailsV3, ConsolidationDetails consolidationDetailsV3) {
        if (shipmentDetailsV3.getAdditionalDetails() != null) {
            consolidationDetailsV3.setSendingAgent(commonUtils.removeIdFromParty(shipmentDetailsV3.getAdditionalDetails().getExportBroker()));
            consolidationDetailsV3.setReceivingAgent(commonUtils.removeIdFromParty(shipmentDetailsV3.getAdditionalDetails().getImportBroker()));
        }
        if (Objects.equals(consolidationDetailsV3.getShipmentType(), DIRECTION_EXP) && CommonUtils.checkAddressNotNull(consolidationDetailsV3.getReceivingAgent())) {
            Long receivingV3BranchId = commonUtils.getReceivingBranch(consolidationDetailsV3.getReceivingAgent().getOrgId(), consolidationDetailsV3.getReceivingAgent().getAddressId());
            consolidationDetailsV3.setReceivingBranch(receivingV3BranchId);
        }
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsEntityTransferPrerequisiteEnabled())) {
            if (!commonUtils.checkIfPartyExists(consolidationDetailsV3.getSendingAgent())) {
                consolidationDetailsV3.setSendingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetailsV3.getCarrierDetails().getOriginPortLocCode()));
            }
            if (!commonUtils.checkIfPartyExists(consolidationDetailsV3.getReceivingAgent())) {
                consolidationDetailsV3.setReceivingAgentCountry(commonUtils.getCountryFromUnLocCode(consolidationDetailsV3.getCarrierDetails().getDestinationPortLocCode()));
            }
        }
    }

    private List<Routings> getV3RoutingsList(ShipmentDetails shipmentDetailsV3, ConsolidationDetails consolidationDetailsV3) {
        List<Routings> v3routings = new ArrayList<>();
        if (shipmentDetailsV3.getRoutingsList() != null && !shipmentDetailsV3.getRoutingsList().isEmpty())
            v3routings = shipmentDetailsV3.getRoutingsList().stream().sorted(Comparator.comparingLong(Routings::getLeg)).toList();
        var routeRequestV3 = v3routings.stream().filter(x -> x.getMode().equals(shipmentDetailsV3.getTransportMode())).findFirst();
        List<Routings> createV3Routes = new ArrayList<>();
        // Generate default Routes if Route Master is enabled
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            createV3Routes.addAll(routingsV3Dao.generateDefaultRouting(consolidationDetailsV3.getCarrierDetails(), shipmentDetailsV3.getTransportMode()));
            consolidationDetailsV3.setRoutingsList(createV3Routes);
        } else {
            if (routeRequestV3.isPresent()) {
                createV3Routes.add(jsonHelper.convertValue(routeRequestV3.get(), Routings.class));
                createV3Routes = createV3ConsoleRoutePayload(createV3Routes);
                consolidationDetailsV3.setRoutingsList(createV3Routes);
            }
        }
        return createV3Routes;
    }

    private List<Routings> createV3ConsoleRoutePayload(List<Routings> v3Routes) {
        List<Routings> responseListV3 = new ArrayList<>();
        for (var route : v3Routes) {
            Routings routingsV3 = new Routings();
            routingsV3.setLeg(1L);
            routingsV3.setPol(route.getPol());
            routingsV3.setPod(route.getPod());
            routingsV3.setMode(route.getMode());
            routingsV3.setEta(route.getEta());
            routingsV3.setEtd(route.getEtd());
            routingsV3.setTransitDays(route.getTransitDays());
            routingsV3.setAta(route.getAta());
            routingsV3.setAtd(route.getAtd());
            routingsV3.setVesselName(route.getVesselName());
            routingsV3.setVoyage(route.getVoyage());
            routingsV3.setCarrier(route.getCarrier());
            routingsV3.setFlightNumber(route.getFlightNumber());
            responseListV3.add(routingsV3);
        }
        return responseListV3;
    }

    private void setContainersInV3Console(List<Containers> containersV3, Long id, ConsolidationDetails consolidationDetailsV3) {
        if (containersV3 != null && !containersV3.isEmpty()) {
            containersV3 = containersV3.stream().map(e -> e.setConsolidationId(id)).toList();
            containersV3 = containerDao.saveAll(containersV3);
        }
        consolidationDetailsV3.setContainersList(containersV3);
    }

    private void createAutoV3EventCreate(ShipmentSettingsDetails shipmentSettingsV3, ConsolidationDetails consolidationDetailsV3) {
        if (shipmentSettingsV3.getAutoEventCreate() != null && shipmentSettingsV3.getAutoEventCreate()) {
            consolidationV3Service.generateV3Events(consolidationDetailsV3);
        }
    }

    private void handlePackingUpdate(List<ListContractResponse.ContractUsage> contractUsages, ShipmentDetails shipmentDetails) throws RunnerException {
        List<PackingV3Request> quotePackingRequests = contractUsages.stream()
                .map(usage -> getPackingRequest(usage, shipmentDetails))
                .toList();
        List<Packing> shipmentPackingList = packingDao.findByShipmentId(shipmentDetails.getId());
        if (shipmentPackingList != null && !CollectionUtils.isEmpty(shipmentPackingList) && !quotePackingRequests.isEmpty()) {
            packingV3Service.deleteBulk(jsonHelper.convertValueToList(shipmentPackingList, PackingV3Request.class), SHIPMENT);
        }
        if (!quotePackingRequests.isEmpty()) {
            packingV3Service.updateBulk(quotePackingRequests, SHIPMENT);
        }
    }

    private BulkContainerResponse handleContainerUpdate(List<ListContractResponse.ContractUsage> contractUsages, ShipmentDetails shipmentDetails) throws RunnerException {
        BulkContainerResponse bulkContainerResponse = new BulkContainerResponse();
        List<ContainerV3Request> quoteContainerRequests = contractUsages.stream()
                .map(usage -> getContainerRequest(usage, shipmentDetails))
                .toList();
        if (shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty() && !quoteContainerRequests.isEmpty()) {
            containerV3Service.deleteBulk(jsonHelper.convertValueToList(shipmentDetails.getContainersList(), ContainerV3Request.class), SHIPMENT);
        }
        if (!quoteContainerRequests.isEmpty()) {
            bulkContainerResponse = containerV3Service.createBulk(quoteContainerRequests, SHIPMENT);
        }
        return bulkContainerResponse;
    }

    private void validateDestinationQuoteIntegrity(ListContractResponse.ContractResponse contractResponse, ShipmentDetails shipmentDetails) throws RunnerException {
        Set<String> contractCargoTypes = Optional.ofNullable(contractResponse.getContract_usage())
                .orElse(Collections.emptyList())
                .stream()
                .map(ListContractResponse.ContractUsage::getFilter_params)
                .filter(Objects::nonNull)
                .flatMap(fp -> Optional.ofNullable(fp.getCargo_type()).orElse(Collections.emptyList()).stream())
                .filter(Objects::nonNull)
                .map(String::trim)
                .collect(Collectors.toSet());

        Set<String> missingContainerCodes = Optional.ofNullable(shipmentDetails.getContainersList())
                .orElse(Collections.emptySet())
                .stream()
                .map(Containers::getContainerCode)
                .filter(code -> code != null && !contractCargoTypes.contains(code.trim()))
                .collect(Collectors.toSet());

        List<String> missingPackTypes = Optional.ofNullable(shipmentDetails.getPackingList())
                .orElse(Collections.emptyList())
                .stream()
                .map(Packing::getPacksType)
                .filter(type -> type != null && !contractCargoTypes.contains(type.trim()))
                .toList();

        if (!missingContainerCodes.isEmpty() || !missingPackTypes.isEmpty()) {
            throw new RunnerException("The quote selected doesn’t have the same package/container information");
        }
    }

    private void populateShipmentDetailsFromContract(ListContractResponse listContractResponse, ShipmentDetails shipmentDetails, boolean hasDestinationContract) {
        if (listContractResponse == null || listContractResponse.getContracts() == null || listContractResponse.getContracts().isEmpty())
            return;
        List<ListContractResponse.ContractResponse> contracts = listContractResponse.getContracts();
        ListContractResponse.ContractResponse contract = contracts.get(0);
        if (hasDestinationContract) {
            shipmentDetails.setDestinationContractId(contract.getContract_id());
            shipmentDetails.setDestinationContractType(contract.getContract_type());
        } else {
            shipmentDetails.setContractId(contract.getContract_id());
            shipmentDetails.setContractType(contract.getContract_type());
        }
        shipmentDetails.setCarrierDetails(npmContractV3Util.createCarrierDetails(contract));
        shipmentDetails.setShipmentType(!contract.getLoad_types().isEmpty() ? contract.getLoad_types().get(0) : null);
        if (contract.getMeta() != null) {
            var meta = contract.getMeta();
            shipmentDetails.setTransportMode(meta.getMode_of_transport());
            shipmentDetails.setDirection(meta.getShipment_movement());
            shipmentDetails.setIncoterms(meta.getIncoterm());
            shipmentDetails.setServiceType(meta.getService_mode());
            if (meta.getBranch_info() != null) {
                var branchInfo = meta.getBranch_info();
                shipmentDetails.setPrimarySalesAgentEmail(branchInfo.getSales_agent_primary_email());
                shipmentDetails.setSecondarySalesAgentEmail(branchInfo.getSales_agent_secondary_email());
                shipmentDetails.setSalesBranch(branchInfo.getId());
            }
        }
    }

    private ListContractResponse getNpmContract(ShipmentDetails shipmentDetails) throws RunnerException {
        String partyForQuote = shipmentDetails.getContractId() != null ? shipmentDetails.getCurrentPartyForQuote() : shipmentDetails.getDestinationCurrentPartyForQuote();
        String contractId = shipmentDetails.getContractId() != null ? shipmentDetails.getContractId() : shipmentDetails.getDestinationContractId();
        String orgCode = extractOrgCode(shipmentDetails, partyForQuote);
        if (contractId == null || orgCode == null) {
            return null;
        }
        ListContractRequest listContractRequest = new ListContractRequest();
        listContractRequest.setCustomer_org_id(orgCode);
        listContractRequest.setOrg_role("DPW");
        listContractRequest.setFilter_contract_states(List.of("ENABLED"));
        listContractRequest.setFilter_contract_id(contractId);

        ResponseEntity<IRunnerResponse> response = npmServiceAdapter.fetchContract(CommonRequestModel.buildRequest(listContractRequest));
        IRunnerResponse body = response.getBody();
        if (!(body instanceof DependentServiceResponse npmResponse)) {
            throw new ValidationException("Invalid response received from NPM: null or incompatible type");
        }
        return jsonHelper.convertValue(npmResponse.getData(), ListContractResponse.class);
    }

    private String extractOrgCode(ShipmentDetails shipmentDetails, String party) {
        if (shipmentDetails == null || party == null) return null;
        return switch (party.toUpperCase()) {
            case CLIENT_PARTY -> shipmentDetails.getClient() != null ? shipmentDetails.getClient().getOrgCode() : null;
            case CONSIGNEE_PARTY ->
                    shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getOrgCode() : null;
            case CONSIGNOR_PARTY ->
                    shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgCode() : null;
            default -> {
                var notifyParty = shipmentDetails.getAdditionalDetails() != null
                        ? shipmentDetails.getAdditionalDetails().getNotifyParty()
                        : null;
                yield notifyParty != null ? notifyParty.getOrgCode() : null;
            }
        };
    }

    private ContainerV3Request getContainerRequest(ListContractResponse.ContractUsage usage, ShipmentDetails shipmentDetails) {
        var request = new ContainerV3Request();
        request.setContainerCount(usage.getMeta() != null ? usage.getMeta().getOriginal_usage() : usage.getUsage());
        var filters = usage.getFilter_params();
        if (filters != null) {
            if (filters.getCargo_type() != null && !filters.getCargo_type().isEmpty())
                request.setContainerCode(filters.getCargo_type().get(0));
            if (filters.getCommodity() != null && !filters.getCommodity().isEmpty())
                request.setCommodityGroup(filters.getCommodity().get(0));
        }
        if (shipmentDetails.getId() != null)
            request.setShipmentsId(shipmentDetails.getId());
        return request;
    }

    private PackingV3Request getPackingRequest(ListContractResponse.ContractUsage usage, ShipmentDetails shipmentDetails) {
        PackingV3Request request = new PackingV3Request();
        npmContractV3Util.setFilterParams(usage.getFilter_params(), request);
        npmContractV3Util.setMetaData(usage, request);
        if (shipmentDetails.getId() != null) {
            request.setShipmentId(shipmentDetails.getId());
        }
        return request;
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

        if (!setIsNullOrEmpty(consolidationDetails)) {
            ShipmentWtVolResponse shipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails.iterator().next());
            jsonHelper.updateValue(consoleShipmentData, shipmentWtVolResponse);
            consoleShipmentData.setConsolidationDetails(consolidationDetails.iterator().next());
        }
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentV3Request shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, ConsoleShipmentData consoleShipmentData) throws RunnerException {
        boolean isCreate = consoleShipmentData.isCreate();
        boolean syncConsole = consoleShipmentData.isSyncConsole();
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
            updateContainerFromCargo(shipmentDetails, oldEntity);
            // update consolidation wt vol
            if (Objects.nonNull(consoleShipmentData.getConsolidationDetails()))
                consolidationV3Service.updateConsolidationCargoSummary(consoleShipmentData.getConsolidationDetails(), jsonHelper.convertValue(consoleShipmentData, ShipmentWtVolResponse.class));
            updateContainerFromCargo(shipmentDetails, oldEntity);
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
            throw new ValidationException("Shipper & Consignee parties can't be selected as same.");

        if (!isStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) {
            if (!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            } else if (!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }
        // all shipment fields before creation or updation
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
        List<String> shipmentIdsAsStrings = shipmentListResponses.stream()
                .map(shipment -> String.valueOf(shipment.getId())).distinct().toList();

        List<CustomerBookingProjection> bookingProjections = shipmentDao.findCustomerBookingProByShipmentIdIn(shipmentIdsAsStrings);

        return bookingProjections.stream()
                .collect(Collectors.toMap(
                        projection -> Long.valueOf(projection.getShipmentId()), // convert String to Long
                        CustomerBookingProjection::getId
                ));
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
        Optional<ShipmentDetails> shipmentDetailsOptional;
        if (Objects.equals(xSource, NETWORK_TRANSFER))
            shipmentDetailsOptional = shipmentDao.findShipmentByIdWithQuery(shipmentId);
        else
            shipmentDetailsOptional = shipmentDao.findById(shipmentId);
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
            if (shipments.getContainerAssignedToShipmentCargo() != null)
                containerIds.add(shipments.getContainerAssignedToShipmentCargo());
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
            if (Objects.nonNull(shipments.getContainerAssignedToShipmentCargo()) && containerIdNumberMap.containsKey(shipments.getContainerAssignedToShipmentCargo())) {
                shipments.setContainerNumberAssignedToShipmentCargo(containerIdNumberMap.get(shipments.getContainerAssignedToShipmentCargo()).getContainerNumber());
                shipments.setContainerCodeAssignedToShipmentCargo(containerIdNumberMap.get(shipments.getContainerAssignedToShipmentCargo()).getContainerCode());
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
            containerV3Util.resetContainerDataForRecalculation(containers);
            containers.setGrossWeightUnit(customerBookingV3Request.getGrossWeightUnit());
            containers.setGrossVolumeUnit(customerBookingV3Request.getVolumeUnit());
            containers.setPacksType(customerBookingV3Request.getPackageType());
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
                    coLoadCarrierName(customerBookingRequest.getCoLoadCarrierName()).
                    coLoadBookingReference(customerBookingRequest.getPartnerBkgNumber()).
                    coLoadMBL(customerBookingRequest.getPartnerBLOrAWBNumber()).
                    carrierBookingRef(customerBookingRequest.getCarrierBookingNumber()).
                    build();
            // Set Department in case single department is available
            consolidationDetailsV3Request.setDepartment(commonUtils.getAutoPopulateDepartment(
                    consolidationDetailsV3Request.getTransportMode(), consolidationDetailsV3Request.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE
            ));

            Pair<ConsolidationDetails, Long> createConsoleResponse = consolidationV3Service.createConsolidationForBooking(CommonRequestModel.buildRequest(consolidationDetailsV3Request), customerBookingRequest);
            ConsolidationDetails consolDetailsResponse = createConsoleResponse.getLeft();

            if (consolDetailsResponse != null) {
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList() != null ? new HashSet<>(consolRequest.getContainersList()) : null;
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        ShipmentV3Request shipmentRequest = getShipmentRequestFromBookingV3(customerBookingRequest, consolidationDetails);
        Long consolidationId = !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getId() : null;
        // Set Department in case single department is available
        shipmentRequest.setDepartment(commonUtils.getAutoPopulateDepartment(
                shipmentRequest.getTransportMode(), shipmentRequest.getDirection(), MdmConstants.SHIPMENT_MODULE
        ));
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
        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest), customerBookingRequest, containerList, consolidationId);
    }

    public ShipmentDetailsV3Response createFromBooking(CommonRequestModel commonRequestModel, CustomerBookingV3Request customerBookingV3Request, Set<ContainerRequest> containerList, Long consolidationId) {
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
            Long shipmentId = shipmentDetails.getId();
            if (consolidationId != null) {
                consolidationV3Service.attachShipments(ShipmentConsoleAttachDetachV3Request.builder().consolidationId(consolidationId).shipmentIds(Collections.singleton(shipmentId)).build());
                processPacksAndContainers(customerBookingV3Request, containerList, consolidationId, shipmentDetails);
            }
            shipmentDao.save(shipmentDetails, false);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            generateAfterSaveEvents(shipmentDetails, shipmentSettingsDetails);
            if (shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails);
            Events events = createAutomatedEvents(shipmentDetails, EventConstants.BKCR, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
            shipmentDetails.getEventsList().add(events);
            List<Packing> updatedPackings = getAndSetPackings(customerBookingV3Request, shipmentId, shipmentDetails);

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (ObjectUtils.isNotEmpty(referenceNumbersRequest)) {
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));
            }

            List<PartiesRequest> shipmentAddressList = request.getShipmentAddresses();
            if (shipmentAddressList != null) {
                List<Parties> updatedParties = partiesDao.saveEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, false), shipmentId, Constants.SHIPMENT_ADDRESSES);
                shipmentDetails.setShipmentAddresses(updatedParties);
            }
            checkContainerAssignedForHbl(shipmentDetails, updatedPackings);
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


    private void processPacksAndContainers(CustomerBookingV3Request customerBookingV3Request, Set<ContainerRequest> containerList, Long consolidationId, ShipmentDetails shipmentDetails) throws RunnerException {

        if (customerBookingV3Request.getPackages() != 0L && (customerBookingV3Request.getPackages() < containerList.size())) {
            throw new ValidationException("Booking packages should not be lesser than the container count. Please enter right amount.");
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        String packUnit = !isStringNullOrEmpty(v1TenantSettingsResponse.getDefaultPackUnit()) ? v1TenantSettingsResponse.getDefaultPackUnit() : PackingConstants.PKG;
        List<PackingV3Request> packingV3RequestList = containerList.stream()
                .map(container -> {
                    PackingV3Request packingV3Request = new PackingV3Request();
                    packingV3Request.setPacks(container.getContainerCount().toString());
                    packingV3Request.setPacksType(packUnit);
                    packingV3Request.setCommodity(container.getCommodityGroup());
                    packingV3Request.setContainerId(container.getId());
                    packingV3Request.setBookingId(customerBookingV3Request.getId());
                    return packingV3Request;
                })
                .toList();

        distributeWeightVolumePackages(packingV3RequestList, customerBookingV3Request);
        assignShipmentIdToPacks(packingV3RequestList, consolidationId);
        BulkPackingResponse bulkPackingResponse = packingV3Service.updateBulk(packingV3RequestList, BOOKING);
        shipmentDetails.setPackingList(jsonHelper.convertValueToList(bulkPackingResponse.getPackingResponseList(), Packing.class));

        List<Containers> containersArrayList = jsonHelper.convertValueToList(containerList, Containers.class);
        for (Containers container : containersArrayList) {
            containerV3Util.resetContainerDataForRecalculation(container);
        }
        List<Packing> packingArrayList = jsonHelper.convertValueToList(bulkPackingResponse.getPackingResponseList(), Packing.class);
        for (int i = 0; i < containersArrayList.size(); i++) {
            containerV3Service.addPackageDataToContainer(containersArrayList.get(i), packingArrayList.get(i));
        }
        containerDao.saveAll(containersArrayList);
        shipmentDetails.setContainersList(new HashSet<>(containersArrayList));
    }

    private void assignShipmentIdToPacks(List<PackingV3Request> packingList, Long consolidationId) {
        Long shipmentId = packingV3Util.getShipmentId(consolidationId);
        for (PackingV3Request packingV3Request : packingList) {
            packingV3Request.setShipmentId(shipmentId);
        }
    }

    private void distributeWeightVolumePackages(List<PackingV3Request> packingList, CustomerBookingV3Request customerBookingV3Request) {
        BigDecimal grossWeight = customerBookingV3Request.getGrossWeight();
        BigDecimal volume = customerBookingV3Request.getVolume();
        Long totalPackages = customerBookingV3Request.getPackages();

        int totalItems = packingList.size();
        if (totalItems == 0 || grossWeight == null || volume == null || totalPackages == null) return;

        // Convert gross weight and volume to long for whole number division
        Long totalWeightLong = grossWeight.longValue();
        Long totalVolumeLong = volume.longValue();

        Long weightPerPack = totalWeightLong / totalItems;
        Long weightRemainder = totalWeightLong % totalItems;

        Long volumePerPack = totalVolumeLong / totalItems;
        Long volumeRemainder = totalVolumeLong % totalItems;

        Long packagesPerPack = totalPackages / totalItems;
        Long packagesRemainder = totalPackages % totalItems;

        for (int i = 0; i < totalItems; i++) {
            PackingV3Request pack = packingList.get(i);
            boolean isLast = (i == totalItems - 1);

            Long finalWeight = weightPerPack + (isLast ? weightRemainder : 0);
            Long finalVolume = volumePerPack + (isLast ? volumeRemainder : 0);
            Long finalPackages = packagesPerPack + (isLast ? packagesRemainder : 0);

            pack.setWeight(BigDecimal.valueOf(finalWeight));
            pack.setWeightUnit(customerBookingV3Request.getGrossWeightUnit());
            pack.setVolume(BigDecimal.valueOf(finalVolume));
            pack.setVolumeUnit(customerBookingV3Request.getVolumeUnit());
            pack.setPacks(String.valueOf(finalPackages));
        }
    }

    private void generateAfterSaveEvents(ShipmentDetails shipmentDetails, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        List<Events> eventsList = new ArrayList<>();
        eventsList = setEventDetails(eventsList, shipmentDetails);
        eventsList = eventsV3Util.createOrUpdateEvents(shipmentDetails, null, eventsList, true);
        if (eventsList != null) {
            commonUtils.updateEventWithMasterData(eventsList);
            List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, shipmentDetails.getId(), Constants.SHIPMENT);
            shipmentDetails.setEventsList(updatedEvents);
            eventsV3Service.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
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

        List<Routings> routingsList = routingsV3Dao.generateDefaultRouting(jsonHelper.convertValue(carrierDetails, CarrierDetails.class), transportMode);

        return commonUtils.convertToList(routingsList, RoutingsRequest.class);

    }

    private ShipmentV3Request getShipmentRequestFromBookingV3(CustomerBookingV3Request customerBookingRequest, Set<ConsolidationDetailsRequest> consolidationDetails) {
        return ShipmentV3Request.builder().
                carrierDetails(CarrierDetailRequest.builder()
                        .eta(customerBookingRequest.getCarrierDetails().getEta())
                        .ata(customerBookingRequest.getCarrierDetails().getAta())
                        .etd(customerBookingRequest.getCarrierDetails().getEtd())
                        .atd(customerBookingRequest.getCarrierDetails().getAtd())
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
                contractId(customerBookingRequest.getContractId()).
                parentContractId(customerBookingRequest.getParentContractId()).
                contractType(customerBookingRequest.getContractStatus()).
                noOfPacks((int) customerBookingRequest.getPackages()).
                goodsDescription(customerBookingRequest.getDescription()).
                marksNum(customerBookingRequest.getMarksnNumbers()).
                packsUnit(customerBookingRequest.getPackageType()).
                weight(customerBookingRequest.getGrossWeight()).
                weightUnit(customerBookingRequest.getGrossWeightUnit()).
                volume(customerBookingRequest.getVolume()).
                volumeUnit(customerBookingRequest.getVolumeUnit()).
                volumetricWeight(customerBookingRequest.getWeightVolume()).
                volumetricWeightUnit(customerBookingRequest.getWeightVolumeUnit()).
                chargable(customerBookingRequest.getChargeable()).
                chargeableUnit(customerBookingRequest.getChargeableUnit()).
                bookingNumber(customerBookingRequest.getCarrierBookingNumber()).
                bookingReference(customerBookingRequest.getBookingNumber()).
                bookingCreatedDate(customerBookingRequest.getBookingDate()).
                shipmentCreatedOn(LocalDateTime.now()).
                client(createPartiesRequest(customerBookingRequest.getCustomer(), customerBookingRequest.getClientCountry(), null)).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee(), customerBookingRequest.getConsigneeCountry(), null)).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor(), customerBookingRequest.getConsignorCountry(), null)).
                shipmentAddresses(customerBookingRequest.getAdditionalParties().stream().map(additionalParty ->
                        createPartiesRequest(additionalParty, additionalParty.getCountryCode(), additionalParty.getType())).toList()).
                additionalDetails(AdditionalDetailV3Request.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty(), customerBookingRequest.getNotifyPartyCountry(), null)).
                        pickupDate(customerBookingRequest.getPickupAtOriginDate()).
                        cargoDeliveredDate(customerBookingRequest.getDeliveryAtDestinationDate()).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                additionalTerms(customerBookingRequest.getAdditionalTerms()).
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
                referenceNumbersList(createReferenceNumbersList(customerBookingRequest.getReferenceNumbersList())).
                sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                consolRef(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getConsolidationNumber() : "").
                masterBill(consolidationDetails != null && !consolidationDetails.isEmpty() ? consolidationDetails.iterator().next().getBol() : null).
                freightLocalCurrency(UserContext.getUser().CompanyCurrency).
                currentPartyForQuote(customerBookingRequest.getCurrentPartyForQuote()).
                autoUpdateWtVol(true).
                paymentTerms(customerBookingRequest.getPaymentTerms()).
                isReefer(customerBookingRequest.getIsReefer()).
                incotermsLocation(customerBookingRequest.getIncotermsLocation()).
                cargoReadinessDate(customerBookingRequest.getCargoReadinessDate()).
                controlled(customerBookingRequest.getControlled()).
                controlledReferenceNumber(customerBookingRequest.getControlledReferenceNumber()).
                partner(customerBookingRequest.getPartner()).
                bookingAgent(customerBookingRequest.getBookingAgent()).
                coLoadCarrierName(customerBookingRequest.getCoLoadCarrierName()).
                coLoadBkgNumber(customerBookingRequest.getPartnerBkgNumber()).
                coLoadBlNumber(customerBookingRequest.getPartnerBLOrAWBNumber()).
                pickupAtOrigin(customerBookingRequest.getPickupAtOrigin()).
                pickupAtOriginType(customerBookingRequest.getPickupAtOriginType()).
                deliveryAtDestination(customerBookingRequest.getDeliveryAtDestination()).
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

    private PartiesRequest createPartiesRequest(PartiesRequest party, String countryCode, String partyType) {
        if (party == null)
            return null;
        return PartiesRequest.builder()
                .type(partyType)
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
        if (referenceNumbers == null) return Collections.emptyList();
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
            if (checkConditionForSEAorROAD(request)) {
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

    public void updateContainerFromCargo(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment) throws RunnerException {
        if (!TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) ||
                Objects.isNull(shipmentDetails.getContainerAssignedToShipmentCargo()) ||
                !isShipmentCargoFieldsChanged(shipmentDetails, oldShipment))
            return;
        containerV3Service.updateAttachedContainersData(List.of(shipmentDetails.getContainerAssignedToShipmentCargo()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, String consoleGuid, boolean isAttached, boolean getMasterData,
                                                               boolean fromNte) throws AuthenticationException {
        validateRequiredParams(consoleId, consoleGuid);

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

    public static void validateRequiredParams(Long consoleId, String consoleGuid) {
        if (consoleId == null && consoleGuid == null) {
            throw new ValidationException("Required parameters missing: consoleId and consoleGuid");
        }
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

        if (Constants.IMP.equals(shipmentDetails.getDirection())) {
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
        if (!isShipmentdg) {
            warning = "Shipment does not have any DG container or package, no need of any dg approval";
            updatedDgStatus = null;
            operationType = DG_REQUEST;
        }

        if ((dgStatus == OCEAN_DG_ACCEPTED || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_COMMERCIAL_REJECTED) && !checkForClass1(shipmentDetails) && warning == null) {
            warning = "Shipment does not have any class1 DG container or package, no need of commercial dg approval";
            updatedDgStatus = OCEAN_DG_ACCEPTED;
            operationType = DG_APPROVE;
        }

        if (dgStatus == OCEAN_DG_COMMERCIAL_ACCEPTED) {
            warning = "Shipment is already in commercial approved state";
            updatedDgStatus = OCEAN_DG_COMMERCIAL_ACCEPTED;
            operationType = COMMERCIAL_APPROVE;
        }

        if ((!isOceanDgUser || dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_COMMERCIAL_REJECTED) && warning == null) {
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

        } catch (Exception ex) {
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

        if (Constants.IMP.equals(shipmentDetails.getDirection())) {
            return "DG approval not required for Import Shipment";
        }

        OceanDGStatus oldDgStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus updatedDgStatus = getDgStatusAfterApprovalResponse(oldDgStatus, request.getStatus());

        if (updatedDgStatus == null) {
            throw new RunnerException(String.format("Ocean DG status value %s is invalid", oldDgStatus));
        }

        if (CollectionUtils.isEmpty(request.getTaskGuids())) {
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
        } catch (Exception ex) {
            log.error("Audit failed for shipmentId: {} and operation: {}. Error: {}", shipmentDetails.getId(), operationType, ex.getMessage(), ex);
        }

        if (updatedDgStatus == OceanDGStatus.OCEAN_DG_ACCEPTED && checkForClass1(shipmentDetails)) {
            updatedDgStatus = OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
        }
        shipmentDetails.setOceanDGStatus(updatedDgStatus);

        shipmentDao.save(shipmentDetails, false);

        return warning;
    }

    protected void fetchDgUserTask(OceanDGRequestV3 request) throws RunnerException {
        CommonV1ListRequest commonV1ListRequest = createCriteriaTaskListRequest(request.getShipmentId().toString(), SHIPMENTS_WITH_SQ_BRACKETS);
        log.info("V1 task list request: {}", jsonHelper.convertToJson(commonV1ListRequest));

        List<Map<String, Object>> mapList;
        try {
            mapList = mdmServiceAdapter.getTaskList(request.getShipmentGuid(), SHIPMENTS_WITH_SQ_BRACKETS, PENDING_ACTION_TASK, DG_OCEAN_APPROVAL);
        } catch (Exception ex) {
            log.error("Failed to fetch pending tasks from MDM with RequestId - {} : {}: ", LoggerHelper.getRequestIdFromMDC(), ex);
            throw new RunnerException(ex.getMessage());
        }


        if (mapList.isEmpty()) return;

        if (mapList.size() > 1) {
            log.error("More than one task in Pending State of oceanDG exist for shipment : " + request.getShipmentId());
        }

        List<String> taskGuids = new ArrayList<>();
        for (Map<String, Object> map : mapList) {
            taskGuids.add(map.get("uuid").toString());
        }
        request.setTaskGuids(taskGuids);

        if (mapList.get(0).containsKey("userEmail")) {
            request.setUserEmail(mapList.get(0).get("userEmail").toString());
        }

    }

    private void closeOceanDgTask(OceanDGRequestV3 request) {
        MdmTaskApproveOrRejectRequest taskUpdateRequest = MdmTaskApproveOrRejectRequest.builder()
                .status(request.getStatus().getName().toUpperCase())
                .approvedOrRejectedBy(UserContext.getUser().getUsername())
                .build();

        if (TaskStatus.APPROVED.equals(request.getStatus())) {
            taskUpdateRequest.setApprovalComments(request.getStatus().getName().toUpperCase());
        } else if (TaskStatus.REJECTED.equals(request.getStatus())) {
            taskUpdateRequest.setRejectedComments(request.getStatus().getName().toUpperCase());
        } else {
            throw new ValidationException("Invalid approval status in request : " + request.getStatus().getName());
        }

        try {
            for (String taskGuid : request.getTaskGuids()) {
                taskUpdateRequest.setTaskUuid(taskGuid);
                mdmServiceAdapter.approveOrRejectTask(taskUpdateRequest);
            }
        } catch (Exception ex) {
            log.error("task approval or rejection is failed for requestId from MDM: {} : {} ", LoggerHelper.getRequestIdFromMDC(), request.getShipmentId());
        }
    }

    private DBOperationType determineOperationTypeAfterApproval(OceanDGStatus dgStatus, OceanDGRequestV3 request) {
        DBOperationType operationType = DG_REQUEST;
        if (dgStatus == OCEAN_DG_REQUESTED) {
            if (request.getStatus() == TaskStatus.APPROVED) {
                operationType = DG_APPROVE;
            } else {
                operationType = DBOperationType.DG_REJECT;
            }
        } else if (dgStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            if (request.getStatus() == TaskStatus.REJECTED) {
                operationType = COMMERCIAL_APPROVE;
            } else {
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
            if (template == null) {
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

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(List.of(criteria1, "and", criteria2), "and", criteria3), "and", criteria4)).build();
    }

    private OceanDGStatus getDgStatusAfterApprovalResponse(OceanDGStatus currentStatus, TaskStatus approvalStatus) {
        if (currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED ?
                    OCEAN_DG_COMMERCIAL_ACCEPTED :
                    OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
        } else if (currentStatus == OCEAN_DG_REQUESTED) {
            return approvalStatus == TaskStatus.APPROVED ?
                    OceanDGStatus.OCEAN_DG_ACCEPTED :
                    OceanDGStatus.OCEAN_DG_REJECTED;
        }
        return null;
    }

    private OceanDGStatus determineDgStatusAfterApproval(OceanDGStatus dgStatus, boolean isOceanDgUser, ShipmentDetails shipmentDetails) {
        boolean isClass1 = checkForClass1(shipmentDetails);
        if (dgStatus == OCEAN_DG_ACCEPTED && !isClass1) {
            return dgStatus;
        }
        if ((dgStatus == OCEAN_DG_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_REJECTED) && isOceanDgUser) {
            return isClass1 ? OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED : OCEAN_DG_ACCEPTED;
        } else {
            return (dgStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || dgStatus == OCEAN_DG_COMMERCIAL_REJECTED) ? OCEAN_DG_COMMERCIAL_REQUESTED : OCEAN_DG_REQUESTED;
        }
    }

    private DBOperationType determineOperationType(OceanDGStatus dgStatus, boolean isOceanDgUser) {
        if (dgStatus == OCEAN_DG_REQUESTED && isOceanDgUser) return DG_APPROVE;
        return dgStatus == OCEAN_DG_REQUESTED
                ? DBOperationType.DG_REQUEST
                : COMMERCIAL_REQUEST;
    }

    private void sendEmailForDGApproval(ShipmentDetails shipmentDetails, String remarks)
            throws RunnerException {
        OceanDGStatus oceanDGStatus = shipmentDetails.getOceanDGStatus();
        OceanDGStatus templateStatus = emailTemplateForDGApproval(oceanDGStatus);

        if (templateStatus == null) {
            throw new RunnerException(String.format("User cannot send email in %s DGStatus", oceanDGStatus));
        }
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        VesselsResponse vesselsResponse = new VesselsResponse();
        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getDGEmailTemplate(emailTemplatesRequestMap)), executorService);
        var vesselResponseFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getVesselsData(shipmentDetails.getCarrierDetails(), vesselsResponse)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, vesselResponseFuture).join();
        Integer roleId = commonUtils.getRoleId(templateStatus);
        List<String> toUserEmails = commonUtils.getUserEmailsByRoleId(roleId);
        TaskCreateResponse taskCreateResponse = commonUtils.createTaskMDM(shipmentDetails, roleId);

        try {
            sendEmailForDGApproval(emailTemplatesRequestMap, toUserEmails, vesselsResponse, templateStatus, shipmentDetails, remarks,
                    taskCreateResponse);
        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }
    }

    private boolean checkForClass1(ShipmentDetails shipmentDetails) {
        if (shipmentDetails == null) return false;
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
        if (currentStatus == null || currentStatus == OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED || currentStatus == OceanDGStatus.OCEAN_DG_REJECTED) {
            return OCEAN_DG_REQUESTED;
        } else if (currentStatus == OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED || currentStatus == OCEAN_DG_COMMERCIAL_REQUESTED || currentStatus == OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED) {
            return OCEAN_DG_COMMERCIAL_REQUESTED;
        } else {
            return null;
        }
    }

    protected void sendEmailForDGApproval(Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap, List<String> toEmailIds,
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

    protected String generateEmailBody(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, String htmlTemplate) {
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
        if (tableTemplate.isEmpty()) return tableTemplate;

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
            if (!Boolean.TRUE.equals(packing.getHazardous())) continue;

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
        if (shipmentDetails.getContainersList() == null) return;
        for (Containers containers : shipmentDetails.getContainersList()) {
            if (!Boolean.TRUE.equals(containers.getHazardous())) continue;

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
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        log.info("{} | attachListShipment | Final Criteria: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listRequest));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, ShipmentService.tableNames);
        if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsPage.getContent(), true, request),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails console) {
        List<ConsoleShipmentMapping> consoleShipMappings = consoleShipmentMappingDao.findByConsolidationIdAll(request.getConsolidationId());
        List<Long> excludeShipments = consoleShipMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();

        if (request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()) {
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE, console.getTransportMode(), EQ, request);
        defaultRequest = CommonUtils.andCriteria(Constants.DIRECTION, console.getShipmentType(), EQ, defaultRequest);
        if (excludeShipments != null && !excludeShipments.isEmpty())
            defaultRequest = CommonUtils.andCriteria(ID, excludeShipments, "NOTIN", defaultRequest);

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


    /**
     * This method will provide Interbranch Pending Notifications - Includes both Pull & Push requests for shipment
     *
     * @param commonRequestModel
     * @return
     */

    @Override
    public ResponseEntity<IRunnerResponse> aibPendingNotification(CommonRequestModel commonRequestModel) {
        AibNotificationRequest request = (AibNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingShipmentActionsResponse> response = new PendingNotificationResponse<>();
        if (Objects.isNull(request.getId())) {
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

        if (commonUtils.getCurrentTenantSettings() == null || !Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled())) {
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

            Set<String> tenantIds = new HashSet<>();
            Set<String> locations = new HashSet<>();
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
            for (var mapping : mappingPage.getContent()) {
                if (!notificationResultMap.containsKey(mapping.getShipmentId())) {
                    notificationResultMap.put(mapping.getShipmentId(), new ArrayList<>());
                }
                if (!consoleVsShipIdMap.containsKey(mapping.getConsolidationId())) {
                    consoleVsShipIdMap.put(mapping.getConsolidationId(), new ArrayList<>());
                }
                consoleVsShipIdMap.get(mapping.getConsolidationId()).add(mapping.getShipmentId());
            }

            consolPage.getContent().stream().forEach(i -> {
                var res = mapToNotification(i, consoleShipmentsMap, v1TenantData, v1LocationData);
                consoleVsShipIdMap.get(i.getId()).forEach(shipId -> notificationResultMap.get(shipId).add(res));
            });

        } catch (Exception e) {
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

            mid = System.currentTimeMillis();
            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper old entity.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            if (Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) && checkIfAlreadyPushRequested(oldEntity.get())) {
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
            shipmentsV3Util.afterSaveforEt(entity, oldConvertedShipment, false, shipmentRequest, shipmentSettingsDetails, false);
            log.info("{} | completeUpdateShipment after save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            ShipmentDetails finalEntity1 = entity;
            String entityPayload = jsonHelper.convertToJson(finalEntity1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalEntity1.getId(), finalEntity1.getGuid())), executorServiceMasterData);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return shipmentDetailsMapper.map(entity);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(ERROR_MESSAGE + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }

    }

    public ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentEtV3Request request, boolean includeGuid) {

        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        if (request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(new HashSet<>(jsonHelper.convertValueToList(request.getConsolidationList().stream().toList(), ConsolidationDetails.class)));

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();

            if (shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty()) {
                for (Containers container : shipmentDetails.getContainersList()) {
                    addAuditLogContainers(container, null, shipmentId, DBOperationType.CREATE.name());
                }
            }

            shipmentsV3Util.afterSaveforEt(shipmentDetails, null, true, request, shipmentSettingsDetails, includeGuid);

            // audit logs
            createAuditLog(shipmentDetails, null, DBOperationType.CREATE.name());

            ShipmentDetails finalShipmentDetails1 = shipmentDetails;
            String entityPayload = jsonHelper.convertToJson(finalShipmentDetails1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalShipmentDetails1.getId(), finalShipmentDetails1.getGuid())), executorService);
        } catch (Exception e) {
            log.error(ERROR_MESSAGE + e.getStackTrace());
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
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException |
                 NoSuchMethodException e) {
            log.error(e.getMessage());
        }
    }


    private boolean isShipmentCargoFieldsChanged(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment) {
        return !Objects.equals(shipmentDetails.getNoOfPacks(), oldShipment.getNoOfPacks()) ||
                !Objects.equals(shipmentDetails.getPacksUnit(), oldShipment.getPacksUnit()) ||
                !Objects.equals(shipmentDetails.getWeight(), oldShipment.getWeight()) ||
                !Objects.equals(shipmentDetails.getWeightUnit(), oldShipment.getWeightUnit()) ||
                !Objects.equals(shipmentDetails.getVolume(), oldShipment.getVolume()) ||
                !Objects.equals(shipmentDetails.getVolumeUnit(), oldShipment.getVolumeUnit());
    }

    protected void setShipmentCargoFields(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment) {
        boolean packsAvailable = packingDao.checkPackingExistsForShipment(shipmentDetails.getId());
        if (packsAvailable) {
            shipmentDetails.setNoOfPacks(oldShipment.getNoOfPacks());
            shipmentDetails.setWeight(oldShipment.getWeight());
            shipmentDetails.setWeightUnit(oldShipment.getWeightUnit());
            shipmentDetails.setVolume(oldShipment.getVolume());
            shipmentDetails.setVolumeUnit(oldShipment.getVolumeUnit());
            shipmentDetails.setVolumetricWeight(oldShipment.getVolumetricWeight());
            shipmentDetails.setVolumetricWeightUnit(oldShipment.getVolumetricWeightUnit());
        }
    }
}
