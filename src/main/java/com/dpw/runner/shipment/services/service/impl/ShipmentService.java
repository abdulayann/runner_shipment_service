package com.dpw.runner.shipment.services.service.impl;


import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AUTO_REJECTION_REMARK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINS_HAZARDOUS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CREATED_AT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ERROR_WHILE_SENDING_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MPK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus.SAILED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;
import static com.dpw.runner.shipment.services.utils.CommonUtils.getIntFromString;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.UpdateConsoleShipmentRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ISequenceIncrementorDao;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AssignAllDialogDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateContainerSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateShipmentSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateShipmentSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentConsoleIdDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.patchRequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.CheckCreditLimitFromV1Request;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.request.oceanDG.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.dto.response.AllShipmentCountResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.DateTimeChangeLogResponse;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.response.GenerateCustomHblResponse;
import com.dpw.runner.shipment.services.dto.response.LatestCargoDeliveryInfo;
import com.dpw.runner.shipment.services.dto.response.MasterDataDescriptionResponse;
import com.dpw.runner.shipment.services.dto.response.MeasurementBasisResponse;
import com.dpw.runner.shipment.services.dto.response.NotesResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.billing.InvoicePostingValidationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceLiteContainerResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceLiteContainerResponse.LiteContainer;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.WayBillNumberFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.CheckActiveInvoiceResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TIContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TIResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.MblDuplicatedLog;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.UnitConversionUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.RestTemplate;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    @Autowired
    ExecutorService executorService;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    @Autowired
    private CSVParsingUtil<ShipmentDetails> parser;

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private MasterDataHelper masterDataHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPackingsSync packingsSync;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private ILogsHistoryService logsHistoryService;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IRoutingsService routingsService;

    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    UserContext userContext;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IEventService eventService;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private IHblDao hblDao;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    IHblService hblService;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISequenceIncrementorDao sequenceIncrementorDao;

    @Autowired
    private IOrderManagementAdapter orderManagementAdapter;

    @Value("${shipmentsKafka.queue}")
    private String senderQueue;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IHblSync hblSync;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private IDateTimeChangeLogService dateTimeChangeLogService;

    private SecureRandom rnd = new SecureRandom();

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private BillingServiceAdapter billingServiceAdapter;

    public static final String CONSOLIDATION_ID = "consolidationId";

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
    private List<String> WEIGHT_UNIT = Arrays.asList("KGS", "G", "DT");
    private List<String> VOLUME_UNIT = Arrays.asList("M3", "L3", "CC");
    private List<String> SHIPPING_LINE = Arrays.asList("DPWC", "MARUSK", "APLU");
    private List<String> LOCATIONS = Arrays.asList("Jabel Ali", "Nava Shiva", "Shanghai", "Vancouver", "Seattle");
    private List<String> PARTY_TYPE = Arrays.asList("CLIENT", "CONSIGNER", "CONSIGNEE");
    private List<String> DIRECTIONS = Arrays.asList("IMP", "EXP");
    private List<String> SOURCE = Arrays.asList("API", "Runner", "Logistics");

    private Map<String, Object> ADDRESS = Map.ofEntries(
            Map.entry("AddressShortCode", "Default")
    );
    private Map<String, Object> ORG = Map.ofEntries(
            Map.entry("TenantName", "DP WORLD LOGISTICS CANADA INC")
    );

    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("clientOrgCode", RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("clientAddressCode", RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(Integer.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("consignerAddressCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("consigneeAddressCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_MODE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.TRANSPORT_MODE).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry(Constants.DIRECTION, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.DIRECTION).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry(Constants.STATUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.STATUS).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry(Constants.JOB_TYPE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.JOB_TYPE).isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.SHIPMENT_ID).isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("screeningStatus").build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName("paidPlace").build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName("placeOfIssue").build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry(Constants.SHIPPING_LINE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.SHIPPING_LINE).isContainsText(true).build()),
            Map.entry(Constants.VESSEL, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VESSEL).build()),
            Map.entry(Constants.VOYAGE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VOYAGE).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry(Constants.ORIGIN_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.ORIGIN_PORT).build()),
            Map.entry(Constants.DESTINATION_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.DESTINATION_PORT).build()),
            Map.entry("originLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originLocCode").build()),
            Map.entry("destinationLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationLocCode").build()),
            Map.entry("originPortLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPortLocCode").build()),
            Map.entry("destinationPortLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPortLocCode").build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("id").build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(String.class).fieldName("consolidationNumber").build()),
            Map.entry(Constants.ORDER_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_NUMBER).build()),
            Map.entry(Constants.ORDER_MANAGEMENT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_NUMBER).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry(Constants.ORDER_MANAGEMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_ID).isContainsText(true).build()),
            Map.entry(Constants.FLIGHT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.FLIGHT_NUMBER).build()),
            Map.entry(CONSOLIDATION_ID, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(Long.class).fieldName("id").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("shipperRef", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(String.class).fieldName("shipperRef").build()),
            Map.entry(CONTAINS_HAZARDOUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).build()),
            Map.entry("shipmentPackStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(ShipmentPackStatus.class).build()),
            Map.entry(Constants.TENANT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.TENANT_ID).build()),
            Map.entry("cargoReadyDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoReadyDate").build()),
            Map.entry("cargoDeliveryDate", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("cargoDeliveryDate").build()),
            Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(CREATED_AT).build())
    );

    @Override
    @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) throws RunnerException {
        List<ShipmentDetails> response = new ArrayList<>();
        /**
         * BL details
         * Measurements
         * carrier
         * pickup
         * Delivery* *
         * Shipment details
         * Parties*
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ShipmentDetails shipmentDetail = createShipmentData();
            /**
             * Carrier Details*
             */

            shipmentDetail = shipmentDao.save(shipmentDetail, false);
            pushShipmentDataToDependentService(shipmentDetail, true, false, shipmentDetail.getContainersList());
        }

        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
        var map = getNotificationMap(PendingNotificationRequest.builder().shipmentIdList(shipmentIdList).build());
        lst.forEach(shipmentDetail -> {
            ShipmentListResponse response = modelMapper.map(shipmentDetail, ShipmentListResponse.class);
            containerCountUpdate(shipmentDetail, response);
            setEventData(shipmentDetail, response);
            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            response.setPendingActionCount(Optional.ofNullable(map.get(shipmentDetail.getId())).map(List::size).orElse(null));
            responseList.add(response);
        });
        try {
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorService);
            var containerDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setContainerTeuData(lst, responseList)), executorService);
            var billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchBillDataForShipments(lst, responseList)), executorService);
            var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorService);
            CompletableFuture.allOf(locationDataFuture, containerDataFuture, billDataFuture, vesselDataFuture, tenantDataFuture).join();
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
        }
        return responseList;
    }

    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    }
                    if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
//            container20Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont20)).count();
//            container40Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont40)).count();
//            container20GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20GP)).count();
//            container20RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20RE)).count();
//            container40GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40GP)).count();
//            container40RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40RE)).count();
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
    }

    private void setEventData(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        if (shipmentDetail.getEventsList() != null) {
            for (Events events : shipmentDetail.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(EventConstants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

    private ShipmentDetails createShipmentData() {
        int random = rnd.nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        StringBuilder salt = new StringBuilder();
        while (salt.length() < length) {
            salt.append(Constants.SALT_CHARS.charAt(Math.abs(this.rnd.nextInt() * Constants.SALT_CHARS.length()) % Constants.SALT_CHARS.length()));
        }
        return salt.toString();
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel)
    {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            if(request.getConsolidationList() != null)
                shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));
            if(request.getContainersList() != null)
                shipmentDetails.setContainersList(jsonHelper.convertValueToList(request.getContainersList(), Containers.class));
            shipmentDetails = getShipment(shipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails, null);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = new ArrayList<>();
            if (request.getPackingList() != null) {
                updatedPackings = packingDao.saveEntityFromShipment(jsonHelper.convertValueToList(request.getPackingList(), Packing.class), shipmentId);
                shipmentDetails.setPackingList(updatedPackings);
            }
            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(jsonHelper.convertValueToList(routingsRequest, Routings.class), shipmentId));

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (referenceNumbersRequest != null)
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            Hbl hbl = null;
            if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
                hbl = hblService.checkAllContainerAssigned(shipmentDetails, shipmentDetails.getContainersList(), updatedPackings);
            }

            List<NotesRequest> notesRequest = request.getNotesList();
            if (notesRequest != null) {
                for(NotesRequest req : notesRequest) {
                    req.setEntityId(shipmentId);
                }
            }
            if (notesRequest != null) {
                for(NotesRequest req : notesRequest) {
                    notesDao.save(jsonHelper.convertValue(req, Notes.class));
                }
            }
            String transactionId = shipmentDetails.getGuid().toString();
            pushShipmentDataToDependentService(shipmentDetails, true, false, null);
            try {
                shipmentDetails.setNotesList(null);
                shipmentSync.syncFromBooking(shipmentDetails, null, notesRequest);
            } catch (Exception e){
                log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
            }

            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
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
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();

        ShipmentDetailsResponse shipmentDetailsResponse = this.createShipment(request, false);

        return ResponseHelper.buildSuccessResponse(shipmentDetailsResponse);
    }

    private ShipmentDetailsResponse createShipment(ShipmentRequest request, boolean includeGuid) {
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        this.setColoadingStation(request);

        ShipmentDetails shipmentDetails = includeGuid ? jsonHelper.convertValue(request, ShipmentDetails.class) : jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);


            boolean syncConsole = beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached);

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();


            if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            {
                for (Containers container: shipmentDetails.getContainersList()) {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(container)
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentId)
                                        .operation(DBOperationType.CREATE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                        log.error(e.getMessage());
                    }
                }
            }

            afterSave(shipmentDetails, null, true, request, shipmentSettingsDetails, syncConsole, removedConsolIds, isNewConsolAttached, includeGuid);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            this.createLogHistoryForShipment(shipmentDetails);
            ShipmentDetails finalShipmentDetails = shipmentDetails;

        } catch (Exception e) {
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }


//        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
//        executorService.shutdownNow();
        return jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
    }

    @Override
    public ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentRequest shipmentRequest) {
        return this.createShipment(shipmentRequest, true);
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentId() == null){
            shipmentDetails.setShipmentId(generateShipmentId(shipmentDetails));
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
        //shipmentDetails = shipmentDao.findById(shipmentDetails.getId()).get();
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        packingDao.save(objectMapper.convertValue(partiesRequest, Packing.class));
    }


    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request) throws RunnerException {
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ShipmentDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= shipmentDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else{
            throw new RunnerException("Either Id or Guid is required");

        }
        return oldEntity;
    }
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException
    {
        List<ConsolidationDetailsRequest> consolidationDetails = new ArrayList<>();
        List<ContainerRequest> containerList = new ArrayList<>();
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(customerBookingRequest.getId(), "CustomerBooking");
        if(isConsoleCreationNeeded(customerBookingRequest))
        {
            ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                            .destination(customerBookingRequest.getCarrierDetails().getDestination())
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                            .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .build()
                    ).
                    consolidationType("STD").
                    transportMode(customerBookingRequest.getTransportType()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    shipmentType(customerBookingRequest.getDirection()).
                    referenceNumber(customerBookingRequest.getBookingNumber()).
                    departureDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            type("Departure").
                            build()
                    ).
                    arrivalDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            type("Arrival").
                            build()
                    ).
                    containersList(customerBookingRequest.getContainersList()).
                    sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                    build();
            ResponseEntity<?> consolidationDetailsResponse = consolidationService.createFromBooking(CommonRequestModel.buildRequest(consolidationDetailsRequest));
            if(consolidationDetailsResponse != null)
            {
                ConsolidationDetailsResponse consolDetailsResponse = (ConsolidationDetailsResponse) (((RunnerResponse)consolidationDetailsResponse.getBody()).getData());
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList();
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().
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
                bookingReference(customerBookingRequest.getBookingNumber()).
                bookingCreatedDate(customerBookingRequest.getBookingDate()).
                shipmentCreatedOn(LocalDateTime.now()).
                client(createPartiesRequest(customerBookingRequest.getCustomer())).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee())).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor())).
                additionalDetails(AdditionalDetailRequest.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty())).
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
                containersList(consolidationDetails != null && consolidationDetails.size() > 0 ? containerList : null).
                packingList(customerBookingRequest.getPackingList() != null ? customerBookingRequest.getPackingList().stream().map(obj -> {
                    if(!StringUtility.isEmpty(obj.getLengthUnit()))
                    {
                        obj.setWidthUnit(obj.getLengthUnit());
                        obj.setHeightUnit(obj.getLengthUnit());
                    }
                    return obj;
                }).collect(Collectors.toList()) : null).
                fileRepoList(customerBookingRequest.getFileRepoList()).
                routingsList(customerBookingRequest.getRoutingList()).
                consolidationList(isConsoleCreationNeeded(customerBookingRequest) ? consolidationDetails : null).
                notesList(createNotes(notes)).
                sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                consolRef(consolidationDetails != null && consolidationDetails.size() > 0 ? consolidationDetails.get(0).getReferenceNumber() : "").
                masterBill(consolidationDetails != null && consolidationDetails.size() > 0 ? consolidationDetails.get(0).getBol() : null).
                freightLocalCurrency(UserContext.getUser().CompanyCurrency).
                currentPartyForQuote(customerBookingRequest.getCurrentPartyForQuote()).
                autoUpdateWtVol(true).
                build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = calculateShipmentWV(jsonHelper.convertValue(shipmentRequest, AutoUpdateWtVolRequest.class));
        shipmentRequest.setNoOfPacks(getIntFromString(autoUpdateWtVolResponse.getNoOfPacks()));
        shipmentRequest.setPacksUnit(autoUpdateWtVolResponse.getPacksUnit());
        shipmentRequest.setWeight(autoUpdateWtVolResponse.getWeight());
        shipmentRequest.setWeightUnit(autoUpdateWtVolResponse.getWeightUnit());
        shipmentRequest.setVolume(autoUpdateWtVolResponse.getVolume());
        shipmentRequest.setVolumeUnit(autoUpdateWtVolResponse.getVolumeUnit());
        shipmentRequest.setChargable(autoUpdateWtVolResponse.getChargable());
        shipmentRequest.setChargeableUnit(autoUpdateWtVolResponse.getChargeableUnit());
        shipmentRequest.setVolumetricWeight(autoUpdateWtVolResponse.getVolumetricWeight());
        shipmentRequest.setVolumetricWeightUnit(autoUpdateWtVolResponse.getVolumetricWeightUnit());
        shipmentRequest.setNetWeight(autoUpdateWtVolResponse.getNetWeight());
        shipmentRequest.setNetWeightUnit(autoUpdateWtVolResponse.getNetWeightUnit());
        shipmentRequest.setInnerPacks(autoUpdateWtVolResponse.getInnerPacks());
        shipmentRequest.setInnerPackUnit(autoUpdateWtVolResponse.getInnerPackUnit());
        shipmentRequest.setOrderManagementId(customerBookingRequest.getOrderManagementId());
        shipmentRequest.setOrderManagementNumber(customerBookingRequest.getOrderManagementNumber());

        if(customerBookingRequest.getOrderManagementId()!=null){
            ShipmentDetails shipmentDetails = null;
            shipmentDetails = orderManagementAdapter.getOrderByGuid(customerBookingRequest.getOrderManagementId());

            if(shipmentDetails!=null){
                if(shipmentDetails.getGoodsDescription()!=null)
                    shipmentRequest.setGoodsDescription(shipmentDetails.getGoodsDescription());

                if(shipmentDetails.getReferenceNumbersList()!=null){
                    List<ReferenceNumbersRequest> referenceNumbersList = jsonHelper.convertValue(shipmentDetails.getReferenceNumbersList(), new TypeReference<List<ReferenceNumbersRequest>>() {});
                    shipmentRequest.setReferenceNumbersList(referenceNumbersList);
                }

                if(shipmentDetails.getAdditionalDetails()!=null){
                    if(shipmentDetails.getAdditionalDetails().getImportBroker()!=null){
                        PartiesRequest importBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getImportBroker(), PartiesRequest.class);
                        shipmentRequest.getAdditionalDetails().setImportBroker(importBroker);
                    }

                    if(shipmentDetails.getAdditionalDetails().getExportBroker()!=null){
                        PartiesRequest exportBroker = jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getExportBroker(), PartiesRequest.class);
                        shipmentRequest.getAdditionalDetails().setExportBroker(exportBroker);
                    }
                }
            }

        }

        shipmentRequest.setContainsHazardous(customerBookingRequest.getIsDg());
        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest));
    }

    public boolean isConsoleCreationNeeded(CustomerBookingRequest customerBookingRequest) {
        return (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL)) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_ROA) &&
                        (Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FTL) || Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL)) ) ||
                (Objects.equals(customerBookingRequest.getTransportType(), Constants.TRANSPORT_MODE_RAI) && Objects.equals(customerBookingRequest.getCargoType(), Constants.CARGO_TYPE_FCL));
    }

    private List<NotesRequest> createNotes(List<Notes> notes){
        if(notes == null) return null;
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

    private PartiesRequest createPartiesRequest(PartiesRequest party)
    {
        if(party == null)
            return null;
        return PartiesRequest.builder()
                .addressCode(party.getAddressCode())
                .addressData(party.getAddressData())
                .orgCode(party.getOrgCode())
                .orgData(party.getOrgData())
                .build();
    }

    private List<PackingRequest> setPackingDetails(List<PackingRequest> packingRequests, String transportMode, Long consolidationId) {
        if(packingRequests != null && packingRequests.size() > 0) {
            for (PackingRequest packingRequest : packingRequests) {
                if(!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                    packingRequest.setConsolidationId(consolidationId);
                }
            }
        }
        return packingRequests;
    }

    private List<ContainerRequest> calculateAutoContainerWeightAndVolume(List<ContainerRequest> containersList, List<PackingRequest> packingList) throws RunnerException {
        if(containersList != null && containersList.size() > 0) {
            for (ContainerRequest containers : containersList) {
                if(packingList != null) {
                    List<PackingRequest> packings = packingList.stream().filter(packing -> Objects.equals(packing.getContainerId(), containers.getId())).toList();
                    BigDecimal totalWeight = BigDecimal.ZERO;
                    BigDecimal totalVolume = BigDecimal.ZERO;
                    if(packings != null && packings.size() > 0) {
                        if(IsStringNullOrEmpty(containers.getGrossWeightUnit()))
                            containers.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
                        if(IsStringNullOrEmpty(containers.getGrossVolumeUnit()))
                            containers.setGrossVolumeUnit(Constants.VOLUME_UNIT_M3);
                        for (PackingRequest packing : packings) {
                            if(!IsStringNullOrEmpty(packing.getWeightUnit()))
                                totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), containers.getGrossWeightUnit()).toString()));
                            if(!IsStringNullOrEmpty(packing.getVolumeUnit()))
                                totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), containers.getGrossVolumeUnit()).toString()));
                            if(Boolean.TRUE.equals(packing.getHazardous()))
                                containers.setHazardous(true);
                        }
                        containers.setGrossWeight(totalWeight);
                        containers.setGrossVolume(totalVolume);
                    }
                }
            }
        }
        return containersList;
    }

    private boolean makeContainersDGFromPack(List<ContainerRequest> containersList, List<PackingRequest> packingList) {
        AtomicBoolean dgApprovalReqd = new AtomicBoolean(false);
        Set<Long> dgConts = new HashSet<>();
        packingList.stream().forEach(pack -> {
            if(Boolean.TRUE.equals(pack.getHazardous())) {
                dgConts.add(pack.getContainerId());
                if(!IsStringNullOrEmpty(pack.getDGClass()))
                    dgApprovalReqd.set(true);
            }
        });
        dgConts.remove(null);
        containersList.stream().forEach(container -> {
            if(!Objects.isNull(container.getId()) && dgConts.contains(container.getId()))
                container.setHazardous(true);
            if(!IsStringNullOrEmpty(container.getDgClass()))
                dgApprovalReqd.set(true);
        });
        return dgApprovalReqd.get();
    }

    public ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = calculateShipmentWV(request);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
            response = calculateVW(request, response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AutoUpdateWtVolResponse calculateShipmentWV(AutoUpdateWtVolRequest request) throws RunnerException {
        AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
        List<Packing> packingList = new ArrayList<>();
        if(request.getPackingList() != null)
            packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Containers> containersList = new ArrayList<>();
        if(request.getContainersList() != null)
            containersList = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
//        if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
//            response = calculatePacksAndPacksUnit(packingList, response);
//        }
        response = calculatePacksAndPacksUnit(packingList, response);
        response = calculateWeightAndVolumeUnit(request, packingList, response);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isPacksPresent = packingList != null && packingList.size() > 0;
        if(!isPacksPresent)
            response = updateShipmentDetails(response, containersList);
        response = calculateVW(request, response, true);
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer().booleanValue()
                || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) || isPacksPresent) {
            ShipmentMeasurementDetailsDto dto = new ShipmentMeasurementDetailsDto();
            response.setPackSummary(packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), dto));
            if(request.getTransportMode() != null && ((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)
            && request.getShipmentType() != null && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL))
            || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR))) {
                response.setInnerPacks(dto.getInnerPacks());
                response.setInnerPackUnit(dto.getInnerPackUnit());
            }
            if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer()
            && request.getPackingList() != null && request.getPackingList().size() > 0) {
                response.setWeight(dto.getWeight());
                response.setWeightUnit(dto.getWeightUnit());
                response.setVolume(dto.getVolume());
                response.setVolumeUnit(dto.getVolumeUnit());
                response.setNetWeight(dto.getNetWeight());
                response.setNetWeightUnit(dto.getNetWeightUnit());
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
            else if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer()){
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(v1TenantSettingsResponse.getP100Branch()) && Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)) {
            response = calculatePacksAndPacksUnitFromContainer(response, containersList);
        }
        return response;
    }

    private AutoUpdateWtVolResponse updateShipmentDetails(AutoUpdateWtVolResponse response, List<Containers> containersList) throws RunnerException { // to account for updateShipmentDetails flag in v1 container summary
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
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        if(containersList != null) {
            for (Containers containers : containersList) {
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    packageCount = packageCount + Long.parseLong(containers.getPacks());
                totalVolume = totalVolume + volume;
                if(containers.getContainerCount() != null)
                    totalContainerCount = totalContainerCount + containers.getContainerCount();
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(containers.getPacks());
            }
        }
        if (containersList.size() > 0 ) {
            packsUnit = setPacksUnit(containersList, packsUnit);
        }
        response.setWeight(new BigDecimal(totalWeight));
        response.setVolume(new BigDecimal(totalVolume));
        response.setWeightUnit(toWeightUnit);
        response.setVolumeUnit(toVolumeUnit);
        response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
        response.setPacksUnit(packsUnit);
        response.setNetWeight(new BigDecimal(tareWeight));
        response.setNetWeightUnit(toWeightUnit);
        return response;
    }

    private AutoUpdateWtVolResponse calculatePacksAndPacksUnitFromContainer(AutoUpdateWtVolResponse response, List<Containers> containersList) {
        if(containersList != null && containersList.size() > 0) {
            String packsUnit = "";
            long packageCount = 0;
            long totalPacks = 0;
            for (Containers container : containersList) {
                if (!IsStringNullOrEmpty(container.getPacks())) {
                    packageCount = packageCount + Integer.parseInt(container.getPacks());
                    totalPacks = totalPacks + Integer.parseInt(container.getPacks());
                }
            };
            packsUnit = setPacksUnit(containersList, packsUnit);
            response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
            response.setPacksUnit(packsUnit);
        }
        return response;
    }

    private String setPacksUnit(List<Containers> containersList, String packsUnit) {
        String firstPacksType = containersList.get(0).getPacksType();
        boolean isSame = containersList.stream()
                .map(Containers::getPacksType)
                .allMatch(packsType -> packsType == null || packsType.equals(firstPacksType));

        if (isSame) {
            packsUnit = firstPacksType;
        } else {
            packsUnit = Constants.MPK;
        }
        return packsUnit;
    }

    private AutoUpdateWtVolResponse calculateWeightAndVolumeUnit(AutoUpdateWtVolRequest request, List<Packing> packings, AutoUpdateWtVolResponse response) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if(IsStringNullOrEmpty(request.getWeightUnit()))
            response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        if(IsStringNullOrEmpty(request.getVolumeUnit()))
            response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(packing.getWeight() != null && !IsStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if(packing.getVolume() != null && !IsStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
            }
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(request, response, false);
        }
        return response;
    }

    private AutoUpdateWtVolResponse calculateVW(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, boolean recalculateVwObInKgAndM3) throws RunnerException{
        if(IsStringNullOrEmpty(request.getTransportMode()))
            return response;
        if(!IsStringNullOrEmpty(response.getWeightUnit()) && !IsStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                response.setChargable(new BigDecimal(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                if(!IsStringNullOrEmpty(request.getShipmentType()) && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                    double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                    response.setChargable(new BigDecimal(Math.max(wtInKg/1000, volInM3)));
                    response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                    if(recalculateVwObInKgAndM3)
                        vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, new BigDecimal(wtInKg), new BigDecimal(volInM3));
                }
            }
            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    private <T> T calculatePacksAndPacksUnit(List<Packing> packings, T response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        String packingUnit = null;
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(!IsStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                if (tempPackingUnit == null) {
                    tempPackingUnit = packing.getPacksType();
                    packingUnit = packing.getPacksType();
                }
                else {
                    if(!IsStringNullOrEmpty(packing.getPacksType()) && tempPackingUnit.equals(packing.getPacksType())) {
                        packingUnit = Constants.MPK;
                    }
                }
            }
        }
        if(response instanceof AutoUpdateWtVolResponse autoUpdateWtVolResponse) {
            autoUpdateWtVolResponse.setNoOfPacks(totalPacks.toString());
            autoUpdateWtVolResponse.setPacksUnit(packingUnit);
        } else if(response instanceof MeasurementBasisResponse measurementBasisResponseas) {
            measurementBasisResponseas.setPackCount(totalPacks);
        }
        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request can't be null");
        }

        Optional<ShipmentDetails>oldEntity =retrieveByIdOrGuid(request);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        // update Ata/Atd in shipment from events
        eventService.updateAtaAtdInShipment(entity.getEventsList(), entity, shipmentSettingsDetails);
        entity = shipmentDao.update(entity, false);
            pushShipmentDataToDependentService(entity, false, Boolean.TRUE.equals(request.getIsAutoSellRequired()), oldEntity.get().getContainersList());
        try {
            shipmentSync.sync(entity, null, null, entity.getGuid().toString(), false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        return ResponseHelper.buildSuccessResponse(objectMapper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(shipmentRequest);
        ShipmentDetailsResponse response = completeUpdateShipment(shipmentRequest);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private ShipmentDetailsResponse completeUpdateShipment(ShipmentRequest shipmentRequest) throws RunnerException {
        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        long id=oldEntity.get().getId();
        Integer previousStatus = oldEntity.get().getStatus();
        if (!oldEntity.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Long> removedConsolIds = new ArrayList<>();
            MutableBoolean isNewConsolAttached = new MutableBoolean(false);

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());

            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);

            if(Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) &&  checkIfAlreadyPushRequested(oldEntity.get())) {
                throw new ValidationException("Push request is already in progress, Cannot change Consolidation Type.");
            }
            boolean syncConsole = beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, removedConsolIds, isNewConsolAttached);

            entity = shipmentDao.update(entity, false);


            try {
                // audit logs
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(entity)
                                .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(entity.getId())
                                .operation(DBOperationType.UPDATE.name()).build()
                );
            }
            catch (Exception e) {
                log.error("Error creating audit service log", e);
            }

            afterSave(entity, oldConvertedShipment, false, shipmentRequest, shipmentSettingsDetails, syncConsole, removedConsolIds, isNewConsolAttached, false);
            this.createLogHistoryForShipment(entity);
            ShipmentDetails finalEntity = entity;
            return shipmentDetailsMapper.map(entity);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    @Override
    public ShipmentDetailsResponse completeUpdateShipmentFromEntityTransfer(ShipmentRequest shipmentRequest) throws RunnerException {
        return this.completeUpdateShipment(shipmentRequest);
    }

    private void setColoadingStation(ShipmentRequest request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    private void setColoadingStation(ShipmentDetails request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(request.getDirection(), Constants.DIRECTION_EXP)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    public void createLogHistoryForShipment(ShipmentDetails shipmentDetails){
        try {
            String entityPayload = jsonHelper.convertToJson(shipmentDetails);
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(shipmentDetails.getId())
                    .entityType(Constants.SHIPMENT).entityGuid(shipmentDetails.getGuid()).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory : " + ex.getMessage());
        }
    }

    private void syncShipment(ShipmentDetails shipmentDetails, Hbl hbl, List<UUID> deletedContGuids, List<Packing> packsForSync, ConsolidationDetails consolidationDetails, boolean syncConsole) {
        String transactionId = shipmentDetails.getGuid().toString();
        try {
            shipmentSync.sync(shipmentDetails, deletedContGuids, null, transactionId, false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        if(hbl != null) {
            try {
                hblSync.sync(hbl, transactionId);
            }
            catch (Exception e) {
                log.error("Error performing sync on hbl entity, {}", e);
            }
        }
        if(syncConsole && consolidationDetails != null) {
            try {
                consolidationSync.sync(consolidationDetails, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
        if(packsForSync != null) {
            try {
                packingsSync.sync(packsForSync, transactionId);
            } catch (Exception e) {
                log.error("Error performing sync on packings list, {}", e);
            }
        }
    }
    private boolean beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached) throws RunnerException{
        CarrierDetails oldCarrierDetails = null;
        if(!isCreate)
            oldCarrierDetails = jsonHelper.convertValue(oldEntity.getCarrierDetails(), CarrierDetails.class);
        CarrierDetails finalOldCarrierDetails = oldCarrierDetails;
        var carrierDetailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.updateUnLocData(shipmentDetails.getCarrierDetails(), finalOldCarrierDetails)));
        List<Long> tempConsolIds = new ArrayList<>();
        Long id = !Objects.isNull(oldEntity) ? oldEntity.getId() : null;
        boolean syncConsole = false;

        if(shipmentDetails.getCarrierDetails() != null) {
            if (shipmentDetails.getTransportMode() != null && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setVoyage(null);
            } else {
                shipmentDetails.getCarrierDetails().setFlightNumber(null);
            }
        }

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            Set<Long> oldConsolIds = Objects.isNull(oldEntity) ? null : oldEntity.getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toSet());
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                    if(!Objects.isNull(oldConsolIds) && oldConsolIds.contains(consolidation.getId()))
                        oldConsolIds.remove(consolidation.getId());
                }
            }
            if(!Objects.isNull(oldConsolIds)) {
                for (Long oldConsoleId: oldConsolIds)
                    removedConsolIds.add(oldConsoleId);
            }

            if(!consolidationDetailsRequests.isEmpty() && (oldEntity == null || oldEntity.getConsolidationList() == null ||  oldEntity.getConsolidationList().size() == 0 || removedConsolIds.size() > 0)) {
                isNewConsolAttached.setTrue();
            }
        }
        else {
            shipmentDetails.setConsolRef(null);
            tempConsolIds = Objects.isNull(oldEntity) ? new ArrayList<>() : oldEntity.getConsolidationList().stream().map(e -> e.getId()).toList();
        }

        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && !isAirDgUser()) {
            if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                if((removedConsolIds != null && !removedConsolIds.isEmpty()) || Boolean.TRUE.equals(isNewConsolAttached.getValue()))
                    throw new RunnerException("You do not have Air DG permissions to attach or detach consolidation as it is a DG Shipment");
            } else {
                if((removedConsolIds != null && !removedConsolIds.isEmpty() && oldEntity != null && oldEntity.getConsolidationList() != null && Boolean.TRUE.equals(oldEntity.getConsolidationList().get(0).getHazardous()))
                    || (consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty() && Boolean.TRUE.equals(consolidationDetailsRequests.get(0).getHazardous()))) {
                    throw new RunnerException("You do not have Air DG permissions to edit this as it is a part of DG Consol");
                }
            }
        }

        List<PackingRequest> packingRequest = shipmentRequest.getPackingList();
        List<ContainerRequest> containerRequest = shipmentRequest.getContainersList();

        if(removedConsolIds != null && removedConsolIds.size() > 0) {
            shipmentDetails.setConsolRef(null);
            List<Containers> allConsolConts = new ArrayList<>();
            for(Long consolidationId: removedConsolIds) {
                List<Containers> containersList = containerDao.findByConsolidationId(consolidationId);
                if(containersList != null && containersList.size() > 0) {
                    allConsolConts.addAll(containersList);
                }
            }
            if(allConsolConts.size() > 0) {
                if(Objects.isNull(containerRequest) && !Objects.isNull(oldEntity))
                    containerRequest = jsonHelper.convertValueToList(oldEntity.getContainersList(), ContainerRequest.class);
                containerRequest.removeIf(obj2 -> allConsolConts.stream().anyMatch(obj1 -> obj1.getId().equals(obj2.getId())));
            }
        }

        boolean oceanDgApprovalReqd = false;
        if(shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate().booleanValue() && packingRequest != null) {
            if(Objects.isNull(containerRequest) && !Objects.isNull(oldEntity))
                containerRequest = jsonHelper.convertValueToList(oldEntity.getContainersList(), ContainerRequest.class);
            containerRequest = calculateAutoContainerWeightAndVolume(containerRequest, packingRequest);
        }
        if(Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            oceanDgApprovalReqd = makeContainersDGFromPack(containerRequest, packingRequest);
        if(oceanDgApprovalReqd && Objects.isNull(shipmentDetails.getOceanDGStatus()))
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);

        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && shipmentDetails.getConsolidationList().size() > 0)
            consolidationId = shipmentDetails.getConsolidationList().get(0).getId();
        List<Containers> updatedContainers = new ArrayList<>();

        if (containerRequest != null) {
            for (ContainerRequest containerRequest1 : containerRequest) {
                containerRequest1.setConsolidationId(consolidationId);
                if(Boolean.TRUE.equals(containerRequest1.getHazardous()))
                    shipmentDetails.setContainsHazardous(true);
            }
            updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequest, Containers.class, isCreate), consolidationId, id, false);
        } else if (!Objects.isNull(oldEntity)){
            updatedContainers = oldEntity.getContainersList();
        }
        shipmentDetails.setContainersList(updatedContainers);
        ConsolidationDetails consolidationDetails = null;

        if(updatedContainers.size() > 0 || (shipmentRequest.getAutoCreateConsole() != null  && shipmentRequest.getAutoCreateConsole())) {
            if((tempConsolIds == null || tempConsolIds.size() == 0) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
                consolidationDetails = createConsolidation(shipmentDetails, updatedContainers);
                if(!Objects.isNull(consolidationDetails)) {
                    shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
                    if(IsStringNullOrEmpty(shipmentDetails.getMasterBill()))
                        shipmentDetails.setMasterBill(consolidationDetails.getBol());
                    syncConsole = true;
                }
            }
        }
        validateBeforeSave(shipmentDetails);


        if(Boolean.TRUE.equals(isNewConsolAttached.getValue())) {
            ConsolidationDetails consolidationDetails1 = shipmentDetails.getConsolidationList().get(0);
            if(shipmentDetails.getCargoDeliveryDate() != null && consolidationDetails1.getLatDate() != null && consolidationDetails1.getLatDate().isAfter(shipmentDetails.getCargoDeliveryDate())) {
                throw new RunnerException("Cargo Delivery Date is lesser than LAT Date.");
            }
            shipmentDetails.setMasterBill(consolidationDetails1.getBol());
            shipmentDetails.setDirection(consolidationDetails1.getShipmentType());
            if (shipmentDetails.getCarrierDetails() == null) {
                shipmentDetails.setCarrierDetails(new CarrierDetails());
            }
            if (consolidationDetails1.getCarrierDetails() != null) {
                shipmentDetails.getCarrierDetails().setVoyage(consolidationDetails1.getCarrierDetails().getVoyage());
                shipmentDetails.getCarrierDetails().setVessel(consolidationDetails1.getCarrierDetails().getVessel());
                shipmentDetails.getCarrierDetails().setShippingLine(consolidationDetails1.getCarrierDetails().getShippingLine());
                shipmentDetails.getCarrierDetails().setAircraftType(consolidationDetails1.getCarrierDetails().getAircraftType());
                shipmentDetails.getCarrierDetails().setCfs(consolidationDetails1.getCarrierDetails().getCfs());

                if(Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                    shipmentDetails.getCarrierDetails().setFlightNumber(consolidationDetails1.getCarrierDetails().getFlightNumber());
                    shipmentDetails.getCarrierDetails().setOriginPort(consolidationDetails1.getCarrierDetails().getOriginPort());
                    shipmentDetails.getCarrierDetails().setDestinationPort(consolidationDetails1.getCarrierDetails().getDestinationPort());
                    shipmentDetails.getCarrierDetails().setEtd(consolidationDetails1.getCarrierDetails().getEtd());
                    shipmentDetails.getCarrierDetails().setEta(consolidationDetails1.getCarrierDetails().getEta());
                    shipmentDetails.getCarrierDetails().setAtd(consolidationDetails1.getCarrierDetails().getAtd());
                    shipmentDetails.getCarrierDetails().setAta(consolidationDetails1.getCarrierDetails().getAta());
                }
            }
            var console = shipmentDetails.getConsolidationList().get(0);
            if (!Objects.isNull(console) && !Objects.isNull(console.getId()))
                awbDao.validateAirMessaging(console.getId());
            if(!isCreate) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", null);
                listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);
                if(!consoleShipmentMappingsForEmails.isEmpty()) {
                    consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
                    List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(e -> e.getConsolidationId()).toList();
                    List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
                    commonUtils.sendRejectionEmailsExplicitly(List.of(shipmentDetails), consoleShipmentMappingsForEmails, new HashSet<>(), otherConsolidationDetails);
                }
            }
        }

        if(shipmentDetails.getReceivingBranch() != null && shipmentDetails.getReceivingBranch() == 0)
            shipmentDetails.setReceivingBranch(null);
        if(shipmentDetails.getTriangulationPartner() != null && shipmentDetails.getTriangulationPartner() == 0)
            shipmentDetails.setTriangulationPartner(null);
        if(shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);

        if(Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentDetails.getId());
            if(!hbls.isEmpty()) {
                hblDao.delete(hbls.get(0));
            }
            shipmentDetails.getAdditionalDetails().setDraftPrinted(false);
        }
        if(checkOriginalPrintedForJobTypeChange(shipmentDetails, oldEntity)){
            throw new ValidationException("Consolidation type cannot be changed as the original BL has been generated for this shipment.");
        }
        if(checkDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails)) {
            List<Awb> awbs = awbDao.findByShipmentId(shipmentDetails.getId());
            if(!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }

        dateTimeChangeLogService.createEntryFromShipment(shipmentRequest, oldEntity);

        if(checkIfLCLConsolidationEligible(shipmentDetails))
            updateShipmentGateInDateAndStatusFromPacks(packingRequest, shipmentDetails);
        CompletableFuture.allOf(carrierDetailsFuture).join();
        return syncConsole;
    }



    public boolean checkIfLCLConsolidationEligible(ShipmentDetails shipmentDetails) {
        if(!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            return false;
        if(!Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
            return false;
        if(!Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType()))
            return false;
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    private void updateShipmentGateInDateAndStatusFromPacks(List<PackingRequest> packingRequests, ShipmentDetails shipmentDetails) throws RunnerException {
        shipmentDetails.setShipmentPackStatus(null);
        if(packingRequests != null && !packingRequests.isEmpty()) {
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.BOOKED);
            boolean fullGated = true;
            boolean partialGated = false;
            boolean fullAssigned = true;
            boolean partialAssigned = false;
            LocalDateTime maxDate = null;
            for (PackingRequest packingRequest: packingRequests) {
                if(packingRequest.getCargoGateInDate() != null) {
                    if(ACTUAL.equals(packingRequest.getDateType()))
                        partialGated = true;
                    else
                        fullGated = false;
                    if(maxDate == null || packingRequest.getCargoGateInDate().isAfter(maxDate)) {
                        shipmentDetails.setShipmentGateInDate(packingRequest.getCargoGateInDate());
                        shipmentDetails.setDateType(packingRequest.getDateType());
                        maxDate = packingRequest.getCargoGateInDate();
                    }
                }
                else
                    fullGated = false;
                if(packingRequest.getContainerId() != null)
                    partialAssigned = true;
                else
                    fullAssigned = false;
            }
            if(partialAssigned)
                shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIALLY_ASSIGNED);
            if(fullAssigned)
                shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.ASSIGNED);
            if(partialGated)
                shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIAL_CARGO_GATE_IN);
            if(fullGated)
                shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.CARGO_GATED_IN);
        }
        if(shipmentDetails.getCarrierDetails() != null && shipmentDetails.getCarrierDetails().getAtd() != null)
            shipmentDetails.setShipmentPackStatus(SAILED);
        if(shipmentDetails.getShipmentGateInDate() != null) {
            if(shipmentDetails.getConsolidationList() != null && !shipmentDetails.getConsolidationList().isEmpty()
                    && shipmentDetails.getConsolidationList().get(0).getCfsCutOffDate() != null) {
                if(shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getConsolidationList().get(0).getCfsCutOffDate()))
                    throw new RunnerException("Shipment Gate In date should not be greater than the CFS Cut Off Date entered at the consolidation level.");
            }
            else if(shipmentDetails.getCarrierDetails().getEtd() != null && shipmentDetails.getShipmentGateInDate().isAfter(shipmentDetails.getCarrierDetails().getEtd()))
                throw new RunnerException("Shipment Gate In Date cannot be greater than ETD.");
        }
    }

    private boolean checkOriginalPrintedForJobTypeChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(oldEntity == null)
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA))
            return false;
        if(!Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP))
            return false;
        if(!Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal()))
            return false;
        return !Objects.equals(shipmentDetails.getJobType(), oldEntity.getJobType());
    }

    private boolean checkDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
        if(oldEntity == null)
            return false;
        if(!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()))
            return false;
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if(!Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT))
            return false;
        return !Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(shipmentDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(shipmentDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    private void validateBeforeSave(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null)
        {
            if(shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
                throw new ValidationException("Consignor & Consignee parties can't be selected as same.");
        }
        if(!IsStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            if(!IsStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            }
            else if(!IsStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }
//        Credit Limit check while shipment creation is removed for now
//        v1ServiceUtil.validateCreditLimit(shipmentDetails.getClient(), ShipmentConstants.SHIPMENT_CREATION, shipmentDetails.getGuid(), false);

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            ConsolidationDetails console = shipmentDetails.getConsolidationList().get(0);
            ConsolidationDetails tempConsole = new ConsolidationDetails();
            tempConsole.setId(console.getId());
            if(console.equals(tempConsole)){
                console = consolidationDetailsDao.findById(console.getId()).get();
                shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(console)));
            }
            shipmentDetails.setConsolRef(shipmentDetails.getConsolidationList().get(0).getReferenceNumber());
        }

    }

    public void validateRaKcDetails(ShipmentDetails shipmentDetails) throws RunnerException {
        Parties consignor = shipmentDetails.getConsigner();
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            List<Parties> orgList = new ArrayList<>();
            if(consignor != null) {
                if(consignor != null && StringUtility.isNotEmpty(consignor.getAddressCode())) {
                    orgList.add(consignor);
                }
            }

            if(shipmentDetails.getId() != null && shipmentDetails.getAdditionalDetails() != null) {
                if(shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode())) {
                    orgList.add(shipmentDetails.getAdditionalDetails().getExportBroker());
                }
            }

            if(orgList.size() > 0) {
                OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(orgList);
                if (orgAddressResponse != null) {
                    Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
                    int countOfExpiredParties = 0;
                    int countOfShipmentRaKcParties = 0;
                    for(var entry : addressMap.entrySet()) {
                        if (entry.getValue() != null && StringUtility.isNotEmpty(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)))) {
                            LocalDateTime agentExpiry = LocalDateTime.parse(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)));
                            // if any one of the agent is not expired will apply the validations as is
                            countOfShipmentRaKcParties++;
                            if (LocalDateTime.now().isAfter(agentExpiry))
                                countOfExpiredParties++;
                        }
                    }
                    if(countOfExpiredParties == countOfShipmentRaKcParties && countOfExpiredParties > 0)
                        return;
                    if(consignor != null) {
                        if (addressMap.containsKey(consignor.getOrgCode() + "#" + consignor.getAddressCode())) {
                            Map<String, Object> addressConsignorAgent = addressMap.get(consignor.getOrgCode() + "#" + consignor.getAddressCode());
                            if (addressConsignorAgent.containsKey(Constants.KNOWN_CONSIGNOR)) {
                                var rakcType = addressConsignorAgent.get(Constants.KNOWN_CONSIGNOR);
                                if (rakcType != null && Boolean.TRUE.equals(rakcType) && (shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                                        shipmentDetails.getSecurityStatus() == null)) {
                                    throw new RunnerException("Screening Status and Security Status is mandatory for KC consginor.");
                                }
                                else if(shipmentDetails.getAdditionalDetails().getScreeningStatus() != null && shipmentDetails.getAdditionalDetails().getScreeningStatus().size() == 1 && shipmentDetails.getAdditionalDetails().getScreeningStatus().get(0).equals("VCK")) {
                                    throw new ValidationException("Please select an additional screening status along with VCK.");
                                }
                            }
                        }
                    }

                    if(shipmentDetails.getId() != null && shipmentDetails.getAdditionalDetails() != null) {
                        if (shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode())) {
                            if (!checkRaStatusFields(shipmentDetails, orgAddressResponse, shipmentDetails.getAdditionalDetails().getExportBroker())) {
                                throw new RunnerException("Screening Status and Security Status is mandatory for RA Origin Agent.");
                            }
                        }
                    }
                }
            }
        }
    }

    public boolean checkRaStatusFields(ShipmentDetails shipmentDetails, OrgAddressResponse orgAddressResponse, Parties parties) throws ValidationException{
        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if (addressMap.containsKey(parties.getOrgCode() + "#" + parties.getAddressCode())) {
            Map<String, Object> addressConsignorAgent = addressMap.get(parties.getOrgCode() + "#" + parties.getAddressCode());
            if (addressConsignorAgent.containsKey(Constants.REGULATED_AGENT)) {
                var rakcType = addressConsignorAgent.get(Constants.REGULATED_AGENT);
                if (rakcType != null && Boolean.TRUE.equals(rakcType)){
                    if(shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                        shipmentDetails.getSecurityStatus() == null){
                        return false;
                    }
                    else if(shipmentDetails.getAdditionalDetails().getScreeningStatus() != null && shipmentDetails.getAdditionalDetails().getScreeningStatus().size() == 1 && shipmentDetails.getAdditionalDetails().getScreeningStatus().get(0).equals("VCK")){
                        throw new ValidationException("Please select an additional screening status along with VCK.");
                    }
                }
            }
        }
        return true;
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, List<Long> removedConsolIds, MutableBoolean isNewConsolAttached, boolean includeGuid) throws RunnerException {
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests = shipmentRequest.getPickupDeliveryDetailsInstructions();

        if (StringUtils.isNotBlank(shipmentDetails.getMasterBill())) {
            List<ConsolidationDetailsProjection> consolidations = consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill());

            consolidations.forEach(consolidation -> {
                try {
                    if( ObjectUtils.isEmpty(oldEntity) || ObjectUtils.notEqual(oldEntity.getMasterBill(), shipmentDetails.getMasterBill())) {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
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

        Long id = shipmentDetails.getId();
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && shipmentDetails.getConsolidationList().size() > 0)
            consolidationId = shipmentDetails.getConsolidationList().get(0).getId();
        Integer previousStatus = !Objects.isNull(oldEntity) ? oldEntity.getStatus() : null;

        List<Containers> updatedContainers = shipmentDetails.getContainersList();
        List<Packing> updatedPackings = new ArrayList<>();
        List<Long> deleteContainerIds = new ArrayList<>();
        List<Packing> packsForSync = null;
        List<UUID> deletedContGuids = new ArrayList<>();

        if(!isCreate){
            if(shipmentRequest.getDeletedContainerIds() != null && shipmentRequest.getDeletedContainerIds().size() > 0) {
                deleteContainerIds = shipmentRequest.getDeletedContainerIds().stream().filter(e -> e.getId() != null).map(e -> e.getId()).toList();
                if(deleteContainerIds != null && deleteContainerIds.size() > 0) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", deleteContainerIds, "IN");
                    Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                    Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                    if(packings != null && packings.getContent() != null && !packings.getContent().isEmpty()) {
                        List<Packing> packingList = new ArrayList<>();
                        for (Packing packing : packings.getContent()) {
                            packing.setContainerId(null);
                            packingList.add(packing);
                        }
                        packingDao.saveAll(packingList);
                        packsForSync = packingList;
                    }
                    listCommonRequest = constructListCommonRequest("id", deleteContainerIds, "IN");
                    Pair<Specification<Containers>, Pageable> pair2 = fetchData(listCommonRequest, Containers.class);
                    Page<Containers> containersPage = containerDao.findAll(pair2.getLeft(), pair2.getRight());
                    if(containersPage != null && !containersPage.isEmpty())
                        deletedContGuids = containersPage.stream().map(e -> e.getGuid()).toList();
                    for (Long containerId : deleteContainerIds) {
                        containerDao.deleteById(containerId);
                    }
                }
            }

            // Update AWB
            if(checkForAwbUpdate(shipmentDetails, oldEntity)) {
                awbDao.updatedAwbInformationEvent(shipmentDetails, oldEntity);
            }
        }

        if (bookingCarriageRequestList != null) {
            List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(commonUtils.convertToEntityList(bookingCarriageRequestList, BookingCarriage.class, isCreate), id);
            shipmentDetails.setBookingCarriagesList(updatedBookingCarriages);
        }
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }

        if (elDetailsRequestList != null) {
            List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(elDetailsRequestList, ELDetails.class, isCreate), id);
            shipmentDetails.setElDetailsList(updatedELDetails);
        }
        if (eventsRequestList != null) {
            List<Events> eventsList = commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate);
            eventsList = createOrUpdateTrackingEvents(shipmentDetails, oldEntity, eventsList, isCreate);
            updateActualFromTracking(eventsList, shipmentDetails);
            if (eventsList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                shipmentDetails.setEventsList(updatedEvents);
                eventService.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
            }
        }
        // create Shipment event on the bases of auto create event flag
        if(isCreate && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate()))
            autoGenerateCreateEvent(shipmentDetails);

        ConsolidationDetails consolidationDetails = updateLinkedShipmentData(shipmentDetails, oldEntity, shipmentRequest);
        if(!Objects.isNull(consolidationDetails)) {
            shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
            syncConsole = true;
        }

        // make removed consolidation non dg if all other shipments are non dg
        if(!isCreate && removedConsolIds != null && !removedConsolIds.isEmpty()) {
            boolean makeConsoleNonDG = checkForDGShipmentAndAirDgFlag(oldEntity); // check if removed shipment was dg
            if(makeConsoleNonDG) {
                consolidationDetails = consolidationDetailsDao.findById(removedConsolIds.get(0)).get();
                if(!checkAttachDgAirShipments(consolidationDetails)) // check if any other attached shipment is dg
                    changeConsolidationDGValues(false, new AtomicBoolean(true), removedConsolIds.get(0), shipmentDetails, consolidationDetails);
            }
        }

        // Sci status update for attach and detach in console mawb
        if(removedConsolIds != null && !removedConsolIds.isEmpty() && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)){
            consolidationService.checkSciForDetachConsole(removedConsolIds.get(0));
        }
        if(Boolean.TRUE.equals(isNewConsolAttached.getValue()) && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            consolidationService.checkSciForAttachConsole(consolidationId);
        }

        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails, previousStatus);

        if (packingRequestList != null) {
            packingRequestList = setPackingDetails(packingRequestList, shipmentDetails.getTransportMode(), consolidationId);
            updatedPackings = packingDao.updateEntityFromShipment(commonUtils.convertToEntityList(packingRequestList, Packing.class, !includeGuid && isCreate), id, deleteContainerIds);
            shipmentDetails.setPackingList(updatedPackings);
        }

        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isCreate), id);
            shipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isCreate), id);
            shipmentDetails.setRoutingsList(updatedRoutings);
        }
        if (serviceDetailsRequestList != null) {
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(serviceDetailsRequestList, ServiceDetails.class, isCreate), id);
            shipmentDetails.setServicesList(updatedServiceDetails);
        }
        if (notesRequestList != null) {
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(notesRequestList, Notes.class, isCreate), id, Constants.SHIPMENT);
            shipmentDetails.setNotesList(updatedNotes);
        }

        if (shipmentAddressList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, isCreate), id, Constants.SHIPMENT_ADDRESSES);
            shipmentDetails.setShipmentAddresses(updatedParties);
        }

        if (pickupDeliveryDetailsRequests != null){
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(pickupDeliveryDetailsRequests, PickupDeliveryDetails.class , isCreate) , id);
            shipmentDetails.setPickupDeliveryDetailsInstructions(pickupDeliveryDetailsList);
        }

        // Create Shipment Route in Console for Auto Attach Consolidation;
        if (shipmentRequest.getReplaceConsoleRoute() != null && shipmentRequest.getReplaceConsoleRoute()){
            createShipmentRouteInConsole(shipmentRequest);
        }
        Hbl hbl = null;
        if(updatedContainers != null && updatedContainers.size() > 0) {
            hbl = hblService.checkAllContainerAssigned(shipmentDetails, updatedContainers, updatedPackings);
        }
        pushShipmentDataToDependentService(shipmentDetails, isCreate, Boolean.TRUE.equals(shipmentRequest.getIsAutoSellRequired()), Optional.ofNullable(oldEntity).map(ShipmentDetails::getContainersList).orElse(null));
        
        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()){
            consolidationDetails = shipmentDetails.getConsolidationList().get(0);
        }
        // Syncing shipment to V1
        syncShipment(shipmentDetails, hbl, deletedContGuids, packsForSync, consolidationDetails, syncConsole);
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
    }

    public List<Events> createOrUpdateTrackingEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> updatedEvents, Boolean isNewShipment) {
        List<Events> newUpdatedEvents = (updatedEvents != null) ? new ArrayList<>(updatedEvents) : new ArrayList<>();

        if (Boolean.FALSE.equals(isNewShipment) && ObjectUtils.isNotEmpty(oldEntity)) {
            updateTrackingEvent(shipmentDetails, oldEntity, newUpdatedEvents);
        }else{
            createTrackingEvents(newUpdatedEvents, shipmentDetails);
        }
        return newUpdatedEvents;
    }

    private Map<String, Events> createEventMap(List<Events> events) {
        Map<String, Events> eventMap = new HashMap<>();
        for (Events event : events) {
            String key = generateKey(event.getEventCode(), event.getSource());
            eventMap.put(key, event);
        }
        return eventMap;
    }

    private String generateKey(String eventCode, String source) {
        return eventCode + "|" + source;
    }

    private void updateTrackingEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events) {
        Map<String, Events> eventMap = createEventMap(events);
        List<String> eventSources = Arrays.asList(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER,
                Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING,
                Constants.MASTER_DATA_SOURCE_CARGOES_USER);

        if (isLclOrFclOrAir(shipmentDetails)) {

            if (isEventChanged(shipmentDetails.getBookingNumber(), oldEntity.getBookingNumber())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources){
                    String key = generateKey(EventConstants.BOCO, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventUpdate(event);
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.BOCO));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }

            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate(),
                    oldEntity.getAdditionalDetails().getCargoDeliveredDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.CADE, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getAdditionalDetails().getCargoDeliveredDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.CADE));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }

            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getPickupDate(),
                            oldEntity.getAdditionalDetails().getPickupDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.CACO, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getAdditionalDetails().getPickupDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.CACO));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }

            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getCustomReleaseDate(),
                            oldEntity.getAdditionalDetails().getCustomReleaseDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.CURE, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getAdditionalDetails().getCustomReleaseDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.CURE));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer(),
                            oldEntity.getAdditionalDetails().getDocTurnedOverToCustomer())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.DOTP, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventUpdate(event);
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.DOTP));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate(),
                            oldEntity.getAdditionalDetails().getProofOfDeliveryDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.PRDE, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.PRDE));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getPickupByConsigneeCompleted(),
                            oldEntity.getAdditionalDetails().getPickupByConsigneeCompleted())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.SEPU, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventUpdate(event);
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.SEPU));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }
        }

        if (isLclOrAir(shipmentDetails)) {
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate(),
                            oldEntity.getAdditionalDetails().getWarehouseCargoArrivalDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.CAFS, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.CAFS));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }

            if (isEventChanged(shipmentDetails.getShipmentGateInDate(), oldEntity.getShipmentGateInDate())) {
                Boolean eventCreated = Boolean.FALSE;
                for(String source: eventSources) {
                    String key = generateKey(EventConstants.CAAW, source);
                    Events event = eventMap.get(key);
                    if (event != null) {
                        handleEventDateTimeUpdate(event, shipmentDetails.getShipmentGateInDate());
                    } else if (Boolean.FALSE.equals(eventCreated)) {
                        events.add(createAutomatedEvents(shipmentDetails, EventConstants.CAAW));
                        eventCreated = Boolean.TRUE;
                    }
                }
            }

        }

        if (isFcl(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getEmptyContainerReturned(),
                        oldEntity.getAdditionalDetails().getEmptyContainerReturned())) {
            Boolean eventCreated = Boolean.FALSE;
            for (String source : eventSources) {
                String key = generateKey(EventConstants.EMCR, source);
                Events event = eventMap.get(key);
                if (event != null) {
                    handleEventUpdate(event);
                } else if (Boolean.FALSE.equals(eventCreated)) {
                    events.add(createAutomatedEvents(shipmentDetails, EventConstants.EMCR));
                    eventCreated = Boolean.TRUE;
                }
            }
        }
    }

    private boolean isEventChanged(Object newValue, Object oldValue) {
        return newValue != null && !newValue.equals(oldValue);
    }

    private boolean isEventBooleanChanged(Boolean newValue, Boolean oldValue) {
        return Boolean.TRUE.equals(newValue) && !Boolean.TRUE.equals(oldValue);
    }

    private void handleEventUpdate(Events event) {
        event.setActual(LocalDateTime.now());
    }

    private void handleEventDateTimeUpdate(Events event, LocalDateTime eventDateTime) {
        event.setActual(eventDateTime);
    }


    private boolean isLclOrFclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isLclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isFcl(ShipmentDetails shipmentDetails) {
        return CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType());
    }


    private void createTrackingEvents(List<Events> events, ShipmentDetails shipmentDetails) {

        if (shipmentDetails.getBookingNumber() != null && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.BOCO));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && shipmentDetails.getAdditionalDetails().getPickupDate() != null && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.CACO));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && shipmentDetails.getAdditionalDetails().getCargoDeliveredDate() != null && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.CADE));
        }

        if (shipmentDetails.getShipmentGateInDate() != null && isLclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.CAAW));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && shipmentDetails.getAdditionalDetails().getCustomReleaseDate() != null && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.CURE));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer()) && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.DOTP));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate() != null && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.PRDE));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate() != null && isLclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.CAFS));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPickupByConsigneeCompleted()) && isLclOrFclOrAir(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.SEPU));
        }

        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getEmptyContainerReturned()) && isFcl(shipmentDetails)) {
            events.add(createAutomatedEvents(shipmentDetails, EventConstants.EMCR));
        }

    }


    private void updateActualFromTracking(List<Events> shipmentEvents, ShipmentDetails shipmentDetails) {

        TrackingServiceApiResponse trackingServiceApiResponse;
        try {
            trackingServiceApiResponse = trackingServiceAdapter.fetchTrackingData(
                    TrackingRequest.builder().referenceNumber(shipmentDetails.getShipmentId()).build());
        } catch (RunnerException e) {
            log.error("Error fetching tracking data for shipment ID {}: {}", shipmentDetails.getShipmentId(), e.getMessage());
            return;
        }

        if (trackingServiceApiResponse == null || trackingServiceApiResponse.getContainers() == null) {
            log.warn("No tracking data available for shipment ID {}", shipmentDetails.getShipmentId());
            return;
        }

        Map<String, Event> containerEventMapFromTracking = trackingServiceApiResponse.getContainers().stream()
                .filter(container -> container.getEvents() != null)
                .flatMap(container -> container.getEvents().stream()
                        .map(event -> new AbstractMap.SimpleEntry<>(
                                container.getContainerNumber() + "-" + event.getEventType(), // Key format: containerNumber-eventType
                                event)))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        shipmentEvents.forEach(shipmentEvent -> {
            if (Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING.equalsIgnoreCase(shipmentEvent.getSource())) {
                EventsResponse shipmentEventsResponse = jsonHelper.convertValue(shipmentEvent, EventsResponse.class);
                String key = shipmentEventsResponse.getContainerNumber() + "-" + shipmentEventsResponse.getEventCode();
                Event eventFromTracking = containerEventMapFromTracking.get(key);

                if (eventFromTracking != null && eventFromTracking.getActualEventTime() != null) {
                    shipmentEvent.setActual(eventFromTracking.getActualEventTime().getDateTime());
                    log.info("Updated actual event time for event code {} in container {}",
                            shipmentEventsResponse.getEventCode(), shipmentEventsResponse.getContainerNumber());
                } else {
                    log.warn("No matching event found or missing actual event time for key: {}", key);
                }
            }
        });
    }

    private boolean checkForAwbUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci())) return true;
        return !Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldEntity.getAdditionalDetails().getEfreightStatus());
    }

    public void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, boolean isCreate, boolean isAutoSellRequired, List<Containers> oldContainers) {
        try {
            if(shipmentDetails.getTenantId() == null)
                shipmentDetails.setTenantId(TenantContext.getCurrentTenant());
            if (IsStringNullOrEmpty(shipmentDetails.getUpdatedBy()))
                shipmentDetails.setUpdatedBy(UserContext.getUser().getUsername());
            ShipmentRequest shipmentRequest = jsonHelper.convertValue(shipmentDetails, ShipmentRequest.class);
            shipmentRequest.setIsAutoSellRequired(isAutoSellRequired);
            KafkaResponse kafkaResponse = producer.getKafkaResponse(shipmentRequest, isCreate);
            log.info("Producing shipment data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, StringUtility.convertToString(shipmentDetails.getGuid()));
        }
        catch (Exception e) {
            log.error("Error Producing shipment to kafka, error is due to " + e.getMessage());
        }
        try {
            if(shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Completed.getValue()) || shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())
                && trackingServiceAdapter.checkIfConsolAttached(shipmentDetails)|| (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT) && !Objects.isNull(shipmentDetails.getMasterBill()))) {
                UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
                List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                if(_utPayload != null) {
                    trackingPayloads.add(_utPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                }
            }
            if(shipmentDetails.getSource() != null && shipmentDetails.getSource().equals(Constants.API)) {
                var events = trackingServiceAdapter.getAllEvents(shipmentDetails,null, shipmentDetails.getBookingReference());
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(shipmentDetails.getBookingReference(),Constants.SHIPMENT, shipmentDetails.getShipmentId(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
        try {
            containerService.pushContainersToDependentServices(shipmentDetails.getContainersList(), oldContainers);
        }
        catch (Exception e) {
            log.error("Error producing message due to " + e.getMessage());
        }
    }

    private void createShipmentRouteInConsole (ShipmentRequest shipmentRequest) throws RunnerException{
        List<ConsolidationDetailsRequest> consoleRequest = shipmentRequest.getConsolidationList();
        List<Routings> createRoutes = new ArrayList<>();
        if(!Objects.isNull(shipmentRequest.getRoutingsList())) {
            if(shipmentRequest.getCreateMainLegRoute() != null && shipmentRequest.getCreateMainLegRoute()){
                List<RoutingsRequest> routeRequestList = shipmentRequest.getRoutingsList().stream().sorted(Comparator.comparingLong(RoutingsRequest::getLeg)).toList();
                var routeRequest = routeRequestList.stream().filter(x -> x.getMode().equals(shipmentRequest.getTransportMode())).findFirst();
                if(routeRequest.isPresent()) {
                    createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                    createRoutes = createConsoleRoutePayload(createRoutes);
                }
            } else {
                createRoutes = commonUtils.convertToEntityList(shipmentRequest.getRoutingsList(), Routings.class);
                createRoutes = createConsoleRoutePayload(createRoutes);
            }
        }
        if(consoleRequest != null && !consoleRequest.isEmpty() && createRoutes != null && !createRoutes.isEmpty()) {
            for (var console : consoleRequest) {
                routingsDao.updateEntityFromConsole(createRoutes, console.getId());
            }
        }
    }

    private List<Routings> createConsoleRoutePayload(List<Routings> routes){
        List<Routings> responseList = new ArrayList<>();
        for (var route : routes){
            Routings routings = new Routings();
            routings.setLeg(1L);
            routings.setPol(route.getPol());
            routings.setPod(route.getPod());
            routings.setMode(route.getMode());
            routings.setEta(route.getEta());
            routings.setEtd(route.getEtd());
            routings.setTransitDays(route.getTransitDays());
            routings.setAta(route.getAta());
            routings.setAtd(route.getAtd());
            routings.setVesselName(route.getVesselName());
            routings.setVoyage(route.getVoyage());
            routings.setCarrier(route.getCarrier());
            routings.setFlightNumber(route.getFlightNumber());
            responseList.add(routings);
        }
        return responseList;
    }

    public ConsolidationDetails createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) throws RunnerException {
        ShipmentSettingsDetails shipmentSettings = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettings.getShipConsolidationContainerEnabled())) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(shipmentDetails.getJobType());
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite())
                    && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                    && (StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) || StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()))) {
                throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.");
            }
            if(StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort())) {
                throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
            }
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            if(shipmentSettings.getShipmentLite() != null && shipmentSettings.getShipmentLite() && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)) {
                consolidationDetails.setPayment(shipmentDetails.getPaymentTerms());
            }
            if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
                consolidationDetails.getCarrierDetails().setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
            }
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setCarrierBookingRef(shipmentDetails.getBookingNumber());
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            if(StringUtility.isNotEmpty(shipmentDetails.getMasterBill())) {
                consolidationDetails.setBol(shipmentDetails.getMasterBill());
            }
            if(Objects.equals(TRANSPORT_MODE_SEA, shipmentDetails.getTransportMode()))
                consolidationDetails.setHazardous(shipmentDetails.getContainsHazardous());
            consolidationService.generateConsolidationNumber(consolidationDetails);
            if(consolidationDetails.getShipmentType() != null && !consolidationDetails.getShipmentType().isEmpty()
            && consolidationDetails.getShipmentType().equals(Constants.IMP) || consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                Parties defaultParty = null;
                try {
                    PartyRequestV2 partyRequestV2 = v1Service.getDefaultOrg();
                    if(!Objects.isNull(partyRequestV2))
                        defaultParty = modelMapper.map(partyRequestV2, Parties.class);
                } catch (Exception ignored) {}
                if(!Objects.isNull(defaultParty)) {
                    if(consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                        consolidationDetails.setSendingAgent(defaultParty);
                        if(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgCode() != null
                            && consolidationDetails.getReceivingAgent().getOrgCode().equals(defaultParty.getOrgCode()))
                            consolidationDetails.setReceivingAgent(null);
                    }
                    else {
                        consolidationDetails.setReceivingAgent(defaultParty);
                        if(consolidationDetails.getSendingAgent() != null && consolidationDetails.getSendingAgent().getOrgCode() != null
                            && consolidationDetails.getSendingAgent().getOrgCode().equals(defaultParty.getOrgCode()))
                            consolidationDetails.setSendingAgent(null);
                    }
                }
            }
            List<Routings> routings = new ArrayList<>();
            if(shipmentDetails.getRoutingsList() != null && shipmentDetails.getRoutingsList().size() > 0)
                routings = shipmentDetails.getRoutingsList().stream().sorted(Comparator.comparingLong(Routings::getLeg)).toList();
            var routeRequest = routings.stream().filter(x -> x.getMode().equals(shipmentDetails.getTransportMode())).findFirst();
            List<Routings> createRoutes = new ArrayList<>();
            if(routeRequest.isPresent()) {
                createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                createRoutes = createConsoleRoutePayload(createRoutes);
                consolidationDetails.setRoutingsList(createRoutes);
            }
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            if(createRoutes != null && !createRoutes.isEmpty()) {
                routingsDao.saveEntityFromConsole(createRoutes, consolidationDetails.getId());
            }
            Long id = consolidationDetails.getId();
            if(containers != null && containers.size() > 0) {
                containers = containers.stream().map(e -> e.setConsolidationId(id)).toList();
                containers = containerDao.saveAll(containers);
            }
            consolidationDetails.setContainersList(containers);
            if(shipmentSettings.getAutoEventCreate() != null && shipmentSettings.getAutoEventCreate()) {
                consolidationService.autoGenerateEvents(consolidationDetails);
            }
            consolidationService.pushShipmentDataToDependentService(consolidationDetails, true, null);
            return consolidationDetails;
        }
        return null;
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
        }
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
        Map<String, Integer> headerMap = new HashMap<>();
        for (int i = 0; i < ShipmentConstants.SHIPMENT_HEADERS.size(); i++) {
            headerMap.put(ShipmentConstants.SHIPMENT_HEADERS.get(i), i);
        }

        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ShipmentList");
            makeHeadersInSheet(sheet, workbook);

            //Filling the data
            List<IRunnerResponse> shipmentListResponseData = convertEntityListToDtoList(shipmentDetailsPage.getContent());
            for (int i = 0; i < shipmentListResponseData.size(); i++) {
                Row itemRow = sheet.createRow(i + 1);
                ShipmentListResponse shipment = (ShipmentListResponse) shipmentListResponseData.get(i);
                String origin = "", destination = "", destinationPort = "", originPort = "";
                if(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null){
                    origin = shipment.getCarrierDetails().getUnlocationData().get("origin");
                    destination = shipment.getCarrierDetails().getUnlocationData().get("destination");
                    destinationPort = shipment.getCarrierDetails().getUnlocationData().get("destinationPort");
                    originPort = shipment.getCarrierDetails().getUnlocationData().get("originPort");
                }
                if(shipment.getCarrierDetails() != null){
                    origin = StringUtility.isEmpty(origin) ? shipment.getCarrierDetails().getOrigin() : origin;
                    destination = StringUtility.isEmpty(destination) ? shipment.getCarrierDetails().getDestination() : destination;
                    destinationPort = StringUtility.isEmpty(destinationPort) ? shipment.getCarrierDetails().getDestinationPort() : destinationPort;
                    originPort = StringUtility.isEmpty(originPort) ? shipment.getCarrierDetails().getOriginPort() : originPort;
                }
                LocalTimeZoneHelper.transformTimeZone(shipment);
                itemRow.createCell(headerMap.get("Shipment Clone")).setCellValue("");
                itemRow.createCell(headerMap.get("Shipment Number")).setCellValue(shipment.getShipmentId());
                itemRow.createCell(headerMap.get("Order Number")).setCellValue(shipment.getOrderManagementNumber());
                itemRow.createCell(headerMap.get("Status")).setCellValue(String.valueOf(ShipmentStatus.values()[(shipment.getStatus())]));
                itemRow.createCell(headerMap.get("Transport Mode")).setCellValue(shipment.getTransportMode());
                itemRow.createCell(headerMap.get("Bill Status")).setCellValue(shipment.getBillStatus());
                itemRow.createCell(headerMap.get("MBL Number")).setCellValue(shipment.getMasterBill());
                itemRow.createCell(headerMap.get("Incoterm")).setCellValue(shipment.getIncoterms());
                itemRow.createCell(headerMap.get("Service Type")).setCellValue(shipment.getServiceType());
                itemRow.createCell(headerMap.get("Release Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getReleaseType());
                itemRow.createCell(headerMap.get("House Bill Type")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getHouseBillType() : "");
                itemRow.createCell(headerMap.get("Delivery Mode")).setCellValue(Objects.isNull(shipment.getDeliveryDetails()) ? "" : shipment.getDeliveryDetails().getDropMode());
                itemRow.createCell(headerMap.get("Consolidation Type")).setCellValue(String.valueOf(shipment.getJobType()));
                itemRow.createCell(headerMap.get("Activity Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getActivityType());
                itemRow.createCell(headerMap.get("Shipment Type")).setCellValue(shipment.getDirection());
                itemRow.createCell(headerMap.get("Carrier")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getShippingLine());
                itemRow.createCell(headerMap.get("Vessel Name/Flight")).setCellValue(shipment.getCarrierDetails() != null ? masterDataUtils.getVesselName(shipment.getCarrierDetails().getVessel()) : "");
                itemRow.createCell(headerMap.get("Flight Number")).setCellValue(Optional.ofNullable(shipment.getCarrierDetails()).map(c -> c.getFlightNumber()).orElse(""));
                itemRow.createCell(headerMap.get("Voyage/Flight No.")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getVoyage());
                itemRow.createCell(headerMap.get("Paid Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get("paidPlace")) : "");
                itemRow.createCell(headerMap.get("Issued Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get("placeOfIssue")) : "");
                itemRow.createCell(headerMap.get("Source1")).setCellValue(String.valueOf(shipment.getSource()));
                itemRow.createCell(headerMap.get("Date of Issue")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfIssue()) ? "" : shipment.getAdditionalDetails().getDateOfIssue().toString());
                itemRow.createCell(headerMap.get("Date of Receipt")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfReceipt()) ? "" : shipment.getAdditionalDetails().getDateOfReceipt().toString());
                itemRow.createCell(headerMap.get("Country of Origin")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getGoodsCO());
                itemRow.createCell(headerMap.get("Notify Party Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getNotifyParty() != null && shipment.getAdditionalDetails().getNotifyParty().getOrgData() != null ?
                        String.valueOf(shipment.getAdditionalDetails().getNotifyParty().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Cargo Type")).setCellValue(shipment.getShipmentType());
                itemRow.createCell(headerMap.get("Origin")).setCellValue(origin);
                itemRow.createCell(headerMap.get("Destination")).setCellValue(destination);
                itemRow.createCell(headerMap.get("Domestic")).setCellValue(String.valueOf(shipment.getIsDomestic()));
                itemRow.createCell(headerMap.get("Route")).setCellValue(shipment.getRoute());
                itemRow.createCell(headerMap.get("Client Name")).setCellValue(shipment.getClient() != null && shipment.getClient().getOrgData() != null ? shipment.getClient().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("Consignor Name")).setCellValue(shipment.getConsigner() != null && shipment.getConsigner().getOrgData() != null ? shipment.getConsigner().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("Consignee Name")).setCellValue(shipment.getConsignee() != null && shipment.getConsignee().getOrgData() != null ? shipment.getConsignee().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("HBL Number")).setCellValue(shipment.getHouseBill());
                itemRow.createCell(headerMap.get("BOE Number")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getBOENumber() : "");
                itemRow.createCell(headerMap.get("Screening Status")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getScreeningStatus() : "");
                itemRow.createCell(headerMap.get("BOE Date")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getBOEDate() != null ? shipment.getAdditionalDetails().getBOEDate().toString() : "");
                itemRow.createCell(headerMap.get("ETD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEtd() != null ? shipment.getCarrierDetails().getEtd().toString() : "");
                itemRow.createCell(headerMap.get("ETA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEta() != null ? shipment.getCarrierDetails().getEta().toString() : "");
                itemRow.createCell(headerMap.get("ATD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAtd() != null ? shipment.getCarrierDetails().getAtd().toString() : "");
                itemRow.createCell(headerMap.get("ATA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAta() != null ? shipment.getCarrierDetails().getAta().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getEstimatedPickupOrDelivery() != null ? shipment.getDeliveryDetails().getEstimatedPickupOrDelivery().toString() : "");
                itemRow.createCell(headerMap.get("Actual Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getActualPickupOrDelivery() != null ? shipment.getDeliveryDetails().getActualPickupOrDelivery().toString() : "");
                itemRow.createCell(headerMap.get("Goods Description")).setCellValue(shipment.getGoodsDescription());
                itemRow.createCell(headerMap.get("Gross Weight")).setCellValue(String.valueOf(shipment.getWeight()));
                itemRow.createCell(headerMap.get("Gross Weight Unit")).setCellValue(shipment.getWeightUnit());
                itemRow.createCell(headerMap.get("Volume")).setCellValue(String.valueOf(shipment.getVolume()));
                itemRow.createCell(headerMap.get("Volume Unit")).setCellValue(shipment.getVolumeUnit());
                itemRow.createCell(headerMap.get("Chargeable Weight")).setCellValue(String.valueOf(shipment.getChargable()));
                itemRow.createCell(headerMap.get("Volumetric Weight")).setCellValue(String.valueOf(shipment.getVolumetricWeight()));
                itemRow.createCell(headerMap.get("No. Of Packages")).setCellValue(String.valueOf(shipment.getNoOfPacks()));
                itemRow.createCell(headerMap.get("Package Type")).setCellValue(String.valueOf((shipment.getPacksUnit())));
                itemRow.createCell(headerMap.get("No. Of Inner Packages")).setCellValue(String.valueOf(shipment.getInnerPacks()));
                itemRow.createCell(headerMap.get("IU")).setCellValue("");
                itemRow.createCell(headerMap.get("Customer Booking Number")).setCellValue(String.valueOf(shipment.getBookingNumber()));
                itemRow.createCell(headerMap.get("Pickup Transporter")).setCellValue(shipment.getPickupDetails() != null && shipment.getPickupDetails().getTransporterDetail() != null && shipment.getPickupDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getPickupDetails().getTransporterDetail().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Delivery Transporter")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getTransporterDetail() != null && shipment.getDeliveryDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getDeliveryDetails().getTransporterDetail().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Job Status")).setCellValue(String.valueOf(shipment.getJobStatus()));
                itemRow.createCell(headerMap.get("Assigned To")).setCellValue(String.valueOf(shipment.getAssignedTo()));
                itemRow.createCell(headerMap.get("Created By")).setCellValue(String.valueOf(shipment.getCreatedBy()));
                itemRow.createCell(headerMap.get("Created Source")).setCellValue(String.valueOf(shipment.getSource()));
                itemRow.createCell(headerMap.get("Updated Date")).setCellValue(String.valueOf(shipment.getUpdatedAt()));
                itemRow.createCell(headerMap.get("20RE")).setCellValue(String.valueOf(shipment.getContainer20RECount()));
                itemRow.createCell(headerMap.get("20GP")).setCellValue(String.valueOf(shipment.getContainer20GPCount()));
                itemRow.createCell(headerMap.get("40RE")).setCellValue(String.valueOf(shipment.getContainer40RECount()));
                itemRow.createCell(headerMap.get("40GP")).setCellValue(String.valueOf(shipment.getContainer40GPCount()));
                itemRow.createCell(headerMap.get("Container Number")).setCellValue(shipment.getContainerNumbers() != null && !shipment.getContainerNumbers().isEmpty() ? shipment.getContainerNumbers().stream().findFirst().get() : "");
                itemRow.createCell(headerMap.get("Created Date")).setCellValue(String.valueOf(shipment.getCreatedAt()));
                itemRow.createCell(headerMap.get("Estimated Cost")).setCellValue(shipment.getTotalEstimatedCost() != null ? shipment.getTotalEstimatedCost().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Revenue")).setCellValue(shipment.getTotalEstimatedRevenue() != null ? shipment.getTotalEstimatedRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Profit")).setCellValue(shipment.getTotalEstimatedProfit() != null ? shipment.getTotalEstimatedProfit().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Profit %")).setCellValue(shipment.getTotalEstimatedProfitPercent() != null ? shipment.getTotalEstimatedProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("Captured Cost")).setCellValue(shipment.getTotalCost() != null ? shipment.getTotalCost().toString() : "");
                itemRow.createCell(headerMap.get("Captured Revenue")).setCellValue(shipment.getTotalRevenue() != null ? shipment.getTotalRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Captured Profit")).setCellValue(shipment.getTotalProfit() != null ? shipment.getTotalProfit().toString() : "");
                itemRow.createCell(headerMap.get("Captured Profit %")).setCellValue(shipment.getTotalProfitPercent() != null ? shipment.getTotalProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Payable Cost")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedCost().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Receivable Revenue")).setCellValue(shipment.getTotalPostedRevenue() != null ? shipment.getTotalPostedRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Profit")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedProfit().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Profit %")).setCellValue(shipment.getTotalPostedProfitPercent() != null ? shipment.getTotalPostedProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("20s Count")).setCellValue(String.valueOf(shipment.getContainer20Count()));
                itemRow.createCell(headerMap.get("40s Count")).setCellValue(String.valueOf(shipment.getContainer40Count()));
                itemRow.createCell(headerMap.get("TEU Count")).setCellValue(shipment.getTeuCount() != null ? shipment.getTeuCount().toString() : null);
                itemRow.createCell(headerMap.get("CreatedBy")).setCellValue(shipment.getCreatedBy());
                itemRow.createCell(headerMap.get("POL")).setCellValue(originPort);
                itemRow.createCell(headerMap.get("POD")).setCellValue(destinationPort);
                itemRow.createCell(headerMap.get("Waybill Number")).setCellValue(String.valueOf(shipment.getWayBillNumber()));
                itemRow.createCell(headerMap.get("Additional Terms")).setCellValue(String.valueOf(shipment.getAdditionalTerms()));
                itemRow.createCell(headerMap.get("Reference Number")).setCellValue(String.valueOf(shipment.getBookingReference()));
                itemRow.createCell(headerMap.get("POL Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("originPort_code")) : "");
                itemRow.createCell(headerMap.get("POD Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destinationPort_code")) : "");
                itemRow.createCell(headerMap.get("Origin Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("origin_code")) : "");
                itemRow.createCell(headerMap.get("Destination Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destination_code")) : "");
            }

            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Shipments_" + timestamp + Constants.XLSX;

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (OutputStream outputStream = new BufferedOutputStream(response.getOutputStream(), 8192 * 10)) {
                workbook.write(outputStream);
            } catch (IOException e) {
                log.error("Time out " + e.getMessage());
            }
        }

    }

    private void makeHeadersInSheet(Sheet sheet, Workbook workbook) {
        Row headerRow = sheet.createRow(0);
        List<String> shipmentHeader = ShipmentConstants.SHIPMENT_HEADERS;

        CellStyle boldStyle = workbook.createCellStyle();
        Font boldFont = workbook.createFont();
        boldFont.setBold(true);
        boldStyle.setFont(boldFont);

        for (int i = 0; i < shipmentHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(shipmentHeader.get(i));
            cell.setCellStyle(boldStyle);
        }
    }


    public ResponseEntity<IRunnerResponse> fullShipmentsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS, Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
//            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullShipmentList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignShipmentContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentContainerAssignRequest request = (ShipmentContainerAssignRequest) commonRequestModel.getData();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
            ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getContainerIds(), "IN");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> containersMap = new HashMap<>();
            if(shipmentSettingsDetails.getMultipleShipmentEnabled() && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {
                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                if(containers != null && containers.getContent() != null) {
                    List<Containers> containersList = containers.getContent();
                    if(!containers.getContent().isEmpty()) {
                        for (Containers container : containersList) {
                            boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                            if((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || !isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {
                                String errorMsg = "This container is already linked to another shipment. Only part Container/Containers are allowed to attach";
                                if(isPart)
                                    errorMsg = "Mentioned container " + container.getContainerNumber() + " is already assigned to a Shipment - " + container.getShipmentsList().get(0).getShipmentId() + ". Please check and retry.";
                                throw new ValidationException(errorMsg);
                            }
                            if(isFCL) {
                                container.setAchievedWeight(container.getAllocatedWeight());
                                container.setAchievedVolume(container.getAllocatedVolume());
                                container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                                container.setWeightUtilization("100");
                                container.setVolumeUtilization("100");
                            }
                            containersMap.put(container.getId(), container);
                        }
                    }
                    if(isFCL)
                        containerDao.saveAll(containersList);
                }
            }
            else {
                containersMap = containers.getContent().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
            }
            shipmentsContainersMappingDao.assignContainers(request.getShipmentId(), request.getContainerIds());
            makeShipmentsDG(containersMap, shipmentDetails);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean IsConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (lclAndSeaOrRoadFlag) {
                if(!containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA)) {
                    lclAndSeaOrRoadFlag = false;
                }
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest(CONSOLIDATION_ID, consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> containersMap = containers.getContent().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
            List<Containers> conts = new ArrayList<>();
            List<Long> containerIds = new ArrayList<>();
            ShipmentDetails shipmentDetails = shipmentDao.findById(containerAssignRequest.getShipmentId()).get();
            if(lclAndSeaOrRoadFlag) {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedWeight() != null
                                && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                            BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                            BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                            if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                                containersList.add(container);
                            }
                            else if(!IsConsolidatorFlag) {
                                conts.add(container);
                            }
                        }
                        else
                            containersList.add(container);
                    }
                }
                if(conts.size() > 0) {
                    for (Containers x : conts) {
                        boolean flag = true;
                        if(x.getShipmentsList() != null && x.getShipmentsList().size() > 0) {
                            for(ShipmentDetails shipmentDetails1 : x.getShipmentsList()) {
                                if(shipmentDetails1.getShipmentType().equals(Constants.CARGO_TYPE_FCL))
                                    flag = false;
                            }
                        }
                        if (flag)
                            containersList.add(x);
                    }
                }

                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                for (Containers container : containersList) {
                    boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                    if ((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {

                    }
                    else {
                        containerIds.add(container.getId());
                    }
                    if (isFCL) {
                        container.setAchievedWeight(container.getAllocatedWeight());
                        container.setAchievedVolume(container.getAllocatedVolume());
                        container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                        container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                        container.setWeightUtilization("100");
                        container.setVolumeUtilization("100");
                    }
                }
                if(isFCL)
                    containerDao.saveAll(containersList);
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(shipmentsContainersMappings.isEmpty()) {
                        containerIds.add(container.getId());
                    }
                }
            }
            if(!Objects.isNull(containerIds) && !containerIds.isEmpty()) {
                shipmentsContainersMappingDao.assignContainers(containerAssignRequest.getShipmentId(), containerIds);
                makeShipmentsDG(containersMap, shipmentDetails);
            }
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void makeShipmentsDG(Map<Long, Containers> containersMap, ShipmentDetails shipmentDetails) throws RunnerException {
        if(!Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
            for(Map.Entry<Long, Containers> map : containersMap.entrySet()) {
                if(Boolean.TRUE.equals(map.getValue().getHazardous())) {
                    shipmentDao.entityDetach(List.of(shipmentDetails));
                    shipmentDetails = shipmentDao.findById(shipmentDetails.getId()).get();
                    shipmentDetails.setContainsHazardous(true);
                    shipmentDetails = shipmentDao.save(shipmentDetails, false);
                    shipmentSync.sync(shipmentDetails, null, null, shipmentDetails.getGuid().toString(), false);
                }
            }
        }
    }

    private List<IRunnerResponse> convertEntityListToFullShipmentList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetail, ShipmentDetailsResponse.class);
            // TODO- check if they want status
//            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
//                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            responseList.add(response);
        });
//        setLocationData(responseList);
//        setContainerTeu(lst, responseList);
//        setBillingData(lst, responseList);
        return responseList;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        int totalPage = 0;
        long totalElements = 0;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            if(Boolean.TRUE.equals(request.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                    PageRequest.of(Math.max(0,request.getPageNo()-1), request.getPageSize()));
                andCriteria("id", eligibleShipmentId.getContent(), "IN", request);
                totalPage = eligibleShipmentId.getTotalPages();
                totalElements = eligibleShipmentId.getTotalElements();
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
            checkWayBillNumberCriteria(request);
            log.info(ShipmentConstants.SHIPMENT_LIST_CRITERIA_PREPARING, LoggerHelper.getRequestIdFromMDC());
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(!Boolean.TRUE.equals(request.getNotificationFlag())) {
                totalPage = shipmentDetailsPage.getTotalPages();
                totalElements = shipmentDetailsPage.getTotalElements();
            }
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                        totalPage,
                        totalElements);
            else {
                List<IRunnerResponse>filtered_list=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filtered_list.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filtered_list,
                        totalPage,
                        totalElements);
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkWayBillNumberCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() > 0)
        {
            checkForWayBillFilter(request.getFilterCriteria());
        }
    }

    private void checkForWayBillFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("wayBillNumber") && filterCriteria.getCriteria().getValue() != null) {

                WayBillNumberFilterRequest wayBillNumberFilterRequest = new WayBillNumberFilterRequest();
                wayBillNumberFilterRequest.setWayBillNumber(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchWayBillNumberFilterGuids(wayBillNumberFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && filterCriteria.getInnerFilter().size() > 0) {
                checkForWayBillFilter(filterCriteria.getInnerFilter());
            }
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(shipmentDetails.get());
            shipmentDao.delete(shipmentDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Shipment details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(retireveShipmentData(commonRequestModel, false));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ShipmentDetailsResponse retireveShipmentData(CommonRequestModel commonRequestModel, boolean measurmentBasis) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        double start = System.currentTimeMillis();
        if(request.getId() == null && request.getGuid() == null) {
            log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id and GUID can't be null. Please provide any one !");
        }
        Long id = request.getId();
        Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
        if(id != null ){
            shipmentDetails = shipmentDao.findById(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            shipmentDetails = shipmentDao.findByGuid(guid);
        }
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING);
        double current = System.currentTimeMillis();
        log.info("Shipment details fetched successfully for Id {} with Request Id {} within: {}ms", id, LoggerHelper.getRequestIdFromMDC(), current - start);
        ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
        id = shipmentDetails.get().getId();
        var notificationMap = getNotificationMap(PendingNotificationRequest.builder().shipmentIdList(List.of(id)).build());
        response.setPendingActionCount(Optional.ofNullable(notificationMap.get(id)).map(List::size).orElse(null));
        log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
        response.setCustomerBookingNotesList(jsonHelper.convertValueToList(notes,NotesResponse.class));
        if(measurmentBasis) {
            calculatePacksAndPacksUnit(shipmentDetails.get().getPackingList(), response);
        } else {
            createShipmentPayload(shipmentDetails.get(), response);
        }
        return response;
    }

    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null && request.getGuid() == null) {
                log.error("Request Id is null for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
            if(request.getId() != null ){
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDetails.get().setNotesList(notesDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            log.info("Shipment details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            response.setCustomerBookingNotesList(commonUtils.convertToDtoList(notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING),NotesResponse.class));
            //containerCountUpdate(shipmentDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            CompletableFuture<ResponseEntity<IRunnerResponse>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ShipmentDetailsResponse> res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(res.getData());
            else
                return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialData(res, request.getIncludeColumns()));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException {

        ShipmentPatchRequest shipmentRequest = (ShipmentPatchRequest) commonRequestModel.getData();
        if ((shipmentRequest.getId() == null && shipmentRequest.getGuid() == null) && (shipmentRequest.getShipmentId() == null || shipmentRequest.getShipmentId().get() == "")) {
            log.error("Request Id is null for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request Id is null");
        }
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetail();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        CarrierPatchRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        // TODO- implement Validation logic
        Long id = null;
        Optional<ShipmentDetails> oldShipmentDetails;
        ShipmentRequest fetchShipmentRequest = new ShipmentRequest();
        fetchShipmentRequest.setId(shipmentRequest.getId() != null ? shipmentRequest.getId().get() : null);
        fetchShipmentRequest.setGuid(shipmentRequest.getGuid());
        if(shipmentRequest.getId() != null || shipmentRequest.getGuid() != null) {
            oldShipmentDetails = retrieveByIdOrGuid(fetchShipmentRequest);
            id = oldShipmentDetails.get().getId();
        }
        else {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentRequest.getShipmentId().get(), "=");
            Pair<Specification<ShipmentDetails>, Pageable> shipmentPair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(shipmentPair.getLeft(), shipmentPair.getRight());
            if(shipmentDetails != null && shipmentDetails.get().count() == 1) {
                oldShipmentDetails = shipmentDetails.get().findFirst();
                id = oldShipmentDetails.get().getId();
            }
            else if(shipmentDetails == null || shipmentDetails.get().count() == 0) {
                log.error("Shipment not available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            else {
                log.error("More than one shipments available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INCORRECT_RESULT_SIZE_EXCEPTION_MSG);
            }
        }
        if (!oldShipmentDetails.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentDetails newShipmentDetails = oldShipmentDetails.get();
            Integer previousStatus = oldShipmentDetails.get().getStatus();
            ShipmentDetails oldEntity = jsonHelper.convertValue(newShipmentDetails, ShipmentDetails.class);
            shipmentDetailsMapper.update(shipmentRequest, newShipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            newShipmentDetails.setId(oldShipmentDetails.get().getId());
            List<Containers> updatedContainers = null;
            Long consolidationId = null;
            if (ObjectUtils.isNotEmpty(newShipmentDetails.getConsolidationList())) {
                consolidationId = newShipmentDetails.getConsolidationList().get(0).getId();
            }
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class), consolidationId, id,
                        false);
            } else {
                updatedContainers = oldShipmentDetails.get().getContainersList();
            }
            newShipmentDetails.setContainersList(updatedContainers);
            AdditionalDetails updatedAdditionalDetails = null;
            if (additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(jsonHelper.convertValue(additionalDetailRequest, AdditionalDetails.class));
                newShipmentDetails.setAdditionalDetails(updatedAdditionalDetails);
            }
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldShipmentDetails.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
                newShipmentDetails.setCarrierDetails(oldShipmentDetails.get().getCarrierDetails());
            }
            newShipmentDetails.setCarrierDetails(oldShipmentDetails.get().getCarrierDetails());
            validateBeforeSave(newShipmentDetails);

            ConsolidationDetails consolidationDetails = updateLinkedShipmentData(newShipmentDetails, oldShipmentDetails.get(), null);
            if(!Objects.isNull(consolidationDetails)) {
                newShipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
            }
            newShipmentDetails = shipmentDao.update(newShipmentDetails, false);

            newShipmentDetails.setContainersList(updatedContainers);
            if (additionalDetailRequest != null) {
                newShipmentDetails.setAdditionalDetails(updatedAdditionalDetails);
            }
            if (carrierDetailRequest != null) {
                newShipmentDetails.setCarrierDetails(updatedCarrierDetails);
            }
            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id);
                newShipmentDetails.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (truckDriverDetailsRequestList != null) {
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id);
                newShipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, null);
                newShipmentDetails.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id);
                newShipmentDetails.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> eventsList = jsonHelper.convertValueToList(eventsRequestList, Events.class);
                eventsList = createOrUpdateTrackingEvents(newShipmentDetails, oldEntity, eventsList, false);
                updateActualFromTracking(eventsList, newShipmentDetails);
                if (eventsList != null) {
                    List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                    newShipmentDetails.setEventsList(updatedEvents);
                    eventService.updateAtaAtdInShipment(updatedEvents, newShipmentDetails, shipmentSettingsDetails);
                }
            }
            // Create events on basis of shipment status Confirmed/Created
            autoGenerateEvents(newShipmentDetails, previousStatus);

            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                newShipmentDetails.setNotesList(updatedNotes);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                newShipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id);
                newShipmentDetails.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id);
                newShipmentDetails.setServicesList(updatedServiceDetails);
            }

            if(fromV1 == null || !fromV1) {
                syncShipment(newShipmentDetails, null, null, null, consolidationDetails, true);
            }

            pushShipmentDataToDependentService(newShipmentDetails, false, false, oldShipmentDetails.get().getContainersList());
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(newShipmentDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ShipmentDetails shipmentDetails = shipmentDao.findById(id).get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            if (lockingUser != null && (Objects.equals(lockingUser, currentUser) ||
                    (!Objects.isNull(PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin)) && !PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin).isEmpty()) ))
                shipmentDetails.setIsLocked(false);
            else
                throw new RunnerException(String.format(ErrorConstants.LOCK_UNLOCK_ERROR, Constants.Shipment, lockingUser));
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        shipmentSync.syncLockStatus(shipmentDetails);
        pushShipmentDataToDependentService(shipmentDetails, false, false, shipmentDetails.getContainersList());
        return ResponseHelper.buildSuccessResponse();
    }

    private String generateShipmentId(ShipmentDetails shipmentDetails) {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();
        String shipmentId = "";
        boolean flag = true;
        int counter = 1;
        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                log.info("CR-ID {} || Inside generateShipmentId: with shipmentID: {} | counter: {}", LoggerHelper.getRequestIdFromMDC(), shipmentId, counter++);
                if(shipmentSettingsList != null && shipmentSettingsList.size() != 0 && shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(shipmentSettingsList.get(0), ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        log.error("Execption during common sequence {}", ignored.getMessage());
                        log.error("Execption occurred for common sequence {}", ignored.getStackTrace());
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                if(StringUtility.isEmpty(shipmentId)) {
                    log.info("CR-ID {} || no common sequence found", LoggerHelper.getRequestIdFromMDC());
                    shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                }
            }
        }
        return shipmentId;
//        if (shipmentSettingsList.isEmpty())
//            return StringUtility.getRandomString(10);
//        return createShipmentSequence(shipmentSettingsList.get(0));
    }

    private String getCustomizedShipmentProcessNumber(ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        // to check the commmon sequence
        var sequenceNumber = productEngine.GetCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.IdentifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null){
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null)
            {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct(tenantProducts);
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequnce table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    public ResponseEntity<IRunnerResponse> syncShipmentAuditLogsToService(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            AuditLogsSyncRequest request = (AuditLogsSyncRequest) commonRequestModel.getData();
            if(request.getGuid() == null)
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(request.getGuid());
            if(oldEntity.isPresent())
                syncEntityConversionService.auditLogsV1ToV2(request.getChangeLogs(), oldEntity.get().getId());
            else
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes, boolean dataMigration, List<AuditLogRequestV2> auditLogRequestV2, String createdBy) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressesRequestList = shipmentRequest.getShipmentAddresses();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic
        UUID guid = shipmentRequest.getGuid();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(guid);

        if (dataMigration) {
            MDC.put("skip-audit-log", "true");
        }

        List<ConsolidationDetails> tempConsolidations = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(consolidation.getGuid());
                if(consolidationDetails.isPresent()) {
                    tempConsolidations.add(consolidationDetails.get());
                }
            }
        }

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            ShipmentDetails oldShipment = null;
            boolean isCreate = true;
            List<Containers> containers;
            if(oldEntity.isPresent()) {
                oldShipment = oldEntity.get();
                id = oldEntity.get().getId();
                containers = oldEntity.get().getContainersList();
                if(containers != null && !containers.isEmpty()) {
                    if(oldContainers == null)
                        oldContainers = new ArrayList<>();
                    oldContainers.addAll(containers);
                }
                isCreate = false;
            }
            shipmentRequest.setConsolidationList(null);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            if (!tempConsolidations.isEmpty())
                entity.setConsolidationList(tempConsolidations);
            entity.setId(id);
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                containerRequestList.forEach(e -> e.setShipmentsList(null));
                if(!tempConsolidations.isEmpty() && !entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.get(0).getId(), "=");
                    Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                    Page<Containers> oldConsolContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                    if(!oldConsolContainers.isEmpty()) {
                        if(oldContainers == null)
                            oldContainers = new ArrayList<>();
                        oldContainers.addAll(oldConsolContainers.getContent());
                    }
                }
                updatedContainers = containerDao.updateEntityFromShipmentV1(jsonHelper.convertValueToList(containerRequestList, Containers.class), oldContainers);
            } else if(!oldEntity.isEmpty()){
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            String operation = DBOperationType.CREATE.name();
            String oldEntityJsonString = null;

            if(entity.getSourceGuid() != null && entity.getGuid() != null && !Objects.equals(entity.getSourceGuid(), entity.getGuid())){
                entity.setEntityTransfer(true);
            }
            if(id == null) {
                entity = shipmentDao.save(entity, true);
                id = entity.getId();
            } else {
                operation = DBOperationType.UPDATE.name();
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                entity = shipmentDao.update(entity, true);
            }

            shipmentDao.saveCreatedDateAndUser(id, createdBy, shipmentRequest.getCreatedAt());

            createAuditLog(entity, oldEntityJsonString, operation);
            if (dataMigration) {
                createV1AuditLogs(entity.getId(), auditLogRequestV2);
            }
//            Not needed, added consolidations while saving shipment
//            attachConsolidations(entity.getId(), tempConsolIds);

            if (bookingCarriageRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
                Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (truckDriverDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<TruckDriverDetails>, Pageable> truckDriverDetailsPair = fetchData(listCommonRequest, TruckDriverDetails.class);
                Page<TruckDriverDetails> oldTruckDriverDetails = truckDriverDetailsDao.findAll(truckDriverDetailsPair.getLeft(), truckDriverDetailsPair.getRight());
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id, oldTruckDriverDetails.stream().toList());
                entity.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> oldConsolPackings = new ArrayList<>();
                if(entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !tempConsolidations.isEmpty()) {
                    listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.get(0).getId(), "=");
                    packingPair = fetchData(listCommonRequest, Packing.class);
                    oldConsolPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight()).stream().toList();
                }
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, oldPackings.stream().toList(), oldConsolPackings, updatedContainers, map);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
                Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
                entity.setServicesList(updatedServiceDetails);
            }
            if (shipmentAddressesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT_ADDRESSES);
                Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
                Page<Parties> oldParties = partiesDao.findAll(pair.getLeft(), pair.getRight());
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(shipmentAddressesRequestList, Parties.class), id, Constants.SHIPMENT_ADDRESSES, oldParties.stream().toList());
                entity.setShipmentAddresses(updatedParties);
            }
            if (notesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
                entity.setNotesList(updatedNotes);
            }
            if (customerBookingNotes != null && customerBookingNotes.size() > 0) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CUSTOMER_BOOKING);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                if(oldNoteList == null || oldNoteList.isEmpty()) {
                    List<Notes> updatedNotes = notesDao.saveEntityFromOtherEntity(jsonHelper.convertValueToList(customerBookingNotes, Notes.class), id, Constants.CUSTOMER_BOOKING);
                }
            }
            List<Containers> oldConts = null;
            if(oldEntity.isPresent())
                oldConts = oldEntity.get().getContainersList();
            if(!dataMigration)
                pushShipmentDataToDependentService(entity, isCreate, false, oldConts);
            ShipmentDetailsResponse response = jsonHelper.convertValue(entity, ShipmentDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    private void createV1AuditLogs(Long id, List<AuditLogRequestV2> auditLogRequestV2) {
        try {
            syncEntityConversionService.auditLogsV1ToV2(auditLogRequestV2, id);
        } catch (Exception ignored) {
            log.error("Error while migrating audit logs for id: " + id);
        }
    }

    private void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation)
    {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(entity)
                            .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class) : null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operation).build()
            );
        }
        catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    public ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            List<Containers> containers = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
            ContainerSummaryResponse response = containerService.calculateContainerSummary(containers, request.getTransportMode(), request.getShipmentType());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculatePackSummaryRequest request = (CalculatePackSummaryRequest) commonRequestModel.getData();
        try {
            List<Packing> packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), new ShipmentMeasurementDetailsDto());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findById(id);
            if(!shipmentDetailsOptional.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
            ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
            Map<String, Object> response = fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
            if(shipmentDetails.getClient() != null && StringUtility.isNotEmpty(shipmentDetails.getClient().getOrgCode())) {
                fetchCreditLimitMasterData(shipmentDetails.getClient().getOrgCode(), shipmentDetails.getClient().getAddressCode(), response);
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * * fetchAllMasterDataByKey to be used for direct API
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     * @return
     */
    @Override
    public Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllMasterDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllUnlocationDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCarrierDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCurrencyDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCommodityTypesInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllWarehouseDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllActivityDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllSalesAgentInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllContainerTypesInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllVesselDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllDGSubstanceDataInSingleCall(shipmentDetailsResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                containerTypeFuture, vesselsFuture, dgSubstanceFuture).join();

        return masterDataResponse;
    }

    /**
     * * createShipmentPayload to be used while retrieving shipment
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     */
    public void createShipmentPayload(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllMasterDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllUnlocationDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCarrierDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCurrencyDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllCommodityTypesInSingleCall(shipmentDetailsResponse, null)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllWarehouseDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllActivityDataInSingleCall(shipmentDetailsResponse, null)), executorService);
            var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllSalesAgentInSingleCall(shipmentDetailsResponse, null)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllContainerTypesInSingleCall(shipmentDetailsResponse, null)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture,
                    activityDataFuture, salesAgentFuture,
                    containerTypeFuture).join();
            Map<Long, ContainerResponse> map = new HashMap<>();
            List<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
            if(containers != null)
                map = containers.stream().collect(Collectors.toMap(ContainerResponse::getId, Function.identity()));
            masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, map);
            masterDataHelper.setTruckDriverDetailsData(shipmentDetailsResponse, map);
            if(!Objects.isNull(shipmentDetails)) {
                shipmentDetailsResponse.setPackSummary(packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto()));
                shipmentDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(shipmentDetails.getContainersList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType()));
            }
            try {
                if(shipmentDetailsResponse.getId() != null) {
                    var awb = awbDao.findByShipmentId(shipmentDetailsResponse.getId());
                    if (awb != null && !awb.isEmpty()) {
                        if (awb.get(0).getAirMessageStatus() != null)
                            shipmentDetailsResponse.setAwbStatus(awb.get(0).getAirMessageStatus());
                        else
                            shipmentDetailsResponse.setAwbStatus(AwbStatus.AWB_GENERATED);
                    }
                }
                if(!shipmentDetailsResponse.getAdditionalDetails().getIsSummaryUpdated())
                    shipmentDetailsResponse.getAdditionalDetails().setSummary(shipmentDetailsResponse.getContainerSummary().getSummary());
            } catch (Exception e) {}
            if(!Objects.isNull(shipmentDetails)) {
                List<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
                if(!Objects.isNull(consolidationList) && !consolidationList.isEmpty()){
                    List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationList.get(0).getId());
                    if(!Objects.isNull(consoleShipmentMappings) && !consoleShipmentMappings.isEmpty())
                        shipmentDetailsResponse.setShipmentCount((long) consoleShipmentMappings.size());
                    else
                        shipmentDetailsResponse.setShipmentCount(0L);
                } else {
                    shipmentDetailsResponse.setShipmentCount(0L);
                }
            } else {
                shipmentDetailsResponse.setShipmentCount(0L);
            }
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_RETRIEVE, ex.getLocalizedMessage());
        }

    }

    public ResponseEntity<IRunnerResponse> cloneShipment(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(checkForDGShipmentAndAirDgFlag(shipmentDetails.get()) && !isAirDgUser())
                throw new ValidationException("You do not have necessary permissions for this.");
            ShipmentRequest cloneShipmentDetails = jsonHelper.convertValue(shipmentDetails.get(), ShipmentRequest.class);
            cloneShipmentDetails.setId(null);
            cloneShipmentDetails.setGuid(null);
            cloneShipmentDetails.setHouseBill(null);
            cloneShipmentDetails.setBookingNumber(null);
            cloneShipmentDetails.setContainersList(null);
            cloneShipmentDetails.setRoutingsList(null);
            cloneShipmentDetails.setShipmentId(null);
            cloneShipmentDetails.setMasterBill(null);
            cloneShipmentDetails.setConsolidationList(null);
            cloneShipmentDetails.setStatus(ShipmentStatus.Created.getValue());
            cloneShipmentDetails.setConsolRef(null);
            cloneShipmentDetails.setEventsList(null);
            cloneShipmentDetails.setBookingReference(null);
            cloneShipmentDetails.setSourceGuid(null);
            cloneShipmentDetails.setClonedGuid(shipmentDetails.get().getGuid());
            cloneShipmentDetails.setContractId(null);
            cloneShipmentDetails.getAdditionalDetails().setDraftPrinted(null);
            cloneShipmentDetails.getAdditionalDetails().setPrintedOriginal(null);
            cloneShipmentDetails.getAdditionalDetails().setSurrenderPrinted(null);
            cloneShipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            cloneShipmentDetails.setAutoUpdateWtVol(false);
            cloneShipmentDetails.setFileStatus(null);

            cloneShipmentDetails.setShipmentCreatedOn(LocalDateTime.now());
            
            if(Constants.TRANSPORT_MODE_SEA.equals(cloneShipmentDetails.getTransportMode()) && Constants.DIRECTION_EXP.equals(cloneShipmentDetails.getDirection()) && !Constants.SHIPMENT_TYPE_DRT.equals(cloneShipmentDetails.getJobType()))
                cloneShipmentDetails.setHouseBill(generateCustomHouseBL(null));

            CommonRequestModel requestModel = CommonRequestModel.buildRequest(cloneShipmentDetails);
            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(cloneShipmentDetails, ShipmentDetailsResponse.class);
            masterDataHelper.addAllUnlocationDataInSingleCall(response, null);
            masterDataHelper.addAllTenantDataInSingleCall(response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> transportInstructionList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIListRequest tiListRequest = (TIListRequest) commonRequestModel.getData();
            if(tiListRequest == null) {
                log.error("Request is empty for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchTransportInstructionList(tiListRequest);
            List<TIResponse> tiResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIResponse.class);
            return ResponseHelper.buildSuccessResponse(tiResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> containerListForTI(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIContainerListRequest tiContainerListRequest = (TIContainerListRequest) commonRequestModel.getData();
            if(tiContainerListRequest == null) {
                log.error("Request is empty for container TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiContainerListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for conatiner TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchContainersListForTI(tiContainerListRequest);
            List<TIContainerResponse> containerResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(containerResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException {
        try {
            ShipmentDetailsResponse response = jsonHelper.convertValue(orderManagementAdapter.getOrder(orderId), ShipmentDetailsResponse.class);
            this.createShipmentPayload(null, response);
            masterDataHelper.addAllMasterDataInSingleCall(response, null);
            masterDataHelper.addAllUnlocationDataInSingleCall(response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomHouseBL(null)).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentFromConsol(Long consolidationId, String bookingNumber) {
        var tenantSettings = commonUtils.getShipmentSettingFromContext();
        // Populate shipment details on basis of tenant settings

        ShipmentDetailsResponse shipment;
        var consolidationResponse = consolidationDetailsDao.findById(consolidationId);

        if (consolidationResponse.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the consolidation with id " + consolidationId);

        var consolidation = modelMapper.map(consolidationResponse.get(), ConsolidationDetailsResponse.class);
        String containerCategory = consolidation.getContainerCategory();
        if(Objects.equals(consolidation.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && consolidationResponse.get().getShipmentsList() != null && !consolidationResponse.get().getShipmentsList().isEmpty()){
            boolean isFcl = true;
            boolean isLcl = true;
            for (var ship: consolidationResponse.get().getShipmentsList()){
                if(!Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL))
                    isFcl = false;
                if(!Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL))
                    isLcl = false;
            }
            if(isFcl)
                containerCategory = Constants.CARGO_TYPE_FCL;
            else if (isLcl)
                containerCategory = Constants.SHIPMENT_TYPE_LCL;
        }

        var origin = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOrigin() : null;
        var destination = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestination() : null;
        var originPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOriginPort() : null;
        var destinationPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestinationPort() : null;
        var voyage = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVoyage() : null;
        var vessel = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVessel() : null;
        var aircrafType = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAircraftType() : null;
        var eta = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEta() : null;
        var etd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEtd() : null;
        var ata = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAta() : null;
        var atd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAtd() : null;
        var consolAllocation = consolidation.getAllocations();
        var consolCarrier = consolidation.getCarrierDetails();
        shipment = ShipmentDetailsResponse.builder()
                .transportMode(consolidation.getTransportMode() == null ? tenantSettings.getDefaultTransportMode() : consolidation.getTransportMode())
                .bookingNumber(bookingNumber)
                .consolidationList(List.of(modelMapper.map(consolidation, ConsolidationListResponse.class)))
                .direction(consolidation.getShipmentType() == null ? tenantSettings.getDefaultShipmentType() : consolidation.getShipmentType())
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .shipmentType(containerCategory == null ? tenantSettings.getDefaultContainerType() : containerCategory)
                .additionalDetails(AdditionalDetailResponse.builder()
                        .SMTPIGMDate(consolidation.getSmtpigmDate())
                        .SMTPIGMNumber(consolidation.getSmtpigmNumber())
                        .inwardDateAndTime(consolidation.getInwardDateAndTime())
                        .releaseType(consolidation.getReleaseType())
                        .warehouseId(consolidation.getWarehouseId())
                        .isInland(consolidation.getIsInland())
                        .original(consolidation.getOriginal())
                        .IGMFileNo(consolidation.getIgmFileNo())
                        .IGMFileDate(consolidation.getIgmFileDate())
                        .IGMInwardDate(consolidation.getIgmInwardDate())
                        .copy(consolidation.getCopy())
                        .customDeclType(consolidation.getDeclarationType())
                        .importBroker(consolidation.getReceivingAgent())
                        .exportBroker(consolidation.getSendingAgent())
                        .build())
                .carrierDetails(CarrierDetailResponse.builder()
                        .ata(ata)
                        .eta(eta)
                        .atd(atd)
                        .etd(etd)
                        .origin(origin)
                        .vessel(vessel)
                        .originPort(originPort)
                        .destinationPort(destinationPort)
                        .shippingLine(consolCarrier != null ? consolCarrier.getShippingLine() : null)
                        .voyage(voyage)
                        .aircraftType(aircrafType)
                        .destination(destination)
                        .flightNumber(consolCarrier != null ? consolCarrier.getFlightNumber() : null)
                        .cfs(consolidation.getCarrierDetails().getCfs())
                        .build())
                .weight(consolAllocation != null ? consolAllocation.getWeight() : null)
                .weightUnit(consolAllocation != null ? consolAllocation.getWeightUnit() : tenantSettings.getWeightChargeableUnit())
                .volume(consolAllocation != null ? consolAllocation.getVolume() : null)
                .volumeUnit(consolAllocation != null ? consolAllocation.getVolumeUnit() : tenantSettings.getVolumeChargeableUnit())
                .chargable(consolAllocation != null ? consolAllocation.getChargable() : null)
                .chargeableUnit(consolAllocation != null ? consolAllocation.getChargeableUnit() : null)
                .paymentTerms(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidation.getShipmentType().equals("EXP")
                        ? consolidation.getPayment() : null)
                .masterBill(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? consolidation.getMawb() : consolidation.getBol())
                .status(0)
                .source(Constants.SYSTEM)
                .createdBy(UserContext.getUser().getUsername())
                .customerCategory(CustomerCategoryRates.CATEGORY_5)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolRef(consolidation.getConsolidationNumber())
                .build();

        if (consolidation.getConsolidationAddresses() != null) {
            consolidation.getConsolidationAddresses().stream().forEach(party -> {
                if (party.getType().equals("NP1")) {
                    shipment.getAdditionalDetails().setNotifyParty(
                            PartiesResponse.builder()
                                    .orgCode(party.getOrgCode())
                                    .addressCode(party.getAddressCode())
                                    .build());
                }
            });
        }

        if(!IsStringNullOrEmpty(shipment.getCarrierDetails().getOrigin())) {
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPaidPlace()))
                shipment.getAdditionalDetails().setPaidPlace(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfIssue()))
                shipment.getAdditionalDetails().setPlaceOfIssue(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfSupply()))
                shipment.getAdditionalDetails().setPlaceOfSupply(shipment.getCarrierDetails().getOrigin());
        }
        if(shipment.getCarrierDetails().getEta() != null) {
            if(shipment.getAdditionalDetails().getDateOfIssue() == null)
                shipment.getAdditionalDetails().setDateOfIssue(shipment.getCarrierDetails().getEta());
            if(shipment.getAdditionalDetails().getDateOfReceipt() == null)
                shipment.getAdditionalDetails().setDateOfReceipt(shipment.getCarrierDetails().getEta());
        }

        PartiesResponse parties;
        if(consolidation.getReceivingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getReceivingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setImportBroker(parties);
        }
        if(consolidation.getSendingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getSendingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setExportBroker(parties);
        }

        //Generate HBL
        if(Constants.TRANSPORT_MODE_SEA.equals(shipment.getTransportMode()) && Constants.DIRECTION_EXP.equals(shipment.getDirection()))
            shipment.setHouseBill(generateCustomHouseBL(null));

        try {
            log.info("Fetching Tenant Model");
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            shipment.setFreightLocalCurrency(currencyCode);
        } catch (Exception e){
            log.error("Failed in fetching tenant data from V1 with error : {}", e);
        }

        createShipmentPayload(modelMapper.map(shipment, ShipmentDetails.class), shipment);

        return ResponseHelper.buildSuccessResponse(shipment);
    }

    public String generateCustomHouseBL(ShipmentDetails shipmentDetails) {
        String res = null;
        if(shipmentDetails != null) {
            res = shipmentDetails.getHouseBill();
        }
        ShipmentSettingsDetails tenantSetting = commonUtils.getShipmentSettingFromContext();
        if(shipmentDetails == null && tenantSetting != null && tenantSetting.getRestrictHblGen()) {
            return null;
        }

        if (shipmentDetails != null && tenantSetting.getCustomisedSequence()) {
            try {
                res = productEngine.getCustomizedBLNumber(shipmentDetails, tenantSetting);
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }

        if(res == null || res.isEmpty()) {
            res = tenantSetting.getHousebillPrefix() ==  null ? "" : tenantSetting.getHousebillPrefix();
            String numberGeneration = tenantSetting.getHousebillNumberGeneration() ==  null ? "" : tenantSetting.getHousebillNumberGeneration();
            switch(numberGeneration) {
                case "Random" :
                    res += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = getShipmentsSerialNumber();
                    res += serialNumber;
                    break;
                default : res = "";
                    break;
            }
        }

        return res;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultShipment() {
        String responseMsg;
        try {
            var tenantSettings = commonUtils.getShipmentSettingFromContext();
            // Populate shipment details on basis of tenant settings
            ShipmentDetailsResponse response = new ShipmentDetailsResponse();
            response.setAdditionalDetails(new AdditionalDetailResponse());
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setDirection(tenantSettings.getDefaultShipmentType());
            response.setShipmentType(tenantSettings.getDefaultContainerType());

            response.setVolumeUnit(tenantSettings.getVolumeChargeableUnit());
            response.setWeightUnit(tenantSettings.getWeightChargeableUnit());
            response.setStatus(0);
            response.setSource(Constants.SYSTEM);
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
            response.setShipmentCreatedOn(LocalDateTime.now());
            response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            response.setAutoUpdateWtVol(true);
            response.setDateType(DateBehaviorType.ESTIMATED);
            //Generate HBL
            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            try {
                log.info("Fetching Tenant Model");
                TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
                String currencyCode = tenantModel.currencyCode;
                response.setFreightLocalCurrency(currencyCode);
                List<UnlocationsResponse> unlocationsResponse = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.convertToString(tenantModel.getUnloco()));
                if (!Objects.isNull(unlocationsResponse) && !unlocationsResponse.isEmpty()) {
                    response.getAdditionalDetails().setPlaceOfIssue(unlocationsResponse.get(0).getLocationsReferenceGUID());
                    response.getAdditionalDetails().setPaidPlace(unlocationsResponse.get(0).getLocationsReferenceGUID());
                    response.getAdditionalDetails().setPlaceOfSupply(unlocationsResponse.get(0).getLocationsReferenceGUID());
                }
            } catch (Exception e){
                log.error("Failed in fetching tenant data from V1 with error : {}", e);
            }

            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));
            //response.setShipmentId(generateShipmentId());

            this.createShipmentPayload(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getMasterDataMappings() {
        String responseMsg;
        try {
            List<MasterDataDescriptionResponse> response = new ArrayList<>();

            //Get current Tenant's setting
            Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
            var tenantSetting = optional.get();
            // get all the master data based on field names of
            response = masterDataUtils.getMasterDataDescription(tenantSetting);

            return  ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel){
        AttachListShipmentRequest request = (AttachListShipmentRequest) commonRequestModel.getData();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (!consolidationDetails.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getConsolidationId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> spec = tuple.getLeft();
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer())
            spec = spec.and(notInConsoleMappingTable());
        else
            spec = spec.and(notInConsoleMappingTable()).and(notInContainerMappingTable());
        if(Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole()))
            commonUtils.setInterBranchContextForHub();
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(spec , tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }
    public static Specification<ShipmentDetails> notInConsoleMappingTable() {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.isEmpty(root.get(Constants.CONSOLIDATION_LIST));
        };
    }
    public static Specification<ShipmentDetails> notInContainerMappingTable() {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.isEmpty(root.get(Constants.CONTAINERS_LIST));
        };
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        boolean setShipmentTypefilter = false;
        boolean isFcl = true;
        boolean isLcl = true;
        List<ShipmentDetails> shipmentDetailsList = consolidationDetails.getShipmentsList();
        if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && shipmentDetailsList != null && !shipmentDetailsList.isEmpty()){
            setShipmentTypefilter = true;
            for (var ship: shipmentDetailsList) {
                if(!Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL))
                    isFcl = false;
                else if (!Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) {
                    isLcl = false;
                }
            }
        }
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationIdAll(request.getConsolidationId());
        List<Long> excludeShipments = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();

        if(request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()){
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE, consolidationDetails.getTransportMode(), "=", request);
        if(excludeShipments != null && !excludeShipments.isEmpty())
            defaultRequest = CommonUtils.andCriteria("id", excludeShipments, "NOTIN", defaultRequest);

        if(!Objects.isNull(consolidationDetails.getShipmentType()))
            CommonUtils.andCriteria(Constants.DIRECTION, consolidationDetails.getShipmentType(), "=", defaultRequest);
        else
            CommonUtils.andCriteria(Constants.DIRECTION, "", Constants.IS_NULL, defaultRequest);
        if(setShipmentTypefilter){
            if(isFcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, Constants.CARGO_TYPE_FCL,"=" , defaultRequest);
            else if (isLcl)
                CommonUtils.andCriteria(Constants.SHIPMENT_TYPE,  Constants.SHIPMENT_TYPE_LCL,"=" , defaultRequest);
        }
        if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && !Objects.isNull(consolidationDetails.getContainerCategory())) {
            CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, consolidationDetails.getContainerCategory(),"=" , defaultRequest);
        }
        CommonUtils.andCriteria(Constants.STATUS, 2, "!=", defaultRequest);
        CommonUtils.andCriteria(Constants.STATUS, 3, "!=", defaultRequest);
        if(checkForNonDGConsoleAndAirDgFlagAndNonDGUser(consolidationDetails))
            CommonUtils.andCriteria(CONTAINS_HAZARDOUS, false, "=", defaultRequest);
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

        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                || Boolean.FALSE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            if (!Objects.isNull(consolidationDetails.getCarrierDetails().getOriginPort()))
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, consolidationDetails.getCarrierDetails().getOriginPort(), "=", defaultRequest);
            else
                CommonUtils.andCriteria(Constants.ORIGIN_PORT, "", Constants.IS_NULL, defaultRequest);
            if (!Objects.isNull(consolidationDetails.getCarrierDetails().getDestinationPort()))
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, consolidationDetails.getCarrierDetails().getDestinationPort(), "=", defaultRequest);
            else
                CommonUtils.andCriteria(Constants.DESTINATION_PORT, "", Constants.IS_NULL, defaultRequest);
        }

        if(Boolean.TRUE.equals(request.getEtaMatch())){
            if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(consolidationDetails.getShipmentType(), Constants.DIRECTION_EXP)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consolidationDetails.getCarrierDetails().getEta())) {
                    LocalDateTime eta = consolidationDetails.getCarrierDetails().getEta();
                    var thresholdETAFrom = eta.plusDays(-1);
                    var thresholdETATo = eta.plusDays(1);

                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETAFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("eta", thresholdETATo, "<=", defaultRequest);
                }
            }
            else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consolidationDetails.getCarrierDetails().getEta()))
                    criteria = Criteria.builder().fieldName("eta").operator("=").value(consolidationDetails.getCarrierDetails().getEta()).build();
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
        if(Boolean.TRUE.equals(request.getEtdMatch())){

            if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                    && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if (!Objects.isNull(consolidationDetails.getCarrierDetails().getEtd())) {
                    LocalDateTime etd = consolidationDetails.getCarrierDetails().getEtd();
                    var thresholdETDFrom = etd.plusDays(-1);
                    var thresholdETDTo = etd.plusDays(1);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDFrom, ">=", defaultRequest);
                    defaultRequest = CommonUtils.andCriteria("etd", thresholdETDTo, "<=", defaultRequest);
                }
            }
            else {
                innerFilers1 = new ArrayList<>();
                if (!Objects.isNull(consolidationDetails.getCarrierDetails().getEtd()))
                    criteria = Criteria.builder().fieldName("etd").operator("=").value(consolidationDetails.getCarrierDetails().getEtd()).build();
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
        if(Boolean.TRUE.equals(request.getScheduleMatch())){
            if(Objects.equals(consolidationDetails.getTransportMode(),Constants.TRANSPORT_MODE_AIR)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getFlightNumber()))
                    criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator("=").value(consolidationDetails.getCarrierDetails().getFlightNumber()).build();
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
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getShippingLine()))
                    criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator("=").value(consolidationDetails.getCarrierDetails().getShippingLine()).build();
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
            else if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVessel()))
                    criteria = Criteria.builder().fieldName(Constants.VESSEL).operator("=").value(consolidationDetails.getCarrierDetails().getVessel()).build();
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
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVoyage()))
                    criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator("=").value(consolidationDetails.getCarrierDetails().getVoyage()).build();
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
        }
        return defaultRequest;
    }

    private boolean isAirDgUser() {
        return UserContext.isAirDgUser();
    }

    private boolean checkForNonDGConsoleAndAirDgFlagAndNonDGUser(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        if(!Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode()))
            return false;
        if(Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return false;
        return !isAirDgUser();
    }

    public boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails){
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return true;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return true;
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return true;
        if(consolidationDetails.getShipmentsList() == null || consolidationDetails.getShipmentsList().isEmpty())
            return false;
        return consolidationDetails.getShipmentsList().stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
    }

    /**
     * back flows data of the current updated shipment to all its sibling shipments attached to the common console
     * @param current_shipment
     * @param old_shipment
     */
    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest) throws RunnerException {
        List<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
        ConsolidationDetails consolidationDetails;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (consolidationList != null && consolidationList.size() > 0) ? consolidationList.get(0) : null;
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if (consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        if(linkedConsol != null && shipmentRequest != null) {
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                .consolidationId(linkedConsol.getId())
                .saveConsol(true)
                .shipmentRequest(shipmentRequest).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
        else if(oldEntity != null && oldEntity.getConsolidationList() != null && !oldEntity.getConsolidationList().isEmpty()) {
            var oldConsolId = oldEntity.getConsolidationList().get(0).getId();
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(oldConsolId)
                    .saveConsol(true)
                    .shipmentRequest(ShipmentRequest.builder().id(shipment.getId()).build()).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
        boolean makeConsoleDG = checkForDGShipmentAndAirDgFlag(shipment);
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if(linkedConsol != null && (oldEntity == null || !Objects.equals(shipment.getMasterBill(),oldEntity.getMasterBill()) ||
                !Objects.equals(shipment.getDirection(),oldEntity.getDirection()) ||
                (shipment.getAdditionalDetails() != null && !Objects.equals(shipment.getAdditionalDetails().getSci(),oldEntity.getAdditionalDetails().getSci())) ||
                (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                (!Objects.equals(shipment.getCarrierDetails().getVoyage(),oldEntity.getCarrierDetails().getVoyage()) ||
                        !Objects.equals(shipment.getCarrierDetails().getVessel(),oldEntity.getCarrierDetails().getVessel()) ||
                        !Objects.equals(shipment.getCarrierDetails().getShippingLine(),oldEntity.getCarrierDetails().getShippingLine()) ||
                        !Objects.equals(shipment.getCarrierDetails().getAircraftType(),oldEntity.getCarrierDetails().getAircraftType())
                )))) {
            consolidationDetails = consolidationDetailsDao.findById(consolidationList.get(0).getId()).get();
            consolidationDetails.setBol(shipment.getMasterBill());
            if(consolidationDetails.getCarrierDetails() == null)
                consolidationDetails.setCarrierDetails(new CarrierDetails());
            consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
            consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
            consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
            consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
            consolidationDetails.setShipmentType(shipment.getDirection());

            if(makeConsoleDG)
                consolidationDetails.setHazardous(true);
            List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationList.get(0).getId(), shipment);
            if (!shipmentIdList.isEmpty()) {
                ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
                Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class, tableNames);
                Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

                List<ShipmentDetails> shipments = page.getContent();
                shipments.stream()
                    .map(i -> {
                        i.setMasterBill(shipment.getMasterBill());
                        i.setDirection(shipment.getDirection());
                        if (shipment.getCarrierDetails() != null) {
                            i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                            i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                            i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                            i.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
                        }
                        if (makeConsoleNonDG.get() && Boolean.TRUE.equals(i.getContainsHazardous()))
                            makeConsoleNonDG.set(false);
                        if(Objects.equals(i.getAdditionalDetails().getSci(), AwbConstants.T1)){
                            makeConsoleSciT1.set(true);
                        }
                        return i;
                    }).toList();
                shipmentDao.saveAll(shipments);
            }
            if(makeConsoleNonDG.get())
                consolidationDetails.setHazardous(false);
            if(makeConsoleSciT1.get() && checkConsoleSciUpdateT1(shipment, oldEntity))
                consolidationDetails.setSci(AwbConstants.T1);
            else if(Objects.equals(consolidationDetails.getSci(), AwbConstants.T1) && !makeConsoleSciT1.get() && oldEntity != null && !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
                consolidationDetails.setSci(null);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            return consolidationDetails;
        }
        else // only execute when above logic execution not required (i.e. saving all shipments not required)
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList, shipment);
    }

    private List<Long> getShipmentIdsExceptCurrentShipment(Long consolidationId, ShipmentDetails shipment) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappings.stream().filter(c -> !Objects.equals(c.getShipmentId(), shipment.getId()))
                .map(ConsoleShipmentMapping::getShipmentId).toList();
    }

    private ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, List<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if(consolidationList != null && !consolidationList.isEmpty()) {
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList.get(0).getId(), shipment, null);
        }
        return null;
    }

    public ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Long consolidationId, ShipmentDetails shipment, ConsolidationDetails consolidationDetails) {
        if(makeConsoleDG) {
            consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
            return saveConsolidationDGValue(true, consolidationDetails);
        }
        if(makeConsoleNonDG.get()) {
            List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationId, shipment);
            makeConsoleNonDG.set(checkIfAllShipmentsAreNonDG(shipmentIdList));
            if(makeConsoleNonDG.get()) {
                consolidationDetails = getConsolidationDetails(consolidationId, consolidationDetails);
                return saveConsolidationDGValue(false, consolidationDetails);
            }
        }
        return null;
    }

    public ConsolidationDetails getConsolidationDetails(Long consolidationId, ConsolidationDetails consolidationDetails) {
        if(!Objects.isNull(consolidationDetails))
            return consolidationDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(consolidationId);
        if(optionalConsolidationDetails.isPresent())
            return optionalConsolidationDetails.get();
        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    public boolean checkIfAllShipmentsAreNonDG(List<Long> shipmentIdList) {
        if (!shipmentIdList.isEmpty()) {
            ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
            listReq = andCriteria(CONTAINS_HAZARDOUS, true, "=", listReq);
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            if(page != null && !page.getContent().isEmpty())
                return false;
        }
        return true;
    }

    public ConsolidationDetails saveConsolidationDGValue(boolean dgFlag, ConsolidationDetails consolidationDetails) {
        if( (!Boolean.TRUE.equals(consolidationDetails.getHazardous()) && dgFlag)
            || (!dgFlag && Boolean.TRUE.equals(consolidationDetails.getHazardous())) ) {
            consolidationDetails.setHazardous(dgFlag);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            return consolidationDetails;
        }
        return null;
    }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return !Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private boolean checkConsoleSciUpdateT1(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        if(shipment.getAdditionalDetails() == null) return false;
        if(Strings.isNullOrEmpty(shipment.getAdditionalDetails().getSci())) return false;
        if(!Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1)) return false;
        return oldEntity == null || !Objects.equals(shipment.getAdditionalDetails().getSci(),oldEntity.getAdditionalDetails().getSci());
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
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
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

    @Override
    public ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getId());
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(shipmentDetails.get().getGuid()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void autoGenerateEvents(ShipmentDetails shipmentDetails, Integer previousStauts) {
        Events response = null;
        if(shipmentDetails.getStatus() != null) {
            if (previousStauts == null || !shipmentDetails.getStatus().equals(previousStauts)) {
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Confirmed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, EventConstants.SHPCNFRM);
                }
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Completed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, EventConstants.SHPCMPLT);
                }
            }
            if(response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    private void autoGenerateCreateEvent(ShipmentDetails shipmentDetails) {
        Events response = null;
        response = createAutomatedEvents(shipmentDetails, EventConstants.SHCR);

        if (shipmentDetails.getEventsList() == null) {
            shipmentDetails.setEventsList(new ArrayList<>());
        }
        shipmentDetails.getEventsList().add(response);
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        // Persist the event
        eventDao.save(events);
        return events;
    }

    public ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null) {
            log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id can't be null");
        }
        Long id = request.getId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
        List<Long> shipmentIdsList = new ArrayList<>();
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
            shipmentIdsList = consoleShipmentMappings.stream().map(x -> x.getShipmentId()).toList();
        }
        ListCommonRequest listCommonRequest = CommonUtils.andCriteria("id", shipmentIdsList, "IN", null);
        return fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    public ResponseEntity<IRunnerResponse> fetchActiveInvoices(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getGuid() == null) {
            log.error("Request guid is null for fetch active invoices with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Shipment Guid can't be null");
        }

        boolean activeCharges = billingServiceAdapter.fetchActiveInvoices(request);
        CheckActiveInvoiceResponse checkActiveInvoiceResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(activeCharges).build();

        /*
        activeCharges false means atleast one of the value is not 0
        return true because active charges are present
         */
        return ResponseHelper.buildSuccessResponse(checkActiveInvoiceResponse);
    }

    public ResponseEntity<IRunnerResponse> showAssignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentConsoleIdDto request = (ShipmentConsoleIdDto) commonRequestModel.getData();
            Long shipmentId = request.getShipmentId();
            Long consolidationId = request.getConsolidationId();
            List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByShipmentId(shipmentId);
            List<Containers> containers = containerDao.findByConsolidationId(consolidationId);
            boolean showDialog = false;
            if(shipmentsContainersMappingList != null && containers != null && containers.size() > 0 &&
                    containers.size() != shipmentsContainersMappingList.size())
            {
                ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
                if(shipmentSettingsDetails.getMultipleShipmentEnabled() == null || !shipmentSettingsDetails.getMultipleShipmentEnabled()) {
                    for (Containers containers1 : containers) {
                        if(containers1.getShipmentsList() == null || containers1.getShipmentsList().size() == 0) {
                            showDialog = true;
                            break;
                        }
                    }
                }
                else
                    showDialog = true;
            }
            int numberOfShipments = 0;
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
            if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                numberOfShipments = consoleShipmentMappings.size();
            AssignAllDialogDto response = new AssignAllDialogDto();
            response.setShowDialog(showDialog);
            response.setNumberOfShipments(numberOfShipments);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> fetchCreditLimit(String orgCode, String addressCode) throws RunnerException {
        if(StringUtility.isEmpty(orgCode)) {
            throw new RunnerException("OrgCode to fetch creditLimit can't be null");
        }
        AddressTranslationRequest.OrgAddressCode orgAddressCode = AddressTranslationRequest.OrgAddressCode.builder().OrgCode(orgCode).AddressCode(addressCode).build();
        try {
            V1DataResponse v1DataResponse = v1Service.fetchCreditLimit(orgAddressCode);
            if(v1DataResponse.entities == null) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            List<CreditLimitResponse> creditLimitResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), CreditLimitResponse.class);
            if(creditLimitResponses == null || creditLimitResponses.size() == 0) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            return ResponseHelper.buildDependentServiceResponse(creditLimitResponses.get(0), 0, 0);
        } catch (Exception e) {
            log.debug("No Data found for org code {} {}", orgCode, e.getMessage());
        }

        return ResponseHelper.buildSuccessResponse();
    }

    public void fetchCreditLimitMasterData(String orgCode, String addressCode, Map<String, Object> response) {
        if(StringUtility.isEmpty(orgCode)) {
            return;
        }
        AddressTranslationRequest.OrgAddressCode orgAddressCode = AddressTranslationRequest.OrgAddressCode.builder().OrgCode(orgCode).AddressCode(addressCode).build();
        try {
            V1DataResponse v1DataResponse = v1Service.fetchCreditLimit(orgAddressCode);
            if(v1DataResponse.entities == null) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ;
            }
            List<CreditLimitResponse> creditLimitResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), CreditLimitResponse.class);
            if(creditLimitResponses == null || creditLimitResponses.size() == 0) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return;
            }
            if(response == null) {
                response = new HashMap<>();
            }
            response.put(Constants.CREDIT_LIMIT, creditLimitResponses.get(0));
        } catch (Exception e) {
            log.debug("No Data found for org code {} {}", orgCode, e.getMessage());
        }
    }

    @Override
    public void updateDateAndStatus(Long id, LocalDateTime date, Integer status) throws RunnerException {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
        if(shipmentDetails.isPresent()) {
            ShipmentDetails shipment = shipmentDetails.get();
//            if(date != null) {
//                shipment.getAdditionalDetails().setDateOfIssue(date);
//            }
            if(status != null) {
                shipment.setStatus(status);
            }
            shipmentDao.save(shipment, false);
            try {
                shipmentSync.sync(shipment, null, null, shipment.getGuid().toString(), false);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchEmails(Long shipmentId, Long consolidationId) {
        if(Objects.isNull(shipmentId) && Objects.isNull(consolidationId)) {
            log.error("Invalid request for fetchEmails");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        if (!Objects.isNull(shipmentId)) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
            if (shipmentDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForShipment(shipmentDetails.get());
        }
        else if (!Objects.isNull(consolidationId)) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolidationId);
            if (consolidationDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetails.get());
        }
        return ResponseHelper.buildFailedResponse(DaoConstants.DAO_INVALID_REQUEST_MSG);
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromV1(CommonRequestModel commonRequestModel){
        CheckCreditLimitFromV1Request request = (CheckCreditLimitFromV1Request) commonRequestModel.getData();
        String checkCreditLimitDocs = IReport.checkCreditLimitDocs(request.getDocType());
        if(!Objects.isNull(checkCreditLimitDocs)){
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(request.getShipmentId());
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
                var response = v1ServiceUtil.validateCreditLimit(modelMapper.map(shipmentDetails.getClient(), Parties.class), checkCreditLimitDocs, shipmentDetails.getGuid(), request.getTaskCreation());
                return ResponseHelper.buildSuccessResponse(response);
            }
            return ResponseHelper.buildFailedResponse("Shipment not exist for given id");
        } else {
            return ResponseHelper.buildFailedResponse("Please send a valid doc type for check credit limit.");
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainerListFromTrackingService(Long shipmentId, Long consolidationId) throws RunnerException {
        try {

            if (shipmentId == null && consolidationId == null) {
                throw new RunnerException("Empty request: please provide either ShipmentId or ConsolidationId");
            }

            Long effectiveShipmentId = getEffectiveShipmentId(shipmentId, consolidationId);

            ShipmentDetails shipmentDetails = shipmentDao.findById(effectiveShipmentId)
                    .orElseThrow(() -> new RunnerException("No shipment present for provided id " + shipmentId));

            TrackingServiceApiResponse trackingResponse = fetchTrackingDataByShipmentId(shipmentDetails.getShipmentId());
            TrackingServiceLiteContainerResponse response = convertToLiteContainerResponse(trackingResponse.getContainers());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            throw new RunnerException("Unexpected error occurred: ", e);
        }
    }

    private TrackingServiceLiteContainerResponse convertToLiteContainerResponse(List<Container> containers) {

        TrackingServiceLiteContainerResponse response = new TrackingServiceLiteContainerResponse();
        List<LiteContainer> liteContainers = new ArrayList<>();

        containers.forEach(container -> {

            TrackingServiceLiteContainerResponse.LiteContainer liteContainer = new TrackingServiceLiteContainerResponse.LiteContainer();

            liteContainer.setContainerNumber(container.getContainerNumber());
            liteContainer.setIdentifierType(container.getIdentifierType());
            liteContainer.setIdentifierValue(container.getIdentifierValue());
            if (container.getContainerBase() != null) {
                liteContainer.setType(container.getContainerBase().getType());
                liteContainer.setSize(container.getContainerBase().getSize());
                liteContainer.setTypeIsoCode(container.getContainerBase().getTypeIsoCode());
                liteContainer.setBolNumber(container.getContainerBase().getBolNumber());
                liteContainer.setBookingNumber(container.getContainerBase().getBookingNumber());
                liteContainer.setSealNumber(container.getContainerBase().getSealNumber());
                liteContainer.setMarks(container.getContainerBase().getMarks());
                liteContainer.setIncoterm(container.getContainerBase().getIncoterm());
                liteContainer.setShipper(container.getContainerBase().getShipper());
                liteContainer.setConsignee(container.getContainerBase().getConsignee());
                liteContainer.setWeight(container.getContainerBase().getWeight());
                liteContainer.setWeightUom(container.getContainerBase().getWeightUom());
                liteContainer.setNumberOfPackages(container.getContainerBase().getNumberOfPackages());
                liteContainer.setPackageType(container.getContainerBase().getPackageType());
                liteContainer.setReeferTemperature(container.getContainerBase().getReeferTemperature());
                liteContainer.setCommodity(container.getContainerBase().getCommodity());
                liteContainer.setLatitude(container.getContainerBase().getLatitude());
                liteContainer.setLongitude(container.getContainerBase().getLongitude());
                liteContainer.setLocation(container.getContainerBase().getLocation());
                liteContainer.setLocationUpdateTime(container.getContainerBase().getLocationUpdateTime());
            }
            liteContainers.add(liteContainer);
        });
        response.setContainers(liteContainers);
        return response;
    }

    private Long getEffectiveShipmentId(Long shipmentId, Long consolidationId) throws RunnerException {
        if (shipmentId != null) {
            return shipmentId;
        }

        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        return consoleShipmentMappingList.stream()
                .findFirst()
                .map(ConsoleShipmentMapping::getShipmentId)
                .orElseThrow(() -> new RunnerException("No shipment present for provided consolidation id " + consolidationId));
    }

    private TrackingServiceApiResponse fetchTrackingDataByShipmentId(String shipmentId) throws RunnerException {
        try {
            return trackingServiceAdapter.fetchTrackingData(
                    TrackingRequest.builder().referenceNumber(shipmentId).build());
        } catch (Exception e) {
            throw new RunnerException("Error getting response from tracking api for shipment id: " + shipmentId + ", Details :" + e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDateTimeChangeUpdates(Long shipmentId) throws RunnerException {
        if(Objects.isNull(shipmentId))
            throw new RunnerException("shipment id can't be null");

        Optional<ShipmentDetails> optional = shipmentDao.findById(shipmentId);
        if(optional.isEmpty())
            throw new RunnerException("No shipment present for provided id");

        ShipmentDetails shipment = optional.get();

        TrackingServiceApiResponse trackingResponse = null;
        try {
            trackingResponse = trackingServiceAdapter.fetchTrackingData(
                    TrackingRequest.builder().referenceNumber(shipment.getShipmentId()).build());
        } catch (Exception ignored) {
            log.error("Error getting response from tracking api for date-time changes");
        }

        LocalDateTime trackingAta = null;
        LocalDateTime trackingAtd = null;
        LocalDateTime trackingEta = null;
        LocalDateTime trackingEtd = null;

        if(trackingResponse != null && trackingResponse.getContainers() != null && !trackingResponse.getContainers().isEmpty()
            && trackingResponse.getContainers().get(0).getJourney() != null) {
            trackingAta = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfArrivalAta()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingAtd = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfDepartureAtd()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingEta = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfArrivalEta()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
            trackingEtd = Optional.ofNullable(trackingResponse.getContainers().get(0).getJourney().getPortOfDepartureEtd()).map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null);
        }

        List<DateTimeChangeLog> shipmentDateLogs = dateTimeChangeLogService.getDateTimeChangeLog(shipmentId);
        Map<DateType, List<DateTimeChangeLog>> dateChangeLogMap = shipmentDateLogs.stream().collect(
            Collectors.groupingBy(DateTimeChangeLog::getDateType)
        );

        UpstreamDateUpdateResponse upstreamDateUpdateResponse = new UpstreamDateUpdateResponse();

        //ata
        upstreamDateUpdateResponse.setAta(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var ataChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ATA), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getAta().setChangeLogs(ataChangeLogsResponse);
        if(trackingAta != null && !CommonUtils.areTimeStampsEqual(trackingAta, shipment.getCarrierDetails().getAta())) {
            upstreamDateUpdateResponse.getAta().setUpdatedDate(trackingAta);
        }
        //atd
        upstreamDateUpdateResponse.setAtd(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var atdChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ATD), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getAtd().setChangeLogs(atdChangeLogsResponse);
        if(trackingAtd != null && !CommonUtils.areTimeStampsEqual(trackingAtd, shipment.getCarrierDetails().getAtd())) {
            upstreamDateUpdateResponse.getAtd().setUpdatedDate(trackingAtd);
        }
        //eta
        upstreamDateUpdateResponse.setEta(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var etaChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ETA), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getEta().setChangeLogs(etaChangeLogsResponse);
        if(trackingEta != null && !CommonUtils.areTimeStampsEqual(trackingEta, shipment.getCarrierDetails().getEta())) {
             upstreamDateUpdateResponse.getEta().setUpdatedDate(trackingEta);
        }
        //etd
        upstreamDateUpdateResponse.setEtd(UpstreamDateUpdateResponse.DateAndLogResponse.builder().build());
        var etdChangeLogsResponse = jsonHelper.convertValueToList(dateChangeLogMap.get(DateType.ETD), DateTimeChangeLogResponse.class);
        upstreamDateUpdateResponse.getEtd().setChangeLogs(etdChangeLogsResponse);
        if(trackingEtd != null && !CommonUtils.areTimeStampsEqual(trackingEtd, shipment.getCarrierDetails().getEtd())) {
            upstreamDateUpdateResponse.getEtd().setUpdatedDate(trackingEtd);
        }

        return ResponseHelper.buildSuccessResponse(upstreamDateUpdateResponse);
    }

    @Override
    public ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, boolean isAttached) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        if (consolidationDetails.isEmpty()) {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
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
            if(!isAttached) {
                var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdAll(consoleId);
                if (consoleShipMappingList == null || consoleShipMappingList.isEmpty()) {
                    return ResponseHelper.buildListSuccessResponse(new ArrayList<>(), 1, 0);
                }
                requestedTypeMap = consoleShipMappingList.stream().collect(Collectors.toMap(ConsoleShipmentMapping::getShipmentId, Function.identity(), (existingValue, newValue) -> existingValue));
                List<Long> shipIds = consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
                CommonUtils.andCriteria("id", shipIds, "IN", request);
            } else {
                CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
            }
        } else {
            CommonUtils.andCriteria(CONSOLIDATION_ID, consoleId, "=", request);
        }
        var response = list(CommonRequestModel.buildRequest(request));
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
        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllShipments(Long consoleId) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        Long attachedShipmentCurrentBranchCount = 0L;
        Long attachedShipmentInterBranchCount = 0L;
        Long pendingAttachmentCount = 0L;
        AllShipmentCountResponse allShipmentCountResponse = new AllShipmentCountResponse();
        if(consolidationDetails.isPresent()) {
            if(Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
                commonUtils.setInterBranchContextForHub();
            }
            List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
            for(ConsoleShipmentMapping consoleShipmentMapping: consoleShipmentMappingList) {
                if(consoleShipmentMapping.getRequestedType() == null) {
                    attachedShipmentCurrentBranchCount++;
                } else if(consoleShipmentMapping.getRequestedType().equals(APPROVE)) {
                    attachedShipmentInterBranchCount++;
                } else if(consoleShipmentMapping.getRequestedType().equals(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED) || consoleShipmentMapping.getRequestedType().equals(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)) {
                    pendingAttachmentCount++;
                }
            }
            allShipmentCountResponse.setAttachedShipmentCurrentBranchCount(attachedShipmentCurrentBranchCount);
            allShipmentCountResponse.setAttachedShipmentInterBranchCount(attachedShipmentInterBranchCount);
            allShipmentCountResponse.setPendingAttachmentCount(pendingAttachmentCount);
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return ResponseHelper.buildSuccessResponse(allShipmentCountResponse);
    }

    public ResponseEntity<IRunnerResponse> getLatestCargoDeliveryDate(Long consoleId) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consoleId);
        LatestCargoDeliveryInfo latestCargoDeliveryInfo = new LatestCargoDeliveryInfo();
        LocalDateTime latestCargoDeliveryDate = null;
        if(consolidationDetails.isPresent()) {
            List<ShipmentDetails> listOfShipmentsAttachedToConsole = consolidationDetails.get().getShipmentsList();
            latestCargoDeliveryDate = getLatestCargoDeliveryDateHelper(listOfShipmentsAttachedToConsole);
            latestCargoDeliveryInfo.setLatestCargoDeliveryDate(latestCargoDeliveryDate);
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return ResponseHelper.buildSuccessResponse(latestCargoDeliveryInfo);
    }

    private LocalDateTime getLatestCargoDeliveryDateHelper(List<ShipmentDetails> listOfShipmentsAttachedToConsole) {
        return listOfShipmentsAttachedToConsole.stream()
                .map(ShipmentDetails::getCargoDeliveryDate)
                .filter(Objects::nonNull)
                .max(Comparator.naturalOrder())
                .orElse(null);
    }

    private void fetchShipmentsAndConsolidationsForPushRequestEmails(Set<Integer> tenantIds, Set<String> usernamesList, List<Long> shipmentIds, ConsolidationDetails consolidationDetails,
                                                                     List<ShipmentDetails> shipmentDetails, List<ConsolidationDetails> otherConsolidationDetails,
                                                                     List<ConsoleShipmentMapping> consoleShipmentMappings, Map<Long, String> requestedUsernameMap) {
        // fetching shipments
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        for(ShipmentDetails shipmentDetails1 : shipmentDetailsPage.getContent()) {
            tenantIds.add(shipmentDetails1.getTenantId());
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            shipmentDetails.add(shipmentDetails1);
        }

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            if(!Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipmentMapping.getConsolidationId());
            } else if(shipmentIds.contains(consoleShipmentMapping.getShipmentId()) && Objects.equals(consoleShipmentMapping.getConsolidationId(), consolidationDetails.getId())){
                requestedUsernameMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            }
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        Page<ConsolidationDetails> consolidationDetailsPage = null;
        if(!otherConsoleIds.isEmpty()) {
            listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, "IN");
            Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
            consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
            for(ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
                tenantIds.add(consolidationDetails1.getTenantId());
                usernamesList.add(consolidationDetails1.getCreatedBy());
                otherConsolidationDetails.add(consolidationDetails1);
            }
        }
    }

    private void constructAndSendEmailsForPushRequestAccept(List<ShipmentDetails> shipmentDetails, ConsolidationDetails consolidationDetails,
                                                            List<ConsolidationDetails> otherConsolidationDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                            Set<ShipmentRequestedType> shipmentRequestedTypes,
                                                            Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap,
                                                            Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap,
                                                            Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests, Map<Long, String> requestedUsernameMap) {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
            shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));
            shipmentDetails.forEach(shipment -> {
                try {
                    commonUtils.sendEmailForPullPushRequestStatus(shipment, consolidationDetails, SHIPMENT_PUSH_ACCEPTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, requestedUsernameMap.get(shipment.getId()));
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
        if(!otherConsolidationDetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationDetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetailsMap;
            consoleShipmentMappings.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        if(consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            commonUtils.sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                        else
                            commonUtils.sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
    }

    public void sendEmailsForPushRequestAccept(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        List<ShipmentDetails> shipmentDetails = new ArrayList<>();
        List<ConsolidationDetails> otherConsolidationDetails = new ArrayList<>();
        Map<Long, String> requestedUsernameMap = new HashMap<>();

        // fetching data from db
        fetchShipmentsAndConsolidationsForPushRequestEmails(tenantIds, usernamesList, shipmentIds, consolidationDetails, shipmentDetails, otherConsolidationDetails, consoleShipmentMappings, requestedUsernameMap);

        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        // constructing and sending emails
        constructAndSendEmailsForPushRequestAccept(shipmentDetails, consolidationDetails, otherConsolidationDetails, consoleShipmentMappings, shipmentRequestedTypes,
                unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, emailTemplatesRequests, requestedUsernameMap);
    }

    private String fetchShipmentsAndConsolidationsForPullRequestEmails(Set<Integer> tenantIds, Set<String> usernamesList, Long consoleId, Long shipmentId,
                                                                     ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                                     List<ConsoleShipmentMapping> consoleShipmentMappings, List<ConsolidationDetails> otherConsolidationdetails) {
        // fetching shipment and console
        tenantIds.add(consolidationDetails.getTenantId());
        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());
        usernamesList.add(consolidationDetails.getCreatedBy());

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        String requestedUsername = null;
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            if(!Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipmentMapping.getConsolidationId());
            } else if(Objects.equals(consoleShipmentMapping.getShipmentId(), shipmentId) && Objects.equals(consoleShipmentMapping.getConsolidationId(), consoleId)){
                requestedUsername = consoleShipmentMapping.getCreatedBy();
            }
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, "IN");
        Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
        for(ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
            usernamesList.add(consolidationDetails1.getCreatedBy());
            tenantIds.add(consolidationDetails1.getTenantId());
            otherConsolidationdetails.add(consolidationDetails1);
        }
        return requestedUsername;
    }

    public void sendEmailsForPullRequestAccept(Long consoleId, Long shipmentId, Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();
        List<ConsolidationDetails> otherConsolidationdetails = new ArrayList<>();

        // fetching data from db
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consoleId);
        String requestedUsername = fetchShipmentsAndConsolidationsForPullRequestEmails(tenantIds, usernamesList, consoleId, shipmentId, shipmentDetails, consolidationDetails, consoleShipmentMappings, otherConsolidationdetails);

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PULL_ACCEPTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, requestedUsername);
        } catch (Exception e) {
            log.error(ERROR_WHILE_SENDING_EMAIL);
        }
        if(!otherConsolidationdetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationdetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            consoleShipmentMappings.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId())) {
                        if(consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                        else
                            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }

    }

    public void sendEmailForPushRequestReject(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String rejectRemarks, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.ID, shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for(ShipmentDetails shipmentDetails1 : shipmentDetails.getContent()) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            tenantIds.add(shipmentDetails1.getTenantId());
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
        }
        if(shipmentDetails != null && !shipmentDetails.getContent().isEmpty())
            shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));

        Map<Long, String> shipmentRequestUserMap = new HashMap<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            shipmentRequestUserMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long shipId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipId), consolidationDetails, SHIPMENT_PUSH_REJECTED, rejectRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, shipmentRequestUserMap.get(shipId));
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPullRequestReject(Long shipmentId, List<Long> consoleIds, Set<ShipmentRequestedType> shipmentRequestedTypes, String rejectRemarks, List<ConsoleShipmentMapping> consoleShipmentMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        // fetching shipment and consolidations
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, consoleIds, "IN");
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ConsolidationDetails> consolidationDetailsMap = new HashMap<>();
        for(ConsolidationDetails consolidationDetails : consolidationDetailsPage.getContent()) {
            consolidationDetailsMap.put(consolidationDetails.getId(), consolidationDetails);
            tenantIds.add(consolidationDetails.getTenantId());
            usernamesList.add(consolidationDetails.getCreatedBy());
        }

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());

        Map<Long, String> consoleRequestUserMap = new HashMap<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            consoleRequestUserMap.put(consoleShipmentMapping.getConsolidationId(), consoleShipmentMapping.getCreatedBy());
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long consoleId : consoleIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetailsMap.get(consoleId), SHIPMENT_PULL_REJECTED, rejectRemarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, consoleRequestUserMap.get(consoleId));
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPushRequested(Long shipmentId, Long consoleId, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();
        setColoadingStation(shipmentDetails);
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consoleId).get();

        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());
        usernamesList.add(consolidationDetails.getCreatedBy());
        tenantIds.add(consolidationDetails.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        try {
            commonUtils.sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PUSH_REQUESTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, null);
        } catch (Exception e) {
            log.error("Error while sending email");
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> updateShipments(UpdateConsoleShipmentRequest request) throws RunnerException {
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        if (isForHubRequest(request)) {
            processHubRequest(request, shipmentRequestedTypes);
        } else {
            processShipmentRequest(request, shipmentRequestedTypes);
        }
        String warning = null;
        if(!shipmentRequestedTypes.isEmpty()) {
            warning = "Template not found, please inform the region users manually";
        }
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    private boolean checkIfAlreadyPushRequested(ShipmentDetails oldEntity) {
        Integer allMappingsCount = consoleShipmentMappingDao.countAllStateMappings(oldEntity.getId());
        return allMappingsCount > 0;
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateInvoicePosting(InvoicePostingValidationRequest request) {
        Set<UUID> shipmentGuids = request.getShipmentGuids().stream().filter(ObjectUtils::isNotEmpty)
                .map(UUID::fromString).collect(Collectors.toSet());

        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByGuids(shipmentGuids);
        List<InvoicePostingValidationResponse> responses = new ArrayList<>();

        shipments.forEach(shipment -> {
            List<ModuleValidationFieldType> missingFields = new ArrayList<>();

            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())) {
                if (Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                        && (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                        || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipment.getShipmentType()))) {
                    if (Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {

                        validateCarrierDetails(shipment, missingFields);
                        validateContainerDetails(shipment, missingFields);
                        validateMblDetails(shipment, missingFields);

                    } else if (ObjectUtils.isNotEmpty(shipment.getJobType())) {

                        validateMblDetails(shipment, missingFields);

                    }
                }
            } else if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shipment.getTransportMode())
                    && Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                    && Constants.SHIPMENT_TYPE_LSE.equalsIgnoreCase(shipment.getShipmentType())
                    && ObjectUtils.isNotEmpty(shipment.getJobType())) {

                validateCarrierDetails(shipment, missingFields);
                validateMawbDetails(shipment, missingFields);

            }

            responses.add(InvoicePostingValidationResponse.builder()
                    .shipmentGuid(shipment.getGuid().toString())
                    .missingFields(missingFields).build());
        });

        return ResponseHelper.buildSuccessResponse(responses);

    }

    public void validateContainerDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getContainersList()) || !isContainerNumberPresent(shipment.getContainersList())) {
            missingFields.add(ModuleValidationFieldType.CONTAINER_DETAILS);
        }
    }

    public void validateCarrierDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        CarrierDetails carrierDetails = shipment.getCarrierDetails();

        if (ObjectUtils.isEmpty(carrierDetails)) {
            missingFields.add(ModuleValidationFieldType.CARRIER);
            return;
        }

        if (ObjectUtils.isEmpty(carrierDetails.getShippingLine())) {
            missingFields.add(ModuleValidationFieldType.CARRIER);
        }
        if (ObjectUtils.isEmpty(carrierDetails.getEtd())) {
            missingFields.add(ModuleValidationFieldType.CARRIER_ETD);
        }
        if (ObjectUtils.isEmpty(carrierDetails.getEta())) {
            missingFields.add(ModuleValidationFieldType.CARRIER_ETA);
        }
    }

    public void validateMblDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getMasterBill())) {
            missingFields.add(ModuleValidationFieldType.MBL_DETAILS);
        }
    }

    public void validateMawbDetails(ShipmentDetails shipment, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(shipment.getMasterBill())) {
            missingFields.add(ModuleValidationFieldType.MAWB_DETAILS);
        }
    }

    private boolean isContainerNumberPresent(List<Containers> containersList) {
        return containersList.stream().allMatch(container -> container.getContainerNumber() != null);
    }

    private boolean isForHubRequest(UpdateConsoleShipmentRequest request) {
        return request.isForHub();
    }

    private void processHubRequest(UpdateConsoleShipmentRequest updateConsoleShipmentRequest, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(updateConsoleShipmentRequest.getConsoleId());
        if (consolidationDetails.isPresent()) {
            if (Boolean.TRUE.equals(updateConsoleShipmentRequest.isForHub()) && Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
                commonUtils.setInterBranchContextForHub();
            }
            if (ShipmentRequestedType.APPROVE.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) { // one console multiple shipments
                updateConsoleShipmentRequest.getListOfShipments().stream().forEach(shipmentId -> {
                    try {
                        consolidationService.attachShipments(updateConsoleShipmentRequest.getShipmentRequestedType(), updateConsoleShipmentRequest.getConsoleId(), List.of(shipmentId));
                    } catch (RunnerException e) {
                        log.error("Error while attaching shipments: {}", e.getMessage(), e);
                        throw new BillingException(e.getMessage());
                    }
                });
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, updateConsoleShipmentRequest.getListOfShipments(), "IN");
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

                consoleShipmentMappingDao.deletePendingStateByShipmentIds(updateConsoleShipmentRequest.getListOfShipments());
                // one console and list of approved shipments for shipment push accepted from console
                // for each shipment pending multiple consolidation auto rejections (shipment push and shipment pull both got rejected)
                sendEmailsForPushRequestAccept(consolidationDetails.get(), updateConsoleShipmentRequest.getListOfShipments(), shipmentRequestedTypes, consoleShipmentMappingsForEmails);
            } else if (ShipmentRequestedType.REJECT.equals(updateConsoleShipmentRequest.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, updateConsoleShipmentRequest.getListOfShipments(), "IN", null);
                listCommonRequest = andCriteria(CONSOLIDATION_ID, updateConsoleShipmentRequest.getConsoleId(), "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
                updateConsoleShipmentRequest.getListOfShipments().stream().forEach(shipmentId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(updateConsoleShipmentRequest.getConsoleId(), shipmentId));
                // one console and multiple shipments (shipment push rejected)
                if(ShipmentRequestedType.REJECT.equals(updateConsoleShipmentRequest.getShipmentRequestedType())) {
                    sendEmailForPushRequestReject(consolidationDetails.get(), updateConsoleShipmentRequest.getListOfShipments(), shipmentRequestedTypes, updateConsoleShipmentRequest.getRejectRemarks(), consoleShipmentMappings);
                }
            }
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
    }

    private void processShipmentRequest(UpdateConsoleShipmentRequest request, Set<ShipmentRequestedType> shipmentRequestedTypes) throws RunnerException { // one shipment and one/multiple console
        if(Boolean.FALSE.equals(request.isForHub())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
        if(request.getConsoleIdsList() == null || request.getConsoleIdsList().isEmpty()) {
            throw new InvalidDataAccessApiUsageException("Console Ids list should not be empty!!!");
        }
        if (ShipmentRequestedType.APPROVE.equals(request.getShipmentRequestedType())) {
            try {
                consolidationService.attachShipments(request.getShipmentRequestedType(), request.getConsoleIdsList().get(0), List.of(request.getShipmentId()));
            } catch (RunnerException e) {
                log.error("Error while attaching shipments: {}", e.getMessage(), e);
                throw new BillingException(e.getMessage());
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, request.getShipmentId(), "=");
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

            consoleShipmentMappingDao.deletePendingStateByShipmentId(request.getShipmentId());
            // one shipment and one console, shipment pull accepted
            // one shipment and multiple console, shipment pull and push rejected
            sendEmailsForPullRequestAccept(request.getConsoleIdsList().get(0), request.getShipmentId(), shipmentRequestedTypes, consoleShipmentMappingsForEmails);
        } else if (ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(request.getShipmentRequestedType())) {
            // fetching from console shipment mapping
            ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, request.getShipmentId(), "=", null);
            listCommonRequest = andCriteria(CONSOLIDATION_ID, request.getConsoleIdsList(), "IN", listCommonRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
            request.getConsoleIdsList().stream().forEach(consoleId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(consoleId, request.getShipmentId()));
            // one shipment and multiple console, shipment pull rejected
            if(ShipmentRequestedType.REJECT.equals(request.getShipmentRequestedType())) {
                sendEmailForPullRequestReject(request.getShipmentId(), request.getConsoleIdsList(), shipmentRequestedTypes, request.getRejectRemarks(), consoleShipmentMappings);
            }
        }
    }


    public ResponseEntity<IRunnerResponse> shipmentRetrieveWithMeasurmentBasis(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            UUID guid = UUID.fromString(request.getGuid());
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(guid);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            MeasurementBasisResponse response = modelMapper.map(shipmentDetails.get(), MeasurementBasisResponse.class);
            calculatePacksAndPacksUnit(shipmentDetails.get().getPackingList(), response);
            calculateContainersAndTeu(response, shipmentDetails.get().getContainersList());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void calculateContainersAndTeu(MeasurementBasisResponse response, List<Containers> containersList) {
        long containerCount = 0;
        Map<String, Long> containerCountMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(containersList)) {
            for(Containers containers : containersList) {
                if(containers.getContainerCount() != null) {
                    containerCount = containerCount + containers.getContainerCount();
                    if(StringUtility.isNotEmpty(containers.getContainerCode())) {
                        containerCountMap.put(containers.getContainerCode(), containerCountMap.getOrDefault(containers.getContainerCode(), 0L) + containers.getContainerCount());
                    }
                }
            }

            response.setTeuCount(masterDataUtils.setContainerTeuDataWithContainers(containersList));
            response.setContainerData(containerCountMap);
            response.setContainerCount(containerCount);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> requestInterBranchConsole(Long shipId, Long consoleId) throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipId);
        for (var consoleShip: consoleShipmentMappings) {
            if (!Objects.equals(consoleShip.getConsolidationId(), consoleId) && Boolean.TRUE.equals(consoleShip.getIsAttachmentDone())) {
                return ResponseHelper.buildFailedResponse("These is already consolidation exist in shipment. Please detach and update shipment first.");
            }
            if (Objects.equals(consoleShip.getConsolidationId(), consoleId)) {
                return ResponseHelper.buildSuccessResponse();
            }
        }
        awbDao.validateAirMessaging(consoleId);
        ConsoleShipmentMapping entity = ConsoleShipmentMapping.builder()
                .shipmentId(shipId)
                .consolidationId(consoleId)
                .isAttachmentDone(false)
                .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                .build();
        consoleShipmentMappingDao.save(entity);
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        sendEmailForPushRequested(shipId, consoleId, shipmentRequestedTypes);
        String warning = null;
        if(!shipmentRequestedTypes.isEmpty()) {
            warning = "Template not found, please inform the region users manually";
        }
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotifications(CommonRequestModel commonRequestModel) {
        PendingNotificationRequest request = (PendingNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingShipmentActionsResponse> response = new PendingNotificationResponse<>();
        if(request.getShipmentIdList() == null || request.getShipmentIdList().isEmpty()) {
            log.info("Received empty request for pending notification in shipments", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(response);
        }
        var notificationMap = getNotificationMap(request);
        response.setNotificationMap(notificationMap);
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<IRunnerResponse> sendEmailForDGApprove(Long shipId) {
       Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipId);
       return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> dgApprovalResponse(OceanDGRequest request) {
        return null;
    }

    private Map<Long, List<PendingShipmentActionsResponse>> getNotificationMap(PendingNotificationRequest request) {
        // Get data of all consolidation pulling this shipment that are not yet attached
        var pullRequestedEnum = ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
        Map<Long, List<PendingShipmentActionsResponse>> notificationResultMap = new HashMap<>();

        if(commonUtils.getCurrentTenantSettings() == null || !Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled())) {
            return notificationResultMap;
        }

        try {
            ListCommonRequest listRequest = constructListCommonRequest("shipmentId", request.getShipmentIdList(), "IN");
            listRequest = andCriteria("requestedType", pullRequestedEnum.name(), "=", listRequest);
            listRequest = andCriteria("isAttachmentDone", false, "=", listRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> consoleShipMappingPair = fetchData(listRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> mappingPage = consoleShipmentMappingDao.findAll(consoleShipMappingPair.getLeft(), consoleShipMappingPair.getRight());

            List<Long> consolidationIds = mappingPage.getContent().stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            final var consoleShipmentsMap = mappingPage.getContent().stream().collect(Collectors.toMap(
                ConsoleShipmentMapping::getConsolidationId, Function.identity(), (oldVal, newVal) -> oldVal)
            );

            commonUtils.setInterBranchContextForColoadStation();

            listRequest = constructListCommonRequest("id", consolidationIds, "IN");
            listRequest.setContainsText(request.getContainsText());
            listRequest.setSortRequest(request.getSortRequest());
            Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listRequest, ConsolidationDetails.class, ConsolidationService.tableNames);
            Page<ConsolidationDetails> consolPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());

            var tenantIdList = new ArrayList<String>();
            var locCodeList =  new ArrayList<String>();
            final CarrierDetails nullCarrierDetails = new CarrierDetails();
            consolPage.getContent().stream().forEach(i -> {
                tenantIdList.add(StringUtility.convertToString(i.getTenantId()));
                var carrierDetails = Optional.ofNullable(i.getCarrierDetails()).orElse(nullCarrierDetails);
                locCodeList.add(carrierDetails.getOriginPort());
                locCodeList.add(carrierDetails.getDestinationPort());
            });
            Map<String, TenantModel> v1TenantData = masterDataUtils.fetchInTenantsList(tenantIdList);
            Map<String, EntityTransferUnLocations> v1LocationData = masterDataUtils.fetchInBulkUnlocations(locCodeList, EntityTransferConstants.LOCATION_SERVICE_GUID);

            masterDataUtils.pushToCache(v1TenantData, CacheConstants.TENANTS);
            masterDataUtils.pushToCache(v1LocationData, CacheConstants.UNLOCATIONS);

            // console id vs list of ship ids
            Map<Long, List<Long>> consolVsShipIdMap = new HashMap<>();

            // generate mapping for shipment id vs list of pulling consol(s)
            for(var mapping : mappingPage.getContent()) {
                if(!notificationResultMap.containsKey(mapping.getShipmentId())) {
                    notificationResultMap.put(mapping.getShipmentId(), new ArrayList<>());
                }
                if(!consolVsShipIdMap.containsKey(mapping.getConsolidationId())) {
                    consolVsShipIdMap.put(mapping.getConsolidationId(), new ArrayList<>());
                }
                consolVsShipIdMap.get(mapping.getConsolidationId()).add(mapping.getShipmentId());
            }

            consolPage.getContent().stream().forEach(i -> {
                var res = mapToNotification(i, consoleShipmentsMap, v1TenantData, v1LocationData);
                consolVsShipIdMap.get(i.getId()).forEach(shipId -> notificationResultMap.get(shipId).add(res));
            });

        }
        catch(Exception e) {
            log.error("Error while generating notification map for input Shipment", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
        }

        return notificationResultMap;
    }

    private PendingShipmentActionsResponse mapToNotification(ConsolidationDetails consol, Map<Long, ConsoleShipmentMapping> consoleShipmentsMap, Map<String, TenantModel> v1TenantData, Map<String, EntityTransferUnLocations> v1LocationData) {
        var carrierDetails = Optional.ofNullable(consol.getCarrierDetails()).orElse(new CarrierDetails());
        var tenantData = Optional.ofNullable(v1TenantData.get(StringUtility.convertToString(consol.getTenantId()))).orElse(new TenantModel());
        return PendingShipmentActionsResponse.builder()
            .consolId(consol.getId())
            .consolidationNumber(consol.getReferenceNumber())
            .masterBill(consol.getMawb())
            .ata(carrierDetails.getAta())
            .atd(carrierDetails.getAtd())
            .eta(carrierDetails.getEta())
            .etd(carrierDetails.getEtd())
            .pol(Optional.ofNullable(v1LocationData.get(carrierDetails.getOriginPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getOriginPort()))
            .pod(Optional.ofNullable(v1LocationData.get(carrierDetails.getDestinationPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getDestinationPort()))
            .lat(consol.getLatDate())
            .branch(tenantData.getCode() + " - " + tenantData.getTenantName())
            .hazardous(consol.getHazardous())
            .requestedBy(consoleShipmentsMap.get(consol.getId()).getCreatedBy())
            .requestedOn(consoleShipmentsMap.get(consol.getId()).getCreatedAt())
            .build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateShipmentSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        CalculateShipmentSummaryRequest request = (CalculateShipmentSummaryRequest) commonRequestModel.getData();

        var response = CalculateShipmentSummaryResponse.builder().build();
        commonUtils.setInterBranchContextForHub();

        if(request.getShipmentIdList() == null || request.getShipmentIdList().isEmpty()) {
            return ResponseHelper.buildSuccessResponse(response);
        }
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(request.getShipmentIdList().stream().collect(Collectors.toSet()));

        double totalWeight = 0;
        double totalVolume = 0;
        double chargeableWeight = 0;
        int totalPacks = 0;
        String packsUnit = null;

        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();

        String transportMode = "";

        for (var ship: shipmentDetailsList) {
            double winDef = UnitConversionUtility.convertUnit(Constants.MASS, ship.getWeight(), ship.getWeightUnit(), toWeightUnit).doubleValue();
            double volDef = UnitConversionUtility.convertUnit(VOLUME, ship.getVolume(), ship.getVolumeUnit(), toVolumeUnit).doubleValue();
            totalWeight = totalWeight + winDef;
            totalVolume = totalVolume + volDef;
            chargeableWeight = chargeableWeight + (ship.getChargable() != null ? ship.getChargable().doubleValue(): 0);

            if(!IsStringNullOrEmpty(ship.getPacksUnit())) {
                if(packsUnit == null)
                    packsUnit = ship.getPacksUnit();
                else if(!packsUnit.equals(ship.getPacksUnit()))
                    packsUnit = MPK;
            }
            if(ship.getNoOfPacks() != null)
                totalPacks = totalPacks + ship.getNoOfPacks();
            transportMode = ship.getTransportMode();
        }
        response.setTotalPacksWithUnit(totalPacks + " " + (packsUnit != null? packsUnit : ""));
        response.setTotalPacksWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalPacksVolume(String.format(Constants.STRING_FORMAT, IReport.ConvertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));

        if(Objects.equals(transportMode, Constants.TRANSPORT_MODE_AIR)) {
            chargeableWeight = CommonUtils.roundOffAirShipment(chargeableWeight);
        }
        chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
        response.setPacksChargeableWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(chargeableWeight), v1TenantSettingsResponse), toWeightUnit));
        return ResponseHelper.buildSuccessResponse(response);
    }

}
