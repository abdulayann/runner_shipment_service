package com.dpw.runner.shipment.services.service.impl;


import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_DETAILS_NULL;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_RETRIEVE_EMPTY_REQUEST;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_DG_SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_CONSOLIDATION_NOT_ALLOWED_MORE_THAN_ONE_SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_FACTOR_FOR_VOL_WT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_DG_CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_FCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_TYPE_DRT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_IMP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_CONTAINER_FIELDS_VALIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ROAD_FACTOR_FOR_VOL_WT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_STD;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_DETACH;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.orCriteria;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.DpsConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateContainerSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ConsoleCalculationsRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ConsoleCalculationsResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ConsolePacksListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ConsolePacksListResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackSummaryDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerShipmentADInConsoleRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerShipmentADInConsoleResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.ConsolidationMapper;
import com.dpw.runner.shipment.services.dto.patchrequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchrequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.AchievedQuantitiesRequest;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.request.AutoAttachConsolidationRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ValidateMawbNumberRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest.ModuleData;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.AutoAttachConsolidationResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.CheckDGShipment;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationExcelExportResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.GenerateCustomHblResponse;
import com.dpw.runner.shipment.services.dto.response.MblCheckResponse;
import com.dpw.runner.shipment.services.dto.response.MeasurementBasisResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ValidateMawbNumberResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.notification.PendingConsolidationActionResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.request.ConsoleBookingIdFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.entity.enums.JobType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.response.consolidation.ConsolidationLiteResponse;
import com.dpw.runner.shipment.services.entity.response.consolidation.IContainerLiteResponse;
import com.dpw.runner.shipment.services.entity.response.consolidation.IShipmentContainerLiteResponse;
import com.dpw.runner.shipment.services.entity.response.consolidation.IShipmentLiteResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCurrency;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ConsolidationDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.impl.CustomConsolidationDetailsRepositoryImpl;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IQuartzJobInfoService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
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
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;
import org.springframework.util.CollectionUtils;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationService implements IConsolidationService {

    @Autowired
    ExecutorService executorService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private CSVParsingUtil<ConsolidationDetails> parser;

    @Autowired
    private CustomConsolidationDetailsRepositoryImpl customConsolidationDetailsRepository;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ILogsHistoryService logsHistoryService;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IPackingService packingService;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IDpsEventService dpsEventService;

    @Autowired
    private UserContext userContext;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentSync shipmentSync;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private BillingServiceAdapter billingServiceAdapter;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IAwbDao awbDao;

    @Autowired
    CacheManager cacheManager;

    @Autowired
    CustomKeyGenerator keyGenerator;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private ConsolidationDetailsMapper consolidationDetailsMapper;

    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IPackingsSync packingsADSync;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    private INetworkTransferService networkTransferService;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private IQuartzJobInfoService quartzJobInfoService;

    @Autowired
    private IQuartzJobInfoDao quartzJobInfoDao;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private ICommonErrorLogsDao commonErrorLogsDao;

    @Autowired
    private INotificationDao notificationDao;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Value("${consolidationsKafka.queue}")
    private String senderQueue;

    @Value("${include.master.data}")
    private Boolean includeMasterData;
    
    private SecureRandom rnd = new SecureRandom();

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
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Long.class).build()),
//            Map.entry("type", RunnerEntityMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("cutoffDate", RunnerEntityMapping.builder().tableName("allocations").dataType(LocalDateTime.class).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).build()),
//            Map.entry("bookingId", RunnerEntityMapping.builder().tableName("BookingCharges").dataType(Long.class).build()),
//            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName("Jobs").dataType(String.class).build()),
            Map.entry(Constants.BOOKING_STATUS_FIELD, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).fieldName(Constants.BOOKING_STATUS_FIELD).build()),
            Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("consolidationType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("containerCategory", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("mawb", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("serviceLevel", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
//            Map.entry("bookingType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("addressCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("originLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originLocCode").build()),
            Map.entry("destinationLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationLocCode").build()),
            Map.entry("originPortLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPortLocCode").build()),
            Map.entry("destinationPortLocCode", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPortLocCode").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("bookingCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("estimatedTerminalCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("hazardousBookingCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build()),
            Map.entry("payment", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).build()),
            Map.entry("reeferCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("shipInstructionCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("terminalCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("verifiedGrossMassCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("bol", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).fieldName("bol").build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).fieldName("houseBill").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("hazardous", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build()),
            Map.entry("shipShipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).fieldName("shipmentType").build()),
            Map.entry("tenantId", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Integer.class).fieldName("tenantId").build()),
            Map.entry(Constants.INTER_BRANCH_CONSOLE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.INTER_BRANCH_CONSOLE).build()),
            Map.entry(Constants.OPEN_FOR_ATTACHMENT, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.OPEN_FOR_ATTACHMENT).build()),
            Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(Constants.CREATED_AT).build()),
            Map.entry(Constants.LAT_DATE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName(Constants.LAT_DATE).build())
            );

    @Override
    public ResponseEntity<IRunnerResponse> fetchConsolidations(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                consolidationDetailsPage.getTotalPages(),
                consolidationDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ConsolidationDetails> lst) {
        return convertEntityListToDtoList(lst, false);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ConsolidationDetails> lst, boolean getMasterData) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ConsolidationListResponse> consolidationListResponses = new ArrayList<>();
        List<Long> consolidationIdList = lst.stream().map(ConsolidationDetails::getId).toList();
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnConsolidation(consolidationIdList, ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.ordinal());
        List<ConsolidationListResponse> listResponses = ConsolidationMapper.INSTANCE.toConsolidationListResponses(
            lst);
        Map<Long, ConsolidationListResponse> responseMap = listResponses.stream()
            .collect(Collectors.toMap(ConsolidationListResponse::getId, response -> response));

        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(consolidationIdList, CONSOLIDATION);
        lst.forEach(consolidationDetails -> {
            ConsolidationListResponse res = responseMap.get(consolidationDetails.getId());
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                res.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            updateHouseBillsShippingIds(consolidationDetails, res);
            containerCountUpdate(consolidationDetails, res, shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer());
            int pendingCount = map.getOrDefault(consolidationDetails.getId(), 0) + notificationMap.getOrDefault(consolidationDetails.getId(), 0);
            res.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            consolidationListResponses.add(res);
        });
        consolidationListResponses.forEach(consolidationDetails -> {
            responseList.add(consolidationDetails);
        });
        this.getMasterDataForList(lst, responseList, getMasterData, true);
        return responseList;
    }

    private List<IRunnerResponse> convertEntityListToDtoListForExport(List<ConsolidationDetails> lst, boolean isShipmentLevelContainer) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ConsolidationListResponse> consolidationListResponses = new ArrayList<>();
        List<ConsolidationExcelExportResponse> listResponses = ConsolidationMapper.INSTANCE.toConsolidationExportListResponses(lst);
        Map<Long, ConsolidationExcelExportResponse> responseMap = listResponses.stream()
            .collect(Collectors.toMap(ConsolidationExcelExportResponse::getId, response -> response));

        lst.forEach(consolidationDetails -> {
            ConsolidationExcelExportResponse res = responseMap.get(consolidationDetails.getId());
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                res.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            updateHouseBillsShippingIds(consolidationDetails, res);
            containerCountUpdate(consolidationDetails, res, isShipmentLevelContainer);

            consolidationListResponses.add(jsonHelper.convertValue(res, ConsolidationListResponse.class));
        });
        consolidationListResponses.forEach(consolidationDetails -> {
            responseList.add(consolidationDetails);
        });
        this.getMasterDataForList(lst, responseList, true, false);
        return responseList;
    }

    private void getMasterDataForList(List<ConsolidationDetails> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData) {
        if(getMasterData || Boolean.TRUE.equals(includeMasterData)) {
            try {
                double _start = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorService);
                var containerTeuData = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setConsolidationContainerTeuData(lst, responseList)), executorService);
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorService);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture =CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorService);
                CompletableFuture.allOf(locationDataFuture, containerTeuData, vesselDataFuture, tenantDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    private void containerCountUpdate(ConsolidationDetails consolidationDetails, ConsolidationListResponse response, boolean isShipmentLevelContainer) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Long containerCount = 0L;
        Set<String> containerNumber = new HashSet<>();
        List<Containers> containersList = new ArrayList<>();
        containersList = consolidationDetails.getContainersList();
        if(isShipmentLevelContainer) {
            List<ShipmentDetails> shipmentDetails = consolidationDetails.getShipmentsList();
            if(shipmentDetails != null && shipmentDetails.size() > 0) {
                containersList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                    if(shipmentDetails1.getContainersList() != null && shipmentDetails1.getContainersList().size() > 0) {
                        containersList.addAll(shipmentDetails1.getContainersList());
                    }
                }
            }
        } else {
            containersList = consolidationDetails.getContainersList();
        }
        if (containersList != null && containersList.size() > 0) {
            for (Containers container : containersList) {
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
                ++containerCount;
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerCount(containerCount);
        response.setContainerNumbers(containerNumber);
    }

    private void containerCountUpdate(ConsolidationDetails consolidationDetails, ConsolidationExcelExportResponse response, boolean isShipmentLevelContainer) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Long containerCount = 0L;
        Set<String> containerNumber = new HashSet<>();
        List<Containers> containersList = new ArrayList<>();
        containersList = consolidationDetails.getContainersList();
        if(isShipmentLevelContainer) {
            List<ShipmentDetails> shipmentDetails = consolidationDetails.getShipmentsList();
            if(shipmentDetails != null && shipmentDetails.size() > 0) {
                containersList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                    if(shipmentDetails1.getContainersList() != null && shipmentDetails1.getContainersList().size() > 0) {
                        containersList.addAll(shipmentDetails1.getContainersList());
                    }
                }
            }
        } else {
            containersList = consolidationDetails.getContainersList();
        }
        if (containersList != null && containersList.size() > 0) {
            for (Containers container : containersList) {
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
                ++containerCount;
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerCount(containerCount);
        response.setContainerNumbers(containerNumber);
    }

    private void updateHouseBillsShippingIds(ConsolidationDetails consol, ConsolidationListResponse consolidationRes) {
        var shipments = consol.getShipmentsList();
        List<String> shipmentIds = null;
        List<String> houseBills = null;
        if (shipments != null)
            shipmentIds = shipments.stream().map(shipment -> shipment.getShipmentId()).toList();
        if (shipmentIds != null)
            houseBills = shipments.stream().map(shipment -> shipment.getHouseBill()).filter(houseBill -> Objects.nonNull(houseBill)).toList();
        consolidationRes.setHouseBills(houseBills);
        consolidationRes.setShipmentIds(shipmentIds);
    }

    private void updateHouseBillsShippingIds(ConsolidationDetails consol, ConsolidationExcelExportResponse consolidationRes) {
        var shipments = consol.getShipmentsList();
        List<String> shipmentIds = null;
        List<String> houseBills = null;
        if (shipments != null)
            shipmentIds = shipments.stream().map(shipment -> shipment.getShipmentId()).toList();
        if (shipmentIds != null)
            houseBills = shipments.stream().map(shipment -> shipment.getHouseBill()).filter(houseBill -> Objects.nonNull(houseBill)).toList();
        consolidationRes.setHouseBills(houseBills);
        consolidationRes.setShipmentIds(shipmentIds);
    }
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {

        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        ConsolidationDetailsResponse response = this.createConsolidation(request, false, false);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private ConsolidationDetailsResponse createConsolidation(ConsolidationDetailsRequest request, boolean includeGuid, boolean isFromET) {
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        syncMainLegRoute(request, null);
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails, Boolean.TRUE.equals(request.getCreatingFromDgShipment()));

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, false, includeGuid, isFromET);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
    }

    @Override
    public ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationDetailsRequest request) {
        return this.createConsolidation(request, true, true);
    }

    @Transactional
    @Override
    public ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel){
        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request is null for Consolidation Create");
        }

        syncMainLegRoute(request, null);
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails, false);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, true, false, false);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel) {

        return ResponseHelper.buildSuccessResponse(this.createConsolidationForBooking(commonRequestModel));
    }

    void getConsolidation(ConsolidationDetails consolidationDetails, boolean creatingFromDgShipment) throws RunnerException{
        generateConsolidationNumber(consolidationDetails);
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, creatingFromDgShipment);

        // audit logs
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(consolidationDetails)
                            .prevData(null)
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
        }
        catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }

    }

    /**
     * Method that generates consol and bol number
     * @param consolidationDetails
     */
    @Override
    public void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException {
        Boolean customisedSequence = shipmentSettingsDao.getCustomisedSequence();

        if(consolidationDetails.getConsolidationNumber() == null) {
            if(Boolean.TRUE.equals(customisedSequence)) {
                String consoleNumber = getCustomizedConsolidationProcessNumber(consolidationDetails, ProductProcessTypes.ReferenceNumber);
                if(consoleNumber != null && !consoleNumber.isEmpty())
                    consolidationDetails.setConsolidationNumber(consoleNumber);
                if (consolidationDetails.getConsolidationNumber() == null || consolidationDetails.getConsolidationNumber().isEmpty())
                    consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
            else {
                consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
        }

        if (StringUtility.isEmpty(consolidationDetails.getBol()) && Objects.equals(commonUtils.getShipmentSettingFromContext().getConsolidationLite(), false)) {
            String bol = getCustomizedConsolidationProcessNumber(consolidationDetails, ProductProcessTypes.BOLNumber);
            if (StringUtility.isEmpty(bol)) {
                bol = generateCustomBolNumber();
            }
            if (StringUtility.isNotEmpty(bol)) {
                consolidationDetails.setBol(bol);
            }
        }
    }

    private String getConsolidationSerialNumber() {
        String maxId = v1Service.getMaxConsolidationId();
//        Long maxId = consolidationDetailsDao.findMaxId();
//        if(maxId == null)
//            maxId = 0L;
//        maxId += 1;
        return maxId;
    }

    private String getCustomizedConsolidationProcessNumber(ConsolidationDetails consolidationDetails, ProductProcessTypes productProcessTypes) throws RunnerException {
        List<TenantProducts> enabledTenantProducts = productEngine.populateEnabledTenantProducts();
        if (productProcessTypes == ProductProcessTypes.ReferenceNumber) {
            // to check the commmon sequence
            var sequenceNumber = productEngine.GetCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
            if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
                return sequenceNumber;
            }
        }
        var identifiedProduct = productEngine.IdentifyProduct(consolidationDetails, enabledTenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessTypes);
        if(sequenceSettings == null){
            return "";
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.getTenantId(), true, null, false);
    }

    public Optional<ConsolidationDetails> retrieveByIdOrGuid(ConsolidationDetailsRequest request) throws RunnerException {
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ConsolidationDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=consolidationDetailsDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= consolidationDetailsDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Consolidation Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
//        if (request.getId() == null) {
//            log.error("Request Id is null for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//        }
            // TODO- implement Validation logic

            Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(request);
            if (!oldEntity.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ConsolidationDetails entity = jsonHelper.convertValue(request, ConsolidationDetails.class);
            entity.setId(oldEntity.get().getId());
            if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
                throw new RunnerException("Provided GUID doesn't match with the existing one !");
            }
            if (entity.getContainersList() == null)
                entity.setContainersList(oldEntity.get().getContainersList());
            beforeSave(entity, oldEntity.get(), false);
            entity = consolidationDetailsDao.update(entity, false);
            syncConsole(entity, false);
            pushShipmentDataToDependentService(entity, false, oldEntity.get().getContainersList());
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class));
        }
        catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
    }

    private void syncConsole(ConsolidationDetails entity, boolean isDirectSync) {
        try {
            consolidationSync.sync(entity, StringUtility.convertToString(entity.getGuid()), isDirectSync);
        } catch (Exception e) {
            log.error("Error performing sync on consol entity, {}", e);
        }
    }

    public void sendEmailForPullRequested(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
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
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long shipmentId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipmentId), consolidationDetails, SHIPMENT_PULL_REQUESTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, null, null);
            } catch (Exception e) {
                log.error("Error while sending email");
            }
        }
    }

    public void sendEmailForDetachShipments(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetails,
                                            Set<ShipmentRequestedType> shipmentRequestedTypes, String remarks) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        List<Long> interbranchShipIds = new ArrayList<>();
        for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
            if(!Objects.equals(shipmentDetails1.getTenantId(), consolidationDetails.getTenantId()))
                interbranchShipIds.add(shipmentDetails1.getId());
        }
        usernamesList.add(consolidationDetails.getCreatedBy());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long shipmentId : interbranchShipIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipmentId), consolidationDetails, SHIPMENT_DETACH, remarks, emailTemplatesRequests, shipmentRequestedTypes, null, null, usernameEmailsMap, v1TenantSettingsMap, null, null);
            } catch (Exception e) {
                log.error("Error while sending email");
            }
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> attachShipments(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<Long> shipmentIds, boolean fromConsolidation) throws RunnerException {
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if(consol.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        ConsolidationDetails consolidationDetails = consol.get();
        HashSet<Long> attachedShipmentIds = new HashSet<>();
        // InterBranch context
        ListCommonRequest shiplistCommonRequest = constructListCommonRequest("id", shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> shipPair = fetchData(shiplistCommonRequest, ShipmentDetails.class);
        if(shipmentRequestedType == null) {
            setInterBranchContext(consolidationDetails.getInterBranchConsole());
        }
        if (!Objects.isNull(consolidationId))
            awbDao.validateAirMessaging(consolidationId);
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
        boolean isConsolePullCall = false;
        if(shipmentRequestedType == null && !shipmentDetailsList.isEmpty() && DIRECTION_IMP.equalsIgnoreCase(shipmentDetailsList.get(0).getDirection())) {
            shipmentRequestedType = APPROVE;
            isConsolePullCall = true;
        }

        // Filter and collect inter-branch shipment details into a separate list
        List<ShipmentDetails> interBranchShipmentDetailsList = shipmentDetailsList.stream()
                .filter(c -> !Objects.equals(c.getTenantId(), consolidationDetails.getTenantId())) // Filter inter-branch shipments
                .toList();

        Map<Long, ShipmentDetails> interBranchImportShipmentMap = interBranchShipmentDetailsList.stream()
                .filter(shipment -> DIRECTION_IMP.equalsIgnoreCase(shipment.getDirection()))
                .collect(Collectors.toMap(
                        ShipmentDetails::getId,   // Key: ID of the shipment
                        shipment -> shipment      // Value: ShipmentDetails object itself
                ));

        Set<Long> interBranchRequestedShipIds = new HashSet<>();
        Set<Long> interBranchApprovedShipIds = new HashSet<>();
        List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = new ArrayList<>(); // auto rejection emails sent when same branch console is accepted
        if(shipmentRequestedType == null) {
            interBranchRequestedShipIds = shipmentDetailsList.stream()
                    .filter(c -> !Objects.equals(c.getTenantId(), UserContext.getUser().TenantId))
                    .map(ShipmentDetails::getId).collect(Collectors.toSet());
            var newShipmentIds = new ArrayList<>(shipmentIds);
            newShipmentIds.removeAll(interBranchRequestedShipIds);
            if (!newShipmentIds.isEmpty()) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, newShipmentIds, "IN", null);
                listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);
                consoleShipmentMappingDao.deletePendingStateByShipmentIds(newShipmentIds);
            }
        } else if(!isConsolePullCall) {
            interBranchApprovedShipIds = shipmentDetailsList.stream()
                    .filter(c -> !Objects.equals(c.getTenantId(), consolidationDetails.getTenantId()))
                    .map(ShipmentDetails::getId).collect(Collectors.toSet());
        }

        if(consolidationId != null && shipmentIds != null && !shipmentIds.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent();

            validationsBeforeAttachShipments(consolidationDetails, consoleShipmentMappings, shipmentIds, consolidationId, shipmentDetailsList, fromConsolidation);

            attachedShipmentIds = consoleShipmentMappingDao.assignShipments(shipmentRequestedType, consolidationId, shipmentIds, consoleShipmentMappings, interBranchRequestedShipIds, interBranchApprovedShipIds, interBranchImportShipmentMap);
            Integer agentCount = 0;
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                agentCount++;
            }
            if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
                agentCount++;
            }
            Set<Parties> originParties = new HashSet<>();
            Set<Parties> destinationParties = new HashSet<>();
            for(ShipmentDetails shipmentDetails : shipmentDetailsList) {
                if(attachedShipmentIds.contains(shipmentDetails.getId()) && !interBranchRequestedShipIds.contains(shipmentDetails.getId())) {
                    if (shipmentDetails.getContainersList() != null) {
                        List<Containers> containersList = shipmentDetails.getContainersList();
                        for (Containers container : containersList) {
                            container.setConsolidationId(consolidationId);
                        }
                        containersList = containerDao.saveAll(containersList);
                        containerService.afterSaveList(containersList, false);
                    }
                    if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getPackingList() != null) {
                        List<Packing> packingList = shipmentDetails.getPackingList();
                        for (Packing packing : packingList) {
                            packing.setConsolidationId(consolidationId);
                        }
                        packingList = packingDao.saveAll(packingList);
                    }
                    if(shipmentDetails.getEventsList() != null) {
                        List<Events> eventsList = shipmentDetails.getEventsList();
                        for(Events event : eventsList) {
                            if (eventDao.shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                                event.setConsolidationId(consolidationId);
                            }
                        }
                        eventDao.saveAll(eventsList);
                    }
                    if (!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole()) && agentCount < 2) {
                        if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                            originParties.add(shipmentDetails.getAdditionalDetails().getExportBroker());
                        }
                        if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                            destinationParties.add(shipmentDetails.getAdditionalDetails().getImportBroker());
                        }
                    }
                    this.createLogHistoryForShipment(shipmentDetails);
                }
            }
            if (!CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent()) && originParties.size() == 1) {
                Parties originAgent = originParties.iterator().next();
                consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(originAgent));
            }
            if (!CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent()) && destinationParties.size() == 1) {
                Parties destinationAgent = destinationParties.iterator().next();
                consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(destinationAgent));
            }
            this.checkSciForAttachConsole(consolidationId);
            // detaching the shipmentDetailsList but still able to access raw entity data, fetch any lazy load data beforehand if need to be used
            shipmentDao.entityDetach(shipmentDetailsList);
            updateLinkedShipmentData(consolidationDetails, null, true, new HashMap<>());
            processInterConsoleAttachShipment(consolidationDetails, shipmentDetailsList);
            // Update pack utilisation if user accepts any pull or push request
            if(ShipmentRequestedType.APPROVE.equals(shipmentRequestedType)) {
                packingService.savePackUtilisationCalculationInConsole(CalculatePackUtilizationRequest.builder()
                    .consolidationId(consolidationId)
                    .shipmentIdList(shipmentIds)
                    .build()
                );
            }
        }
        if(checkForNonDGConsoleAndAirDGFlag(consolidationDetails) || checkForOceanNonDGConsolidation(consolidationDetails)) {
            List<ShipmentDetails> shipments = shipmentDetailsList.stream().filter(x -> Boolean.TRUE.equals(x.getContainsHazardous())).toList();
            if(shipments != null && !shipments.isEmpty()) {
                consolidationDetails.setHazardous(true);
                if(!checkConsolidationTypeValidation(consolidationDetails))
                {
                    throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
                }
                consolidationDetailsDao.update(consolidationDetails, false, true);
            }
        }
        interBranchRequestedShipIds.retainAll(attachedShipmentIds);
        if(!interBranchImportShipmentMap.isEmpty() && isConsolePullCall) {
            for(ShipmentDetails shipmentDetails: interBranchImportShipmentMap.values()) {
                var emailTemplatesRequestsModel = commonUtils.getEmailTemplates(IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL);
                if(Objects.isNull(emailTemplatesRequestsModel) || emailTemplatesRequestsModel.isEmpty())
                    shipmentRequestedTypes.add(APPROVE);
                if(shipmentRequestedTypes.isEmpty())
                    sendImportShipmentPullAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequestsModel);
            }
        }

        if(!interBranchRequestedShipIds.isEmpty()) // send email for pull requested when called from controller directly
            sendEmailForPullRequested(consolidationDetails, interBranchRequestedShipIds.stream().toList(), shipmentRequestedTypes);
        if(!consoleShipmentMappingsForEmails.isEmpty()) { // send email for pull/push rejected for other consolidations when called from controller directly
            List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(e -> e.getConsolidationId()).toList();
            List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
            commonUtils.sendRejectionEmailsExplicitly(shipmentDetailsList, consoleShipmentMappingsForEmails, shipmentRequestedTypes, otherConsolidationDetails);
        }
        try {
            consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), false);
        }
        catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        String warning = null;
        if(!shipmentRequestedTypes.isEmpty()) {
            warning = "Template not found, please inform the region users manually";
        }
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    private ResponseEntity<IRunnerResponse> sendImportShipmentPullAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, List<EmailTemplatesRequest> emailTemplatesRequestsModel) {

        var emailTemplateModel = emailTemplatesRequestsModel.stream().findFirst().orElse(new EmailTemplatesRequest());
        List<String> toEmailsList = new ArrayList<>();
        List<String> ccEmailsList = new ArrayList<>();
        if(shipmentDetails.getCreatedBy() != null)
            toEmailsList.add(shipmentDetails.getCreatedBy());
        if(shipmentDetails.getAssignedTo() != null)
            toEmailsList.add(shipmentDetails.getAssignedTo());

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.add(shipmentDetails.getTenantId());

        Map<String, Object> dictionary = new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), executorService);

        CompletableFuture.allOf(carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture).join();
        commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, consolidationDetails.getTenantId(), false);
        ccEmailsList.addAll(new ArrayList<>(toEmailIds));
        ccEmailsList.addAll(new ArrayList<>(ccEmailIds));
        if(shipmentDetails.getCreatedBy() == null || shipmentDetails.getAssignedTo() == null) {
            toEmailIds.clear();
            ccEmailIds.clear();
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, shipmentDetails.getTenantId(), true);
            toEmailsList.addAll(new ArrayList<>(toEmailIds));
        }

        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, toEmailsList, ccEmailsList);

        return ResponseHelper.buildSuccessResponse();
    }

    public void validationsBeforeAttachShipments(ConsolidationDetails consolidationDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                  List<Long> shipmentIds, Long consolidationId, List<ShipmentDetails> shipmentDetailsList, boolean fromConsolidation) throws RunnerException {
        int existingShipments = consolidationDetails.getShipmentsList() != null ? consolidationDetails.getShipmentsList().size() : 0;
        if(Boolean.TRUE.equals(consolidationDetails.getHazardous()) && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) &&  existingShipments + shipmentIds.size() > 1) {
            throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, we can have only 1 shipment");
        }
        if(!listIsNullOrEmpty(consoleShipmentMappings)) {
            for (ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
                if(!consoleShipmentMapping.getConsolidationId().equals(consolidationId) && Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone()))
                    throw new RunnerException("Multiple consolidations are attached to the shipment, please verify.");
            }
        }

        boolean anyInterBranchShipment = false;
        for(ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if(shipmentDetails.getCargoDeliveryDate() != null && consolidationDetails.getLatDate() != null && consolidationDetails.getLatDate().isAfter(shipmentDetails.getCargoDeliveryDate())) {
                throw new RunnerException("Shipment " + shipmentDetails.getShipmentId() +" Cargo Delivery Date is lesser than LAT Date.");
            }
            if(checkForAirDGFlag(consolidationDetails) && !Objects.equals(consolidationDetails.getTenantId(), shipmentDetails.getTenantId())) {
                anyInterBranchShipment = true;
                if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                    if(fromConsolidation)
                        throw new RunnerException(String.format(AIR_CONSOLIDATION_NOT_ALLOWED_WITH_INTER_BRANCH_DG_SHIPMENT, shipmentDetails.getShipmentId()));
                    else
                        throw new RunnerException(String.format(AIR_DG_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_CONSOLIDATION, consolidationDetails.getConsolidationNumber()));

                }
            }
        }

        if(checkForAirDGFlag(consolidationDetails) && Boolean.TRUE.equals(consolidationDetails.getHazardous())) {
            if(existingShipments + shipmentIds.size() > 1) {
                if(fromConsolidation)
                    throw new RunnerException(AIR_DG_CONSOLIDATION_NOT_ALLOWED_MORE_THAN_ONE_SHIPMENT);
                else
                    throw new RunnerException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails.getConsolidationNumber()));
            }
            if(anyInterBranchShipment)
                throw new RunnerException(String.format(AIR_SHIPMENT_NOT_ALLOWED_WITH_INTER_BRANCH_DG_CONSOLIDATION, consolidationDetails.getConsolidationNumber()));
        }

        // Iterate through the list of shipment details to perform a DPS Implication check.
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            // Check if the specific implication (CONCR) is already present for the current shipment's GUID.
            // If true, throw a RunnerException with a detailed error message including the shipment ID.
            if (Boolean.TRUE.equals(dpsEventService.isImplicationPresent(Set.of(shipmentDetails.getGuid().toString()), DpsConstants.CONCR))) {
                throw new RunnerException(DpsConstants.DPS_ERROR_2 + " : " + shipmentDetails.getShipmentId());
            }
        }

    }

    private void setInterBranchContext(Boolean isInterBranchConsole) {
        if (Boolean.TRUE.equals(isInterBranchConsole))
            commonUtils.setInterBranchContextForHub();
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    private boolean checkForNonDGConsoleAndAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!checkForAirDGFlag(consolidationDetails))
            return false;
        return !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    private boolean checkForOceanNonDGConsolidation(ConsolidationDetails consolidationDetails)
    {
        return Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> detachShipments(ConsolidationDetails consol, List<Long> shipmentIds) throws RunnerException {
        return detachShipmentsHelper(consol, shipmentIds, null);
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> detachShipments(Long consolidationId, List<Long> shipmentIds, String remarks) throws RunnerException {
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if(consol.isPresent())
            return detachShipmentsHelper(consol.get(), shipmentIds, remarks);
        return ResponseHelper.buildFailedResponse("Consol is null");
    }

    private void processInterConsoleDetachShipment(ConsolidationDetails console, List<Long> shipmentIds){
        try {
            boolean isInterBranchConsole = console.getInterBranchConsole();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            boolean isNetworkTransferEntityEnabled = Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
            if(!isInterBranchConsole || !isNetworkTransferEntityEnabled ||(shipmentIds==null || shipmentIds.isEmpty()))
                return;
            Set<Long> shipmentIdsSet = new HashSet<>(shipmentIds);
            List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(shipmentIdsSet);
            for(ShipmentDetails shipmentDetails: shipmentDetailsList) {
                if(shipmentDetails.getReceivingBranch()!=null)
                    networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(), shipmentDetails.getId(), SHIPMENT);
            }
        } catch(Exception e){
            log.error("Error in detach shipment process: ", e.getMessage());
        }
    }

    public ResponseEntity<IRunnerResponse> detachShipmentsHelper(ConsolidationDetails consol, List<Long> shipmentIds, String remarks) throws RunnerException {
        List<ShipmentDetails> shipmentDetails = Optional.ofNullable(shipmentIds)
                .filter(ObjectUtils::isNotEmpty).map(ids -> shipmentDao.findShipmentsByIds(ids.stream().collect(Collectors.toSet())))
                .orElse(Collections.emptyList());
        validateShipmentDetachment(shipmentDetails);

        if (Boolean.TRUE.equals(consol.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
        }

        boolean saveSeaPacks = false;
        Long consolidationId = consol.getId();
        List<Packing> packingList = new ArrayList<>();
        List<ShipmentDetails> shipmentDetailsToSave = new ArrayList<>();
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
            for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
                if(Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails1.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails1.getContainsHazardous()) &&
                        (OceanDGStatus.OCEAN_DG_REQUESTED.equals(shipmentDetails1.getOceanDGStatus()) || OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED.equals(shipmentDetails1.getOceanDGStatus())))
                    throw new RunnerException("Shipment " + shipmentDetails1.getShipmentId() + " is in " + shipmentDetails1.getOceanDGStatus() + " state, first get the required approval");
            }
            List<Containers> allContainersList = new ArrayList<>();
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetail = shipmentDetailsMap.get(shipId);
                if(shipmentDetail.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetail.getContainersList();
                    Map<Long, List<Packing>> containerPacksMap = new HashMap<>();
                    if(Constants.TRANSPORT_MODE_SEA.equals(shipmentDetail.getTransportMode()) && !listIsNullOrEmpty(shipmentDetail.getPackingList())) {
                        for(Packing packing: shipmentDetail.getPackingList()) {
                            if(packing.getContainerId() != null) {
                                if(containerPacksMap.containsKey(packing.getContainerId()))
                                    containerPacksMap.get(packing.getContainerId()).add(packing);
                                else
                                    containerPacksMap.put(packing.getContainerId(), new ArrayList<>(Collections.singletonList(packing)));
                                packing.setContainerId(null);
                                packingList.add(packing);
                                saveSeaPacks = true;
                            }
                        }
                    }
                    for(Containers container : containersList) {
                        if(Constants.TRANSPORT_MODE_SEA.equals(shipmentDetail.getTransportMode())) {
                            if(CARGO_TYPE_FCL.equals(shipmentDetail.getShipmentType())) {
                                containerService.changeContainerWtVolForSeaFCLDetach(container);
                            } else {
                                if(containerPacksMap.containsKey(container.getId())) {
                                    List<Packing> packs = containerPacksMap.get(container.getId());
                                    for(Packing packing : packs) {
                                        containerService.changeContainerWtVolForSeaLCLDetach(container, packing);
                                    }
                                }
                            }
                        }
                    }
                    allContainersList.addAll(containersList);
                }
                shipmentsContainersMappingDao.detachListShipments(allContainersList.stream().map(Containers::getId).toList(), removedShipmentIds, false);
                containerDao.saveAll(allContainersList);
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerService.afterSaveList(allContainersList, false)), executorService);

                if (shipmentDetail.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetail.getPackingList() != null) {
                    packingList = shipmentDetail.getPackingList();
                    for (Packing packing : packingList) {
                        packing.setConsolidationId(null);
                    }
                    packingList = packingDao.saveAll(packingList);
                }
                if(shipmentDetail.getEventsList() != null) {
                    var eventsList = shipmentDetail.getEventsList();
                    for(Events event : eventsList) {
                        event.setConsolidationId(null);
                    }
                    eventDao.saveAll(eventsList);
                }
                shipmentDetail.setConsolRef(null);
                shipmentDetail.setMasterBill(null);
                this.createLogHistoryForShipment(shipmentDetail);
            }
            if(saveSeaPacks)
                packingList = packingDao.saveAll(packingList);
            shipmentDetailsToSave = shipmentDetailsMap.values().stream().toList();
            shipmentDao.saveAll(shipmentDetailsToSave);
        }
        if(checkAttachDgAirShipments(consol)){
            consol.setHazardous(false);
            consolidationDetailsDao.save(consol, false);
        }
        if(Objects.equals(consol.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            this.checkSciForDetachConsole(consolidationId);
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        if(remarks != null)
            sendEmailForDetachShipments(consol, shipmentDetails, shipmentRequestedTypes, remarks);
        String transactionId = consol.getGuid().toString();
        if(packingList != null) {
            try {
                packingsADSync.sync(packingList, transactionId);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_SYNCING_PACKS);
            }
        }
        try {
            consolidationSync.sync(consol, transactionId, false);
        } catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        syncShipmentsList(shipmentDetailsToSave, transactionId);
        String warning = null;
        if(!shipmentRequestedTypes.isEmpty()) {
            warning = "Mail Template not found, please inform the region users individually";
        }
        processInterConsoleDetachShipment(consol, shipmentIds);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    private void validateShipmentDetachment(List<ShipmentDetails> shipmentDetails) {
        validateOutstandingDuesForShipments(shipmentDetails);
    }

    private void validateOutstandingDuesForShipments(List<ShipmentDetails> shipmentDetails) {
        try {
            // Retrieve tenant settings
            V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();

            // Exit if shipment details are empty or if split billing is not enabled
            if (ObjectUtils.isEmpty(shipmentDetails) || !Boolean.TRUE.equals(tenantSettings.getEnableConsolSplitBillCharge())) {
                return;
            }

            // Filter non-empty shipments with transport mode "AIR" or "SEA"
            List<ShipmentDetails> eligibleShipments = shipmentDetails.stream()
                    .filter(ObjectUtils::isNotEmpty)
                    .filter(shp -> Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(shp.getTransportMode())
                            || Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shp.getTransportMode()))
                    .toList();

            // Exit if no eligible shipments found
            if (eligibleShipments.isEmpty()) {
                log.info("No eligible shipments found for AIR or SEA transport mode.");
                return;
            }

            // Map shipments by their GUIDs for lookup, handle potential null GUIDs
            Map<String, ShipmentDetails> shipmentMapByGuid = eligibleShipments.stream()
                    .filter(shp -> shp.getGuid() != null)
                    .collect(toMap(ship -> ship.getGuid().toString(), Function.identity()));

            // Prepare request for billing due summary
            BillingBulkSummaryBranchWiseRequest branchWiseRequest = createBillingBulkSummaryBranchWiseRequest(eligibleShipments);

            // Fetch billing summaries, handle potential null response from billing service
            List<BillingDueSummary> billingDueSummaries = billingServiceAdapter.fetchBillingDueSummary(branchWiseRequest);
            if (billingDueSummaries == null) {
                throw new BillingException("Billing service returned null for billing due summaries.");
            }

            // Filter summaries with outstanding dues, check for null entries
            List<BillingDueSummary> summariesWithOutstandingDues = billingDueSummaries.stream()
                    .filter(Objects::nonNull)
                    .filter(summary -> Boolean.TRUE.equals(summary.getDueRemaining()))
                    .toList();

            // If there are shipments with outstanding dues, throw an exception
            if (ObjectUtils.isNotEmpty(summariesWithOutstandingDues)) {
                List<String> shipmentIdsWithDues = summariesWithOutstandingDues.stream()
                        .map(summary -> {
                            ShipmentDetails shipment = shipmentMapByGuid.get(summary.getModuleGuid());
                            return (shipment != null) ? shipment.getShipmentId() : null;
                        }).filter(Objects::nonNull).toList();

                if (!shipmentIdsWithDues.isEmpty()) {
                    throw new BillingException("The following House(s) " + String.join(", ", shipmentIdsWithDues)
                            + " cannot be detached as there are still apportioned costs associated with them.");
                }
            }
        }  catch (Exception e) {
            throw new BillingException(e.getMessage(), e);
        }
    }

    private BillingBulkSummaryBranchWiseRequest createBillingBulkSummaryBranchWiseRequest(List<ShipmentDetails> shipmentDetails) {
        BillingBulkSummaryBranchWiseRequest branchWiseRequest = new BillingBulkSummaryBranchWiseRequest();
        branchWiseRequest.setModuleType(Constants.SHIPMENT);

        branchWiseRequest.setModuleData(
                shipmentDetails.stream()
                        .map(shp -> ModuleData.builder()
                                .branchId(shp.getTenantId().toString())
                                .moduleGuid(shp.getGuid().toString()).build())
                        .toList()
        );
        return branchWiseRequest;
    }

    @Override
    public void checkSciForDetachConsole(Long consoleId) throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        List<Long> shipIdList = consoleShipmentMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
        List<Awb> mawbs = awbDao.findByConsolidationId(consoleId);
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consoleId);
        if (consol.isPresent() && mawbs != null && !mawbs.isEmpty()) {
            Awb mawb = mawbs.get(0);
            if (mawb.getAwbCargoInfo() != null && !Objects.equals(mawb.getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) && Objects.equals(mawb.getAwbCargoInfo().getSci(),
                    AwbConstants.T1)) {
                if (!shipIdList.isEmpty()) {
                    List<Awb> awbs = awbDao.findByShipmentIdList(shipIdList);
                    if (awbs != null && !awbs.isEmpty()) {
                        var isShipmentSciT1 = awbs.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1)).findAny();
                        if (isShipmentSciT1.isEmpty()) {
                            mawb.setAirMessageResubmitted(false);
                            mawb.getAwbCargoInfo().setSci(null);
                            awbDao.save(mawb);
                            consol.get().setSci(null);
                            consolidationDetailsDao.save(consol.get(), false);
                        }
                    }
                } else {
                    mawb.getAwbCargoInfo().setSci(null);
                    mawb.setAirMessageResubmitted(false);
                    awbDao.save(mawb);
                    consol.get().setSci(null);
                    consolidationDetailsDao.save(consol.get(), false);
                }
            }
        }
    }

    @Override
    public void checkSciForAttachConsole(Long consoleId) throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        List<Long> shipIdList = consoleShipmentMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
        List<Awb> mawbs = awbDao.findByConsolidationId(consoleId);
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consoleId);
        if(consol.isPresent() && mawbs != null && !mawbs.isEmpty() && !shipIdList.isEmpty()){
            Awb mawb = mawbs.get(0);
            if(mawb.getAwbCargoInfo() != null && !Objects.equals(mawb.getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) && !Objects.equals(mawb.getAwbCargoInfo().getSci(), AwbConstants.T1)){
                List<Awb> awbs = awbDao.findByShipmentIdList(shipIdList);
                if(awbs != null && !awbs.isEmpty()) {
                    var isShipmentSciT1 = awbs.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1)).findAny();
                    if (!isShipmentSciT1.isEmpty()) {
                        mawb.getAwbCargoInfo().setSci(AwbConstants.T1);
                        mawb.setAirMessageResubmitted(false);
                        awbDao.save(mawb);
                        consol.get().setSci(AwbConstants.T1);
                        consolidationDetailsDao.save(consol.get(), false);
                    }
                }
            }
        }
    }

    private boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails){
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return false;
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        if(consolidationDetails.getShipmentsList() == null || consolidationDetails.getShipmentsList().isEmpty())
            return true;
        Boolean isDgShipmentAttached = consolidationDetails.getShipmentsList().stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
        return !isDgShipmentAttached;
    }

    @Transactional
    public ResponseEntity<HttpStatus> detachShipmentsReverseSync(Long consolidationId, List<Long> shipmentIds) {
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipId), true);
                    }
                    containerDao.saveAll(containersList);
                }
            }
        }

        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException {
        ConsolidationPatchRequest consolidationDetailsRequest = (ConsolidationPatchRequest) commonRequestModel.getData();
        // Get old entity to be updated
        ConsolidationDetailsRequest req = new ConsolidationDetailsRequest();
        req.setId(consolidationDetailsRequest.getId());
        req.setGuid(consolidationDetailsRequest.getGuid());
        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(req);
        try {
            ConsolidationDetails entity = oldEntity.get();
            long id = oldEntity.get().getId();
            consolidationDetailsMapper.update(consolidationDetailsRequest, entity);

            setInterBranchContext(entity.getInterBranchConsole());

            // Carrier Details Update
            CarrierPatchRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldEntity.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
            }
            entity.setCarrierDetails(oldEntity.get().getCarrierDetails());

            beforeSave(entity, oldEntity.get(), false);
            entity = consolidationDetailsDao.update(entity, false);

            List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
            List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
            List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
            List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
            List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
            List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
            List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
            List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
            List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

            if(containerRequestList != null) {
                List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class), id, (Long) null, true);
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION);
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (truckDriverDetailsRequestList != null) {
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromConsole(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class), id);
//                entity.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddressRequest != null) {
                List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES);
                entity.setConsolidationAddresses(updatedFileRepos);
            }
            if(fromV1 == null || !fromV1) {
                try {
                    consolidationSync.sync(entity, StringUtility.convertToString(entity.getGuid()), false);
                } catch (Exception e){
                    log.error("Error performing sync on consolidation entity, {}", e);
                }
            }
            pushShipmentDataToDependentService(entity, false, oldEntity.get().getContainersList());

            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
            response.setPackSummary(packingService.calculatePackSummary(entity.getPackingList(), entity.getTransportMode(), entity.getContainerCategory(), new ShipmentMeasurementDetailsDto()));
            return ResponseHelper.buildSuccessResponse(response);

        } catch (Exception e){
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        ConsolidationDetailsResponse response = this.completeUpdateConsolidation(consolidationDetailsRequest, false);

        return ResponseHelper.buildSuccessResponse(response);
    }

    private ConsolidationDetailsResponse completeUpdateConsolidation(ConsolidationDetailsRequest consolidationDetailsRequest, boolean isFromET) throws RunnerException {
        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(consolidationDetailsRequest);
        if (!oldEntity.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, consolidationDetailsRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        consolidationDetailsRequest.setShipmentsList(null);

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            syncMainLegRoute(consolidationDetailsRequest, oldEntity.get());
            ConsolidationDetails entity = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            setInterBranchContext(entity.getInterBranchConsole());
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            ConsolidationDetails oldConvertedConsolidation = jsonHelper.convertValue(oldEntity.get(), ConsolidationDetails.class);

            beforeSave(entity, oldEntity.get(), false);

            entity = consolidationDetailsDao.update(entity, false);
            try {
                // audit logs
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(entity)
                                .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                                .parent(ConsolidationDetails.class.getSimpleName())
                                .parentId(entity.getId())
                                .operation(DBOperationType.UPDATE.name()).build()
                );
            }
            catch (Exception e) {
                log.error("Error writing audit service log", e);
            }

            afterSave(entity, oldConvertedConsolidation, consolidationDetailsRequest, false, shipmentSettingsDetails, false, false, isFromET);
            ConsolidationDetails finalEntity1 = entity;
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(finalEntity1)), executorService);
            ConsolidationDetails finalEntity = entity;
            return jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RuntimeException(e.getMessage());
        }
    }

    @Override
    public ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException {
        return this.completeUpdateConsolidation(consolidationDetailsRequest, true);
    }

    /**
     * update data to all the shipments linked to console
     * @param console
     * @param oldEntity
     */
    public List<ShipmentDetails> updateLinkedShipmentData(ConsolidationDetails console, ConsolidationDetails oldEntity, Boolean fromAttachShipment,
            Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        List<ShipmentDetails> shipments = null;
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(console.getEfreightStatus(), Constants.EAW)){
            shipments = getShipmentsList(console.getId());
            var shipmentlist = shipments.stream().filter(x-> Objects.equals(x.getAdditionalDetails().getEfreightStatus(), Constants.NON)).toList();
            if(shipmentlist != null && !shipmentlist.isEmpty()){
                throw new RunnerException("EFreight status can only be EAP or NON as one of the Shipment has EFreight status as NON");
            }
        }
        if(console != null && (oldEntity == null ||  !Objects.equals(console.getBol(),oldEntity.getBol()) ||
                !Objects.equals(console.getShipmentType(),oldEntity.getShipmentType()) ||
                !CollectionUtils.isEmpty(console.getRoutingsList()) ||
                !Objects.equals(console.getCarrierBookingRef(),oldEntity.getCarrierBookingRef()) ||
                (console.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                (!Objects.equals(console.getCarrierDetails().getVoyage(),oldEntity.getCarrierDetails().getVoyage()) ||
                        !Objects.equals(console.getCarrierDetails().getVessel(),oldEntity.getCarrierDetails().getVessel()) ||
                        !Objects.equals(console.getCarrierDetails().getShippingLine(),oldEntity.getCarrierDetails().getShippingLine()) ||
                        !Objects.equals(console.getCarrierDetails().getAircraftType(),oldEntity.getCarrierDetails().getAircraftType()) ||
                        !Objects.equals(console.getCarrierDetails().getCfs(), oldEntity.getCarrierDetails().getCfs()) ||
                        !Objects.equals(console.getReceivingBranch(), oldEntity.getReceivingBranch()) ||
                        !Objects.equals(console.getTriangulationPartner(), oldEntity.getTriangulationPartner()) ||
                        !Set.copyOf(Optional.ofNullable(console.getTriangulationPartnerList()).orElse(List.of())
                                        .stream().filter(Objects::nonNull).map(TriangulationPartner::getTriangulationPartner).toList())
                                .equals(Set.copyOf(Optional.ofNullable(oldEntity.getTriangulationPartnerList()).orElse(List.of())
                                        .stream().filter(Objects::nonNull).map(TriangulationPartner::getTriangulationPartner).toList())) ||
                        !Objects.equals(console.getDocumentationPartner(), oldEntity.getDocumentationPartner()) ||
                        !Objects.equals(console.getCarrierDetails().getFlightNumber(), oldEntity.getCarrierDetails().getFlightNumber()) ||
                        !Objects.equals(console.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) ||
                        !Objects.equals(console.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort()) ||
                        !Objects.equals(console.getCarrierDetails().getEtd(), oldEntity.getCarrierDetails().getEtd()) ||
                        !Objects.equals(console.getCarrierDetails().getEta(), oldEntity.getCarrierDetails().getEta()) ||
                        !Objects.equals(console.getCarrierDetails().getAtd(), oldEntity.getCarrierDetails().getAtd()) ||
                        !Objects.equals(console.getCarrierDetails().getAta(), oldEntity.getCarrierDetails().getAta())
                )) || !dgStatusChangeInShipments.isEmpty() ||
                !CommonUtils.checkSameParties(console.getSendingAgent(), oldEntity.getSendingAgent()) ||
                !CommonUtils.checkSameParties(console.getReceivingAgent(), oldEntity.getReceivingAgent()))) {
            if(shipments == null)
                shipments = getShipmentsList(console.getId());
            for(ShipmentDetails i: shipments) {
                i.setConsolRef(console.getReferenceNumber());
                i.setMasterBill(console.getBol());
                i.setDirection(console.getShipmentType());
                i.setBookingNumber(console.getCarrierBookingRef());
                if (Boolean.TRUE.equals(console.getInterBranchConsole())) {
                    i.setTriangulationPartnerList(console.getTriangulationPartnerList() != null ? new ArrayList<>(console.getTriangulationPartnerList()) : null);
                    i.setTriangulationPartner(console.getTriangulationPartner());
                    i.setDocumentationPartner(console.getDocumentationPartner());
                    if (!Boolean.TRUE.equals(i.getIsReceivingBranchAdded()))
                        i.setReceivingBranch(console.getReceivingBranch());
                }
                if (console.getCarrierDetails() != null) {
                    i.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
                    i.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
                    i.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
                    i.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
                    i.getCarrierDetails().setCfs(console.getCarrierDetails().getCfs());
                    if(fromAttachShipment != null && fromAttachShipment){
                        i.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
                        i.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
                        i.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                    }
                    if(Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                        i.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                        i.getCarrierDetails().setOriginPort(console.getCarrierDetails().getOriginPort());
                        i.getCarrierDetails().setDestinationPort(console.getCarrierDetails().getDestinationPort());
                        i.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
                        i.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
                        i.getCarrierDetails().setAtd(console.getCarrierDetails().getAtd());
                        i.getCarrierDetails().setAta(console.getCarrierDetails().getAta());
                    }
                }
                if(dgStatusChangeInShipments.containsKey(i.getId())) {
                    i.setContainsHazardous(dgStatusChangeInShipments.get(i.getId()).getContainsHazardous());
                    i.setOceanDGStatus(dgStatusChangeInShipments.get(i.getId()).getOceanDGStatus());
                }
                if(checkConsolidationEligibleForCFSValidation(console) &&
                        checkIfShipmentDateGreaterThanConsole(i.getShipmentGateInDate(), console.getCfsCutOffDate()))
                    throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
                syncMainCarriageRoutingToShipment(console.getRoutingsList(), i, true);
                if(!Boolean.TRUE.equals(console.getInterBranchConsole())) {
                    if (i.getAdditionalDetails() != null && !CommonUtils.checkSameParties(console.getSendingAgent(), i.getAdditionalDetails().getExportBroker())) {
                        i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
                    } else if (i.getAdditionalDetails() == null) {
                        i.setAdditionalDetails(new AdditionalDetails());
                        i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
                    }

                    if (i.getAdditionalDetails() != null && !CommonUtils.checkSameParties(console.getReceivingAgent(), i.getAdditionalDetails().getImportBroker())) {
                        i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
                    } else if (i.getAdditionalDetails() == null) {
                        i.setAdditionalDetails(new AdditionalDetails());
                        i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
                    }
                }
            }

            shipmentDao.saveAll(shipments);
            if(fromAttachShipment != null && fromAttachShipment) {
                syncShipmentsList(shipments, StringUtility.convertToString(console.getGuid()));
            }
        }
        return shipments;
    }

    private void processInterConsoleAttachShipment(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        try {
            if (!isValidRequest(console, shipments)) return;

            List<Long> shipmentIds = getShipmentIds(shipments);

            Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap = getShipmentNetworkTransferMap(shipmentIds);

            Long consoleReceivingBranch = console.getReceivingBranch();
            List<Long> networkTransferListToDelete = new ArrayList<>();

            for (ShipmentDetails shipment : shipments) {
                processNTEConsoleShipment(consoleReceivingBranch, shipment, shipmentNetworkTransferMap, networkTransferListToDelete);
            }

            if (!networkTransferListToDelete.isEmpty()) {
                networkTransferDao.deleteByIdsAndLog(networkTransferListToDelete);
            }
        } catch (Exception e) {
            log.error("Error in attach shipment process: ", e.getMessage());
        }
    }

    private boolean isValidRequest(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return Boolean.TRUE.equals(console.getInterBranchConsole()) && shipments != null && !shipments.isEmpty() && Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
    }

    private List<Long> getShipmentIds(List<ShipmentDetails> shipments) {
        return shipments.stream().map(ShipmentDetails::getId).filter(Objects::nonNull).toList();
    }

    private Map<Long, Map<Integer, NetworkTransfer>> getShipmentNetworkTransferMap(List<Long> shipmentIds) {
        return networkTransferDao.getInterConsoleNTList(shipmentIds, Constants.SHIPMENT).stream()
                .collect(Collectors.groupingBy(
                        NetworkTransfer::getEntityId,
                        Collectors.toMap(NetworkTransfer::getTenantId, transfer -> transfer)
                ));
    }

    private void processNTEConsoleShipment(Long consoleReceivingBranch, ShipmentDetails shipment,
                                           Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap,
                                           List<Long> networkTransferListToDelete) {
        Long receivingBranch = shipment.getReceivingBranch();
        if (receivingBranch == null) return;

        Map<Integer, NetworkTransfer> tenantMap = shipmentNetworkTransferMap.get(shipment.getId());
        NetworkTransfer networkTransfer = tenantMap != null ? tenantMap.get(receivingBranch.intValue()) : null;

        if (Objects.equals(receivingBranch, consoleReceivingBranch)) {
            if (networkTransfer != null && networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED) {
                networkTransferListToDelete.add(networkTransfer.getId());
            }
        } else if (networkTransfer == null) {
            networkTransferService.processNetworkTransferEntity(
                    receivingBranch, null, Constants.SHIPMENT,
                    shipment, null, Constants.DIRECTION_IMP, null, true);
        }
    }

    @Override
    public void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes) throws RunnerException {
        if(CollectionUtils.isEmpty(consolidationRoutings) || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()))
            return;
        List<Routings> shipmentMainCarriageRouting = new ArrayList<>();
        List<Routings> shipmentRoutingList = Optional.ofNullable(shipmentDetails.getRoutingsList()).orElse(new ArrayList<>());
        shipmentDetails.setRoutingsList(shipmentRoutingList);

        // sync consolidation routings to linked shipment
        consolidationRoutings.stream()
                .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage()))
                .forEach(consolRoute -> {
                    // Look for this POL POD main carriage routing in shipment routings list
                    // update/create
                    var syncedRoute = jsonHelper.convertCreateValue(consolRoute, Routings.class);
                    syncedRoute.setConsolidationId(null);
                    syncedRoute.setShipmentId(shipmentDetails.getId());
                    syncedRoute.setBookingId(null);
                    shipmentMainCarriageRouting.add(syncedRoute);
                });


        List<Routings> existingMainCarriageRoutings = shipmentDetails.getRoutingsList().stream().filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage())).toList();
        int count = 0;
        if(existingMainCarriageRoutings != null)
        {
            for(var routing: shipmentMainCarriageRouting)
            {
                if(count < existingMainCarriageRoutings.size()) {
                    routing.setId(existingMainCarriageRoutings.get(count).getId());
                    routing.setGuid(existingMainCarriageRoutings.get(count).getGuid());
                    count++;
                }
            }
        }

        // Logic to regroup all shipment routings with updated leg sequence
        // Assumption -> order of routes is as follows; Otherwise legs will have a chaotic order for user
        // 1. PRE_CARRIAGE
        // 2. MAIN_CARRIAGE
        // 3. ON_CARRIAGE
        AtomicLong legCount = new AtomicLong(1);
        List<Routings> finalShipmentRouteList = new ArrayList<>();
        List<Routings> preCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream().filter(i -> RoutingCarriage.PRE_CARRIAGE.equals(i.getCarriage())).toList();
        List<Routings> onCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream().filter(i -> RoutingCarriage.ON_CARRIAGE.equals(i.getCarriage())).toList();

        // Merge routings list
        mergeRoutingList(preCarriageShipmentRoutes, finalShipmentRouteList, legCount);
        mergeRoutingList(shipmentMainCarriageRouting, finalShipmentRouteList, legCount);
        mergeRoutingList(onCarriageShipmentRoutes, finalShipmentRouteList, legCount);

        if(saveRoutes)
            routingsDao.updateEntityFromShipment(finalShipmentRouteList, shipmentDetails.getId());

        // Assign routing list to shipment routing
        shipmentDetails.setRoutingsList(finalShipmentRouteList);
    }

    /**
     * Merges carriage routes back into shipment routing list
     * @param carriageRoute input routing list
     * @param shipmentRoutingsList shipment routing list to be merged into
     * @param legCount
     */
    private void mergeRoutingList(List<Routings> carriageRoute, List<Routings> shipmentRoutingsList, AtomicLong legCount) {
        if(carriageRoute.isEmpty())
            return;

        carriageRoute.forEach(i -> {
            i.setLeg(legCount.get());
            legCount.incrementAndGet();
            shipmentRoutingsList.add(i);
        });
    }

    private void syncShipmentsList(List<ShipmentDetails> shipments, String transactionId) {
        for (ShipmentDetails shipmentDetails : shipments) {
            try {
                shipmentSync.sync(shipmentDetails, null, null, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }
    }

    private List<ShipmentDetails> getShipmentsList(Long consoleId) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        List<Long> shipmentIdList = consoleShipmentMappings.stream().map(ConsoleShipmentMapping :: getShipmentId).toList();
        ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);
        Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        return new ArrayList<>(page.getContent());
    }

    private ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
        String responseMsg;
        try {
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if(consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                if(Objects.equals(weight, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setWeightUtilization( String.valueOf( (consolidatedWeight.divide(weight, 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if(Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization( String.valueOf( (consolidatedVolume.divide(volume, 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            return consolidationDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateUtilization(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            consolidationDetails = calculateConsolUtilization(consolidationDetails);
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ConsolidationDetails getConsoleForCalculations(ConsoleCalculationsRequest request) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getId()).orElse(new ConsolidationDetails());
        consolidationDetails.setAllocations(jsonHelper.convertValue(request.getAllocations(), Allocations.class));
        consolidationDetails.setAchievedQuantities(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class));
        consolidationDetails.setTransportMode(request.getTransportMode());
        consolidationDetails.setContainerCategory(request.getContainerCategory());
        return consolidationDetails;
    }

    private ConsoleCalculationsResponse getConsoleCalculationsResponse(ConsolidationDetails consolidationDetails) {
        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if(consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != consolidationDetails.getAllocations().getWeightUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), consolidationDetails.getAllocations().getWeightUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedWeight(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(consolidationDetails.getAllocations().getWeightUnit());
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != consolidationDetails.getAllocations().getVolumeUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), consolidationDetails.getAllocations().getVolumeUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedVolume(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(consolidationDetails.getAllocations().getVolumeUnit());
            }
            consolidationDetails = calculateConsolUtilization(consolidationDetails);
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateChargeable(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            String transportMode = consolidationDetails.getTransportMode();
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            BigDecimal weight = consolidationDetails.getAllocations().getWeight();
            String weightUnit = consolidationDetails.getAllocations().getWeightUnit();
            BigDecimal volume = consolidationDetails.getAllocations().getVolume();
            String volumeUnit = consolidationDetails.getAllocations().getVolumeUnit();
            if (weightUnit != null && volumeUnit != null) {
                VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
                consolidationDetails.getAllocations().setChargable(vwOb.getChargeable());
                if (transportMode == Constants.TRANSPORT_MODE_AIR) {
                    BigDecimal charge = consolidationDetails.getAllocations().getChargable();
                    BigDecimal half = new BigDecimal("0.50");
                    BigDecimal floor = charge.setScale(0, BigDecimal.ROUND_FLOOR);
                    if (charge.subtract(half).compareTo(floor) <= 0 && charge.compareTo(floor) != 0) {
                        charge = floor.add(half);
                    } else {
                        charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
                    }
                    consolidationDetails.getAllocations().setChargable(charge);
                }
                if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    volume = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    consolidationDetails.getAllocations().setChargable(weight.divide(new BigDecimal("1000")).max(volume));
                    vwOb = calculateVolumeWeight(transportMode, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, weight, volume);
                }
                consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
            }
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void setDescGoodsAndhandlingInfoMap(Map<Long, String> descOfGoodsMap, Map<Long, String> handlingInfoMap, Map<Long, String> hblNumberMap, Long containerId, String goodsDesc, String handlingInfo, String hblNumber) {
        if(!IsStringNullOrEmpty(goodsDesc)) {
            if(descOfGoodsMap.containsKey(containerId))
                descOfGoodsMap.put(containerId, descOfGoodsMap.get(containerId) + "\n" + goodsDesc);
            else
                descOfGoodsMap.put(containerId, goodsDesc);
        }
        if(!IsStringNullOrEmpty(handlingInfo)) {
            if(handlingInfoMap.containsKey(containerId))
                handlingInfoMap.put(containerId, handlingInfoMap.get(containerId) + "\n" + handlingInfo);
            else
                handlingInfoMap.put(containerId, handlingInfo);
        }
        if(!IsStringNullOrEmpty(hblNumber)) {
            if(hblNumberMap.containsKey(containerId))
                hblNumberMap.put(containerId, hblNumberMap.get(containerId) + ", " + hblNumber);
            else
                hblNumberMap.put(containerId, hblNumber);
        }
    }

    private void calculateDescOfGoodsAndHandlingInfo(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, boolean isAutoUpdate) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        Map<Long, String> descOfGoodsMap = new HashMap<>();
        Map<Long, String> handlingInfoMap = new HashMap<>();
        Map<Long, String> hblNumberMap = new HashMap<>();
        Map<Long, ShipmentDetails> shipmentNumberMap = new HashMap<>();
        boolean lcl = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean autoUpdate = isAutoUpdate || (consolidationDetails.getAutoUpdateGoodsDesc() != null && consolidationDetails.getAutoUpdateGoodsDesc());
        Set<Long> containerSelfDataAdded = new HashSet<>();
        if(consolidationDetails.getShipmentsList() != null && consolidationDetails.getShipmentsList().size() > 0) {
            for(ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                shipmentNumberMap.put(shipmentDetails.getId(), shipmentDetails);
                boolean setContData = autoUpdate;
                if(autoUpdate && lcl) {
                    if(shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate()) {
                        if(shipmentDetails.getPackingList() != null && shipmentDetails.getPackingList().size() > 0) {
                            for (Packing packing : shipmentDetails.getPackingList()) {
                                if(packing.getContainerId() != null) {
                                    setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, packing.getContainerId(), packing.getGoodsDescription(), packing.getHandlingInfo(), null);
                                }
                            }
                        }
                        setContData = false;
                    }
                }
                if(setContData || !IsStringNullOrEmpty(shipmentDetails.getHouseBill())) {
                    if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
                        for(Containers containers : shipmentDetails.getContainersList()) {
                            if(setContData && !containerSelfDataAdded.contains(containers.getId()) && lcl) {
                                containerSelfDataAdded.add(containers.getId());
                                setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, containers.getId(), containers.getDescriptionOfGoods(), containers.getHandlingInfo(), shipmentDetails.getHouseBill());
                            }
                            else
                                setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, containers.getId(), null, null, shipmentDetails.getHouseBill());
                        }
                    }
                }

            }
        }

        if(consolidationDetailsResponse.getContainersList() != null && consolidationDetailsResponse.getContainersList().size() > 0) {
            for (ContainerResponse containerResponse : consolidationDetailsResponse.getContainersList()) {
                Map<String, String> tempMap = new HashMap<>();
                if(descOfGoodsMap.containsKey(containerResponse.getId()))
                    tempMap.put("descriptionOfGoods", descOfGoodsMap.get(containerResponse.getId()));
                else
                    tempMap.put("descriptionOfGoods", containerResponse.getDescriptionOfGoods());
                if(handlingInfoMap.containsKey(containerResponse.getId()))
                    tempMap.put("handlingInfo", handlingInfoMap.get(containerResponse.getId()));
                else
                    tempMap.put("handlingInfo", containerResponse.getHandlingInfo());
                if(hblNumberMap.containsKey(containerResponse.getId()))
                    containerResponse.setHblNumber(hblNumberMap.get(containerResponse.getId()));
                containerResponse.setTextFieldData(tempMap);
            }
        }

        if(!isAutoUpdate && consolidationDetailsResponse.getPackingList() != null && !consolidationDetailsResponse.getPackingList().isEmpty()) {
            for (PackingResponse packingResponse : consolidationDetailsResponse.getPackingList()) {
                if(packingResponse.getShipmentId() != null && shipmentNumberMap.containsKey(packingResponse.getShipmentId())) {
                    packingResponse.setShipmentNumber(shipmentNumberMap.get(packingResponse.getShipmentId()).getShipmentId());
                    packingResponse.setShipmentHazardous(shipmentNumberMap.get(packingResponse.getShipmentId()).getContainsHazardous());
                }
            }
        }
    }

    public VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException {
        String responseMsg;
        try {
            VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
            if (!weightUnit.isEmpty() && !volumeUnit.isEmpty() && !transportMode.isEmpty()) {
                switch (transportMode) {
                    case Constants.TRANSPORT_MODE_SEA:
                    case Constants.TRANSPORT_MODE_RAI:
                    case Constants.TRANSPORT_MODE_FSA:
                        BigDecimal volInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        vwOb.setChargeable(volInM3.multiply(BigDecimal.valueOf(10)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(10)));
                        vwOb.setChargeableUnit(Constants.VOLUME_UNIT_M3);

                        BigDecimal wtInTn = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        wtInTn = wtInTn.divide(BigDecimal.valueOf(1000));
                        BigDecimal wv = new BigDecimal(convertUnit(Constants.VOLUME, wtInTn, Constants.VOLUME_UNIT_M3, volumeUnit).toString());
                        vwOb.setVolumeWeight(wv);
                        vwOb.setVolumeWeightUnit(volumeUnit);
                        break;
                    case Constants.TRANSPORT_MODE_AIR:
                    case Constants.TRANSPORT_MODE_FAS:
                    case Constants.TRANSPORT_MODE_ROA:
                        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        BigDecimal vlInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        BigDecimal factor = BigDecimal.valueOf(AIR_FACTOR_FOR_VOL_WT);
                        if (transportMode.equals(Constants.TRANSPORT_MODE_ROA)) {
                            factor = BigDecimal.valueOf(ROAD_FACTOR_FOR_VOL_WT);
                        }
                        BigDecimal wvInKG = vlInM3.multiply(factor);
                        if (wtInKG.compareTo(wvInKG) < 0) {
                            wtInKG = wvInKG;
                        }
                        vwOb.setChargeable(wtInKG.multiply(BigDecimal.valueOf(100)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(100)));
                        vwOb.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
                        BigDecimal WV = new BigDecimal(convertUnit(Constants.MASS, wvInKG, Constants.WEIGHT_UNIT_KG, weightUnit).toString());
                        vwOb.setVolumeWeight(WV);
                        vwOb.setVolumeWeightUnit(weightUnit);
                        break;
                    default:
                }
            }
            return vwOb;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchievedValues(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentGridChangeResponse response = new ShipmentGridChangeResponse();
            Long consolidationId = commonRequestModel.getId();
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolidationId).get();
            calculateAchievedValues(consolidationDetails, response, consolidationDetails.getShipmentsList());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, List<ShipmentDetails> shipmentDetailsList) throws Exception {
        // Not to process the achieved values in case of AIR transport mode
        if(Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(consolidationDetails.getTransportMode())) {
            return;
        }
        if (consolidationDetails.getOverride() != null && consolidationDetails.getOverride()) {
            return;
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        String weightChargeableUnit = Constants.WEIGHT_UNIT_KG;
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            weightChargeableUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        String volumeChargeableUnit = Constants.VOLUME_UNIT_M3;
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            volumeChargeableUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        BigDecimal sumWeight = new BigDecimal(0);
        BigDecimal sumVolume = new BigDecimal(0);
        if (shipmentDetailsList != null && !shipmentDetailsList.isEmpty()) {
            for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                sumWeight = sumWeight.add(new BigDecimal(convertUnit(Constants.MASS, shipmentDetails.getWeight(), shipmentDetails.getWeightUnit(), weightChargeableUnit).toString()));
                sumVolume = sumVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit(), volumeChargeableUnit).toString()));
            }
            response.setSummaryShipmentsCount(shipmentDetailsList.size());
        }
        else {
            response.setSummaryShipmentsCount(0);
        }
        Double consoleTeu = 0.0;
        Double shipmentTeu = 0.0;
        long consoleCont = 0l;
        long shipmentCont = 0l;
        if(consolidationDetails.getContainersList() != null && consolidationDetails.getContainersList().size() > 0) {
            Map<String, Object> cacheMap = new HashMap<String, Object>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(consolidationDetails.getContainersList(), ContainerResponse.class);
            if (!Objects.isNull(containerResponseList))
                containerResponseList.forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));
            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            for(Containers containers : consolidationDetails.getContainersList()) {
                Object cache = null;
                if(cacheMap.isEmpty()) {
                    var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
                    if(!Objects.isNull(resp)) cache = resp.get();
                } else {
                    cache = cacheMap.get(containers.getContainerCode());
                }
                EntityTransferContainerType object = (EntityTransferContainerType) cache;
                if(containers.getContainerCount() != null) {
                    consoleCont = consoleCont + containers.getContainerCount();
                    if(containers.getShipmentsList() != null && containers.getShipmentsList().size() > 0) {
                        shipmentCont = shipmentCont + containers.getContainerCount();
                    }
                    if(object != null && object.getTeu() != null) {
                        consoleTeu = consoleTeu + (containers.getContainerCount() * object.getTeu());
                        if(containers.getShipmentsList() != null && containers.getShipmentsList().size() > 0) {
                            shipmentTeu = shipmentTeu + (containers.getContainerCount() * object.getTeu());
                        }
                    }
                }
            }
        }
        response.setSummaryConsoleTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse));
        response.setSummaryConsolContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));

        if(consolidationDetails.getAchievedQuantities() == null)
            consolidationDetails.setAchievedQuantities(new AchievedQuantities());
        consolidationDetails.getAchievedQuantities().setConsolidatedWeight(sumWeight);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(weightChargeableUnit);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolume(sumVolume);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(volumeChargeableUnit);

        consolidationDetails = calculateConsolUtilization(consolidationDetails);

        String transportMode = consolidationDetails.getTransportMode();
        if(consolidationDetails.getAllocations() == null)
            consolidationDetails.setAllocations(new Allocations());
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, sumWeight, sumVolume);

        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());
        if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                consolidationDetails.getContainerCategory() != null && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)) {
            BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
            BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
        }
        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());

        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        response.setSummaryWeight(IReport.ConvertToWeightNumberFormat(sumWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.ConvertToVolumeNumberFormat(sumVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);
        if(!IsStringNullOrEmpty(consolidationDetails.getContainerCategory()) && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)
                && !IsStringNullOrEmpty(consolidationDetails.getTransportMode()) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            double volInM3 = convertUnit(Constants.VOLUME, sumVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, sumWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    public ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            List<Containers> containers = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
            ContainerSummaryResponse response = containerService.calculateContainerSummary(containers, request.getTransportMode(), request.getContainerCategory());
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
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getContainerCategory(), new ShipmentMeasurementDetailsDto());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> calculatePackUtilisation(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculatePackUtilizationRequest request = (CalculatePackUtilizationRequest) commonRequestModel.getData();
        try {
            commonUtils.setInterBranchContextForHub();
            if(Boolean.FALSE.equals(request.getIsHub())) {
                commonUtils.setInterBranchContextForColoadStation();
            }
            PackSummaryResponse packSummaryResponse = packingService.calculatePacksUtilisationForConsolidation(request);
            CalculatePackUtilizationResponse response = jsonHelper.convertValue(packSummaryResponse, CalculatePackUtilizationResponse.class);
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
    public ResponseEntity<IRunnerResponse> listPacksForAssignDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolePacksListRequest request = (ConsolePacksListRequest) commonRequestModel.getData();
            ConsolePacksListResponse response = new ConsolePacksListResponse();
            response.setPacksList(new ArrayList<>());
            int size = 1;
            response.setIsFCL(false);
            if(request.getIsAssign()) {
                List<Long> shipIds = new ArrayList<>();
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getConsolidationId());
                if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                    shipIds = consoleShipmentMappings.stream().map(e -> e.getShipmentId()).toList();
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipIds, "IN", null);
                listCommonRequest = andCriteria(Constants.CONTAINER_ID, "", "ISNULL", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());

                List<Long> contShipIds = new ArrayList<>();
                List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerId(request.getContainerId());
                if(shipmentsContainersMappingList != null && shipmentsContainersMappingList.size() > 0) {
                    contShipIds = shipmentsContainersMappingList.stream().map(e -> e.getShipmentId()).toList();
                }

                Map<Long, ShipmentDetails> map = new HashMap<>();
                List<Long> shipmentsIncluded = new ArrayList<>();
                if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
                    shipmentsIncluded = packings.getContent().stream().map(e -> e.getShipmentId()).toList();
                    for (Packing packing : packings) {
                        ConsolePacksListResponse.PacksList packsList = jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, packing.getShipmentId());
                        if(shipmentDetails != null) {
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                        }
                        response.getPacksList().add(packsList);
                        size++;
                    }
                }
                for(Long shipmentId: shipIds) {
                    if(shipmentsIncluded.indexOf(shipmentId) < 0 && contShipIds.indexOf(shipmentId) < 0) {
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, shipmentId);
                        if(shipmentDetails != null && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                            ConsolePacksListResponse.PacksList packsList = new ConsolePacksListResponse.PacksList();
                            packsList.setShipmentId(shipmentDetails.getId());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setId(size * -1L);
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            response.getPacksList().add(packsList);
                            size++;
                        }
                    }
                }
            }
            else {
                List<Long> shipIds = new ArrayList<>();
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getConsolidationId());
                if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                    shipIds = consoleShipmentMappings.stream().map(e -> e.getShipmentId()).toList();
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipIds, "IN", null);
                listCommonRequest = andCriteria(Constants.CONTAINER_ID, request.getContainerId(), "=", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());

                List<Long> contShipIds = new ArrayList<>();
                List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerId(request.getContainerId());
                if(shipmentsContainersMappingList != null && shipmentsContainersMappingList.size() > 0) {
                    contShipIds = shipmentsContainersMappingList.stream().map(e -> e.getShipmentId()).toList();
                }
                Map<Long, ShipmentDetails> map = new HashMap<>();

                if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
                    for (Packing packing : packings) {
                        ConsolePacksListResponse.PacksList packsList = jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, packing.getShipmentId());
                        if(shipmentDetails != null) {
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                        }
                        response.getPacksList().add(packsList);
                        size++;
                    }
                }

                if(contShipIds != null && contShipIds.size() == 1 && (packings == null || packings.getContent() == null || packings.getContent().isEmpty())) {
                    for(Long shipmentId: shipIds) {
                        if(shipmentId.equals(contShipIds.get(0))) {
                            ShipmentDetails shipmentDetails = getShipmentFromMap(map, shipmentId);
                            if(shipmentDetails != null && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                                response.setIsFCL(true);
                                ConsolePacksListResponse.PacksList packsList = new ConsolePacksListResponse.PacksList();
                                packsList.setShipmentId(shipmentDetails.getId());
                                packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                                packsList.setId(size * -1L);
                                packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                                packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                                packsList.setShipmentType(shipmentDetails.getShipmentType());
                                packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                                response.getPacksList().add(packsList);
                                size++;
                            }
                        }
                    }
                }
            }
            try {
                var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllMasterDataInSingleCallPacksList(response)), executorService);
                var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllCommodityTypesInSingleCallPacksList(response)), executorService);
                CompletableFuture.allOf(masterListFuture, commodityTypesFuture).join();
            }
            catch (Exception e) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, e.getLocalizedMessage());
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCallPacksList (ConsolePacksListResponse consolePacksListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();
            if(!Objects.isNull(consolePacksListResponse.getPacksList()))
                consolePacksListResponse.getPacksList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ConsolePacksListResponse.PacksList.class, fieldNameKeyMap, ConsolePacksListResponse.PacksList.class.getSimpleName(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(!Objects.isNull(consolePacksListResponse.getPacksList()))
                consolePacksListResponse.getPacksList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolePacksListResponse.PacksList.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCallPacksList(ConsolePacksListResponse consolePacksListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            if(!Objects.isNull(consolePacksListResponse.getPacksList()))
                consolePacksListResponse.getPacksList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, ConsolePacksListResponse.PacksList.class, fieldNameKeyMap, ConsolePacksListResponse.PacksList.class.getSimpleName(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if(!Objects.isNull(consolePacksListResponse.getPacksList()))
                consolePacksListResponse.getPacksList().forEach(r -> r.setCommodityMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolePacksListResponse.PacksList.class.getSimpleName()), CacheConstants.COMMODITY, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignPacksAndShipments(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerShipmentADInConsoleRequest request = (ContainerShipmentADInConsoleRequest) commonRequestModel.getData();
            ContainerShipmentADInConsoleResponse response = new ContainerShipmentADInConsoleResponse();
            Optional<Containers> containersOpt = containerDao.findById(request.getContainer().getId());
            Containers container = null;
            if(containersOpt.isPresent()) {
                container = containersOpt.get();
            }
            if(container != null && request.getPacksList() != null && request.getPacksList().size() > 0) {
                BigDecimal weight = container.getAchievedWeight();
                BigDecimal volume = container.getAchievedVolume();
                if(weight == null) {
                    weight = BigDecimal.ZERO;
                    container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                }
                if(volume == null) {
                    volume = BigDecimal.ZERO;
                    container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                }
                List<Long> contShipIds = new ArrayList<>();
                if(container.getShipmentsList() != null && container.getShipmentsList().size() > 0)
                    contShipIds = container.getShipmentsList().stream().map(e -> e.getId()).toList();
                Set<Long> shipmentsIncluded = new HashSet<>();
                boolean isFCL = false;
                boolean isFCLAlready = contShipIds.size() == 1 && container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL);
                for(ContainerShipmentADInConsoleRequest.PacksList pack : request.getPacksList()) {
                    if(!shipmentsIncluded.contains(pack.getShipmentId()) && contShipIds.indexOf(pack.getShipmentId()) < 0)
                        shipmentsIncluded.add(pack.getShipmentId());
                    if(pack.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        isFCL = true;
                        if( (isFCL || isFCLAlready) && contShipIds.size() + shipmentsIncluded.size() > 1 ) {
                            throw new ValidationException("Mentioned container is already assigned or being assigned to a FCL Shipment - " + pack.getShipmentNumber() + " along with some other Shipment. Please check and retry!");
                        }
                    }
                    if(pack.getWeight() != null && !IsStringNullOrEmpty(pack.getWeightUnit()) && !IsStringNullOrEmpty(container.getAchievedWeightUnit()))
                        weight = weight.add(new BigDecimal(convertUnit(Constants.MASS, pack.getWeight(), pack.getWeightUnit(), container.getAchievedWeightUnit()).toString()));
                    if(pack.getVolume() != null && !IsStringNullOrEmpty(pack.getVolumeUnit()) && !IsStringNullOrEmpty(container.getAchievedVolumeUnit()))
                        volume = volume.add(new BigDecimal(convertUnit(Constants.VOLUME, pack.getVolume(), pack.getVolumeUnit(), container.getAchievedVolumeUnit()).toString()));
                    if(Boolean.TRUE.equals(pack.getHazardous())) {
                        container.setHazardous(true);
                        if(IsStringNullOrEmpty(container.getDgClass()) || IsStringNullOrEmpty(container.getUnNumber()) || IsStringNullOrEmpty(container.getProperShippingName()))
                            throw new ValidationException(OCEAN_DG_CONTAINER_FIELDS_VALIDATION);
                    }
                }
                if( (isFCL || isFCLAlready) && contShipIds.size() + shipmentsIncluded.size() > 1 ) {
                    if(container.getShipmentsList().size() > 0 && container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        throw new ValidationException("Mentioned container is already assigned or being assigned to a FCL Shipment - " + container.getShipmentsList().get(0).getShipmentId() + " along with some other Shipment. Please check and retry!");
                    }
                }
                if(isFCL || isFCLAlready) {
                    weight = container.getAllocatedWeight();
                    volume = container.getAllocatedVolume();
                }
                ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
                if(shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator()
                && ((weight != null && weight.compareTo(container.getAllocatedWeight()) > 0) || (volume != null && volume.compareTo(container.getAllocatedVolume()) > 0))) {
                    if(request.getIsConfirmedByUser() != null && request.getIsConfirmedByUser().booleanValue()) {
                        container = attachContainer(request, shipmentsIncluded, container, weight, volume);
                    }
                    else {
                        response.setRequireConfirmationFromUser(true);
                        return ResponseHelper.buildSuccessResponse(response);
                    }
                }
                else {
                    container = attachContainer(request, shipmentsIncluded, container, weight, volume);
                }
                makeShipmentsDG(container, shipmentsIncluded);
            }
            else {
                response.setContainer(jsonHelper.convertValue(request.getContainer(), ContainerResponse.class));
            }
            response.setRequireConfirmationFromUser(false);
            response.setContainer(jsonHelper.convertValue(container, ContainerResponse.class));
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void makeShipmentsDG(Containers container, Set<Long> shipIds) throws RunnerException {
        if(Boolean.TRUE.equals(container.getHazardous())) {
            ListCommonRequest listCommonRequest = andCriteria(Constants.CONTAINS_HAZARDOUS, false, "=", null);
            listCommonRequest = andCriteria(Constants.ID, shipIds, "IN", listCommonRequest);
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            List<ShipmentDetails> shipmentDetails = shipmentDetailsPage.getContent();
            if(shipmentDetails.isEmpty())
                return;
            shipmentDetails.forEach(e -> {
                e.setContainsHazardous(true);
                commonUtils.changeShipmentDGStatusToReqd(e, commonUtils.checkIfDGClass1(container.getDgClass()));
            });
            shipmentDetails = shipmentDao.saveAll(shipmentDetails);
            for (ShipmentDetails shipmentDetails1 : shipmentDetails) {
                try {
                    shipmentSync.sync(shipmentDetails1, null, null, StringUtility.convertToString(UUID.randomUUID()), false);
                } catch (Exception e) {
                    log.error("Error performing sync on shipment entity, {}", e);
                }
            }
        }
    }

    private Containers attachContainer(ContainerShipmentADInConsoleRequest request, Set<Long> newShipmentsIncluded, Containers container, BigDecimal weight, BigDecimal volume) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getPacksList().stream().filter(e -> e.getId() != null && e.getId() > 0).map(e -> e.getId()).toList(), "IN");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
        List<Packing> packingList = null;
        if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
            packingList = packings.getContent();
            for(Packing pack: packingList) {
                pack.setContainerId(container.getId());
            }
            packingDao.saveAll(packingList);
        }
        container.setAchievedWeight(weight);
        container.setAchievedVolume(volume);
        container = containerService.calculateUtilization(container);
        containerDao.save(container);
        shipmentsContainersMappingDao.assignShipments(container.getId(), newShipmentsIncluded.stream().toList(), false);
        try {
            packingsADSync.sync(packingList, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error(SyncingConstants.ERROR_SYNCING_PACKS);
        }
        return container;
    }

    @Override
    public ResponseEntity<IRunnerResponse> detachPacksAndShipments(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerShipmentADInConsoleRequest request = (ContainerShipmentADInConsoleRequest) commonRequestModel.getData();
            ContainerResponse response = null;
            Optional<Containers> containersOpt = containerDao.findById(request.getContainer().getId());
            Containers container = null;
            if(containersOpt.isPresent()) {
                container = containersOpt.get();
            }
            if(container != null && request.getPacksList() != null && request.getPacksList().size() > 0) {
                BigDecimal weight = container.getAchievedWeight();
                BigDecimal volume = container.getAchievedVolume();
                if(weight == null) {
                    weight = BigDecimal.ZERO;
                    container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                }
                if(volume == null) {
                    volume = BigDecimal.ZERO;
                    container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                }
                Set<Long> shipmentdIdSet = new HashSet<>();
                List<Packing> packingList = new ArrayList<>();
                for (ContainerShipmentADInConsoleRequest.PacksList packing : request.getPacksList()) {
                    shipmentdIdSet.add(packing.getShipmentId());
                    if(packing.getId() > 0) {
                        Packing pack = null;
                        pack = packingDao.findById(packing.getId()).get();
                        if(!packing.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                            if(pack.getWeight() != null && !IsStringNullOrEmpty(pack.getWeightUnit()) && !IsStringNullOrEmpty(container.getAchievedWeightUnit()))
                                weight = weight.subtract(new BigDecimal(convertUnit(Constants.MASS, pack.getWeight(), pack.getWeightUnit(), container.getAchievedWeightUnit()).toString()));
                            if(pack.getVolume() != null && !IsStringNullOrEmpty(pack.getVolumeUnit()) && !IsStringNullOrEmpty(container.getAchievedVolumeUnit()))
                                volume = volume.subtract(new BigDecimal(convertUnit(Constants.VOLUME, pack.getVolume(), pack.getVolumeUnit(), container.getAchievedVolumeUnit()).toString()));
                        }
                        pack.setContainerId(null);
                        packingDao.save(pack);
                        packingList.add(pack);
                    }
                }
                container.setAchievedWeight(weight);
                container.setAchievedVolume(volume);
                container = containerService.calculateUtilization(container);
                Set<Long> removeShipmentIds = new HashSet<>();
                if(shipmentdIdSet.size() > 0) {
                    for(Long shipmentId : shipmentdIdSet) {
                        ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentId, "=", null);
                        listCommonRequest = andCriteria(Constants.CONTAINER_ID, container.getId(), "=", listCommonRequest);
                        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                        Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                        if(packings == null || packings.getContent() == null || packings.getContent().isEmpty())
                            removeShipmentIds.add(shipmentId);
                    }
                }
                if(removeShipmentIds.size() == 1 && container.getShipmentsList() != null && container.getShipmentsList().size() == 1) {
                    if(request.getIsFCL() || container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        containerService.changeContainerWtVolForSeaFCLDetach(container);
                    }
                }
                container = containerDao.save(container);
                shipmentsContainersMappingDao.detachShipments(container.getId(), removeShipmentIds.stream().toList(), false);
                containerService.afterSave(container, false);
                try {
                    packingsADSync.sync(packingList, UUID.randomUUID().toString());
                }
                catch (Exception e) {
                    log.error(SyncingConstants.ERROR_SYNCING_PACKS);
                }
                response = jsonHelper.convertValue(container, ContainerResponse.class);
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ShipmentDetails getShipmentFromMap(Map<Long, ShipmentDetails> map, Long id) {
        ShipmentDetails shipmentDetails = null;
        if(map.containsKey(id))
            shipmentDetails = map.get(id);
        else {
            Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(id);
            if(shipmentDetails1 != null && shipmentDetails1.isPresent()) {
                shipmentDetails = shipmentDetails1.get();
                map.put(shipmentDetails.getId(), shipmentDetails);
            }
        }
        return shipmentDetails;
    }

    private Containers getContainerFromMap(Map<Long, Containers> map, Long id) {
        Containers containers = null;
        if(map.containsKey(id))
            containers = map.get(id);
        else {
            Optional<Containers> containers1 = containerDao.findById(id);
            if(containers1 != null && containers1.isPresent()) {
                containers = containers1.get();
                map.put(containers.getId(), containers);
            }
        }
        return containers;
    }

    public ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            // create common list request for consolidation id
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for  Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            CompletableFuture<ResponseEntity<IRunnerResponse>> consolidationsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ConsolidationDetailsResponse> res = (RunnerResponse<ConsolidationDetailsResponse>) consolidationsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(res.getData());
            else{
                return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(res.getData(), request.getIncludeColumns()));
            }
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> fullConsolidationsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
            }
            checkBookingIdCriteria(request);
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullConsolidationList(consolidationDetailsPage.getContent()),
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullConsolidationList(consolidationDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToFullConsolidationList(List<ConsolidationDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(consolidationDetails -> {
            ConsolidationDetailsResponse response = modelMapper.map(consolidationDetails, ConsolidationDetailsResponse.class);
            responseList.add(response);
        });
        return responseList;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return list(commonRequestModel, false);
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        int totalPages = 0;
        long totalElements = 0;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
            }
            if(Boolean.TRUE.equals(request.getNotificationFlag())) {
                Page<Long> eligibleConsolId = consolidationDetailsDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED,
                    PageRequest.of(Math.max(0,request.getPageNo()-1), request.getPageSize()));

                List<Long> consoleIds = notificationDao.findEntityIdsByEntityType(CONSOLIDATION);

                Set<Long> uniqueConsolIds = new HashSet<>(eligibleConsolId.getContent());
                uniqueConsolIds.addAll(consoleIds);

                List<Long> combinedConsolIds = new ArrayList<>(uniqueConsolIds);

                andCriteria("id", combinedConsolIds, "IN", request);

                totalElements = combinedConsolIds.size();
                int pageSize = request.getPageSize();
                totalPages = (int) ((totalElements + pageSize - 1) / pageSize);
            }
            checkBookingIdCriteria(request);
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            if(!Boolean.TRUE.equals(request.getNotificationFlag())) {
                totalPages = consolidationDetailsPage.getTotalPages();
                totalElements = consolidationDetailsPage.getTotalElements();
            }
            if(request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty()) {
                List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent(), getMasterData);
                log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildListSuccessResponse(
                        consoleResponse,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(consolidationDetailsPage.getContent(), getMasterData)){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkBookingIdCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() > 0)
        {
            checkForBookingIdFilter(request.getFilterCriteria());
        }
    }

    private void checkForBookingIdFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("bookingId") && filterCriteria.getCriteria().getValue() != null) {

                ConsoleBookingIdFilterRequest consoleBookingIdFilterRequest = new ConsoleBookingIdFilterRequest();
                consoleBookingIdFilterRequest.setIntraBookingId(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchBookingIdFilterGuids(consoleBookingIdFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && filterCriteria.getInnerFilter().size() > 0) {
                checkForBookingIdFilter(filterCriteria.getInnerFilter());
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
                log.error("Request is empty for Consolidation async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Consolidation async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                    consolidationDetailsPage.getTotalPages(),
                    consolidationDetailsPage.getTotalElements()));
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
                log.debug("Request is empty for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(consolidationDetails.get());
            consolidationDetailsDao.delete(consolidationDetails.get());
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Consolidation details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveForNTE(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.error(ConsolidationConstants.CONSOLIDATION_RETRIEVE_NULL_REQUEST, LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id can't be null.");
            }
            Long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(id);
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            List<TriangulationPartner> triangulationPartners = consolidationDetails.get().getTriangulationPartnerList();
            Long currentTenant = TenantContext.getCurrentTenant().longValue();
            if ((triangulationPartners == null
                    || triangulationPartners.stream()
                        .filter(Objects::nonNull)
                        .noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
                    && !Objects.equals(consolidationDetails.get().getReceivingBranch(), currentTenant)) {
                throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE);
            } else if (triangulationPartners == null
                    && !Objects.equals(consolidationDetails.get().getTriangulationPartner(), TenantContext.getCurrentTenant().longValue())
                    && !Objects.equals(consolidationDetails.get().getReceivingBranch(), TenantContext.getCurrentTenant().longValue())) {
                throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE);
            }
            calculateAchievedValuesForRetrieve(consolidationDetails.get());

            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, id, LoggerHelper.getRequestIdFromMDC());
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            createConsolidationPayload(consolidationDetails.get(), response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return retrieveById(commonRequestModel, false);
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(retrieveConsolidationData(commonRequestModel, getMasterData));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ConsolidationDetailsResponse retrieveConsolidationData(CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null && request.getGuid() == null) {
            log.error("Request Id and Guid are null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id and GUID can't be null. Please provide any one !");
        }
        Long id = request.getId();
        Optional<ConsolidationDetails> consolidationDetails = Optional.ofNullable(null);
        if(id != null ){
            consolidationDetails = consolidationDetailsDao.findById(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            consolidationDetails = consolidationDetailsDao.findByGuid(guid);
        }
        if (!consolidationDetails.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, id, LoggerHelper.getRequestIdFromMDC());
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getMergeContainers()) && consolidationDetails.get().getContainersList() != null && !consolidationDetails.get().getContainersList().isEmpty()) {
            consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
        }
        calculateAchievedValuesForRetrieve(consolidationDetails.get());
        ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnConsolidation(Arrays.asList(consolidationDetails.get().getId()), ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(consolidationDetails.get().getId()), CONSOLIDATION);
        int pendingCount = map.getOrDefault(consolidationDetails.get().getId(), 0) + notificationMap.getOrDefault(consolidationDetails.get().getId(), 0);
        response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
        createConsolidationPayload(consolidationDetails.get(), response, getMasterData);

        return response;
    }

    private void calculateAchievedValuesForRetrieve(ConsolidationDetails consolidationDetails) {
        try {
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), consolidationDetails.getShipmentsList());
        } catch (Exception e) {
            log.error("Error while calculating achieved values for Consolidation with Id " + consolidationDetails.getId());
        }
    }

    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getMergeContainers()) && consolidationDetails.get().getContainersList() != null && !consolidationDetails.get().getContainersList().isEmpty()) {
                consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
            }
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            createConsolidationPayload(consolidationDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationDetailsDao.findConsolidationByIdWithQuery(id);
            if(!consolidationDetailsOptional.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
            ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
            Map<String, Object> response = fetchAllMasterDataByKey(consolidationDetails, consolidationDetailsResponse);
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Object> response = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllVesselDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllDGSubstanceDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, vesselDataFuture, dgSubstanceFuture, containerTypeFuture).join();

        return response;
    }

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        createConsolidationPayload(consolidationDetails, consolidationDetailsResponse, true);
    }

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, boolean getMasterData) {
        try {
            double _start = System.currentTimeMillis();
            if(getMasterData) {
                var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
                CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture).join();
            }
            this.calculationsOnRetrieve(consolidationDetails, consolidationDetailsResponse);
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                consolidationDetailsResponse.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.CONSOLE_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
            if(consolidationDetailsResponse.getId() != null) {
                var awb = awbDao.findByConsolidationId(consolidationDetailsResponse.getId());
                if (awb != null && !awb.isEmpty()) {
                    if (awb.get(0).getAirMessageStatus() != null)
                        consolidationDetailsResponse.setAwbStatus(awb.get(0).getAirMessageStatus());
                    else
                        consolidationDetailsResponse.setAwbStatus(AwbStatus.AWB_GENERATED);
                    if(awb.get(0).getLinkedHawbAirMessageStatus() != null)
                        consolidationDetailsResponse.setLinkedHawbStatus(awb.get(0).getLinkedHawbAirMessageStatus());
                }
            }
            if (Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA))
                fetchTruckerInfo(consolidationDetails.getId(), consolidationDetailsResponse);
        }  catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, ex.getLocalizedMessage());
        }
    }

    private void fetchTruckerInfo(Long id, ConsolidationDetailsResponse consolidationDetailsResponse) {
        List<Long> shipmentIds = consoleShipmentMappingDao.findByConsolidationId(id).stream().map(ConsoleShipmentMapping::getShipmentId).collect(toList());
        List<TruckDriverDetailsResponse> truckingInfo = new ArrayList<>();
        if (!shipmentIds.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentIds, "IN");
            Pair<Specification<TruckDriverDetails>, Pageable> pair = fetchData(listCommonRequest, TruckDriverDetails.class);
            Page<TruckDriverDetails> truckDriverDetailsPage = truckDriverDetailsDao.findAll(pair.getLeft(), pair.getRight());
            List<TruckDriverDetails> truckDriverDetails = truckDriverDetailsPage.stream().toList();
            if (!truckDriverDetails.isEmpty()) {
                ListCommonRequest listReq = constructListCommonRequest("id", shipmentIds, "IN");
                Pair<Specification<ShipmentDetails>, Pageable> pair1 = fetchData(listReq, ShipmentDetails.class);
                Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair1.getLeft(), pair1.getRight());
                var map = shipmentDetailsPage.getContent().stream().collect(toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
                List<ContainerResponse> containers = consolidationDetailsResponse.getContainersList();
                Map<Long, ContainerResponse> contMap = new HashMap<>();
                if(containers != null)
                    contMap = containers.stream().collect(Collectors.toMap(ContainerResponse::getId, Function.identity()));
                Map<Long, ContainerResponse> finalContMap = contMap;
                truckDriverDetails.forEach(truckDriverDetail -> {
                    var details = jsonHelper.convertValue(truckDriverDetail, TruckDriverDetailsResponse.class);
                    details.setShipmentNumber(map.get(truckDriverDetail.getShipmentId()));
                    if(details.getContainerId() != null && finalContMap.containsKey(details.getContainerId())) {
                        details.setContainerNumber(finalContMap.get(details.getContainerId()).getContainerNumber());
                    }
                    truckingInfo.add(details);
                });
            }
        }
        consolidationDetailsResponse.setTruckDriverDetails(truckingInfo);
    }
    private void calculationsOnRetrieve(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        try {
            consolidationDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(consolidationDetails.getContainersList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory()));
            consolidationDetailsResponse.setPackSummary(packingService.calculatePackSummary(consolidationDetails.getPackingList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory(), new ShipmentMeasurementDetailsDto()));
            calculateChargeable(CommonRequestModel.buildRequest(jsonHelper.convertValue(consolidationDetailsResponse, ConsoleCalculationsRequest.class)));
            calculateDescOfGoodsAndHandlingInfo(consolidationDetails, consolidationDetailsResponse, false);
        }
        catch (Exception e) {
            log.error("Error in calculations while creating console response payload");
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            if(masterDataResponse != null) {
                if (!Objects.isNull(consolidationDetailsResponse.getAllocations()))
                    listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAllocations(), Allocations.class, fieldNameKeyMap, Allocations.class.getSimpleName(), cacheMap));
                if (!Objects.isNull(consolidationDetailsResponse.getAchievedQuantities()))
                    listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAchievedQuantities(), AchievedQuantities.class, fieldNameKeyMap, AchievedQuantities.class.getSimpleName(), cacheMap));
                if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                    consolidationDetailsResponse.getRoutingsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));
                if(!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                    consolidationDetailsResponse.getPackingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId(), cacheMap)));
                if(!Objects.isNull(consolidationDetailsResponse.getReferenceNumbersList()))
                    consolidationDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId(), cacheMap)));
                if(!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                    consolidationDetailsResponse.getContainersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));
            }

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
                if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                    consolidationDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap) );
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            if(masterDataResponse != null) {
                if(!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                    consolidationDetailsResponse.getContainersList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));
                if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                    consolidationDetailsResponse.getRoutingsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));
                if (!Objects.isNull(consolidationDetailsResponse.getArrivalDetails()))
                    locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getArrivalDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Arrival", cacheMap)));
                if (!Objects.isNull(consolidationDetailsResponse.getDepartureDetails()))
                    locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getDepartureDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Departure", cacheMap)));
            }

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap) );
                if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                    consolidationDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COUNTRIES, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            if(masterDataResponse != null) {
                if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList())) {
                    Set<String> finalCarrierList = carrierList;
                    consolidationDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));
                    carrierList = finalCarrierList;
                }
            }

            Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> currencyList = new HashSet<>(masterDataUtils.createInBulkCurrencyRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES, currencyList, new EntityTransferCurrency(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.CURRENCIES, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCurrencyDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            if(masterDataResponse != null) {
                if (!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                    consolidationDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId(), cacheMap)));
            }

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if(masterDataResponse == null) {
                if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                    consolidationDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY, cacheMap)));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.TENANTS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTenantDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> wareHouseTypes = new HashSet<>(masterDataUtils.createInBulkWareHouseRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES, wareHouseTypes, new WareHouseResponse(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.WAREHOUSES, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllWarehouseDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));
            if (!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                consolidationDetailsResponse.getRoutingsList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));

            Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllDGSubstanceDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> dgSubstanceIdList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                consolidationDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId(), cacheMap)));

            Map v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES, dgSubstanceIdList, new EntityTransferDGSubstance(), cacheMap);

            if(masterDataResponse == null) { }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllDGSubstanceDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            if(masterDataResponse == null) {
                if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                    consolidationDetailsResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE, cacheMap)));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private List<Containers> mergeContainers(List<Containers> containersList, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException{
        try {
            List<Containers> response = new ArrayList<>();
            HashMap<String, List<Containers>> map = new HashMap<>();
            for(Containers container: containersList) {
                if(IsStringNullOrEmpty(container.getContainerCode()) || IsStringNullOrEmpty(container.getContainerNumber()) || IsStringNullOrEmpty(container.getHblDeliveryMode())) {
                    response.add(container);
                    continue;
                }
                String key = container.getContainerCode() + container.getContainerNumber() + container.getHblDeliveryMode();
                if(map.containsKey(key)) {
                    List<Containers> temp = map.get(key);
                    temp.add(container);
                    map.put(key, temp);
                }
                else {
                    List<Containers> containersList1 = new ArrayList<>();
                    containersList1.add(container);
                    map.put(key, containersList1);
                }
            }
            if(map.size() > 0) {
                for (Map.Entry<String, List<Containers>> entry : map.entrySet()) {
                    List<Containers> value = entry.getValue();
                    if(value.size() == 1) {
                        response.add(value.get(0));
                    }
                    else if(entry.getValue().size() > 0){
                        Containers finalContainer = value.get(0);
                        finalContainer.setId(null);
                        finalContainer.setGuid(null);
                        for (int i=1; i < value.size(); i++) {
                            finalContainer = convertIntoMergedConts(finalContainer, value.get(i), shipmentSettingsDetails);
                        }
                        response.add(finalContainer);
                    }
                }
            }
            return response;
        } catch (Exception e) {
            log.error("Error while merging consol containers");
            throw new RunnerException(e.getMessage());
        }
    }

    private Containers convertIntoMergedConts(Containers finalContainer, Containers container, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException{
        try {
            finalContainer.setDescriptionOfGoods(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods()));
//        finalContainer.setHa(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods())); TODO- missing handling info in containers
            if(!IsStringNullOrEmpty(finalContainer.getPacks()) && !IsStringNullOrEmpty(container.getPacks()))
                finalContainer.setPacks(String.valueOf(new BigDecimal(finalContainer.getPacks()).add(new BigDecimal(container.getPacks()))));
            else if(!IsStringNullOrEmpty(container.getPacks())) {
                finalContainer.setPacks(container.getPacks());
            }
            finalContainer.setMarksNums(mergeTextField(finalContainer.getMarksNums(), container.getMarksNums()));
            finalContainer.setCustomsReleaseCode(mergeTextField(finalContainer.getCustomsReleaseCode(), container.getCustomsReleaseCode()));
            finalContainer.setPacrNumber(mergeTextField(finalContainer.getPacrNumber(), container.getPacrNumber()));
            finalContainer.setContainerComments(mergeTextField(finalContainer.getContainerComments(), container.getContainerComments()));
            if(!IsStringNullOrEmpty(finalContainer.getPacksType()) && !IsStringNullOrEmpty(container.getPacksType()) && !container.getPacksType().equals(finalContainer.getPacksType()))
                finalContainer.setPacksType(Constants.MPK);
            // Net Weight field
            if(finalContainer.getNetWeight() != null && container.getNetWeight() != null && finalContainer.getNetWeightUnit() != null && container.getNetWeightUnit() != null) {
                if(finalContainer.getNetWeightUnit().equals(container.getNetWeightUnit()))
                    finalContainer.setNetWeight(finalContainer.getNetWeight().add(container.getNetWeight()));
                else {
                    finalContainer.setNetWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getNetWeight(), finalContainer.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                                    convertUnit(Constants.MASS, container.getNetWeight(), container.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setNetWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getNetWeight() != null){
                finalContainer.setNetWeightUnit(container.getNetWeightUnit());
                finalContainer.setNetWeight(container.getNetWeight());
            }
            // Gross Weight field
            if(finalContainer.getGrossWeight() != null && container.getGrossWeight() != null && finalContainer.getGrossWeightUnit() != null && container.getGrossWeightUnit() != null) {
                if(finalContainer.getGrossWeightUnit().equals(container.getGrossWeightUnit()))
                    finalContainer.setGrossWeight(finalContainer.getGrossWeight().add(container.getGrossWeight()));
                else {
                    finalContainer.setGrossWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getGrossWeight(), finalContainer.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getGrossWeight(), container.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getGrossWeight() != null){
                finalContainer.setGrossWeightUnit(container.getGrossWeightUnit());
                finalContainer.setGrossWeight(container.getGrossWeight());
            }
            // Tare Weight field
            if(finalContainer.getTareWeight() != null && container.getTareWeight() != null && finalContainer.getTareWeightUnit() != null && container.getTareWeightUnit() != null) {
                if(finalContainer.getTareWeightUnit().equals(container.getTareWeightUnit()))
                    finalContainer.setTareWeight(finalContainer.getTareWeight().add(container.getTareWeight()));
                else {
                    finalContainer.setTareWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getTareWeight(), finalContainer.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getTareWeight(), container.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setTareWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getTareWeight() != null){
                finalContainer.setTareWeightUnit(container.getTareWeightUnit());
                finalContainer.setTareWeight(container.getTareWeight());
            }
            // Gross Volume field
            if(finalContainer.getGrossVolume() != null && container.getGrossVolume() != null && finalContainer.getGrossVolumeUnit() != null && container.getGrossVolumeUnit() != null) {
                if(finalContainer.getGrossVolumeUnit().equals(container.getGrossVolumeUnit()))
                    finalContainer.setGrossVolume(finalContainer.getGrossVolume().add(container.getGrossVolume()));
                else {
                    finalContainer.setGrossVolume( new BigDecimal(convertUnit(Constants.VOLUME, finalContainer.getGrossVolume(), finalContainer.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() +
                            convertUnit(Constants.VOLUME, container.getGrossVolume(), container.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossVolumeUnit(shipmentSettingsDetails.getVolumeChargeableUnit());
                }
            }
            else if(container.getGrossVolume() != null){
                finalContainer.setGrossVolumeUnit(container.getGrossVolumeUnit());
                finalContainer.setGrossVolume(container.getGrossVolume());
            }
            // Measurement field
            if(finalContainer.getMeasurement() != null && container.getMeasurement() != null && finalContainer.getMeasurementUnit() != null && container.getMeasurementUnit() != null) {
                if(finalContainer.getMeasurementUnit().equals(container.getMeasurementUnit()))
                    finalContainer.setMeasurement(finalContainer.getMeasurement().add(container.getMeasurement()));
                else {
                    finalContainer.setMeasurement( new BigDecimal(convertUnit(Constants.LENGTH, finalContainer.getMeasurement(), finalContainer.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() +
                            convertUnit(Constants.LENGTH, container.getMeasurement(), container.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() ));
                    finalContainer.setMeasurementUnit(shipmentSettingsDetails.getMeasurementChargeableUnit());
                }
            }
            else if(container.getMeasurement() != null){
                finalContainer.setMeasurementUnit(container.getMeasurementUnit());
                finalContainer.setMeasurement(container.getMeasurement());
            }
            return finalContainer;
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private String mergeTextField(String a, String b) {
        if(!IsStringNullOrEmpty(b) && !IsStringNullOrEmpty(b))
            return a + "\n" + b;
        if(!IsStringNullOrEmpty(b))
            return b;
        if(!IsStringNullOrEmpty(a))
            return a;
        return null;
    }

    public ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(id).get();
        String lockingUser = consolidationDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (consolidationDetails.getIsLocked() != null && consolidationDetails.getIsLocked()) {
            if (lockingUser != null && (Objects.equals(lockingUser, currentUser) ||
                    (!Objects.isNull(PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin)) && !PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin).isEmpty()) ))
                consolidationDetails.setIsLocked(false);
            else
                throw new RunnerException(String.format(ErrorConstants.LOCK_UNLOCK_ERROR, Constants.Consolidation, lockingUser));
        } else {
            consolidationDetails.setIsLocked(true);
            consolidationDetails.setLockedBy(currentUser);
        }
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
        consolidationSync.syncLockStatus(consolidationDetails);
        pushShipmentDataToDependentService(consolidationDetails, false, consolidationDetails.getContainersList());

        return ResponseHelper.buildSuccessResponse();
    }

//    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
//        return (List<T>) runnerListResponse.getData();
//    }
//
//    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
//        return (List<T>) runnerListResponse.getData();
//    }
//
//    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
//        return (T) runnerResponse.getData();
//    }
//
//    private Containers convertRequestToEntity(ContainerRequest request) {
//        return jsonHelper.convertValue(request, Containers.class);
//    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel, boolean dataMigration, String createdBy, LocalDateTime createdDate) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<PartiesRequest> consolidationAddresses = consolidationDetailsRequest.getConsolidationAddresses();
        CarrierDetailRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
        AchievedQuantitiesRequest achievedQuantitiesRequest = consolidationDetailsRequest.getAchievedQuantities();
        AllocationsRequest allocationsRequest = consolidationDetailsRequest.getAllocations();


        UUID guid = consolidationDetailsRequest.getGuid();
        Optional<ConsolidationDetails> oldEntity = Optional.empty();
        try {
            oldEntity = consolidationDetailsDao.findByGuid(guid);
        }
        catch (Exception e){
        }

        List<ShipmentDetails> tempShipIds = new ArrayList<>();

        List<Long> newShipList = new ArrayList<>();
        List<ShipmentRequest> shipmentRequests = consolidationDetailsRequest.getShipmentsList();
        if(shipmentRequests != null && !shipmentRequests.isEmpty()) {
            for(ShipmentRequest shipmentRequest : shipmentRequests) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(shipmentRequest.getGuid());
                if(shipmentDetails.isPresent() && shipmentDetails.get().getId() != null) {
                    ShipmentDetails shipmentDetails1 = new ShipmentDetails();
                    shipmentDetails1.setId(shipmentDetails.get().getId());
                    tempShipIds.add(shipmentDetails1);
                    newShipList.add(shipmentDetails.get().getId());
                }
            }
        }


        try {
            Long id = null;
            ConsolidationDetails oldConsolidation = null;
            boolean isCreate = true;
            if(oldEntity.isPresent()) {
                oldConsolidation = oldEntity.get();
                id = oldEntity.get().getId();
                List<Long> oldShipList = oldConsolidation.getShipmentsList().stream().map(e -> e.getId()).toList();
                oldShipList = oldShipList.stream().filter(item -> !newShipList.contains(item)).toList();
                detachShipmentsReverseSync(oldEntity.get().getId(), oldShipList);
                isCreate = false;
            }
            ConsolidationDetails entity = objectMapper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            entity.setShipmentsList(tempShipIds);
            entity.setId(id);

            if(id == null) {
                entity = consolidationDetailsDao.save(entity, true);
            } else {
                entity = consolidationDetailsDao.update(entity, true);
            }
            // Set id for linking child entitites
            id = entity.getId();

            consolidationDetailsDao.saveCreatedDateAndUser(id, createdBy, createdDate);

            if(containerRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                if(!entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && entity.getShipmentsList() != null && !entity.getShipmentsList().isEmpty()) {
                    for (ShipmentDetails shipmentDetails : entity.getShipmentsList()) {
                        listCommonRequest = orCriteria(Constants.SHIPMENTS_LIST, shipmentDetails.getId(), "CONTAINS", listCommonRequest);
                    }
                }
                Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> oldContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                List<Containers> updatedContainers = containerDao.updateEntityFromConsolidationV1(commonUtils.convertToEntityList(containerRequestList, Containers.class), id, oldContainers.stream().toList());
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                if(entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && entity.getShipmentsList() != null && !entity.getShipmentsList().isEmpty()) {
                    for (ShipmentDetails shipmentDetails : entity.getShipmentsList()) {
                        listCommonRequest = orCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", listCommonRequest);
                    }
                }
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList());
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddresses != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION_ADDRESSES);
                Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
                Page<Parties> oldParties = partiesDao.findAll(pair.getLeft(), pair.getRight());
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddresses, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES, oldParties.stream().toList());
                entity.setConsolidationAddresses(updatedParties);
            }
            List<Containers> oldConts = null;
            if(oldEntity.isPresent())
                oldConts = oldEntity.get().getContainersList();
            if(!dataMigration)
                pushShipmentDataToDependentService(entity, isCreate, oldConts);
            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RunnerException(e.getMessage());
        }
    }

    private void makeHeadersInSheet(Sheet sheet, Workbook workbook) {
        Row headerRow = sheet.createRow(0);
        List<String> consolidationHeader = ConsolidationConstants.CONSOLIDATION_HEADER;

        CellStyle boldStyle = workbook.createCellStyle();
        Font boldFont = workbook.createFont();
        boldFont.setBold(true);
        boldStyle.setFont(boldFont);

        for (int i = 0; i < consolidationHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(consolidationHeader.get(i));
            cell.setCellStyle(boldStyle);
        }
    }

    private CarrierDetails mapCustomCarrierToCarrier(ConsolidationLiteResponse consolidationLiteResponse) {

        CarrierDetails carrierDetails = jsonHelper.convertValue(consolidationLiteResponse, CarrierDetails.class);
        carrierDetails.setId(consolidationLiteResponse.getCarrierId());

        return carrierDetails;
    }

    private Containers mapToContainer(IContainerLiteResponse containerLiteResponse){
        Containers containers = new Containers();
        containers.setConsolidationId(containerLiteResponse.getConsolidationId());
        containers.setContainerCode(containerLiteResponse.getContainerCode());
        return containers;
    }

    private ShipmentDetails mapToShipment(IShipmentLiteResponse shipmentLiteResponse){
        return ShipmentDetails.builder()
            .houseBill(shipmentLiteResponse.getHouseBill())
            .shipmentId(shipmentLiteResponse.getShipmentId())
            .build();
    }

    private Map<Long, List<ShipmentDetails>> populateConsolidationShipmentMap(List<IShipmentContainerLiteResponse> responses) {
        // Initialize the map
        Map<Long, List<ShipmentDetails>> consolidationShipmentMap = new HashMap<>();

        // Group responses by consolidation ID
        Map<Long, List<IShipmentContainerLiteResponse>> groupedByConsolId = responses.stream()
            .filter(response -> response.getConsolId() != null) // Filter out null ConsolId
            .collect(Collectors.groupingBy(IShipmentContainerLiteResponse::getConsolId));

        // Process each group to build ShipmentDetails and populate the map
        for (Map.Entry<Long, List<IShipmentContainerLiteResponse>> entry : groupedByConsolId.entrySet()) {
            Long consolId = entry.getKey();
            List<IShipmentContainerLiteResponse> groupedResponses = entry.getValue();

            // Map grouped responses to ShipmentDetails
            List<ShipmentDetails> shipmentDetailsList = groupedResponses.stream()
                .filter(response -> response.getShipId() != null) // Filter out null Shipment
                .collect(Collectors.groupingBy(IShipmentContainerLiteResponse::getShipId)) // Group by Shipment ID
                .entrySet()
                .stream()
                .map(shipmentEntry -> {
                    Long shipmentId = shipmentEntry.getKey(); // id of shipment
                    List<IShipmentContainerLiteResponse> shipmentResponses = shipmentEntry.getValue();

                    // Build Container list
                    List<Containers> containers = shipmentResponses.stream()
                        .map(response -> {
                            Containers container = new Containers();
                            container.setContainerCode(response.getContainerCode());
                            container.setContainerCount(response.getContainerCount());
                            container.setContainerNumber(response.getContainerNumber());
                            return container;
                        })
                        .collect(Collectors.toList());

                    // Create ShipmentDetails
                    ShipmentDetails details = new ShipmentDetails();
                    details.setShipmentId(shipmentResponses.get(0).getShipmentId());
                    details.setHouseBill(shipmentResponses.get(0).getHouseBill()); // Assuming houseBill is the same for all entries of a shipment
                    details.setId(shipmentId); // Assuming shipId is the same for all entries of a shipment
                    details.setContainersList(containers);

                    return details;
                })
                .collect(Collectors.toList());

            // Add to the consolidation map
            consolidationShipmentMap.put(consolId, shipmentDetailsList);
        }

        return consolidationShipmentMap;
    }

    public List<ConsolidationDetails> addRelationShipFields(List<ConsolidationLiteResponse> consolidationLiteResponseList, boolean isShipmentLevelContainer){
           List<ConsolidationDetails> consolidationDetailsList = new ArrayList<>();

        List<Long> consolidationIds = consolidationLiteResponseList.stream()
            .map(ConsolidationLiteResponse::getId)
            .collect(Collectors.toList());

        List<IContainerLiteResponse> containerLiteResponseList = containerDao.findAllLiteContainer(consolidationIds);

        Map<Long, List<ShipmentDetails>> consolidationShipmentMap = new HashMap<>();

        if(isShipmentLevelContainer){
        List<IShipmentContainerLiteResponse> shipmentContainerLiteResponses = consolidationDetailsDao.findShipmentDetailsWithContainersByConsolidationIds(consolidationIds);
            consolidationShipmentMap = populateConsolidationShipmentMap(shipmentContainerLiteResponses);
        }else {
            List<IShipmentLiteResponse> shipmentLiteResponseList = consolidationDetailsDao.findIShipmentsByConsolidationIds(
                consolidationIds);
            consolidationShipmentMap = shipmentLiteResponseList.stream()
                .collect(Collectors.groupingBy(IShipmentLiteResponse::getConsolId,
                    Collectors.mapping(this::mapToShipment, Collectors.toList())));
        }

        Map<Long, List<Containers>> consolidationContainerMap = containerLiteResponseList.stream()
            .collect(Collectors.groupingBy(IContainerLiteResponse::getConsolidationId,
                Collectors.mapping(this::mapToContainer, Collectors.toList())));

       for(ConsolidationLiteResponse consolidationLiteResponse : consolidationLiteResponseList){
           ConsolidationDetails consolidationDetails = objectMapper.convertValue(consolidationLiteResponse, ConsolidationDetails.class);
           consolidationDetails.setCarrierDetails(mapCustomCarrierToCarrier(consolidationLiteResponse));
           consolidationDetails.setContainersList(consolidationContainerMap.get(consolidationLiteResponse.getId()));
           consolidationDetails.setShipmentsList(consolidationShipmentMap.get(consolidationDetails.getId()));
           consolidationDetailsList.add(consolidationDetails);
       }

       return consolidationDetailsList;
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationLiteResponse> consolidationDetailsPageLite = customConsolidationDetailsRepository.findAllLiteConsol(tuple.getLeft(), tuple.getRight());

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isShipmentLevelContainer = shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer();
        List<ConsolidationDetails> consolidationDetailsList = addRelationShipFields(consolidationDetailsPageLite.getContent(), isShipmentLevelContainer);

        List<IRunnerResponse> consoleResponse = convertEntityListToDtoListForExport(consolidationDetailsList, isShipmentLevelContainer);

        log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        Map<String, Integer> headerMap = new HashMap<>();
        for (int i = 0; i < ConsolidationConstants.CONSOLIDATION_HEADER.size(); i++) {
            headerMap.put(ConsolidationConstants.CONSOLIDATION_HEADER.get(i), i);
        }
        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ConsolidationList");
            makeHeadersInSheet(sheet, workbook);

            for (int i = 0; i < consoleResponse.size(); i++) {
                Row itemRow = sheet.createRow(i + 1);
                ConsolidationListResponse consol = (ConsolidationListResponse) consoleResponse.get(i);
                LocalTimeZoneHelper.transformTimeZone(consol);
                itemRow.createCell(headerMap.get("Consolidation Type")).setCellValue(consol.getConsolidationType() != null ? consol.getConsolidationType() : "");
                itemRow.createCell(headerMap.get("Consolidation Number")).setCellValue(consol.getConsolidationNumber() != null ? consol.getConsolidationNumber() : "");
                itemRow.createCell(headerMap.get("Transport Mode")).setCellValue(consol.getTransportMode() != null ? consol.getTransportMode() : "");
                itemRow.createCell(headerMap.get("Cargo Type")).setCellValue(consol.getShipmentType() != null ? consol.getShipmentType() : "");
                itemRow.createCell(headerMap.get("ETA")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getEta() != null ? consol.getCarrierDetails().getEta().toString() : "");
                itemRow.createCell(headerMap.get("ATA")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getAta() != null ? consol.getCarrierDetails().getAta().toString() : "");
                itemRow.createCell(headerMap.get("ETD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getEtd() != null ? consol.getCarrierDetails().getEtd().toString() : "");
                itemRow.createCell(headerMap.get("ATD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getAtd() != null ? consol.getCarrierDetails().getAtd().toString() : "");
                itemRow.createCell(headerMap.get("Domestic")).setCellValue(consol.getIsDomestic() != null ? consol.getIsDomestic().toString() : "");
                itemRow.createCell(headerMap.get("Created By")).setCellValue(consol.getCreatedBy() != null ? consol.getCreatedBy() : "");
                itemRow.createCell(headerMap.get("Voyage/Flight No")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getVoyage() != null ? consol.getCarrierDetails().getVoyage() : "");
                itemRow.createCell(headerMap.get("Payment Terms")).setCellValue(consol.getPayment() != null ? consol.getPayment() : "");
                itemRow.createCell(headerMap.get("Carrier")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getShippingLine() != null ? consol.getCarrierDetails().getShippingLine() : "");
                itemRow.createCell(headerMap.get("Cutoff Date")).setCellValue(consol.getBookingCutoff() != null ? consol.getBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("HBL / HAWB")).setCellValue(consol.getHouseBills() != null && !consol.getHouseBills().isEmpty() ? consol.getHouseBills().get(0) : "");
                itemRow.createCell(headerMap.get("Estimated Terminal Cutoff")).setCellValue(consol.getEstimatedTerminalCutoff() != null ? consol.getEstimatedTerminalCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Terminal Cutoff")).setCellValue(consol.getTerminalCutoff() != null ? consol.getTerminalCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Booking Cutoff")).setCellValue(consol.getBookingCutoff() != null ? consol.getBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Shipping Instruction Cutoff")).setCellValue(consol.getShipInstructionCutoff() != null ? consol.getShipInstructionCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Hazardous Booking Cutoff")).setCellValue(consol.getHazardousBookingCutoff() != null ? consol.getHazardousBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("VGM Cutoff")).setCellValue(consol.getVerifiedGrossMassCutoff() != null ? consol.getVerifiedGrossMassCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Reefer Cutoff")).setCellValue(consol.getReeferCutoff() != null ? consol.getReeferCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Booking Type")).setCellValue(consol.getBookingType() != null ? consol.getBookingType() : "");
                itemRow.createCell(headerMap.get("Reference Number")).setCellValue(consol.getReferenceNumber() != null ? consol.getReferenceNumber() : "");
                itemRow.createCell(headerMap.get("Carrier Booking Status")).setCellValue(consol.getBookingStatus() != null ? consol.getBookingStatus() : "");
                itemRow.createCell(headerMap.get("Carrier Booking Number")).setCellValue(consol.getBookingNumber() != null ? consol.getBookingNumber() : "");
                itemRow.createCell(headerMap.get("Container Count")).setCellValue(consol.getContainerCount() != null ? consol.getContainerCount().toString() : "");
                itemRow.createCell(headerMap.get("POL")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? consol.getCarrierDetails().getUnlocationData().get("originPort_name") : "");
                itemRow.createCell(headerMap.get("POD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? consol.getCarrierDetails().getUnlocationData().get("destinationPort_name") : "");
                itemRow.createCell(headerMap.get("MBL / MAWB")).setCellValue(consol.getMawb() != null ? consol.getMawb() : "");
                itemRow.createCell(headerMap.get("POL Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("originPort_code")) : "");
                itemRow.createCell(headerMap.get("POD Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destinationPort_code")) : "");
                itemRow.createCell(headerMap.get("Origin Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("origin_code")) : "");
                itemRow.createCell(headerMap.get("Destination Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destination_code")) : "");
                itemRow.createCell(headerMap.get("Origin")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("origin_name")) : "");
                itemRow.createCell(headerMap.get("Destination")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destination_name")) : "");
            }

            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Consolidations_" + timestamp + Constants.XLSX;

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (OutputStream outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }
        }

    }

    private boolean checkConsolidationTypeValidation(ConsolidationDetails consolidationDetails) {
        return !(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && Boolean.TRUE.equals(consolidationDetails.getHazardous()) && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && !StringUtility.isEmpty(consolidationDetails.getConsolidationType()) && !Constants.CONSOLIDATION_TYPE_AGT.equals(consolidationDetails.getConsolidationType()) && !Constants.CONSOLIDATION_TYPE_CLD.equals(consolidationDetails.getConsolidationType()));
    }

    private void changeDgShipmentMapValues(ShipmentDetails shipmentDetails, Map<Long, ShipmentDetails> dgStatusChangeInShipments, Containers container) throws RunnerException {
        ShipmentDetails shipmentDetails1 = shipmentDetails;
        if(dgStatusChangeInShipments.containsKey(shipmentDetails.getId()))
            shipmentDetails1 = dgStatusChangeInShipments.get(shipmentDetails.getId());
        boolean valueChanged = false;
        if(!Boolean.TRUE.equals(shipmentDetails1.getContainsHazardous())) {
            valueChanged = true;
            shipmentDetails1.setContainsHazardous(true);
        }
        if(commonUtils.checkIfAnyDGClass(container.getDgClass()))
            valueChanged = valueChanged || commonUtils.changeShipmentDGStatusToReqd(shipmentDetails1, commonUtils.checkIfDGClass1(container.getDgClass()));
        if(valueChanged)
            dgStatusChangeInShipments.put(shipmentDetails1.getId(), shipmentDetails1);
    }

    private void changeShipmentDGValuesFromContainer(Containers container, Map<Long, Containers> oldContainersMap,
                                                     Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {
        Containers oldContainer = null;
        if(container.getId() != null && oldContainersMap.containsKey(container.getId())) {
            oldContainer = oldContainersMap.get(container.getId());
            if(commonUtils.checkIfDGFieldsChangedInContainer(jsonHelper.convertValue(container, ContainerRequest.class), oldContainer)
                    && !listIsNullOrEmpty(oldContainer.getShipmentsList())) {
                for(ShipmentDetails shipmentDetails: oldContainer.getShipmentsList()) {
                    changeDgShipmentMapValues(shipmentDetails, dgStatusChangeInShipments, container);
                }
            }
        }
    }

    private void dgOceanFlowsAndValidations(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()))
        {
            List<Containers> containersList = consolidationDetails.getContainersList();
            if(!listIsNullOrEmpty(containersList))
            {
                Map<Long, Containers> oldContainersMap = new HashMap<>();
                if(!Objects.isNull(oldEntity))
                    oldContainersMap = oldEntity.getContainersList().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
                for(Containers container: containersList)
                {
                    if(!Boolean.TRUE.equals(container.getHazardous()))
                        continue;
                    consolidationDetails.setHazardous(true);
                    if(!Objects.isNull(oldEntity))
                        changeShipmentDGValuesFromContainer(container, oldContainersMap, dgStatusChangeInShipments);
                }
            }
        }
        if(!checkConsolidationTypeValidation(consolidationDetails))
            throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
    }

    private void beforeSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isCreate) throws Exception {
        CarrierDetails oldCarrierDetails = null;
        if(!Boolean.TRUE.equals(isCreate))
            oldCarrierDetails = jsonHelper.convertValue(oldEntity.getCarrierDetails(), CarrierDetails.class);
        if(Objects.isNull(consolidationDetails.getInterBranchConsole()))
            consolidationDetails.setInterBranchConsole(false);
        /* Future to populate unloc code in consoliation child entities*/
        var populateUnlocCodeFuture = getPopulateUnlocCodeFuture(consolidationDetails, oldEntity);

        if (Objects.isNull(consolidationDetails.getSourceTenantId()))
            consolidationDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
        log.info("Executing consolidation before save");
        Map<Long, ShipmentDetails> dgStatusChangeInShipments = new HashMap<>();
        dgOceanFlowsAndValidations(consolidationDetails, oldEntity, dgStatusChangeInShipments);
        List<ShipmentDetails> shipmentDetails = null;
        if(!isCreate){
            // This method will only work for non air transport modes , validation check moved inside the method
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), oldEntity.getShipmentsList());
            shipmentDetails = updateLinkedShipmentData(consolidationDetails, oldEntity, false, dgStatusChangeInShipments);
        }
        if(!Boolean.TRUE.equals(isCreate) && checkConsolidationEligibleForCFSValidation(consolidationDetails)) {
            if(shipmentDetails == null)
                shipmentDetails = getShipmentsList(consolidationDetails.getId());
            if(!listIsNullOrEmpty(shipmentDetails)) {
                for(ShipmentDetails i: shipmentDetails) {
                    if(checkIfShipmentDateGreaterThanConsole(i.getShipmentGateInDate(), consolidationDetails.getCfsCutOffDate()))
                        throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
                }
            }
        }

        if(consolidationDetails.getCarrierDetails() != null) {
            if (consolidationDetails.getTransportMode() != null && consolidationDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setVoyage(null);
            } else {
                consolidationDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
        if(consolidationDetails.getReceivingBranch() != null && consolidationDetails.getReceivingBranch() == 0)
            consolidationDetails.setReceivingBranch(null);
        if(consolidationDetails.getDocumentationPartner() != null && consolidationDetails.getDocumentationPartner() == 0)
            consolidationDetails.setDocumentationPartner(null);
        if(ObjectUtils.isNotEmpty(consolidationDetails.getTriangulationPartnerList())
                && consolidationDetails.getTriangulationPartnerList().size() == 1) {
            TriangulationPartner triangulationPartner = consolidationDetails.getTriangulationPartnerList().get(0);
            if (triangulationPartner != null && Long.valueOf(0).equals(triangulationPartner.getTriangulationPartner()))
                consolidationDetails.setTriangulationPartnerList(null);
        } else if (consolidationDetails.getTriangulationPartnerList() == null
                && consolidationDetails.getTriangulationPartner() != null
                && consolidationDetails.getTriangulationPartner() == 0) {
            consolidationDetails.setTriangulationPartner(null);
        }
        if(checkDisableFetchConditionForAwb(consolidationDetails, oldEntity, commonUtils.getShipmentSettingFromContext())) {
            List<Awb> awbs = awbDao.findByConsolidationId(consolidationDetails.getId());
            if(!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }
        if (Boolean.TRUE.equals(isCreate))
            consolidationDetails.setOpenForAttachment(true);

        this.checkInterBranchPermission(consolidationDetails, oldEntity);

        // Ignore events payload to avoid transaction issues bypassing consolidationDao.update(...);
        // Update happens in after save from request body
        consolidationDetails.setEventsList(null);
        populateUnlocCodeFuture.join();
    }

    private CompletableFuture<Void> getPopulateUnlocCodeFuture(ConsolidationDetails entity, ConsolidationDetails oldEntity) {
        CarrierDetails finalOldCarrierDetails = Optional.ofNullable(oldEntity).map(ConsolidationDetails::getCarrierDetails).orElse(null);
        List<Routings> finalOldRoutings = Optional.ofNullable(oldEntity).map(ConsolidationDetails::getRoutingsList).orElse(Collections.emptyList());

        /* Set to extract the unlocations from entities whose unloc code needs to be saved */
        Set<String> unlocationsSet = Collections.synchronizedSet(new HashSet<>());
        Map<String, EntityTransferUnLocations> unLocationsMap = new ConcurrentHashMap<>();

        CompletableFuture.allOf(
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(entity.getCarrierDetails(), finalOldCarrierDetails, unlocationsSet), executorService),
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(entity.getRoutingsList(), finalOldRoutings, unlocationsSet), executorService)
        ).join();
        CompletableFuture<Void> fetchUnlocationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(unlocationsSet, unLocationsMap)), executorService)
                .thenCompose(v -> CompletableFuture.allOf(
                        CompletableFuture.runAsync(() -> commonUtils.updateCarrierUnLocData(entity.getCarrierDetails(), unLocationsMap), executorService),
                        CompletableFuture.runAsync(() -> commonUtils.updateRoutingUnLocData(entity.getRoutingsList(), unLocationsMap), executorService)
                ));

        return fetchUnlocationDataFuture;
    }

    private void checkInterBranchPermission(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(((Objects.isNull(oldEntity) && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) || (!Objects.isNull(oldEntity)
                && !Objects.equals(consolidationDetails.getInterBranchConsole(), oldEntity.getInterBranchConsole())))
                && (!UserContext.getUser().getPermissions().containsKey(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)
                || Boolean.FALSE.equals(UserContext.getUser().getPermissions().get(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)))) {
                throw new ValidationException("User don't have InterBranch Consolidation Permission to change InterBranch Flag");
        }
    }

    public void validateRaKcForConsol(ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettingsResponse) throws RunnerException {
        Parties sendingAgent = consolidationDetails.getSendingAgent();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            List<Parties> orgList = new ArrayList<>();
            if(sendingAgent != null && StringUtility.isNotEmpty(sendingAgent.getAddressCode())) {
                orgList.add(sendingAgent);
            }

            if(!orgList.isEmpty()) {
                OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(orgList);
                if (orgAddressResponse != null) {
                    Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
                    int countOfExpiredParties = 0;
                    int countOfRaKCParties = 0;
                    for(var entry : addressMap.entrySet()) {
                        if (entry.getValue() != null && StringUtility.isNotEmpty(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)))) {
                            LocalDateTime agentExpiry = LocalDateTime.parse(StringUtility.convertToString(entry.getValue().get(KCRA_EXPIRY)));
                            // if any one of the agent is not expired will apply the validations as is
                            countOfRaKCParties++;
                            if (LocalDateTime.now().isAfter(agentExpiry))
                                countOfExpiredParties++;
                        }
                    }
                    if(countOfExpiredParties == countOfRaKCParties && countOfExpiredParties > 0)
                        return;
                    if (sendingAgent != null && addressMap.containsKey(sendingAgent.getOrgCode() + "#" + sendingAgent.getAddressCode())) {
                        Map<String, Object> addressConsignorAgent = addressMap.get(sendingAgent.getOrgCode() + "#" + sendingAgent.getAddressCode());
                        if (addressConsignorAgent.containsKey(Constants.REGULATED_AGENT)) {
                            var rakcType = addressConsignorAgent.get(Constants.REGULATED_AGENT);
                            if (rakcType != null && Boolean.TRUE.equals(rakcType)){
                                if(consolidationDetails.getScreeningStatus() == null ||
                                    consolidationDetails.getScreeningStatus().isEmpty() ||
                                    consolidationDetails.getSecurityStatus() == null || consolidationDetails.getSecurityStatus().isEmpty()){
                                    throw new RunnerException("Screening Status and Security Status is mandatory for RA.");
                                }else if(consolidationDetails.getScreeningStatus() != null && consolidationDetails.getScreeningStatus().size() == 1 && consolidationDetails.getScreeningStatus().get(0).equals("VCK")){
                                    throw new ValidationException("Please select an additional screening status along with VCK.");
                                }
                            }
                        }
                    }

                    if (consolidationDetails.getId() != null && consolidationDetails.getSendingAgent() != null && StringUtility.isNotEmpty(consolidationDetails.getSendingAgent().getAddressCode()) && !checkRaStatusFields(consolidationDetails, orgAddressResponse, consolidationDetails.getSendingAgent())) {
                        throw new RunnerException("Screening Status and Security Status is mandatory for RA Origin Agent.");
                    }
                }
            }
        }
    }

    public boolean checkConsolidationEligibleForCFSValidation(ConsolidationDetails consolidationDetails) {
        if(!Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()))
            return false;
        if(!Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType()))
            return false;
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    public boolean checkIfShipmentDateGreaterThanConsole(LocalDateTime shipmentDate, LocalDateTime consolidationDate) {
        if(shipmentDate == null)
            return false;
        if(consolidationDate == null)
            return false;
        return shipmentDate.isAfter(consolidationDate);
    }

    public boolean checkRaStatusFields(ConsolidationDetails consolidationDetails, OrgAddressResponse orgAddressResponse, Parties parties) {
        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if (addressMap.containsKey(parties.getOrgCode() + "#" + parties.getAddressCode())) {
            Map<String, Object> addressConsignorAgent = addressMap.get(parties.getOrgCode() + "#" + parties.getAddressCode());
            if (addressConsignorAgent.containsKey(Constants.REGULATED_AGENT)) {
                var rakcType = addressConsignorAgent.get(Constants.REGULATED_AGENT);
                if (rakcType != null && Boolean.TRUE.equals(rakcType) && (consolidationDetails.getScreeningStatus() == null ||
                        consolidationDetails.getScreeningStatus().isEmpty() ||
                        consolidationDetails.getSecurityStatus() == null || consolidationDetails.getSecurityStatus().isEmpty())) {
                    return false;
                }
            }
        }
        return true;
    }

    private boolean checkDisableFetchConditionForAwb(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
        if(oldEntity == null)
            return false;
        if(!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag())){
            return false;
        }
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        return !Objects.equals(consolidationDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(consolidationDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(consolidationDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    private void afterSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ConsolidationDetailsRequest consolidationDetailsRequest, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean includeGuid, boolean isFromET) throws RunnerException{
        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

        Long id = consolidationDetails.getId();

        if (Boolean.FALSE.equals(isCreate)) {
            // Update AWB
            if(checkForAwbUpdate(consolidationDetails, oldEntity)) {
                awbDao.updatedAwbInformationEvent(consolidationDetails, oldEntity);
            }
        }

        if(containerRequestList != null && (shipmentSettingsDetails.getMergeContainers() == null || !shipmentSettingsDetails.getMergeContainers())
            && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
            List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class, (!isFromBooking && !includeGuid) && isCreate), id, (Long) null, true);
            consolidationDetails.setContainersList(updatedContainers);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class, (!isFromBooking && !includeGuid) && isCreate), id);
            consolidationDetails.setPackingList(updatedPackings);
        }

        // Events section
        if (eventsRequestList != null && !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            eventsRequestList = setEventDetails(eventsRequestList, consolidationDetails);
            List<Events> eventsList = new ArrayList<>(commonUtils.convertToEntityList(eventsRequestList, Events.class, !Boolean.TRUE.equals(isFromBooking) && isCreate));
            commonUtils.removeDuplicateTrackingEvents(eventsList);
            commonUtils.updateEventWithMasterData(eventsList);
            eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.CONSOLIDATION);
            consolidationDetails.setEventsList(eventsList);
        }
        if (Boolean.TRUE.equals(isCreate) && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate())) {
            generateEvents(consolidationDetails);
        }

        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromConsole(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isFromBooking ? false : isCreate), id);
//            consolidationDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setRoutingsList(updatedRoutings);
        }
        if (consolidationAddressRequest != null) {
            List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedFileRepos);
        }

        if(!isFromET) {
            pushShipmentDataToDependentService(consolidationDetails, isCreate, Optional.ofNullable(oldEntity).map(ConsolidationDetails::getContainersList).orElse(null));
            this.pushAllShipmentDataToDependentService(consolidationDetails);
        }
        try {
            if (!isFromBooking)
                consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), isFromBooking);
        } catch (Exception e){
            log.error("Error performing sync on consolidation entity, {}", e);
        }
        if (consolidationDetails.getShipmentsList() != null) {
            consolidationDetails.getShipmentsList().forEach(shipment -> {
                if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipment)), executorService);
            });
        }

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consolidationDetails, oldEntity)), executorService);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerAutomaticTransfer(consolidationDetails, oldEntity, false)), executorService);
    }

    private void pushAllShipmentDataToDependentService(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails.getShipmentsList() != null) {
            List<Long> shipmentIds = consolidationDetails.getShipmentsList().stream().map(BaseEntity::getId).toList();
            if (!shipmentIds.isEmpty()) {
                List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
                for (ShipmentDetails shipment : shipments) {
                    dependentServiceHelper.pushShipmentDataToDependentService(shipment, false, false, shipment.getContainersList());
                }
            }
        }
    }

    private void processNetworkTransferEntity(Long tenantId, Long oldTenantId, ConsolidationDetails consolidationDetails, String jobType, Boolean isInterBranchConsole) {
        try{
            networkTransferService.processNetworkTransferEntity(tenantId, oldTenantId, Constants.CONSOLIDATION, null,
                    consolidationDetails, jobType, null, isInterBranchConsole);
        } catch (Exception ex) {
            log.error("Exception during processing Network Transfer entity for Consolidation Number: {} with exception: {}", consolidationDetails.getConsolidationNumber(), ex.getMessage());
        }

    }

    public void createOrUpdateNetworkTransferEntity(ShipmentSettingsDetails shipmentSettingsDetails, ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        try{
            boolean isNetworkTransferEntityEnabled = Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
            boolean isInterBranchConsole = Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
            if(isNetworkTransferEntityEnabled && isInterBranchConsole)
                processInterBranchEntityCase(consolidationDetails, oldEntity);

            if (consolidationDetails.getTransportMode()!=null &&
                    !Constants.TRANSPORT_MODE_RAI.equals(consolidationDetails.getTransportMode())
                    && Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled())) {
                processNetworkTransferEntity(consolidationDetails.getReceivingBranch(),
                        oldEntity != null ? oldEntity.getReceivingBranch() : null, consolidationDetails,
                        reverseDirection(consolidationDetails.getShipmentType()), isInterBranchConsole);

                if (consolidationDetails.getTriangulationPartnerList() != null && !isInterBranchConsole) {
                    List<Long> currentPartners = commonUtils.getTriangulationPartnerList(consolidationDetails.getTriangulationPartnerList());
                    List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                            : Collections.emptyList();

                    // Determine new tenant IDs by removing old partners from the current partners
                    Set<Long> newTenantIds = new HashSet<>(currentPartners);
                    newTenantIds.removeAll(oldPartners);

                    // Determine old tenant IDs by removing current partners from the old partners
                    Set<Long> oldTenantIds = new HashSet<>(oldPartners);
                    oldTenantIds.removeAll(currentPartners);

                    // Process new tenant IDs for network transfer
                    newTenantIds.forEach(newTenantId -> {
                        processNetworkTransferEntity(newTenantId, null, consolidationDetails, Constants.DIRECTION_CTS, false);
                    });

                    // Process old tenant IDs for removal from network transfer
                    oldTenantIds.forEach(oldTenantId -> {
                        processNetworkTransferEntity(null, oldTenantId, consolidationDetails, Constants.DIRECTION_CTS, false);
                    });
                } else if (consolidationDetails.getTriangulationPartner() != null && !isInterBranchConsole) {
                    processNetworkTransferEntity(consolidationDetails.getTriangulationPartner(),
                            oldEntity != null ? oldEntity.getTriangulationPartner() : null, consolidationDetails, Constants.DIRECTION_CTS, false);
                } else if(consolidationDetails.getTriangulationPartnerList() == null) {
                    List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                            : Collections.emptyList();
                    Set<Long> oldTenantIds = new HashSet<>(oldPartners);
                    oldTenantIds.forEach(oldTenantId ->
                            processNetworkTransferEntity(null, oldTenantId, consolidationDetails, Constants.DIRECTION_CTS, false)
                    );
                }
            }
        } catch (Exception ex) {
            log.error("Exception during creation or updation of Network Transfer entity for Consolidation Number: {} with exception: {}", consolidationDetails.getConsolidationNumber(), ex.getMessage());
        }

    }

    private void deleteAllConsoleErrorsLogs(ConsolidationDetails consolidationDetails){
        List<Long> shipmentIds = new ArrayList<>();
        if(consolidationDetails.getShipmentsList()!=null)
            shipmentIds = consolidationDetails.getShipmentsList().stream().map(BaseEntity::getId).toList();
        commonErrorLogsDao.deleteAllConsoleAndShipmentErrorsLogs(consolidationDetails.getId(), shipmentIds);
    }

    private void processInterBranchEntityCase(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        boolean isConsoleBranchUpdate = oldEntity != null && oldEntity.getReceivingBranch() != null
                && !oldEntity.getReceivingBranch().equals(consolidationDetails.getReceivingBranch());

        if (consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            processShipments(consolidationDetails, isConsoleBranchUpdate);
        } else if (oldEntity != null && !oldEntity.getShipmentsList().isEmpty()) {
            deleteNetworkTransferForOldShipments(oldEntity);
        }
    }

    private Map<Long, Map<Integer, NetworkTransfer>> getNetworkTransferMap(ConsolidationDetails consolidationDetails){
        List<Long> shipmentIds = consolidationDetails.getShipmentsList().stream().map(ShipmentDetails::getId).toList();

        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(shipmentIds, Constants.SHIPMENT);
        Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTranferMap = null;
        if(networkTransferList!=null){
            shipmentNetworkTranferMap = networkTransferList.stream()
                    .collect(Collectors.groupingBy(
                            NetworkTransfer::getEntityId,
                            Collectors.toMap(NetworkTransfer::getTenantId, transfer -> transfer)
                    ));
        }
        return shipmentNetworkTranferMap;
    }

    private void processShipments(ConsolidationDetails consolidationDetails, boolean isConsoleBranchUpdate) {
        Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTranferMap = getNetworkTransferMap(consolidationDetails);

        List<ShipmentDetails> shipmentsForNte = new ArrayList<>();
        List<ShipmentDetails> shipmentsToDelete = new ArrayList<>();

        for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
            if(shipmentDetails.getReceivingBranch()==null)
                continue;
            NetworkTransfer existingNTE = shipmentNetworkTranferMap!=null ? shipmentNetworkTranferMap.getOrDefault(shipmentDetails.getId(), new HashMap<>())
                    .get(shipmentDetails.getReceivingBranch().intValue()): null;

            if (shipmentDetails.getReceivingBranch() != null && !Objects.equals(consolidationDetails.getReceivingBranch(), shipmentDetails.getReceivingBranch())) {
                if (existingNTE == null) {
                    shipmentsForNte.add(shipmentDetails);
                }
                processConsoleBranchUpdate(isConsoleBranchUpdate, existingNTE);
            } else {
                shipmentsToDelete.add(shipmentDetails);
            }
        }

        shipmentsForNte.forEach(shipmentDetails ->
                networkTransferService.processNetworkTransferEntity(
                        shipmentDetails.getReceivingBranch(), null, SHIPMENT, shipmentDetails,
                        null, Constants.DIRECTION_IMP, null, true)
        );

        shipmentsToDelete.forEach(shipmentDetails ->
                networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                        shipmentDetails.getId(), Constants.SHIPMENT)
        );
    }

    private void processConsoleBranchUpdate(boolean isConsoleBranchUpdate, NetworkTransfer existingNTE){
        if (isConsoleBranchUpdate && existingNTE != null && existingNTE.getEntityPayload() != null
                && existingNTE.getStatus() != NetworkTransferStatus.REASSIGNED
                && existingNTE.getStatus() != NetworkTransferStatus.ACCEPTED) {
            existingNTE.setEntityPayload(null);
            existingNTE.setStatus(NetworkTransferStatus.SCHEDULED);
            networkTransferDao.save(existingNTE);
        }
    }

    private void deleteNetworkTransferForOldShipments(ConsolidationDetails oldEntity) {
        oldEntity.getShipmentsList().forEach(shipmentDetails ->
                networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                        oldEntity.getId(), Constants.SHIPMENT)
        );
    }

    public void triggerAutomaticTransfer(ConsolidationDetails consolidationDetails,
                                         ConsolidationDetails oldEntity, Boolean isDocOrHawbNumAdded) {
        try {

            Boolean isReceivingBranchEmpty = ObjectUtils.isEmpty(consolidationDetails.getReceivingBranch()) && oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getReceivingBranch());
            if(Boolean.TRUE.equals(isReceivingBranchEmpty) || isInvalidForTransfer(consolidationDetails)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurations = quartzJobInfoService.getActiveFileTransferConfigurations(consolidationDetails.getTransportMode());
            if (ObjectUtils.isEmpty(fileTransferConfigurations)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(Math.toIntExact(consolidationDetails.getReceivingBranch()), consolidationDetails.getId(), CONSOLIDATION);
            if(optionalNetworkTransfer.isPresent() && (optionalNetworkTransfer.get().getStatus()==NetworkTransferStatus.TRANSFERRED ||
                    optionalNetworkTransfer.get().getStatus()==NetworkTransferStatus.ACCEPTED)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    consolidationDetails.getTenantId(), consolidationDetails.getId(), CONSOLIDATION);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);

            CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();
            if (carrierDetails==null || (ObjectUtils.isEmpty(carrierDetails.getEta()) && ObjectUtils.isEmpty(carrierDetails.getEtd()) &&
                    ObjectUtils.isEmpty(carrierDetails.getAta()) && ObjectUtils.isEmpty(carrierDetails.getAtd()))) {
                if(quartzJobInfo!=null && quartzJobInfo.getJobStatus() == JobState.QUEUED)
                    quartzJobInfoService.deleteJobById(quartzJobInfo.getId());
                String errorMessage = "Please enter the Eta, Etd, Ata and Atd to retrigger the transfer";
                SendConsoleValidationResponse sendConsoleValidationResponse = SendConsoleValidationResponse.builder().isError(true).consoleErrorMessage(errorMessage).build();
                commonErrorLogsDao.logConsoleAutomaticTransferErrors(sendConsoleValidationResponse, consolidationDetails.getId(), new ArrayList<>());
                return;
            }

            if (ObjectUtils.isEmpty(quartzJobInfo) && oldEntity == null) {
                createOrUpdateQuartzJob(consolidationDetails, null);
            } else if (shouldUpdateExistingJob(quartzJobInfo, oldEntity, consolidationDetails, isDocOrHawbNumAdded, optionalNetworkTransfer)) {
                createOrUpdateQuartzJob(consolidationDetails, quartzJobInfo);
            }
        } catch (Exception e) {
            log.error("Exception during creation or updation of Automatic transfer flow for consolidation Id: {} with exception: {}", consolidationDetails.getId(), e.getMessage());
        }
    }

    private boolean isInvalidForTransfer(ConsolidationDetails consolidationDetails) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled())
                || ObjectUtils.isEmpty(consolidationDetails.getReceivingBranch())
                || (consolidationDetails.getTransportMode()!=null && Constants.TRANSPORT_MODE_RAI.equals(consolidationDetails.getTransportMode()))
                || Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
    }

    private boolean shouldUpdateExistingJob(QuartzJobInfo quartzJobInfo, ConsolidationDetails oldEntity, ConsolidationDetails consolidationDetails, Boolean isDocAdded, Optional<NetworkTransfer> optionalNetworkTransfer) {

        return (isValidforAutomaticTransfer(quartzJobInfo, consolidationDetails, oldEntity, isDocAdded))
                || (isValidReceivingBranchChange(consolidationDetails, oldEntity, optionalNetworkTransfer));
    }

    private boolean isValidDateChange(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        CarrierDetails newCarrierDetails = consolidationDetails.getCarrierDetails();
        if(oldEntity!=null && oldEntity.getCarrierDetails()!=null && newCarrierDetails!=null){
            CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
            return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta())
                    || isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd())
                    || isValueChanged(newCarrierDetails.getAta(), oldCarrierDetails.getAta())
                    || isValueChanged(newCarrierDetails.getAtd(), oldCarrierDetails.getAtd());
        }
        return false;
    }

    private boolean isValidReceivingBranchChange(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Optional<NetworkTransfer> optionalNetworkTransfer) {

        if (oldEntity == null) {
            return false;
        }

        if (oldEntity.getReceivingBranch()==null) {
            return true;
        }

        boolean isBranchChanged = !Objects.equals(oldEntity.getReceivingBranch(), consolidationDetails.getReceivingBranch());
        if (!isBranchChanged) {
            return false;
        }

        Optional<NetworkTransfer> oldOptionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(oldEntity.getReceivingBranch()), oldEntity.getId(), CONSOLIDATION);

        if(oldOptionalNetworkTransfer.isEmpty())
            return true;

        return oldOptionalNetworkTransfer
                .map(networkTransfer -> networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED)
                .orElse(false) || optionalNetworkTransfer.isEmpty();
    }

    private void createOrUpdateQuartzJob(ConsolidationDetails consolidationDetails, QuartzJobInfo existingJob) {
        CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();

        LocalDateTime jobTime = quartzJobInfoService.getQuartzJobTime(
                carrierDetails.getEta(), carrierDetails.getEtd(), carrierDetails.getAta(), carrierDetails.getAtd(),
                consolidationDetails.getTransportMode());

        if(jobTime == null)
            return;

        QuartzJobInfo quartzJobInfo = (existingJob != null) ? existingJob : createNewQuartzJob(consolidationDetails);
        quartzJobInfo.setJobStatus(JobState.QUEUED);
        quartzJobInfo.setErrorMessage(null);
        quartzJobInfo.setStartTime(jobTime);

        QuartzJobInfo newQuartzJobInfo = quartzJobInfoDao.save(quartzJobInfo);

        if(existingJob!=null && quartzJobInfoService.isJobWithNamePresent(newQuartzJobInfo.getId().toString())){
            quartzJobInfoService.updateSimpleJob(newQuartzJobInfo);
        }else{
            quartzJobInfoService.createSimpleJob(newQuartzJobInfo);
        }
        deleteAllConsoleErrorsLogs(consolidationDetails);
    }

    private QuartzJobInfo createNewQuartzJob(ConsolidationDetails consolidationDetails) {
        return QuartzJobInfo.builder()
                .entityId(consolidationDetails.getId())
                .entityType(CONSOLIDATION)
                .tenantId(consolidationDetails.getTenantId())
                .jobType(JobType.SIMPLE_JOB)
                .build();
    }


    private boolean isValidforAutomaticTransfer(QuartzJobInfo quartzJobInfo, ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isDocAdded) {
        if (isValidDateChange(consolidationDetails, oldEntity))
            return true;

        if(quartzJobInfo==null ||(quartzJobInfo.getJobStatus() != JobState.ERROR))
            return false;

        if(Boolean.TRUE.equals(isDocAdded))
            return true;

        CarrierDetails newCarrierDetails = consolidationDetails.getCarrierDetails();

        // If oldCarrierDetails is null, check if newCarrierDetails has any populated fields.
        if (oldEntity == null || oldEntity.getCarrierDetails() == null) {
            return newCarrierDetails.getEta() != null ||
                    newCarrierDetails.getEtd() != null ||
                    newCarrierDetails.getAta() != null ||
                    newCarrierDetails.getAtd() != null;
        }

        if (isAirStandardCase(consolidationDetails)) {
            return isAirStandardCaseChanged(oldEntity, newCarrierDetails);
        }
        if (isAirNonStandardNonDrtCase(consolidationDetails)) {
            return isAirNonStandardNonDrtCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
        }

        if (isSeaCase(consolidationDetails)) {
            return isSeaCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
        }

        // Compare individual fields for changes.
        return isDefaultCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
    }


    private boolean isAirStandardCaseChanged(ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd());
    }

    private boolean isAirStandardCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR) &&
                Objects.equals(consolidationDetails.getConsolidationType(), SHIPMENT_TYPE_STD);
    }

    private boolean isAirNonStandardNonDrtCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isValueChanged(consolidationDetails.getBol(), oldEntity.getBol());
    }

    private boolean isAirNonStandardNonDrtCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR) &&
                !Objects.equals(consolidationDetails.getConsolidationType(), SHIPMENT_TYPE_STD) &&
                !Objects.equals(consolidationDetails.getConsolidationType(), CONSOLIDATION_TYPE_DRT);
    }

    private boolean isSeaCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isValueChanged(newCarrierDetails.getVessel(), oldCarrierDetails.getVessel()) ||
                isValueChanged(newCarrierDetails.getShippingLine(), oldCarrierDetails.getShippingLine()) ||
                isValueChanged(newCarrierDetails.getVoyage(), oldCarrierDetails.getVoyage()) ||
                isAgentChanged(consolidationDetails, oldEntity) ||
                isValueChanged(consolidationDetails.getBol(), oldEntity.getBol());
    }

    private boolean isSeaCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_SEA);
    }

    private boolean isAgentChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        return (consolidationDetails.getSendingAgent() != null && oldEntity.getSendingAgent() == null) ||
                (consolidationDetails.getSendingAgent() != null && oldEntity.getSendingAgent() != null &&
                        isValueChanged(consolidationDetails.getSendingAgent().getOrgCode(), oldEntity.getSendingAgent().getOrgCode())) ||
                (consolidationDetails.getReceivingAgent() != null && oldEntity.getReceivingAgent() == null) ||
                (consolidationDetails.getReceivingAgent() != null && oldEntity.getReceivingAgent() != null &&
                        isValueChanged(consolidationDetails.getReceivingAgent().getOrgCode(), oldEntity.getReceivingAgent().getOrgCode()));
    }

    private boolean isDefaultCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isAgentChanged(consolidationDetails, oldEntity);
    }

    private boolean isValueChanged(Object newValue, Object oldValue) {
        return (oldValue != null && newValue==null) || (newValue != null && !newValue.equals(oldValue));
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

    public void syncMainLegRoute(ConsolidationDetailsRequest consolidationDetailsRequest, ConsolidationDetails oldEntity) {
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()) && Constants.TRANSPORT_MODE_AIR.equals(consolidationDetailsRequest.getTransportMode())) {
            List<RoutingsRequest> routingsRequests = consolidationDetailsRequest.getRoutingsList();
            if (oldEntity == null || !Objects.equals(consolidationDetailsRequest.getCarrierDetails().getFlightNumber(), oldEntity.getCarrierDetails().getFlightNumber())
                    || !Objects.equals(consolidationDetailsRequest.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine())) {
                routingsRequests.stream().filter(i -> (RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage())
                                && Objects.equals(consolidationDetailsRequest.getCarrierDetails().getOriginPort(), i.getPol())
                                && Objects.equals(consolidationDetailsRequest.getCarrierDetails().getDestinationPort(), i.getPod())))
                        .forEach(i -> {
                            i.setFlightNumber(consolidationDetailsRequest.getCarrierDetails().getFlightNumber());
                            i.setCarrier(consolidationDetailsRequest.getCarrierDetails().getShippingLine());
                        });
            }
        }
    }

    private List<EventsRequest> setEventDetails(List<EventsRequest> eventsRequestList, ConsolidationDetails consolidationDetails) {
        if(eventsRequestList != null && !eventsRequestList.isEmpty()) {
            for (EventsRequest req : eventsRequestList) {
                req.setConsolidationId(consolidationDetails.getId());
            }
        }
        return eventsRequestList;
    }

    private boolean checkForAwbUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(consolidationDetails.getSci(), oldEntity.getSci())) return true;
        return !Objects.equals(consolidationDetails.getEfreightStatus(), oldEntity.getEfreightStatus());
    }

    private void createLogHistoryForConsole(ConsolidationDetails consolidationDetails){
        try {
            String entityPayload = jsonHelper.convertToJson(consolidationDetails);
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(consolidationDetails.getId())
                    .entityType(Constants.CONSOLIDATION).entityGuid(consolidationDetails.getGuid()).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Consolidation : " + ex.getMessage());
        }
    }
    private void createLogHistoryForShipment(ShipmentDetails shipmentDetails){
        try {
            String entityPayload = jsonHelper.convertToJson(shipmentDetails);
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(shipmentDetails.getId())
                    .entityType(Constants.SHIPMENT).entityGuid(shipmentDetails.getGuid()).entityPayload(entityPayload).tenantId(shipmentDetails.getTenantId()).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory : " + ex.getMessage());
        }
    }

    @Override
    public void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, List<Containers> oldContainers)
    {
        try {
            if(consolidationDetails.getTenantId() == null)
                consolidationDetails.setTenantId(TenantContext.getCurrentTenant());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(consolidationDetails, isCreate);
            kafkaResponse.setTransactionId(UUID.randomUUID().toString());
            log.info("Producing consolidation data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, StringUtility.convertToString(consolidationDetails.getGuid()));
        }
        catch (Exception e) {
            log.error("Error Producing consolidation to kafka, error is due to " + e.getMessage());
        }
        try {
            if(trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails) || trackingServiceAdapter.checkIfAwbExists(consolidationDetails)) {
                UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails);
                List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                if(_utPayload != null) {
                    trackingPayloads.add(_utPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from consolidation with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                }
            }
            if(consolidationDetails != null) {
                var events = trackingServiceAdapter.getAllEvents(null,consolidationDetails, consolidationDetails.getReferenceNumber());
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(consolidationDetails.getReferenceNumber(),Constants.CONSOLIDATION, consolidationDetails.getConsolidationNumber(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from consolidation with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, true);
                }
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        try {
            containerService.pushContainersToDependentServices(consolidationDetails.getContainersList(), oldContainers);
        }
        catch (Exception e) {
            log.error("Error producing message due to " + e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getConsolFromShipment(Long shipmentId) {
        ConsolidationDetailsResponse consol;
        Optional<ShipmentDetails> shipmentRes = shipmentDao.findById(shipmentId);

        if (shipmentRes.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the ShipmentId with id " + shipmentId);

        var shipment = modelMapper.map(shipmentRes.get(), ShipmentDetailsResponse.class);

        var additionalDetails = shipment.getAdditionalDetails();
        var shipmentCarrierDetails = shipment.getCarrierDetails();
        var tenantSettings = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
        if (tenantSettings.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the shipment settings for tenant id " + TenantContext.getCurrentTenant());

        boolean isPayment = tenantSettings.get().getShipmentLite()
                && shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipment.getDirection().equals(Constants.DIRECTION_EXP);

        boolean isMawb = tenantSettings.get().getShipmentLite()
                && shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipment.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT);

        String transportMode = shipment.getTransportMode() == null ? tenantSettings.get().getDefaultTransportMode(): shipment.getTransportMode();
        String sci = null;
        if(Objects.equals(additionalDetails.getSci(), AwbConstants.T1)) {
            sci = AwbConstants.T1;
        }

        consol = ConsolidationDetailsResponse.builder()
                .consolidationType(shipment.getJobType())
                .transportMode(transportMode)
                .containerCategory(shipment.getShipmentType() == null ? tenantSettings.get().getDefaultContainerType() : shipment.getShipmentType())
                .declarationType(additionalDetails != null ? additionalDetails.getCustomDeclType() : null)
                .carrierDetails(CarrierDetailResponse.builder()
                        .origin(shipmentCarrierDetails != null ? shipmentCarrierDetails.getOriginPort() : null)
                        .destination(shipmentCarrierDetails != null ? shipmentCarrierDetails.getDestinationPort() : null)
                        .vessel(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVessel() : null)
                        .originPort(shipmentCarrierDetails != null ? shipmentCarrierDetails.getOriginPort() : null)
                        .destinationPort(shipmentCarrierDetails != null ? shipmentCarrierDetails.getDestinationPort() : null)
                        .eta(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEta() : null)
                        .etd(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEtd() : null)
                        .ata(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAta() : null)
                        .atd(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAtd() : null)
                        .aircraftType(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAircraftType() : null)
                        .flightNumber(shipmentCarrierDetails != null ? shipmentCarrierDetails.getFlightNumber() : null)
                        .shippingLine(shipmentCarrierDetails != null ? shipmentCarrierDetails.getShippingLine() : null) // carrier
                        .voyage(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVoyage() : null)
                        .build())
                .departureDetails(ArrivalDepartureDetailsResponse.builder().firstForeignPort(shipmentCarrierDetails.getOriginPort())
                        .lastForeignPort(shipmentCarrierDetails.getDestinationPort()).build())
                .releaseType(additionalDetails != null ? additionalDetails.getReleaseType() : null)
                .original(additionalDetails != null ? additionalDetails.getOriginal() : null)
                .copy(additionalDetails != null ? additionalDetails.getCopy() : null)
                .sci(sci)
                .allocations(AllocationsResponse.builder()
//                        .weight(shipment.getWeight()) // commented just like the v1 code
                        .weightUnit(shipment.getWeightUnit())
//                        .volume(shipment.getVolume())
                        .volumeUnit(shipment.getVolumeUnit())
//                        .chargable(shipment.getChargable())
                        .chargeableUnit(shipment.getChargeableUnit())
                        .build())
                .shipmentType(shipment.getDirection() == null ? null : shipment.getDirection())
                .igmFileDate(additionalDetails != null ? additionalDetails.getIGMFileDate() : null)
                .igmFileNo(additionalDetails != null ? additionalDetails.getIGMFileNo() : null)
                .smtpigmDate(additionalDetails != null ? additionalDetails.getSMTPIGMDate() : null)
                .smtpigmNumber(additionalDetails != null ? additionalDetails.getSMTPIGMNumber() : null)
                .igmInwardDate(additionalDetails != null ? additionalDetails.getIGMInwardDate() : null)
                .inwardDateAndTime(additionalDetails != null ? additionalDetails.getInwardDateAndTime() : null)
                .warehouseId(additionalDetails != null ? additionalDetails.getWarehouseId() : null)
                .bol(shipment.getMasterBill() != null && !shipment.getMasterBill().isEmpty() ? shipment.getMasterBill() : ((transportMode == null || !transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) ? generateCustomBolNumber() : null))
                .referenceNumber(shipment.getBookingReference())
                .payment(isPayment ? shipment.getPaymentTerms() : null)
                .mawb(isMawb ? shipment.getMasterBill() : null)
                .createdBy(UserContext.getUser().getUsername())
                .modeOfBooking(StringUtils.equals(transportMode, Constants.TRANSPORT_MODE_SEA) ? Constants.INTTRA : null)
                .creatingFromDgShipment(shipment.getContainsHazardous())
                .openForAttachment(true)
                //.isLinked(true)
                .build();

        consol.setDepartment(commonUtils.getAutoPopulateDepartment(
                consol.getTransportMode(), consol.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE
        ));

        if(additionalDetails != null) {
            PartiesResponse parties;
            if(additionalDetails.getImportBroker() != null) {
                parties = jsonHelper.convertValue(additionalDetails.getImportBroker(), PartiesResponse.class);
                parties.setId(null);
                parties.setGuid(null);
                consol.setReceivingAgent(parties);
            }
            if(additionalDetails.getExportBroker() != null) {
                parties = jsonHelper.convertValue(additionalDetails.getExportBroker(), PartiesResponse.class);
                parties.setId(null);
                parties.setGuid(null);
                consol.setSendingAgent(parties);
            }
        }
        if(!commonUtils.checkIfPartyExists(consol.getSendingAgent())) {
            consol.setSendingAgentCountry(commonUtils.getCountryFromUnLocCode(shipmentCarrierDetails.getOriginPortLocCode()));
        }
        if(!commonUtils.checkIfPartyExists(consol.getReceivingAgent())) {
            consol.setReceivingAgentCountry(commonUtils.getCountryFromUnLocCode(shipmentCarrierDetails.getDestinationPortLocCode()));
        }

        createConsolidationPayload(modelMapper.map(consol, ConsolidationDetails.class), consol);

        return ResponseHelper.buildSuccessResponse(consol);
    }
    @Override
    public ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel){
        AutoAttachConsolidationRequest request = (AutoAttachConsolidationRequest) commonRequestModel.getData();
        AutoAttachConsolidationResponse response = new AutoAttachConsolidationResponse();

        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }

        List<Integer> itemTypeList = new ArrayList<>();
        itemTypeList.add(MasterDataType.CONSOLIDATION_CHECK_ORDER.getId());
        itemTypeList.add(MasterDataType.CONSOL_CHECK_ETD_ETD_THRESHOLD.getId());
        itemTypeList.add(MasterDataType.AUTO_ATTACH_TRANSPORT.getId());
        List<Object> masterDataCriteria = Arrays.asList(
                Arrays.asList(MasterDataConstants.ITEM_TYPE),
                "in",
                Arrays.asList(itemTypeList)
        );
        CommonV1ListRequest masterDataRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(masterDataCriteria).build();
        V1DataResponse masterDataResponse = v1Service.fetchMasterData(masterDataRequest);
        List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(masterDataResponse.entities, EntityTransferMasterLists.class);

        var applicableTransportModes = masterLists.stream()
                .filter(x -> x.getItemType() == (int)MasterDataType.AUTO_ATTACH_TRANSPORT.getId())
                .map(EntityTransferMasterLists::getItemDescription)
                .map(description -> description != null ? Arrays.asList(description.split(",")): null)
                .findFirst();
        List<String> applicableTransportModesList = applicableTransportModes.orElse(null);

        boolean isConditionSatisfied = false;
        boolean isMasterBillPresent = false;
        ListCommonRequest consolListRequest = request.getFilterCriteria() != null ? request : null;

        if(!Strings.isNullOrEmpty(request.getTransportMode()) && ((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) || (applicableTransportModesList != null &&
                applicableTransportModesList.contains(request.getTransportMode().toUpperCase())))) {

            consolListRequest = CommonUtils.andCriteria("transportMode", request.getTransportMode(), "=", consolListRequest);
            consolListRequest = CommonUtils.andCriteria(Constants.OPEN_FOR_ATTACHMENT, true, "=", consolListRequest);
            if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                    && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
                if(!Objects.isNull(request.getDirection()))
                    consolListRequest = CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, request.getDirection(), "=", consolListRequest);
                if(!Objects.isNull(request.getShipmentType()))
                    consolListRequest = CommonUtils.andCriteria(Constants.CONTAINER_CATEGORY, request.getShipmentType(), "=", consolListRequest);

                if (InterBranchContext.getContext().getHubTenantIds() != null
                        && !InterBranchContext.getContext().getHubTenantIds().isEmpty()) {
                    List<FilterCriteria> criterias = consolListRequest.getFilterCriteria();
                    List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
                    Criteria criteria = Criteria.builder().fieldName(Constants.INTER_BRANCH_CONSOLE).operator("=").value(true).build();
                    FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                    List<FilterCriteria> innerFilers1 = new ArrayList<>();
                    innerFilers1.add(filterCriteria);
                    criteria = Criteria.builder().fieldName(Constants.TENANT_ID).operator("IN").value(InterBranchContext.getContext().getHubTenantIds()).build();
                    filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("and").build();
                    innerFilers1.add(filterCriteria);
                    FilterCriteria filterCriteria1 = FilterCriteria.builder().innerFilter(innerFilers1).build();
                    List<FilterCriteria> innerFilers2 = new ArrayList<>();
                    innerFilers2.add(filterCriteria1);

                    criteria = Criteria.builder().fieldName(Constants.TENANT_ID).operator("=").value(UserContext.getUser().TenantId).build();
                    filterCriteria1 = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                    innerFilers2.add(filterCriteria1);

                    FilterCriteria filterCriteria2 = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers2).build();
                    innerFilters.add(filterCriteria2);
                }

                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentIdAll(request.getShipId());
                List<Long> excludeConsolidation = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
                if(excludeConsolidation != null && !excludeConsolidation.isEmpty())
                    consolListRequest = CommonUtils.andCriteria("id", excludeConsolidation, "NOTIN", consolListRequest);
                isConditionSatisfied = true;
            } else {
                if (!Strings.isNullOrEmpty(request.getMasterBill())) {
                    consolListRequest = CommonUtils.andCriteria("bol", request.getMasterBill(), "=", consolListRequest);
                    isConditionSatisfied = true;
                    isMasterBillPresent = true;
                    response.setFilteredDetailName("Master Bill");
                } else if (request.getEta() != null && request.getEtd() != null) {
                    var thresholdDetails = masterLists.stream()
                            .filter(x -> x.getItemType() == (int) MasterDataType.CONSOL_CHECK_ETD_ETD_THRESHOLD.getId())
                            .map(EntityTransferMasterLists::getItemDescription)
                            .findFirst();
                    Long thresholdLimit = 0L;
                    if (thresholdDetails.isPresent()) {
                        thresholdLimit = Long.parseLong(thresholdDetails.get());
                    }
                    Long negativeThresholdLimit = thresholdLimit * -1;
                    LocalDateTime eta = request.getEta();
                    LocalDateTime etd = request.getEtd();

                    var thresholdETAFrom = eta.plusDays(negativeThresholdLimit);
                    var thresholdETATo = eta.plusDays(thresholdLimit + 1).plusSeconds(-1);
                    var thresholdETDFrom = etd.plusDays(negativeThresholdLimit);
                    var thresholdETDTo = etd.plusDays(thresholdLimit + 1).plusSeconds(-1);

                    var etaAndETDCriteria = CommonUtils.andCriteria("eta", thresholdETAFrom, ">=", consolListRequest);
                    etaAndETDCriteria = CommonUtils.andCriteria("eta", thresholdETATo, "<=", etaAndETDCriteria);
                    etaAndETDCriteria = CommonUtils.andCriteria("etd", thresholdETDFrom, ">=", etaAndETDCriteria);
                    etaAndETDCriteria = CommonUtils.andCriteria("etd", thresholdETDTo, "<=", etaAndETDCriteria);

                    var priorityList = masterLists.stream()
                            .filter(x -> x.getItemType() == (int) MasterDataType.CONSOLIDATION_CHECK_ORDER.getId())
                            .sorted((y1, y2) -> {
                                int itd1 = Integer.parseInt(y1.getItemDescription());
                                int itd2 = Integer.parseInt(y2.getItemDescription());
                                return Integer.compare(itd1, itd2);
                            })
                            .map(EntityTransferMasterLists::getItemValue)
                            .toList();
                    for (var item : priorityList) {
                        switch (item.toUpperCase()) {
                            case "VOYAGE NUMBER":
                                if (StringUtility.isNotEmpty(request.getVoyageNumber())) {
                                    consolListRequest = CommonUtils.andCriteria("voyage", request.getVoyageNumber(), "=", etaAndETDCriteria);
                                    isConditionSatisfied = true;
                                    response.setFilteredDetailName("Voyage Number");
                                }
                                break;
                            case "VESSEL NAME":
                                if (StringUtility.isNotEmpty(request.getVessel())) {
                                    consolListRequest = CommonUtils.andCriteria("vessel", request.getVessel(), "=", etaAndETDCriteria);
                                    isConditionSatisfied = true;
                                    response.setFilteredDetailName("Vessel Name");
                                }
                                break;
                            case "ORIGIN PORT/ DESTINATION PORT":
                                if (StringUtility.isNotEmpty(request.getPol()) && StringUtility.isNotEmpty(request.getPod())) {
                                    if (!Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled()) || !Objects.equals(Constants.TRANSPORT_MODE_AIR, request.getTransportMode()))
                                        consolListRequest = CommonUtils.andCriteria("originPort", request.getPol(), "=", etaAndETDCriteria);
                                    consolListRequest = CommonUtils.andCriteria("destinationPort", request.getPod(), "=", consolListRequest);
                                    isConditionSatisfied = true;
                                    response.setFilteredDetailName("Origin Port/ Destination Port");
                                }
                                break;
                            default:
                        }
                        if (isConditionSatisfied)
                            break;
                    }
                }
            }
            if (isConditionSatisfied){
                Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(consolListRequest, ConsolidationDetails.class, tableNames);
                Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
                List<ConsolidationDetailsResponse> consolidationDetailsResponseList = new ArrayList<>();
                if (!isMasterBillPresent && Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(request.getDirection(), Constants.DIRECTION_EXP)
                        && (Objects.equals(request.getShipmentType(), Constants.CARGO_TYPE_FCL) || Objects.equals(request.getShipmentType(), Constants.SHIPMENT_TYPE_LCL))) {
                    for (var console : consolidationDetailsPage.getContent()) {
                        ConsolidationDetailsResponse consolidationDetailsResponse = this.partyCheckForConsole(console, request);
                        if (consolidationDetailsResponse != null) {
                            consolidationDetailsResponseList.add(consolidationDetailsResponse);
                        }
                    }
                }
                else {
                    consolidationDetailsResponseList = jsonHelper.convertValueToList(consolidationDetailsPage.getContent(), ConsolidationDetailsResponse.class);
                }

                for (var console : consolidationDetailsResponseList){
                    if(console.getConsolidationNumber() == null)
                        console.setConsolidationNumber("");
                    if(console.getCarrierDetails().getVoyage() == null)
                        console.getCarrierDetails().setVoyage("");
                }
                List<IRunnerResponse> responseList = new ArrayList<>();
                consolidationDetailsResponseList.forEach(responseList::add);
                try {
                    var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorService);
                    var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorService);
                    var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorService);
                    CompletableFuture.allOf(vesselDataFuture, tenantDataFuture, locationDataFuture).join();
                }
                catch (Exception ex) {
                    log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
                }
                response.setConsolidationDetailsList(responseList);
                return ResponseHelper.buildSuccessResponse(response, consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        }

        return ResponseHelper.buildSuccessResponse(response);
    }

    private ConsolidationDetailsResponse partyCheckForConsole(ConsolidationDetails consolidationDetails, AutoAttachConsolidationRequest request) {
        List<ShipmentDetails> shipmentDetailsList = consolidationDetails.getShipmentsList();
        ShipmentSettingsDetails  shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        Parties client;
        Parties consigner;
        Parties consignee;
        boolean isLcl = true;
        boolean isFcl = true;
        if(shipmentDetailsList != null && !shipmentDetailsList.isEmpty()) {
            client = shipmentDetailsList.get(0).getClient();
            consigner = shipmentDetailsList.get(0).getConsigner();
            consignee = shipmentDetailsList.get(0).getConsignee();
            if(Objects.equals(shipmentDetailsList.get(0).getShipmentType(), Constants.CARGO_TYPE_FCL))
                isLcl = false;
            else if(Objects.equals(shipmentDetailsList.get(0).getShipmentType(), Constants.SHIPMENT_TYPE_LCL))
                isFcl = false;
            else {
                isLcl = false;
                isFcl = false;
            }
            if(shipmentDetailsList.size() > 1) {
                for (var ship : shipmentDetailsList) {
                    if(Objects.equals(ship.getShipmentType(), Constants.CARGO_TYPE_FCL))
                        isLcl = false;
                    else if(Objects.equals(ship.getShipmentType(), Constants.SHIPMENT_TYPE_LCL))
                        isFcl = false;
                    else {
                        isLcl = false;
                        isFcl = false;
                    }
                    if (client == null || ship.getClient() == null || (!Objects.equals(client.getOrgCode(), ship.getClient().getOrgCode()) || !Objects.equals(client.getAddressCode(), ship.getClient().getAddressCode())))
                        client = new Parties();
                    if(consigner == null || ship.getConsigner() == null || (!Objects.equals(consigner.getOrgCode(), ship.getConsigner().getOrgCode()) || !Objects.equals(consigner.getAddressCode(), ship.getConsigner().getAddressCode())))
                        consigner = new Parties();
                    if(consignee == null || ship.getConsignee() == null || (!Objects.equals(consignee.getOrgCode(), ship.getConsignee().getOrgCode()) || !Objects.equals(consignee.getAddressCode(), ship.getConsignee().getAddressCode())))
                        consignee = new Parties();
                }
            }
        } else {
            return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        }
        if(isLcl && Objects.equals(request.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) {
            return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        }

        //Party check added based on flag
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getEnablePartyCheckForConsolidation()) && Objects.equals(request.getShipmentType(), Constants.CARGO_TYPE_FCL) && isFcl &&
                ((request.getClient() != null && request.getClient().getOrgCode() != null && client != null && Objects.equals(request.getClient().getOrgCode(), client.getOrgCode()) && Objects.equals(request.getClient().getAddressCode(), client.getAddressCode())) ||
                (request.getConsigner() != null && request.getConsigner().getOrgCode() != null && consigner != null && Objects.equals(request.getConsigner().getOrgCode(), consigner.getOrgCode()) && Objects.equals(request.getConsigner().getAddressCode(), consigner.getAddressCode())) ||
                (request.getConsignee() != null && request.getConsignee().getOrgCode() != null && consignee != null && Objects.equals(request.getConsignee().getOrgCode(), consignee.getOrgCode()) && Objects.equals(request.getConsignee().getAddressCode(), consignee.getAddressCode())))) {
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
            response.setClient(jsonHelper.convertValue(client, PartiesResponse.class));
            response.setConsigner(jsonHelper.convertValue(consigner, PartiesResponse.class));
            response.setConsignee(jsonHelper.convertValue(consignee, PartiesResponse.class));
            return response;
        } else if (Boolean.FALSE.equals(shipmentSettingsDetails.getEnablePartyCheckForConsolidation()) && isFcl && Objects.equals(request.getShipmentType(), Constants.CARGO_TYPE_FCL)) {
            return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        }
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationDetailsDao.findById(id);
            if(!consolidationDetailsOptional.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
            ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
            calculateDescOfGoodsAndHandlingInfo(consolidationDetails, consolidationDetailsResponse, true);
            return ResponseHelper.buildSuccessResponse(consolidationDetailsResponse.getContainersList());
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainerPackSummary(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            ContainerPackSummaryDto response = new ContainerPackSummaryDto();
            response.setPacksList(new ArrayList<>());
            List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(id);
            if(consoleShipmentMappingList != null && consoleShipmentMappingList.size() > 0) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, consoleShipmentMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList(), "IN", null);
                andCriteria(Constants.CONTAINER_ID, "", "ISNOTNULL", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                Map<Long, ShipmentDetails> shipMap = new HashMap<>();
                Map<Long, Containers> contMap = new HashMap<>();
                if(packings != null && packings.getContent() != null && !packings.getContent().isEmpty()) {
                    for (Packing packing : packings.getContent()) {
                        ContainerPackSummaryDto.PacksList packsList = jsonHelper.convertValue(packing, ContainerPackSummaryDto.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(shipMap, packing.getShipmentId());
                        packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                        packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                        packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                        packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                        try { packsList.setShipmentStatus(ShipmentStatus.fromValue(shipmentDetails.getStatus())); }
                        catch (Exception e) { }
                        try { Containers containers = getContainerFromMap(contMap, packing.getContainerId());
                            packsList.setContainerNumber(containers.getContainerNumber()); }
                        catch (Exception e) { }
                        response.getPacksList().add(packsList);
                    }
                }
            }
            try {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<MasterListRequest> listRequests = new HashSet<>();
                if(!Objects.isNull(response.getPacksList()))
                    response.getPacksList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ContainerPackSummaryDto.PacksList.class, fieldNameKeyMap, ContainerPackSummaryDto.PacksList.class.getSimpleName(), cacheMap)));
                MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
                masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
                masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));
                Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
                Set<String> keys = new HashSet<>();
                commonUtils.createMasterDataKeysList(listRequests, keys);
                masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);
                if(!Objects.isNull(response.getPacksList()))
                    response.getPacksList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ContainerPackSummaryDto.PacksList.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap)));
            } catch (Exception e) { }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private boolean shouldGenerateBol(String transportMode) {
        return !Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultConsolidation() {
        String responseMsg;
        try {
            var tenantSettings = commonUtils.getShipmentSettingFromContext();
            // Populate shipment details on basis of tenant settings
            ConsolidationDetailsResponse response = new ConsolidationDetailsResponse();
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setContainerCategory(tenantSettings.getDefaultContainerType());
            response.setShipmentType(tenantSettings.getDefaultShipmentType());
            response.setBol(shouldGenerateBol(response.getTransportMode()) ? generateCustomBolNumber() : null);
            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(response.getTransportMode())) {
                response.setModeOfBooking(Constants.INTTRA);
            }
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCreatedAt(LocalDateTime.now());
            response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

            // Populate default department
            response.setDepartment(commonUtils.getAutoPopulateDepartment(
                    response.getTransportMode(), response.getShipmentType(), MdmConstants.CONSOLIDATION_MODULE
            ));

            try {
                log.info("Fetching Tenant Model");
                TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
                Optional.ofNullable(masterDataUtils.fetchUnlocationByOneIdentifier(
                        EntityTransferConstants.ID,
                        StringUtility.convertToString(tenantModel.getUnloco())
                    ))
                    .filter(list -> !list.isEmpty())
                    .map(list -> list.get(0).getLocationsReferenceGUID())
                    .ifPresent(response::setPlaceOfIssue);

                PartiesResponse partiesResponse = v1ServiceUtil.getDefaultAgentOrg(tenantModel);
                if(Constants.DIRECTION_EXP.equals(response.getShipmentType())) {
                    response.setSendingAgent(partiesResponse);
                } else if(Constants.DIRECTION_IMP.equals(response.getShipmentType())) {
                    response.setReceivingAgent(partiesResponse);
                }
            } catch (Exception e){
                log.error("Failed in fetching tenant data from V1 with error : {}", e);
            }
            this.createConsolidationPayload(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_RETRIEVE_EMPTY_REQUEST, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!consolidationDetails.isPresent()) {
                log.debug(CONSOLIDATION_DETAILS_NULL, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().id(consolidationDetails.get().getId()).build());
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
                log.error(CONSOLIDATION_RETRIEVE_EMPTY_REQUEST, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error(ConsolidationConstants.CONSOLIDATION_RETRIEVE_NULL_REQUEST, LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getId());
            if (!consolidationDetails.isPresent()) {
                log.debug(CONSOLIDATION_DETAILS_NULL, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().guid(consolidationDetails.get().getGuid()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> checkContainerEditingRequiredForOceanDg(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_RETRIEVE_EMPTY_REQUEST, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("Request is null");
            }
            if (request.getId() == null) {
                log.error(ConsolidationConstants.CONSOLIDATION_RETRIEVE_NULL_REQUEST, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("Id is null");
            }
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getId());
            if (!consolidationDetails.isPresent()) {
                log.debug(CONSOLIDATION_DETAILS_NULL, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            Map<Long, Boolean> containerIdDgAllowedMap = new HashMap<>();
            if(consolidationDetails.get().getContainersList() != null)
            {
                for(Containers containers: consolidationDetails.get().getContainersList()) {
                    boolean allowEdit = true;
                    if(containers.getShipmentsList() != null)
                    {
                        for(ShipmentDetails shipmentDetails: containers.getShipmentsList()) {
                            if(Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) &&
                                    (OceanDGStatus.OCEAN_DG_REQUESTED.equals(shipmentDetails.getOceanDGStatus()) || OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED.equals(shipmentDetails.getOceanDGStatus()))) {
                                allowEdit = false;
                                break;
                            }
                        }
                    }
                    containerIdDgAllowedMap.put(containers.getId(), allowEdit);
                }
            }
            return ResponseHelper.buildSuccessResponse(containerIdDgAllowedMap);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomBolNumber()).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    public String generateCustomBolNumber() {
        String res = null;
        ShipmentSettingsDetails tenantSetting = commonUtils.getShipmentSettingFromContext();

        if(tenantSetting.getConsolidationLite() != null && tenantSetting.getConsolidationLite() && tenantSetting.getBolNumberGeneration() == null) {
            return  res;
        }

        if(tenantSetting.getBolNumberGeneration() != null) {
            res = tenantSetting.getBolNumberPrefix() != null ? tenantSetting.getBolNumberPrefix() : "";
            switch(tenantSetting.getBolNumberGeneration()) {
                case Random :
                    res += StringUtility.getRandomString(10);
                    break;
                default :
                    String serialNumber = v1Service.getMaxConsolidationId();
                    res += serialNumber;
                    break;
            }
        }

        return res;
    }

    // Create Auto event

    public void generateEvents(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getEventsList() == null) {
            consolidationDetails.setEventsList(new ArrayList<>());
        }
        consolidationDetails.getEventsList().add(createEvent(consolidationDetails, EventConstants.COCR));
    }

    private Events createEvent(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(LocalDateTime.now());
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.CONSOLIDATION);
        events.setEntityId(consolidationDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setConsolidationId(consolidationDetails.getId());
        events.setDirection(consolidationDetails.getShipmentType());
        // Persist the event
        eventDao.save(events);
        return events;
    }
    public ResponseEntity<IRunnerResponse> validateMawbNumber(CommonRequestModel commonRequestModel) {
        ValidateMawbNumberRequest request = (ValidateMawbNumberRequest) commonRequestModel.getData();
        ValidateMawbNumberResponse response = ValidateMawbNumberResponse.builder().success(true).build();
        if(Strings.isNullOrEmpty(request.getMawb())){
            return ResponseHelper.buildSuccessResponse(response);
        }
        if(!consolidationDetailsDao.isMAWBNumberValid(request.getMawb())){
            throw new ValidationException("Please enter a valid MAWB number.");
        }
        String mawbAirlineCode = request.getMawb().substring(0, 3);
        V1DataResponse v1DataResponse =  fetchCarrierDetailsFromV1(mawbAirlineCode, request.getType());
        List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
        if (carrierDetails == null || carrierDetails.isEmpty())
            throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");
        var correspondingCarrier = carrierDetails.get(0).getItemValue();
        if (!Strings.isNullOrEmpty(request.getShippingLine()) && !request.getShippingLine().equals(correspondingCarrier)){
            response = ValidateMawbNumberResponse.builder()
                    .success(false)
                    .message("MAWB Number prefix is not matching with entered Flight Carrier. Do you want to proceed?")
                    .build();
        }
        return ResponseHelper.buildSuccessResponse(response);
    }
    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode, String type) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("AirlineCode"), "=", mawbAirlineCode));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("HasAirPort"), "=", 1));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        carrierListObject.setType(type);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> updateConsoleBookingFields(CommonRequestModel commonRequestModel){
        ConsoleBookingRequest request = (ConsoleBookingRequest) commonRequestModel.getData();
        int rowsAffected = consolidationDetailsDao.updateConsoleBookingFields(request);
        if(rowsAffected == 0){
            throw new ValidationException("No Consolidation Exist with given guid: " + request.getGuid());
        }
        return ResponseHelper.buildSuccessResponse();
    }

    public ResponseEntity<IRunnerResponse> showCreateBooking(String operation) throws RunnerException {
        if(operation.equals("CREATE") && (PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_CREATE) == null || PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_CREATE).size() == 0)){
            throw new RunnerException("You don't have necessary permission to create Carrier Booking.");
        }

        if(operation.equals("VIEW") && (PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_VIEW) == null || PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_VIEW).size() == 0)){
            throw new RunnerException("You don't have necessary permission to view Carrier Booking.");
        }

        return new ResponseEntity<>(null, HttpStatus.OK);
    }

    public ResponseEntity<IRunnerResponse> consolidationRetrieveWithMeasurmentBasis(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            UUID guid = UUID.fromString(request.getGuid());
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(guid);
            if (!consolidationDetails.isPresent()) {
                log.debug(CONSOLIDATION_DETAILS_NULL, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            MeasurementBasisResponse response = null;
            if (consolidationDetails.get().getAllocations() != null) {
                response = modelMapper.map(consolidationDetails.get().getAllocations(), MeasurementBasisResponse.class);
            } else {
                response = new MeasurementBasisResponse();
            }
            calculatePacksAndPacksUnit(consolidationDetails.get().getPackingList(), response);
            calculateContainersAndTeu(response, consolidationDetails.get().getContainersList());
            calculateChargableAndChargableUnit(consolidationDetails.get(), response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> mblCheck(String mblNumber) {
        List<ConsolidationDetailsProjection> consolidationDetails = consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber);

        if (ObjectUtils.isNotEmpty(consolidationDetails)) {
            String message = "The MBL provided already exists for " +
                    consolidationDetails.stream()
                            .map(cd -> "Consolidation No. " + cd.getConsolidationNumber() + " in branch " + getTenantId(cd))
                            .collect(Collectors.joining(", ")) +
                    ". Request to either consolidation transfer or attachment to the existing consolidation";

            return ResponseHelper.buildSuccessResponse(MblCheckResponse.builder().message(message).build());
        }

        return ResponseHelper.buildSuccessResponse(MblCheckResponse.builder().build());
    }

    private String getTenantId(ConsolidationDetailsProjection sd) {
        Integer tenantId = sd.getTenantId();
        try {
            return v1Service.getTenantName(List.of(tenantId)).stream().findFirst().orElse(tenantId.toString());
        } catch (Exception e) {
            return tenantId.toString();
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

    private void calculateChargableAndChargableUnit(ConsolidationDetails consolidationDetails, MeasurementBasisResponse response) throws RunnerException {
        PackSummaryResponse summaryResponse = packingService.calculatePackSummary(consolidationDetails.getPackingList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory(), new ShipmentMeasurementDetailsDto());
        if (summaryResponse != null) {
            response.setChargable(summaryResponse.getChargeableWeight());
            response.setChargeableUnit(summaryResponse.getPacksChargeableWeightUnit());
        }
    }

    private <T> T calculatePacksAndPacksUnit(List<Packing> packings, T response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        if(!CollectionUtils.isEmpty(packings)) {
            for (Packing packing : packings) {
                if(!IsStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
            }
        }
        if(response instanceof MeasurementBasisResponse measurementBasisResponse) {
            measurementBasisResponse.setPackCount(totalPacks);
        }
        return response;
    }

    public void validateCarrierDetails(ConsolidationDetails consolidation, List<ModuleValidationFieldType> missingFields) {
        CarrierDetails carrierDetails = consolidation.getCarrierDetails();

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

    public void validateMawbDetails(ConsolidationDetails consolidation, List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isEmpty(consolidation.getBol())) {
            missingFields.add(ModuleValidationFieldType.MAWB_DETAILS);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotifications(CommonRequestModel commonRequestModel) {
        PendingNotificationRequest request = (PendingNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingConsolidationActionResponse> response = new PendingNotificationResponse<>();
        if (request.getConsolidationIdList() == null || request.getConsolidationIdList().isEmpty()) {
            log.info("Received empty request for pending notification in consolidation", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(response);
        }
        var notificationMap = getNotificationMap(request);
        response.setNotificationMap(notificationMap);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private Map<Long, List<PendingConsolidationActionResponse>> getNotificationMap(PendingNotificationRequest request) {
        // Get data of all shipments pushing to be attached to this consol
        var pushRequestedEnum = ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
        Map<Long, List<PendingConsolidationActionResponse>> notificationResultMap = new HashMap<>();

        if(commonUtils.getCurrentTenantSettings() == null || !(Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) &&
            Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsColoadingMAWBStationEnabled()))) {
            return notificationResultMap;
        }

        try {
            ListCommonRequest listRequest = constructListCommonRequest("consolidationId", request.getConsolidationIdList(), "IN");
            listRequest = andCriteria("requestedType", pushRequestedEnum.name(), "=", listRequest);
            listRequest = andCriteria("isAttachmentDone", false, "=", listRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> consoleShipMappingPair = fetchData(listRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> mappingPage = consoleShipmentMappingDao.findAll(consoleShipMappingPair.getLeft(), consoleShipMappingPair.getRight());

            List<Long> shipmentIds = mappingPage.getContent().stream().map(ConsoleShipmentMapping::getShipmentId).toList();
            final var consoleShipmentsMap = mappingPage.getContent().stream().collect(Collectors.toMap(
                ConsoleShipmentMapping::getShipmentId, Function.identity(), (oldVal, newVal) -> oldVal)
            );

            commonUtils.setInterBranchContextForHub();

            listRequest = constructListCommonRequest("id", shipmentIds, "IN");
            listRequest.setContainsText(request.getContainsText());
            listRequest.setSortRequest(request.getSortRequest());
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class, ShipmentService.tableNames);
            Page<ShipmentDetails> shipmentsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            var tenantIdList = new HashSet<String>();
            var locCodeList =  new HashSet<String>();
            final CarrierDetails nullCarrierDetails = new CarrierDetails();
            shipmentsPage.getContent().stream().forEach(i -> {
                tenantIdList.add(StringUtility.convertToString(i.getTenantId()));
                var carrierDetails = Optional.ofNullable(i.getCarrierDetails()).orElse(nullCarrierDetails);
                locCodeList.add(carrierDetails.getOriginPort());
                locCodeList.add(carrierDetails.getDestinationPort());
            });
            Map<String, TenantModel> v1TenantData = masterDataUtils.fetchInTenantsList(tenantIdList);
            Map<String, EntityTransferUnLocations> v1LocationData = masterDataUtils.fetchInBulkUnlocations(locCodeList, EntityTransferConstants.LOCATION_SERVICE_GUID);

            masterDataUtils.pushToCache(v1TenantData, CacheConstants.TENANTS, tenantIdList, new TenantModel(), null);
            masterDataUtils.pushToCache(v1LocationData, CacheConstants.UNLOCATIONS, locCodeList, new EntityTransferUnLocations(), null);

            // console id vs list of ship ids
            Map<Long, List<Long>> shipmentVsConsolIdMap = new HashMap<>();

            // generate mapping for shipment id vs list of pulling consol(s)
            for(var mapping : mappingPage.getContent()) {
                if(!notificationResultMap.containsKey(mapping.getConsolidationId())) {
                    notificationResultMap.put(mapping.getConsolidationId(), new ArrayList<>());
                }
                if(!shipmentVsConsolIdMap.containsKey(mapping.getShipmentId())) {
                    shipmentVsConsolIdMap.put(mapping.getShipmentId(), new ArrayList<>());
                }
                shipmentVsConsolIdMap.get(mapping.getShipmentId()).add(mapping.getConsolidationId());
            }

            shipmentsPage.getContent().stream().forEach(i -> {
                var res = mapToNotification(i, consoleShipmentsMap, v1TenantData, v1LocationData);
                shipmentVsConsolIdMap.get(i.getId()).forEach(shipId -> notificationResultMap.get(shipId).add(res));
            });

        }
        catch(Exception e) {
            log.error("Error while generating notification map for input Consolidation", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
        }

        return notificationResultMap;
    }

    private PendingConsolidationActionResponse mapToNotification(ShipmentDetails shipment, Map<Long, ConsoleShipmentMapping> consoleShipmentsMap, Map<String, TenantModel> v1TenantData, Map<String, EntityTransferUnLocations> v1LocationData) {
        var carrierDetails = Optional.ofNullable(shipment.getCarrierDetails()).orElse(new CarrierDetails());
        var tenantData = Optional.ofNullable(v1TenantData.get(StringUtility.convertToString(shipment.getTenantId()))).orElse(new TenantModel());
        return PendingConsolidationActionResponse.builder()
            .shipmentId(shipment.getId())
            .shipmentNumber(shipment.getShipmentId())
            .houseBill(shipment.getHouseBill())
            .ata(carrierDetails.getAta())
            .atd(carrierDetails.getAtd())
            .eta(carrierDetails.getEta())
            .etd(carrierDetails.getEtd())
            .pol(Optional.ofNullable(v1LocationData.get(carrierDetails.getOriginPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getOriginPort()))
            .pod(Optional.ofNullable(v1LocationData.get(carrierDetails.getDestinationPort())).map(EntityTransferUnLocations::getLookupDesc).orElse(carrierDetails.getDestinationPort()))
            .branch(tenantData.getCode() + " - " + tenantData.getTenantName())
            .branchDisplayName(tenantData.displayName)
            .hazardous(shipment.getContainsHazardous())
            .delivery(shipment.getCargoDeliveryDate())
            .packs(StringUtility.convertToString(shipment.getNoOfPacks()) + StringUtility.convertToString(shipment.getPacksUnit()))
            .weight(StringUtility.convertToString(shipment.getWeight()) + StringUtility.convertToString(shipment.getWeightUnit()))
            .volume(StringUtility.convertToString(shipment.getVolume()) + StringUtility.convertToString(shipment.getVolumeUnit()))
            .chargeable(StringUtility.convertToString(shipment.getChargable()) + StringUtility.convertToString(shipment.getChargeableUnit()))
            .requestedBy(consoleShipmentsMap.get(shipment.getId()).getCreatedBy())
            .requestedOn(consoleShipmentsMap.get(shipment.getId()).getCreatedAt())
            .build();
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDGShipment(CommonRequestModel commonRequestModel)
    {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        if(commonGetRequest.getId() == null)
            throw new ValidationException("Consolidation Id is required");
        var consolidationId = commonGetRequest.getId();
        var console = consolidationDetailsDao.findById(consolidationId);
        if(!console.isPresent())
        {
            throw new ValidationException("No Consolidation found for the Id: " + consolidationId);
        }
        var shipments = console.get().getShipmentsList();
        Boolean isDgShipmentPresent = false;
        if(shipments != null && !shipments.isEmpty())
        {
            for(var shipment: shipments)
            {
                if(Boolean.TRUE.equals(shipment.getContainsHazardous()))
                {
                    isDgShipmentPresent = true;
                    break;
                }
            }
        }
        var dgShipmentResponse = CheckDGShipment.builder().isDGShipmentPresent(isDgShipmentPresent).build();
        return ResponseHelper.buildSuccessResponse(dgShipmentResponse);
    }

    @Override
    public ResponseEntity<IRunnerResponse> listRequestedConsolidationForShipment(CommonRequestModel commonRequestModel, boolean getMasterData) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long shipId = commonGetRequest.getId();
        var consoleShipMappingList = consoleShipmentMappingDao.findByShipmentIdAll(shipId);
        if (CommonUtils.listIsNullOrEmpty(consoleShipMappingList)) {
            return ResponseHelper.buildListSuccessResponse(new ArrayList<>(), 1, 0);
        }
        List<Long> consoleIds = consoleShipMappingList.stream().filter(x -> (Boolean.TRUE.equals(x.getIsAttachmentDone()) ||
                Objects.equals(x.getRequestedType(), ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED))).map(ConsoleShipmentMapping::getConsolidationId).toList();
        var requestedTypeMap = consoleShipMappingList.stream().collect(Collectors.toMap(ConsoleShipmentMapping::getConsolidationId, Function.identity(), (existingValue, newValue) -> existingValue));

        ListCommonRequest request = CommonUtils.constructListCommonRequest("id", consoleIds, "IN");
        var response = list(CommonRequestModel.buildRequest(request), getMasterData);

        if (response.getBody() instanceof RunnerListResponse<?> responseList) {
            for (var resp : responseList.getData()) {
                if (resp instanceof ConsolidationListResponse consolidationListResponse
                        && requestedTypeMap.containsKey(consolidationListResponse.getId())
                        && !Objects.isNull(requestedTypeMap.get(consolidationListResponse.getId()).getRequestedType())) {
                    consolidationListResponse.setRequestedType(requestedTypeMap.get(consolidationListResponse.getId()).getRequestedType().getDescription());
                    consolidationListResponse.setRequestedBy(requestedTypeMap.get(consolidationListResponse.getId()).getCreatedBy());
                    consolidationListResponse.setRequestedOn(requestedTypeMap.get(consolidationListResponse.getId()).getCreatedAt());
                }
            }
        }
        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> cancel(CommonRequestModel commonRequestModel) throws RunnerException {
        return null;
    }

}
