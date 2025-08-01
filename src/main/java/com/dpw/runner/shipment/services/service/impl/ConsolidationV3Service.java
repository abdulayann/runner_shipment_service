package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.ConsolidationMapper;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.notification.PendingConsolidationActionResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.request.ConsoleBookingIdFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.dto.v3.request.*;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.exception.response.ShipmentDetachResponse;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto.Meta;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto.Triggers;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationValidationV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
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

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.helpers.ResponseHelper.buildDependentServiceResponse;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;
import static java.util.stream.Collectors.toMap;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationV3Service implements IConsolidationV3Service {

    @Autowired
    ExecutorService executorService;
    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private ConsolidationValidationV3Util consolidationValidationV3Util;

    @Autowired
    private ConsolidationV3Util consolidationV3Util;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private KafkaProducer producer;

    @Value("${consolidationsKafka.queue}")
    private String senderQueue;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private ILogsHistoryService logsHistoryService;
    @Autowired
    private BillingServiceAdapter billingServiceAdapter;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPackingV3Service packingV3Service;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IRoutingsV3Service routingsV3Service;

    @Autowired
    private IEventsV3Service eventV3Service;

    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentV3Service;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private INotificationDao notificationDao;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IAwbDao awbDao;

    @Autowired
    private INetworkTransferService networkTransferService;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private IEventService eventService;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    private NetworkTransferV3Util networkTransferV3Util;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Value("${include.master.data}")
    private Boolean includeMasterData;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;

    @Override
    @Transactional
    public ConsolidationDetailsV3Response create(ConsolidationDetailsV3Request request) {
        return this.createConsolidation(request, false);
    }
    public static final String TEMPLATE_NOT_FOUND_MESSAGE = "Template not found, please inform the region users manually";

    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
        Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Long.class).build()),
        Map.entry("cutoffDate", RunnerEntityMapping.builder().tableName("allocations").dataType(LocalDateTime.class).build()),
        Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).build()),
        Map.entry(Constants.BOOKING_STATUS_FIELD, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).fieldName(Constants.BOOKING_STATUS_FIELD).build()),
        Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
        Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).build()),
        Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("consolidationType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry(TRANSPORT_MODE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("containerCategory", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("mawb", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
        Map.entry("serviceLevel", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
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
        Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName("updatedAt").build()),
        Map.entry("hazardous", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build()),
        Map.entry("shipShipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).fieldName("shipmentType").build()),
        Map.entry(TENANT_ID, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Integer.class).fieldName(TENANT_ID).build()),
        Map.entry(Constants.INTER_BRANCH_CONSOLE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.INTER_BRANCH_CONSOLE).build()),
        Map.entry(Constants.OPEN_FOR_ATTACHMENT, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.OPEN_FOR_ATTACHMENT).build()),
        Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(Constants.CREATED_AT).build()),
        Map.entry(Constants.LAT_DATE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName(Constants.LAT_DATE).build())
    );

    private ConsolidationDetailsV3Response createConsolidation(ConsolidationDetailsV3Request request, boolean isFromET) {

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, false);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        ConsolidationDetailsV3Response consolidationDetailsV3Response = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsV3Response.class);

        if (!isFromET) {
            triggerPushToDownStream(consolidationDetails, null, Constants.CONSOLIDATION_AFTER_SAVE);
            triggerPushToDownStream(consolidationDetails, null, Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);
        }

        return consolidationDetailsV3Response;
    }

    @Transactional
    @Override
    public Pair<ConsolidationDetails, Long> createConsolidationForBooking(CommonRequestModel commonRequestModel, CustomerBookingV3Request customerBookingV3Request){
        ConsolidationDetailsV3Request request = (ConsolidationDetailsV3Request) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request is null for Consolidation Create");
        }

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        Long containerAssignedToShipmentCargo = null;
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);
            populateOriginDestinationAgentDetailsForBookingConsolidation(consolidationDetails);
            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);
            Long id = consolidationDetails.getId();
            containerAssignedToShipmentCargo = setContainerAndPackingList(consolidationDetails, true, shipmentSettingsDetails, true, false, customerBookingV3Request.getContainersList(), id, customerBookingV3Request.getPackingList(), customerBookingV3Request);
            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, true);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        Pair<ConsolidationDetails, Long> consolidationDetailsLongPair = Pair.of(consolidationDetails, containerAssignedToShipmentCargo);

        triggerPushToDownStream(consolidationDetails, null, Constants.CONSOLIDATION_AFTER_SAVE);
        triggerPushToDownStream(consolidationDetails, null, Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);

        return consolidationDetailsLongPair;
    }

    @Override
    @Transactional
    public ConsolidationDetailsV3Response completeUpdate(ConsolidationDetailsV3Request consolidationDetailsRequest) throws RunnerException {
        return this.completeUpdateConsolidation(consolidationDetailsRequest, false);
    }

    private ConsolidationDetailsV3Response completeUpdateConsolidation(ConsolidationDetailsV3Request consolidationDetailsRequest, boolean isFromET) throws RunnerException {
        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(consolidationDetailsRequest);
        if (!oldEntity.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, consolidationDetailsRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ConsolidationDetails entity = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            setInterBranchContext(entity.getInterBranchConsole());
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            ConsolidationDetails oldConvertedConsolidation = jsonHelper.convertValue(oldEntity.get(), ConsolidationDetails.class);

            beforeSave(entity, oldEntity.get(), false);

            entity = consolidationDetailsDao.updateV3(entity);
            processAuditServiceLogs(entity, oldEntityJsonString);
            afterSave(entity, oldConvertedConsolidation, consolidationDetailsRequest, false, shipmentSettingsDetails, false);
            ConsolidationDetails finalEntity1 = entity;
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(finalEntity1)), executorService);
            ConsolidationDetailsV3Response consolidationDetailsV3Response = jsonHelper.convertValue(entity, ConsolidationDetailsV3Response.class);

            if (!isFromET) {
                triggerPushToDownStream(entity, oldConvertedConsolidation, Constants.CONSOLIDATION_AFTER_SAVE);
                triggerPushToDownStream(entity, oldConvertedConsolidation, Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);
            }

            return consolidationDetailsV3Response;
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e.getMessage());
        }
    }

    private void processAuditServiceLogs(ConsolidationDetails entity, String oldEntityJsonString) {
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
    }

    public void triggerPushToDownStream(ConsolidationDetails consolidationDetails, ConsolidationDetails oldConsolidationDetails,
        String sourceInfo) {
        PushToDownstreamEventDto pushToDownstreamEventDto = null;
        Boolean isCreate = oldConsolidationDetails == null ? Boolean.TRUE : Boolean.FALSE;

        if (Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING.equalsIgnoreCase(sourceInfo)) {
            List<ShipmentDetails> shipmentDetailsList = findShipmentForTrackingService(consolidationDetails, oldConsolidationDetails);

            pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                    .parentEntityId(consolidationDetails.getId())
                    .parentEntityName(CONSOLIDATION)
                    .meta(Meta.builder()
                            .sourceInfo(sourceInfo)
                            .isCreate(isCreate)
                            .tenantId(consolidationDetails.getTenantId()).build()).build();

            if(ObjectUtils.isNotEmpty(shipmentDetailsList)) {
                List<Triggers> triggers = new ArrayList<>();

                for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                    Triggers vTrigger = Triggers.builder()
                            .entityId(shipmentDetails.getId())
                            .entityName(SHIPMENT).build();
                    triggers.add(vTrigger);
                }
                pushToDownstreamEventDto.setTriggers(triggers);
            }
        } else if (Constants.CONSOLIDATION_AFTER_SAVE.equalsIgnoreCase(sourceInfo)) {
            pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                    .parentEntityId(consolidationDetails.getId())
                    .parentEntityName(CONSOLIDATION)
                    .meta(Meta.builder()
                            .isCreate(isCreate)
                            .sourceInfo(sourceInfo)
                            .tenantId(consolidationDetails.getTenantId()).build()).build();
        }

        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, LoggerHelper.getRequestIdFromMDC());
    }

    public Optional<ConsolidationDetails> retrieveByIdOrGuid(ConsolidationDetailsV3Request request) throws RunnerException {
        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ConsolidationDetails> oldEntity;

        oldEntity = getConsolidationDetails(request.getId(), request.getGuid());
        return oldEntity;
    }

    public Optional<ConsolidationDetails> getConsolidationDetails(Long id, UUID guid) throws RunnerException {
        Optional<ConsolidationDetails> oldEntity;
        if(id!=null){
            oldEntity=consolidationDetailsDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, id, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else if(guid!=null){
            oldEntity= consolidationDetailsDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Consolidation Details is null for GUID {} with Request GUID {}", guid, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else{
            throw new RunnerException("Either Id or Guid is required");

        }
        return oldEntity;
    }

    private void beforeSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isCreate) throws Exception {
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
        consolidationValidationV3Util.checkCFSValidation(consolidationDetails, isCreate, shipmentDetails);

        if(consolidationDetails.getCarrierDetails() != null) {
            if (consolidationDetails.getTransportMode() != null && consolidationDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setVoyage(null);
            } else {
                consolidationDetails.getCarrierDetails().setFlightNumber(null);
            }
        }

        if(!Boolean.TRUE.equals(isCreate)){
            // This method will only work for non air transport modes , validation check moved inside the method
            updateLinkedShipmentData(consolidationDetails, oldEntity, false, dgStatusChangeInShipments, true);
        }

        if(consolidationDetails.getDocumentationPartner() != null && consolidationDetails.getDocumentationPartner() == 0)
            consolidationDetails.setDocumentationPartner(null);
        setReceivingAndTriangulationBranch(consolidationDetails);
        saveAwb(consolidationDetails, oldEntity);
        if (Boolean.TRUE.equals(isCreate))
            consolidationDetails.setOpenForAttachment(true);

        populateUnlocCodeFuture.join();
    }

    private void dgOceanFlowsAndValidations(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()))
        {
            List<Containers> containersList = consolidationDetails.getContainersList();
            if(!listIsNullOrEmpty(containersList))
            {
                Map<Long, Containers> oldContainersMap = getOldContainersMap(oldEntity);
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
        if(!consolidationValidationV3Util.checkConsolidationTypeValidation(consolidationDetails)) {
            throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
        }
    }

    private Map<Long, Containers> getOldContainersMap(ConsolidationDetails oldEntity) {
        Map<Long, Containers> oldContainersMap = new HashMap<>();
        if(!Objects.isNull(oldEntity))
            oldContainersMap = oldEntity.getContainersList().stream().collect(Collectors.toMap(e -> e.getId(), c -> c));
        return oldContainersMap;
    }

    private void changeShipmentDGValuesFromContainer(Containers container, Map<Long, Containers> oldContainersMap,
        Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {
        Containers oldContainer = null;
        if(container.getId() != null && oldContainersMap.containsKey(container.getId())) {
            oldContainer = oldContainersMap.get(container.getId());
            if(commonUtils.checkIfDGFieldsChangedInContainer(jsonHelper.convertValue(container, ContainerRequest.class), oldContainer)
                && !setIsNullOrEmpty(oldContainer.getShipmentsList())) {
                for(ShipmentDetails shipmentDetails: oldContainer.getShipmentsList()) {
                    changeDgShipmentMapValues(shipmentDetails, dgStatusChangeInShipments, container);
                }
            }
        }
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

    void getConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException{
        getConsolidation(consolidationDetails, false);
    }

    void getConsolidation(ConsolidationDetails consolidationDetails, boolean allowDGValueChange) throws RunnerException{
        generateConsolidationNumber(consolidationDetails);
        consolidationDetails = consolidationDetailsDao.saveV3(consolidationDetails, allowDGValueChange);

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

    @Override
    public void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException {
        Boolean customisedSequence = shipmentSettingsDao.getCustomisedSequence();

        if(consolidationDetails.getConsolidationNumber() == null) {
            if(Boolean.TRUE.equals(customisedSequence)) {
                String consoleNumber = getCustomizedConsolidationProcessNumber(consolidationDetails, ProductProcessTypes.ReferenceNumber);
                if(consoleNumber != null && !consoleNumber.isEmpty())
                    consolidationDetails.setConsolidationNumber(consoleNumber);
                setConsolidationNumber(consolidationDetails);
                setReferenceNumber(consolidationDetails);
            }
            else {
                consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                setReferenceNumber(consolidationDetails);
            }
        }

        setBolConsolidation(consolidationDetails);
    }

    private void setConsolidationNumber(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getConsolidationNumber() == null || consolidationDetails.getConsolidationNumber().isEmpty())
            consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
    }

    private void setReferenceNumber(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
            consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
    }

    private void setBolConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException {
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

    private void afterSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ConsolidationDetailsV3Request consolidationDetailsRequest, Boolean isCreate,
            ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking) throws RunnerException {
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

        Long id = consolidationDetails.getId();

        if (Boolean.FALSE.equals(isCreate) && (checkForAwbUpdate(consolidationDetails, oldEntity))) {
                awbDao.updatedAwbInformationEvent(consolidationDetails, oldEntity);
        }

        processRequestLists(consolidationDetails, isCreate, isFromBooking, referenceNumbersRequestList, id, consolidationAddressRequest);

        syncShipmentDataInPlatform(consolidationDetails);

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consolidationDetails, oldEntity)), executorService);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(consolidationDetails, oldEntity, false)), executorService);
    }

    private void syncShipmentDataInPlatform(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getShipmentsList() != null) {
            consolidationDetails.getShipmentsList().forEach(shipment -> {
                if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipment)), executorService);
            });
        }
    }

    private Long setContainerAndPackingList(ConsolidationDetails consolidationDetails, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails,
                                            Boolean isFromBooking, boolean includeGuid, List<ContainerV3Request> containerRequestList, Long id,
                                            List<PackingV3Request> packingRequestList, CustomerBookingV3Request customerBookingV3Request) throws RunnerException {
        Long containerAssignedToShipmentCargo = null;
        if(containerRequestList != null && !Boolean.TRUE.equals(shipmentSettingsDetails.getMergeContainers())
                && !Boolean.TRUE.equals(shipmentSettingsDetails.getIsShipmentLevelContainer())) {
            List<ContainerV3Request> splittedContainers = splitContainerIntoLineItems(containerRequestList);
            List<Containers> containers = commonUtils.convertToEntityList(splittedContainers, Containers.class, (!isFromBooking && !includeGuid) && isCreate);
            containerAssignedToShipmentCargo = shipmentV3Service.assignFirstBookingContainerToShipmentCargo(containers, customerBookingV3Request);
            List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(containers, id, (Long) null, true);
            if(!listIsNullOrEmpty(updatedContainers))
                containerAssignedToShipmentCargo = updatedContainers.get(0).getId();
            consolidationDetails.setContainersList(updatedContainers);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class, (!isFromBooking && !includeGuid) && isCreate), id);
            consolidationDetails.setPackingList(updatedPackings);
        }
        return containerAssignedToShipmentCargo;
    }

    private List<ContainerV3Request> splitContainerIntoLineItems(List<ContainerV3Request> containerRequestList) {
        List<ContainerV3Request> expandedContainers = new ArrayList<>();
        if (!containerRequestList.isEmpty()) {
            for(ContainerV3Request containerV3Request: containerRequestList) {
                Long count = containerV3Request.getContainerCount();
                for (int i = 0; i < count; i++) {
                    ContainerV3Request newContainer = jsonHelper.convertValue(containerV3Request, ContainerV3Request.class);
                    newContainer.setId(null);
                    newContainer.setGuid(null);
                    newContainer.setBookingId(null);
                    newContainer.setContainerCount(1L);
                    expandedContainers.add(newContainer);
                }
            }
        }
        return expandedContainers;
    }

    private boolean checkForAwbUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(consolidationDetails.getSci(), oldEntity.getSci())) return true;
        return !Objects.equals(consolidationDetails.getEfreightStatus(), oldEntity.getEfreightStatus());
    }

    private void processRequestLists(ConsolidationDetails consolidationDetails, Boolean isCreate, Boolean isFromBooking, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id,
                                     List<PartiesRequest> consolidationAddressRequest) throws RunnerException {
        boolean create = !Boolean.TRUE.equals(isFromBooking) && isCreate;
        if (referenceNumbersRequestList != null) {

            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, create), id);
            consolidationDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (consolidationAddressRequest != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class,
                create), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedParties);
        }
    }

    private String getConsolidationSerialNumber() {
        return v1Service.getMaxConsolidationId();
    }

    public String generateCustomBolNumber() {
        String res = null;
        ShipmentSettingsDetails tenantSetting = commonUtils.getShipmentSettingFromContext();

        if(tenantSetting.getConsolidationLite() != null && tenantSetting.getConsolidationLite() && tenantSetting.getBolNumberGeneration() == null) {
            return  res;
        }

        if(tenantSetting.getBolNumberGeneration() != null) {
            res = tenantSetting.getBolNumberPrefix() != null ? tenantSetting.getBolNumberPrefix() : "";
            if (tenantSetting.getBolNumberGeneration() == GenerationType.Random) {
                res += StringUtility.getRandomString(10);
            } else {
                String serialNumber = v1Service.getMaxConsolidationId();
                res += serialNumber;
            }
        }

        return res;
    }

    private String getCustomizedConsolidationProcessNumber(ConsolidationDetails consolidationDetails, ProductProcessTypes productProcessTypes) throws RunnerException {
        List<TenantProducts> enabledTenantProducts = productEngine.populateEnabledTenantProducts();
        if (productProcessTypes == ProductProcessTypes.ReferenceNumber) {
            // to check the commmon sequence
            var sequenceNumber = productEngine.getCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
            if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
                return sequenceNumber;
            }
        }
        var identifiedProduct = productEngine.identifyProduct(consolidationDetails, enabledTenantProducts);
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

    protected void createLogHistoryForConsole(ConsolidationDetails consolidationDetails){
        try {
            String entityPayload = jsonHelper.convertToJson(consolidationDetails);
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(consolidationDetails.getId())
                    .entityType(Constants.CONSOLIDATION).entityGuid(consolidationDetails.getGuid()).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Consolidation : " + ex.getMessage());
        }
    }

    public void populateOriginDestinationAgentDetailsForBookingConsolidation(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails != null && !Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType()) && !CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
                consolidationDetails.setSendingAgent(v1ServiceUtil.getDefaultAgentOrgParty(null));
                if(consolidationDetails.getSendingAgent() != null && consolidationDetails.getSendingAgent().getOrgData() != null && consolidationDetails.getSendingAgent().getOrgData().get("TenantId") != null)
                    consolidationDetails.setOriginBranch(Long.valueOf(consolidationDetails.getSendingAgent().getOrgData().get("TenantId").toString()));
            } else if (Constants.DIRECTION_IMP.equals(consolidationDetails.getShipmentType()) && !CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
                consolidationDetails.setReceivingAgent(v1ServiceUtil.getDefaultAgentOrgParty(null));
            }
        }
    }

    private CompletableFuture<Void> getPopulateUnlocCodeFuture(ConsolidationDetails entity, ConsolidationDetails oldEntity) {
        CarrierDetails finalOldCarrierDetails = Optional.ofNullable(oldEntity).map(ConsolidationDetails::getCarrierDetails).orElse(null);

        /* Set to extract the unlocations from entities whose unloc code needs to be saved */
        Set<String> unlocationsSet = Collections.synchronizedSet(new HashSet<>());
        Map<String, EntityTransferUnLocations> unLocationsMap = new ConcurrentHashMap<>();

        CompletableFuture.allOf(
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(entity.getCarrierDetails(), finalOldCarrierDetails, unlocationsSet), executorService)
        ).join();
        return CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(unlocationsSet, unLocationsMap)), executorService)
                .thenCompose(v -> CompletableFuture.allOf(
                        CompletableFuture.runAsync(() -> commonUtils.updateCarrierUnLocData(entity.getCarrierDetails(), unLocationsMap), executorService)
                ));
    }

    /**
     * Calculates the achieved weight, volume, and chargeable quantities for the given consolidation details. Updates the response object with the calculated values.
     *
     * @param consolidationDetails the consolidation details
     * @param response             the response object to be updated
     * @param shipmentDetailsList  the set of shipment details to process
     * @throws RunnerException if any exception occurs during the calculation
     */
    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeV3Response response, Set<ShipmentDetails> shipmentDetailsList)
            throws RunnerException {

        if (Boolean.TRUE.equals(consolidationDetails.getOverride())) {
            return;
        }

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        List<Long> containerIds = getContainerIds(consolidationDetails);
        Set<Long> assignedContainerIds = new HashSet<>(containerV3Service.findContainerIdsAttachedToEitherPackingOrShipment(containerIds));

        String weightChargeableUnit = getOrDefault(shipmentSettingsDetails.getWeightChargeableUnit(), Constants.WEIGHT_UNIT_KG);
        String volumeChargeableUnit = getOrDefault(shipmentSettingsDetails.getVolumeChargeableUnit(), Constants.VOLUME_UNIT_M3);

        AggregationResult aggregationResult = aggregateShipments(consolidationDetails.getTransportMode(), shipmentDetailsList, weightChargeableUnit, volumeChargeableUnit);

        response.setSummaryShipmentsCount(aggregationResult.shipmentCount);
        calculateConsoleShipmentTeuCount(consolidationDetails, response, v1TenantSettingsResponse, assignedContainerIds);

        if (consolidationDetails.getAchievedQuantities() == null) {
            consolidationDetails.setAchievedQuantities(new AchievedQuantities());
        }

        consolidationDetails.getAchievedQuantities().setConsolidatedWeight(aggregationResult.sumWeight);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(weightChargeableUnit);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolume(aggregationResult.sumVolume);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(volumeChargeableUnit);

        consolidationDetails = calculateConsolUtilization(consolidationDetails);

        if (consolidationDetails.getAllocations() == null) {
            consolidationDetails.setAllocations(new Allocations());
        }

        String transportMode = consolidationDetails.getTransportMode();
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, aggregationResult.sumWeight, aggregationResult.sumVolume);

        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());

        String cargoType = consolidationDetails.getContainerCategory();
        boolean isSeaLCL = commonUtils.isSeaLCL(transportMode, cargoType);
        boolean isRoadLCLorLTL = commonUtils.isRoadLCLorLTL(transportMode, cargoType);
        if (isSeaLCL || isRoadLCLorLTL) {
            applyLclSeaOverrides(consolidationDetails);
        }

        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());

        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        response.setSummaryWeight(IReport.convertToWeightNumberFormat(aggregationResult.sumWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.convertToVolumeNumberFormat(aggregationResult.sumVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);
        response.setSummaryDGShipments(getSummaryDGShipments(shipmentDetailsList));
        response.setSummaryContainer(getSummaryContainer(consolidationDetails.getContainersList(), assignedContainerIds));
        response.setSummaryDgPacks(getSummaryDgPacks(aggregationResult.packingList));
        response.setTotalPacks(aggregationResult.packs);
        response.setPackType(aggregationResult.packsType);
        if(TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode) || TRANSPORT_MODE_ROA.equalsIgnoreCase(transportMode)) {
            response.setShipmentSummaryChargeableSum(IReport.convertToWeightNumberFormat(aggregationResult.chargeable, v1TenantSettingsResponse) + " " + Constants.WEIGHT_UNIT_KG);
            response.setShipmentSummaryWeightVolumeSum(IReport.convertToWeightNumberFormat(aggregationResult.weightVolume, v1TenantSettingsResponse) + " " + Constants.WEIGHT_UNIT_KG);
        } else {
            response.setShipmentSummaryChargeableSum(IReport.convertToVolumeNumberFormat(aggregationResult.chargeable, v1TenantSettingsResponse) + " " + VOLUME_UNIT_M3);
            response.setShipmentSummaryWeightVolumeSum(IReport.convertToVolumeNumberFormat(aggregationResult.weightVolume, v1TenantSettingsResponse) + " " + VOLUME_UNIT_M3);
        }

        if (canSetChargableWeight(consolidationDetails)) {
            double volInM3 = convertUnit(Constants.VOLUME, aggregationResult.sumVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, aggregationResult.sumWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    private String getOrDefault(String unit, String defaultUnit) {
        return ObjectUtils.isNotEmpty(unit) ? unit : defaultUnit;
    }

    private List<Long> getContainerIds(ConsolidationDetails details) {
        if (details.getContainersList() == null) return new ArrayList<>();
        return details.getContainersList().stream()
                .map(Containers::getId)
                .distinct()
                .toList();
    }

    private void applyLclSeaOverrides(ConsolidationDetails consolidationDetails) throws RunnerException {
        BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS,
                consolidationDetails.getAllocations().getWeight(),
                consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
        BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME,
                consolidationDetails.getAllocations().getVolume(),
                consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
    }

    private static class AggregationResult {
        BigDecimal sumWeight;
        BigDecimal sumVolume;
        int packs;
        String packsType;
        int shipmentCount;
        BigDecimal chargeable;
        BigDecimal weightVolume;
        List<Packing> packingList;
    }

    private AggregationResult aggregateShipments(String transportMode, Set<ShipmentDetails> shipments, String weightUnit, String volumeUnit) throws RunnerException {
        AggregationResult result = new AggregationResult();
        result.sumWeight = BigDecimal.ZERO;
        result.sumVolume = BigDecimal.ZERO;
        result.chargeable = BigDecimal.ZERO;
        result.weightVolume = BigDecimal.ZERO;
        result.packs = 0;
        result.packsType = null;
        result.shipmentCount = 0;
        result.packingList = new ArrayList<>();

        if (ObjectUtils.isNotEmpty(shipments)) {
            for (ShipmentDetails sd : shipments) {
                result.sumWeight = result.sumWeight.add(
                        new BigDecimal(convertUnit(Constants.MASS, sd.getWeight(), sd.getWeightUnit(), weightUnit).toString()));
                result.sumVolume = result.sumVolume.add(
                        new BigDecimal(convertUnit(Constants.VOLUME, sd.getVolume(), sd.getVolumeUnit(), volumeUnit).toString()));
                result.packs += (sd.getNoOfPacks() != null ? sd.getNoOfPacks() : 0);
                result.packsType = determinePackType(result.packsType, sd.getPacksUnit());
                if(sd.getPackingList() != null) result.packingList.addAll(sd.getPackingList());

                // chargeable and weight volume sum
                if(TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode) || TRANSPORT_MODE_ROA.equalsIgnoreCase(transportMode)) {
                    result.chargeable = result.chargeable.add(
                            new BigDecimal(convertUnit(Constants.MASS, sd.getChargable(), sd.getChargeableUnit(), WEIGHT_UNIT_KG).toString()));
                    result.weightVolume = result.weightVolume.add(
                            new BigDecimal(convertUnit(Constants.MASS, sd.getVolumetricWeight(), sd.getVolumetricWeightUnit(), WEIGHT_UNIT_KG).toString()));
                } else {
                    result.chargeable = result.chargeable.add(
                            new BigDecimal(convertUnit(VOLUME, sd.getChargable(), sd.getChargeableUnit(), VOLUME_UNIT_M3).toString()));
                    result.weightVolume = result.weightVolume.add(
                            new BigDecimal(convertUnit(VOLUME, sd.getVolumetricWeight(), sd.getVolumetricWeightUnit(), VOLUME_UNIT_M3).toString()));
                }
            }
            result.shipmentCount = shipments.size();
        }

        // Ensure packsType is assigned a defualt value
        if (isStringNullOrEmpty(result.packsType)) {
            result.packsType = PackingConstants.PKG;
        }

        return result;
    }

    private String determinePackType(String existing, String incoming) {
        if (isStringNullOrEmpty(existing)) return incoming;
        if (!isStringNullOrEmpty(incoming) && !Objects.equals(existing, incoming)) {
            return PackingConstants.PKG;
        }
        return existing;
    }

    /**
     * Returns the number of hazardous (DG) packs over the total number of packs.
     *
     * @param packingList the list of {@link Packing} objects to evaluate; may be {@code null} or empty
     * @return a string in the format "DGPacks / TotalPacks", or {@code null} if the input list is {@code null} or empty,
     *         or if an error occurs
     */
    protected String getSummaryDgPacks(List<Packing> packingList) {
        try {
            if (listIsNullOrEmpty(packingList)) {
                log.info("packingList is null or empty.");
                return null;
            }

            long totalPacks = 0;
            long totalDGPacks = 0;

            for (Packing packing : packingList) {
                if(packing.getPacks() != null) {
                    totalPacks += Long.valueOf(packing.getPacks());
                    if (Boolean.TRUE.equals(packing.getHazardous())) {
                        totalDGPacks += Long.valueOf(packing.getPacks());
                    }
                }
            }

            return totalDGPacks + " / " + totalPacks;
        } catch (Exception e) {
            log.error("Error while calculating DG pack summary", e);
            return null;
        }
    }

    /**
     * Returns a summary string showing the number of assigned containers over the total number of containers.
     *
     * @param containersList        the list of {@link Containers} to evaluate; may be {@code null} or empty
     * @param assignedContainerIds  the set of container IDs that are considered assigned; may be {@code null}
     * @return a string in the format "AssignedContainers / TotalContainers", or {@code null} if the input list is {@code null} or empty,
     *         or if an error occurs during processing
     */
    protected String getSummaryContainer(List<Containers> containersList, Set<Long> assignedContainerIds) {
        try {
            if (Objects.isNull(containersList)) {
                containersList = new ArrayList<>();
            }

            if (assignedContainerIds == null) {
                log.info("assignedContainerIds is null. Treating all containers as unassigned.");
                assignedContainerIds = Collections.emptySet();
            }

            long totalContainers = 0;
            long totalAssignedContainers = 0;

            for (Containers container : containersList) {
                long count = container.getContainerCount();
                totalContainers += count;
                if (assignedContainerIds.contains(container.getId())) {
                    totalAssignedContainers += count;
                }
            }

            return totalAssignedContainers + " / " + totalContainers;
        } catch (Exception e) {
            log.error("Error while calculating container summary", e);
            return null;
        }
    }

    /**
     * Returns a summary string showing the number of hazardous shipments (DG) over the total number of shipments.
     *
     * @param shipmentDetailsList a set of {@link ShipmentDetails} objects to evaluate;
     *                            may be {@code null} or empty
     * @return a string in the format "DGShipments / TotalShipments", or {@code null} if the input is {@code null} or empty,
     *         or if an error occurs during processing
     */
    protected String getSummaryDGShipments(Set<ShipmentDetails> shipmentDetailsList) {
        try {
            if (setIsNullOrEmpty(shipmentDetailsList)) {
                return null;
            }

            long totalShipments = shipmentDetailsList.size();
            long dgShipments = shipmentDetailsList.stream()
                    .filter(sd -> Boolean.TRUE.equals(sd.getContainsHazardous()))
                    .count();

            return dgShipments + " / " + totalShipments;
        } catch (Exception e) {
            log.error("Error while calculating DG shipments summary", e);
            return null;
        }
    }


    protected void calculateConsoleShipmentTeuCount(ConsolidationDetails consolidationDetails, ShipmentGridChangeV3Response response, V1TenantSettingsResponse v1TenantSettingsResponse, Set<Long> assignedContainerIds) {
        // Initialize the TEU (Twenty-foot Equivalent Unit) and container counters for console and shipment.
        Double consoleTeu = 0.0;
        Double assignedConsoleTeu = 0.0;
        Double shipmentTeu = 0.0;
        long consoleCont = 0L;
        long shipmentCont = 0L;

        // Check if the containers list is not null or empty before proceeding.
        if (!CollectionUtils.isEmpty(consolidationDetails.getContainersList())) {

            // Initialize cache maps for storing container-related data and mappings.
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            Set<String> uniqueContainerNumbers = new HashSet<>();

            // Populate cacheMap and containerTypes using the provided process method.
            processCacheAndContainerResponseList(consolidationDetails, containerTypes, fieldNameKeyMap, cacheMap);

            // Iterate over each container to calculate relevant counts and TEUs.
            for (Containers containers : consolidationDetails.getContainersList()) {

                // Retrieve the cached container type entityTransferContainerType for the current container.
                Object cache = getEntityTransferObjectCache(containers, cacheMap);
                EntityTransferContainerType entityTransferContainerType = (EntityTransferContainerType) cache;

                // Process only if the container count is not null.
                if (containers.getContainerCount() != null) {

                    // Calculate console container count based on category
                    consoleCont = getConsoleContainerCount(consolidationDetails, containers, uniqueContainerNumbers, consoleCont);

                    // Calculate shipment container count using a helper method.
                    shipmentCont = getShipmentContainerCount(containers, shipmentCont);

                    // If container type has a valid TEU value, update console, assigned, and shipment TEU counts accordingly
                    if (isEntityTransferContainerTypeTeu(entityTransferContainerType)) {

                        // Update the console TEU count by multiplying container count with the TEU value.
                        consoleTeu += (containers.getContainerCount() * entityTransferContainerType.getTeu());
                        if(assignedContainerIds.contains(containers.getId())) {
                            assignedConsoleTeu += (containers.getContainerCount() * entityTransferContainerType.getTeu());
                        }

                        // Update the shipment TEU count using a separate method.
                        shipmentTeu = getShipmentTeu(containers, shipmentTeu, entityTransferContainerType);
                    }
                }
            }
        }

        String totalConsolTeu = IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse);
        String totalAssignedConsoleTeu = IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(assignedConsoleTeu), 0, v1TenantSettingsResponse);

        // Format and set the calculated TEU and container counts in the response.
        response.setSummaryConsoleTEU(totalConsolTeu);
        response.setSummaryConsolContainer(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));
        response.setSummaryAssignedTEUs(totalAssignedConsoleTeu + " / "+totalConsolTeu);
    }

    private boolean isEntityTransferContainerTypeTeu(EntityTransferContainerType entityTransferContainerType){
        return entityTransferContainerType != null && entityTransferContainerType.getTeu() != null;
    }
    /**
     * Calculates the total console container count based on the container category. If the category is LCL (Less than Container Load), it counts unique container numbers.
     * Otherwise, it sums up the container count directly.
     *
     * @param consolidationDetails   The details of the consolidation, including container category.
     * @param container              The individual container to be processed.
     * @param uniqueContainerNumbers A set storing unique container numbers for LCL shipments.
     * @param consoleCont            The current console container count to be updated.
     * @return The updated console container count after processing the current container.
     */
    private long getConsoleContainerCount(ConsolidationDetails consolidationDetails, Containers container, Set<String> uniqueContainerNumbers, long consoleCont) {

        // Check if the container category is LCL (Less than Container Load) or LTL.
        if (commonUtils.isLCLorLTL(consolidationDetails.getContainerCategory())) {

            // For LCL, count unique container numbers only.
            if (ObjectUtils.isNotEmpty(container.getContainerNumber())) {
                uniqueContainerNumbers.add(container.getContainerNumber()); // Add to the unique set if it's not empty.
                return uniqueContainerNumbers.size(); // Return the size of the unique set.
            }
        } else {
            // For other categories, simply add the container count to the total.
            return consoleCont + container.getContainerCount();
        }

        // Return the existing console count if no conditions are met.
        return consoleCont;
    }

    public VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException {
        String responseMsg;
        try {
            VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
            if (!weightUnit.isEmpty() && !volumeUnit.isEmpty() && !transportMode.isEmpty()) {
                switch (transportMode) {
                    case Constants.TRANSPORT_MODE_SEA, Constants.TRANSPORT_MODE_RAI, Constants.TRANSPORT_MODE_FSA:
                        BigDecimal wtInTn = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        wtInTn = wtInTn.divide(BigDecimal.valueOf(1000));
                        volume = new BigDecimal(convertUnit(VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());

                        vwOb.setChargeable(wtInTn.max(volume));
                        vwOb.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                        vwOb.setVolumeWeight(wtInTn);
                        vwOb.setVolumeWeightUnit(Constants.VOLUME_UNIT_M3);
                        break;
                    case Constants.TRANSPORT_MODE_AIR, Constants.TRANSPORT_MODE_ROA, Constants.TRANSPORT_MODE_FAS:
                        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        BigDecimal vlInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        BigDecimal factor = BigDecimal.valueOf(AIR_FACTOR_FOR_VOL_WT);
                        if (transportMode.equals(Constants.TRANSPORT_MODE_ROA)) {
                            factor = BigDecimal.valueOf(ROAD_FACTOR_FOR_VOL_WT);
                        }
                        BigDecimal wvInKG = vlInM3.multiply(factor);
                        BigDecimal tempValue;
                        if (wtInKG.compareTo(wvInKG) < 0) {
                            tempValue = wvInKG;
                        } else
                            tempValue = wtInKG;
                        tempValue = CommonUtils.roundBigDecimal(tempValue.multiply(BigDecimal.valueOf(100)), 0, RoundingMode.CEILING);

                        vwOb.setChargeable(tempValue.divide(BigDecimal.valueOf(100)));
                        vwOb.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
                        vwOb.setVolumeWeight(wvInKG);
                        vwOb.setVolumeWeightUnit(Constants.WEIGHT_UNIT_KG);
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

    protected ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
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
                    consolidationDetails.getAchievedQuantities().setWeightUtilization(String.valueOf((consolidatedWeight.divide(weight, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if(Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization(String.valueOf((consolidatedVolume.divide(volume, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
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
    public void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, ConsolidationDetails oldEntity)
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
                List<ShipmentDetails> shipmentDetailsList = findShipmentForTrackingService(consolidationDetails, oldEntity);
                for(ShipmentDetails shipmentDetails : shipmentDetailsList) {
                    UniversalTrackingPayload universalTrackingPayload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(
                            consolidationDetails, shipmentDetails);
                    List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                    if (universalTrackingPayload != null) {
                        trackingPayloads.add(universalTrackingPayload);
                        var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                        log.info(
                                "Producing tracking service payload from consolidation with RequestId: {} and payload: {}",
                                LoggerHelper.getRequestIdFromMDC(), jsonBody);
                        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,
                                false);
                    }
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
            containerService.pushContainersToDependentServices(consolidationDetails.getContainersList(), oldEntity != null ? oldEntity.getContainersList() : null, null);
        }
        catch (Exception e) {
            log.error("Error producing message due to " + e.getMessage());
        }
    }

    public List<ShipmentDetails> findShipmentForTrackingService(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        if(oldEntity == null) return Collections.emptyList();

        // check MBL / MAWB / Carrier Scac change
        if(isMasterDataChange(consolidationDetails, oldEntity)){
            return new ArrayList<>(consolidationDetails.getShipmentsList());
        }
        // check Container Number change
        return findContainerNumberChangeShipment(consolidationDetails, oldEntity);
    }

    private List<ShipmentDetails> findContainerNumberChangeShipment(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        Map<Long, String> oldContainerMap = buildContainerMap(oldEntity.getContainersList());
        Map<Long, String> containerMap = buildContainerMap(consolidationDetails.getContainersList());

        List<Long> containerIds = new ArrayList<>();
        for (Long id : oldContainerMap.keySet()) {
            String oldContainerNumber = oldContainerMap.get(id);
            String newContainerNumber = containerMap.getOrDefault(id, null);

            if (newContainerNumber == null) {
                continue;
            }

            if (!Objects.equals(oldContainerNumber, newContainerNumber)) {
                containerIds.add(id);
            }
        }

        List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerIdIn(containerIds);

        Set<Long> shipmentIds = shipmentsContainersMappingList.stream()
                .map(ShipmentsContainersMapping::getShipmentId)
                .collect(Collectors.toSet());

        return shipmentDao.findShipmentsByIds(shipmentIds);
    }

    private Map<Long, String> buildContainerMap(List<Containers> containers) {
        Map<Long, String> containerMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(containers)) {
            for (Containers c : containers) {
                if (c.getId() != null) {
                    containerMap.put(c.getId(), c.getContainerNumber());
                }
            }
        }
        return containerMap;
    }

    private boolean isMasterDataChange(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        boolean isMawbChange =  !Objects.equals(consolidationDetails.getMawb(), oldEntity.getMawb());
        boolean isCarrierSpacChange = false;
        if(consolidationDetails.getCarrierDetails() != null) {
            isCarrierSpacChange  = !(Objects.equals(
                    consolidationDetails.getCarrierDetails().getShippingLine(),
                    oldEntity.getCarrierDetails().getShippingLine()));
        }

        return isMawbChange || isCarrierSpacChange;
    }

    protected void setReceivingAndTriangulationBranch(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails.getReceivingBranch() != null && consolidationDetails.getReceivingBranch() == 0)
            consolidationDetails.setReceivingBranch(null);
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
    }

    protected void saveAwb(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) throws RunnerException {
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
    }

    protected boolean checkDisableFetchConditionForAwb(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
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

    private void setInterBranchContext(Boolean isInterBranchConsole) {
        if (Boolean.TRUE.equals(isInterBranchConsole))
            commonUtils.setInterBranchContextForHub();
    }

    /**
     * Calculates the achieved weight and volume for shipments in a consolidation,
     * determines chargeable weight, updates allocations, and prepares a summary response.
     *
     * <p>If no specific shipment IDs are provided, the method calculates for all shipments
     * under the given consolidation. Otherwise, it calculates for the provided subset of shipments.</p>
     *
     * @param request the request containing the consolidation ID and optional list of shipment IDs
     * @return {@link ShipmentGridChangeV3Response} containing the updated shipment summary
     * @throws RunnerException if there's an error during data retrieval or calculation
     */
    @Override
    public ShipmentGridChangeV3Response calculateAchievedValues(CalculateAchievedValueRequest request) throws RunnerException {
        ShipmentGridChangeV3Response response = new ShipmentGridChangeV3Response();
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(request.getConsolidationId());

        Set<ShipmentDetails> shipmentsToCalculate;

        if (ObjectUtils.isEmpty(request.getShipmentIds())) {
            // Calculate for all shipments under consolidation
            shipmentsToCalculate = new HashSet<>(consolidationDetails.getShipmentsList());
        } else {
            // Fetch and calculate only for specified shipment IDs
            List<ShipmentDetails> shipmentDetailsList = shipmentV3Service.getShipmentsFromId(new ArrayList<>(request.getShipmentIds()));
            shipmentsToCalculate = new HashSet<>(shipmentDetailsList);
        }

        calculateAchievedValues(consolidationDetails, response, shipmentsToCalculate);
        return response;
    }


    /**
     * Checks if chargeable weight can be set based on container category and transport mode.
     *
     * @param consolidationDetails the details to evaluate
     * @return true if the conditions are met, false otherwise
     */
    private boolean canSetChargableWeight(ConsolidationDetails consolidationDetails) {
        String transportMode = consolidationDetails.getTransportMode();
        String containerCategory = consolidationDetails.getContainerCategory();

        boolean isSeaLCL = commonUtils.isSeaLCL(transportMode, containerCategory);
        boolean isAir = TRANSPORT_MODE_AIR.equals(transportMode);
        boolean isRoadLCLorLTL = commonUtils.isRoadLCLorLTL(transportMode, containerCategory);

        // Valid if LCL + SEA or any AIR transport or ROAD + LCL or LTL
        return isSeaLCL || isAir || isRoadLCLorLTL;
    }

    /**
     * Returns shipment container count if shipment list is present.
     */
    private long getShipmentContainerCount(Containers containers, long shipmentCont) {
        if (ObjectUtils.isNotEmpty(containers.getShipmentsList())) {
            shipmentCont += containers.getContainerCount();
        }
        return shipmentCont;
    }

    /**
     * Returns shipment TEU count if shipment list is present.
     */
    private Double getShipmentTeu(Containers containers, Double shipmentTeu, EntityTransferContainerType entityTransferContainerType) {
        if (ObjectUtils.isNotEmpty(containers.getShipmentsList())) {
            shipmentTeu += containers.getContainerCount() * entityTransferContainerType.getTeu();
        }
        return shipmentTeu;
    }

    /**
     * Retrieves the container type entity from cache.
     */
    Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
        if (cacheMap.isEmpty()) {
            Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
            if(cache == null) return null;
            var cached = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
            return cached != null ? cached.get() : null;
        } else {
            return cacheMap.get(containers.getContainerCode());
        }
    }

    /**
     * Prepares container type cache from the consolidation container list.
     */
    private void processCacheAndContainerResponseList(ConsolidationDetails consolidationDetails,
            Set<String> containerTypes,
            Map<String, Map<String, String>> fieldNameKeyMap,
            Map<String, Object> cacheMap) {

        List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(
                consolidationDetails.getContainersList(), ContainerResponse.class);

        if (containerResponseList != null) {
            containerResponseList.forEach(containerResponse ->
                    containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(
                            containerResponse,
                            Containers.class,
                            fieldNameKeyMap,
                            Containers.class.getSimpleName() + containerResponse.getId(),
                            cacheMap
                    ))
            );
        }

        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
    }

    public void generateV3Events(ConsolidationDetails consolidationDetailsV3) {
        if (consolidationDetailsV3.getEventsList() == null) {
            consolidationDetailsV3.setEventsList(new ArrayList<>());
        }
        consolidationDetailsV3.getEventsList().add(createV3Event(consolidationDetailsV3, EventConstants.COCR));
    }

    private Events createV3Event(ConsolidationDetails consolidationDetails, String eventCode) {
        Events eventsV3 = new Events();
        // Set event fields from consolidation
        eventsV3.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
        eventsV3.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        eventsV3.setIsPublicTrackingEvent(true);
        eventsV3.setEntityType(Constants.CONSOLIDATION);
        eventsV3.setEntityId(consolidationDetails.getId());
        eventsV3.setTenantId(TenantContext.getCurrentTenant());
        eventsV3.setEventCode(eventCode);
        eventsV3.setConsolidationId(consolidationDetails.getId());
        eventsV3.setDirection(consolidationDetails.getShipmentType());
        // Persist the event
        eventDao.save(eventsV3);
        return eventsV3;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String attachShipments(ShipmentConsoleAttachDetachV3Request request) throws RunnerException {

        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        boolean isConsolePullCall = false;

        // Extract request details
        List<Long> shipmentIds = request.getShipmentIds().stream().toList();
        Long consolidationId = request.getConsolidationId();
        ShipmentRequestedType shipmentRequestedType = request.getShipmentRequestedType();
        boolean fromConsolidation = request.isFromConsolidation();

        // Ensure request is valid and has required IDs
        consolidationValidationV3Util.validateConsolidationIdAndShipmentIds(consolidationId, shipmentIds);

        // Fetch the corresponding consolidation record from the database
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);

        if(!Boolean.TRUE.equals(consolidationDetails.getOpenForAttachment()))
            throw new RunnerException("Attach/Detach not allowed");

        // Set inter-branch context if type is not explicitly provided
        if (Boolean.TRUE.equals(fromConsolidation))
            setContextIfNeededForHub(shipmentRequestedType, consolidationDetails);

        // Validate messaging logic for air consoles
        awbDao.validateAirMessaging(consolidationId);
        log.info("Air messaging validated for consolidationId: {}", consolidationId);

        ShipmentWtVolResponse oldShipmentWtVolResponse = calculateShipmentWtVol(consolidationDetails);

        // Fetch shipment details for all requested IDs
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));

        // Detect if this is an import direction console-pull scenario
        if (shipmentRequestedType == null
                && ObjectUtils.isNotEmpty(shipmentDetailsList)
                && DIRECTION_IMP.equalsIgnoreCase(shipmentDetailsList.get(0).getDirection())) {
            shipmentRequestedType = APPROVE;
            isConsolePullCall = true;
        }

        // Get a map of inter-branch import shipments
        Map<Long, ShipmentDetails> interBranchImportShipmentMap = getInterBranchImportShipmentMap(shipmentDetailsList, consolidationDetails);

        // Track inter-branch shipments that are requested or approved
        Set<Long> interBranchRequestedShipIds = new HashSet<>();
        Set<Long> interBranchApprovedShipIds = new HashSet<>();

        // Store mappings of shipments that will be used to send auto-rejection emails (only for same-branch duplicates)
        List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = new ArrayList<>();

        if (shipmentRequestedType == null) {
            // Identify inter-branch shipments by comparing tenant IDs
            interBranchRequestedShipIds = shipmentDetailsList.stream()
                    .filter(shipmentDetails -> ObjectUtils.notEqual(shipmentDetails.getTenantId(), UserContext.getUser().getTenantId()))
                    .map(ShipmentDetails::getId)
                    .collect(Collectors.toSet());

            // Extract local shipments (i.e., same branch)
            var newShipmentIds = new ArrayList<>(shipmentIds);
            newShipmentIds.removeAll(interBranchRequestedShipIds);

            if (ObjectUtils.isNotEmpty(newShipmentIds)) {
                // Prepare filter criteria to fetch pending state mappings for same-branch shipments
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, newShipmentIds, "IN", null);
                listCommonRequest = andCriteria(Constants.IS_ATTACHMENT_DONE, false, "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);

                // Get mappings for email notifications before deletion
                consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(
                        consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(),
                        ConsoleShipmentMapping.class
                );

                // Delete any pending mappings for these shipments
                consoleShipmentMappingDao.deletePendingStateByShipmentIds(newShipmentIds);
            }
        } else if (!isConsolePullCall) {
            // Collect inter-branch shipments (i.e., different tenant from consolidation) that are being explicitly approved
            interBranchApprovedShipIds = shipmentDetailsList.stream()
                    .filter(shipmentDetails -> ObjectUtils.notEqual(shipmentDetails.getTenantId(), consolidationDetails.getTenantId()))
                    .map(ShipmentDetails::getId)
                    .collect(Collectors.toSet());
        }

        // Fetch all consol shipment mappings from shipment ids
        List<ConsoleShipmentMapping> consoleShipmentMappings = getConsoleShipmentMappingsFromShipmentIds(shipmentIds);

        // Final validation before attachment process begins
        consolidationValidationV3Util.validationsBeforeAttachShipments(consolidationDetails, consoleShipmentMappings, shipmentIds, consolidationId, shipmentDetailsList,
                fromConsolidation);

        // Attach the shipments and track those that were successfully attached
        HashSet<Long> attachedShipmentIds = consoleShipmentMappingDao.assignShipments(shipmentRequestedType, consolidationId, shipmentIds,
                consoleShipmentMappings, interBranchRequestedShipIds, interBranchApprovedShipIds, interBranchImportShipmentMap);

        // Collect party details from attached shipments for further processing (agents, parties)
        Set<Parties> originParties = new HashSet<>();
        Set<Parties> destinationParties = new HashSet<>();

        // Populate parties and perform post-processing on attached shipments
        processShipmentDetailsList(consolidationId, shipmentDetailsList, attachedShipmentIds, interBranchRequestedShipIds, consolidationDetails, originParties, destinationParties);

        // Set the sending and receiving agents based on party info and consolidation data
        processSendingAgentAndReceivingAgent(consolidationDetails, originParties, destinationParties);

        // Perform SCI check on the updated console (e.g., validity or legal compliance)
        checkSciForAttachConsole(consolidationId);

        // Detach shipment entities to ensure they are no longer tracked by the persistence context
        shipmentDao.entityDetach(shipmentDetailsList);

        // Refresh linked shipment data after attachment process
        updateLinkedShipmentData(consolidationDetails, null, true, new HashMap<>(), fromConsolidation);

        // If any inter-console linkage needs to be handled, process it now
        processInterConsoleAttachShipment(consolidationDetails, shipmentDetailsList);

        // update consolidation Cargo Summary
        updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);

        // Check if special console (e.g., non-dangerous goods) needs post-attachment logic
        processNonDgConsole(consolidationDetails, shipmentDetailsList);

        // Intersect attached inter-branch shipments with what was initially marked
        interBranchRequestedShipIds.retainAll(attachedShipmentIds);

        // Process logic for attached inter-branch imports
        processInterBranchImportShipmentMap(interBranchImportShipmentMap, isConsolePullCall,
                shipmentRequestedTypes, consolidationDetails);

        // Send email notifications for accepted and rejected shipment mappings
        sendAcceptedAndRejectionEmails(interBranchRequestedShipIds, consolidationDetails,
                shipmentRequestedTypes, consoleShipmentMappingsForEmails, shipmentDetailsList);


        String warning = null;
        if (!shipmentRequestedTypes.isEmpty()) {
            warning = TEMPLATE_NOT_FOUND_MESSAGE;
        }
        return warning;
    }

    @NotNull
    private List<ConsoleShipmentMapping> getConsoleShipmentMappingsFromShipmentIds(List<Long> shipmentIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
        Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
        return consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent();
    }


    private void sendAcceptedAndRejectionEmails(Set<Long> interBranchRequestedShipIds, ConsolidationDetails consolidationDetails, Set<ShipmentRequestedType> shipmentRequestedTypes,
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails, List<ShipmentDetails> shipmentDetailsList) {
        if (!interBranchRequestedShipIds.isEmpty()) // send email for pull requested when called from controller directly
        {
            sendEmailForPullRequested(consolidationDetails, interBranchRequestedShipIds.stream().toList(), shipmentRequestedTypes);
        }
        if (!consoleShipmentMappingsForEmails.isEmpty()) { // send email for pull/push rejected for other consolidations when called from controller directly
            List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(e -> e.getConsolidationId()).toList();
            List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
            commonUtils.sendRejectionEmailsExplicitly(shipmentDetailsList, consoleShipmentMappingsForEmails, shipmentRequestedTypes, otherConsolidationDetails);
        }
    }

    public void sendEmailForPullRequested(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests = new EnumMap<>(ShipmentRequestedType.class);
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
        for (ShipmentDetails shipmentDetails1 : shipmentDetails.getContent()) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(
                Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(
                Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull)
                        .toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)),
                executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for (Long shipmentId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipmentId), consolidationDetails, SHIPMENT_PULL_REQUESTED, null, emailTemplatesRequests,
                        shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, null, null);
            } catch (Exception e) {
                log.error("Error while sending email");
            }
        }
    }

    private void processInterBranchImportShipmentMap(Map<Long, ShipmentDetails> interBranchImportShipmentMap, boolean isConsolePullCall,
            Set<ShipmentRequestedType> shipmentRequestedTypes, ConsolidationDetails consolidationDetails) {
        if (!interBranchImportShipmentMap.isEmpty() && isConsolePullCall) {
            for (ShipmentDetails shipmentDetails : interBranchImportShipmentMap.values()) {
                var emailTemplatesRequestsModel = commonUtils.getEmailTemplates(IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL);
                if (Objects.isNull(emailTemplatesRequestsModel) || emailTemplatesRequestsModel.isEmpty()) {
                    shipmentRequestedTypes.add(APPROVE);
                }
                if (shipmentRequestedTypes.isEmpty()) {
                    sendImportShipmentPullAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequestsModel);
                }
            }
        }
    }

    protected ResponseEntity<IRunnerResponse> sendImportShipmentPullAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
            List<EmailTemplatesRequest> emailTemplatesRequestsModel) {

        var emailTemplateModel = emailTemplatesRequestsModel.stream().findFirst().orElse(new EmailTemplatesRequest());
        List<String> toEmailsList = new ArrayList<>();
        List<String> ccEmailsList = new ArrayList<>();
        if (shipmentDetails.getCreatedBy() != null) {
            toEmailsList.add(shipmentDetails.getCreatedBy());
        }
        if (shipmentDetails.getAssignedTo() != null) {
            toEmailsList.add(shipmentDetails.getAssignedTo());
        }

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.add(shipmentDetails.getTenantId());

        Map<String, Object> dictionary = new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(
                        () -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)),
                executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(
                Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull)
                        .toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)),
                executorService);

        CompletableFuture.allOf(carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture).join();
        commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, consolidationDetails.getTenantId(), false);
        ccEmailsList.addAll(new ArrayList<>(toEmailIds));
        ccEmailsList.addAll(new ArrayList<>(ccEmailIds));
        if (shipmentDetails.getCreatedBy() == null || shipmentDetails.getAssignedTo() == null) {
            toEmailIds.clear();
            ccEmailIds.clear();
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, shipmentDetails.getTenantId(), true);
            toEmailsList.addAll(new ArrayList<>(toEmailIds));
        }

        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, toEmailsList, ccEmailsList);

        return ResponseHelper.buildSuccessResponse();
    }

    private void processNonDgConsole(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetailsList) {
        if (checkForNonDGConsoleAndAirDGFlag(consolidationDetails) || checkForOceanNonDGConsolidation(consolidationDetails)) {
            List<ShipmentDetails> shipments = shipmentDetailsList.stream().filter(sd -> Boolean.TRUE.equals(sd.getContainsHazardous())).toList();
            if (ObjectUtils.isNotEmpty(shipments)) {
                consolidationDetails.setHazardous(true);
                if (!consolidationValidationV3Util.checkConsolidationTypeValidation(consolidationDetails)) {
                    throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
                }
                consolidationDetailsDao.updateV3(consolidationDetails, true);
            }
        }
    }

    public void updateConsolidationCargoSummary(ConsolidationDetails consolidationDetails, ShipmentWtVolResponse oldShipmentWtVolResponse) throws RunnerException {
        if(consolidationDetails == null)
            return;
        List<ShipmentDetails> shipmentDetailsList = getShipmentDetailsListFromConsoleMapping(consolidationDetails.getId());
        updateConsolidationCargoSummary(consolidationDetails, shipmentDetailsList, oldShipmentWtVolResponse);
    }

    public void updateConsolidationCargoSummary(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetailsList,
                                                ShipmentWtVolResponse oldShipmentWtVolResponse) throws RunnerException {
        if(consolidationDetails == null)
            return;
        List<Containers> containersList = containerDao.findByConsolidationId(consolidationDetails.getId());
        ShipmentWtVolResponse newShipmentWtVolResponse = calculateShipmentWtVol(consolidationDetails.getTransportMode(), shipmentDetailsList, containersList);
        AchievedQuantities achievedQuantities = consolidationDetails.getAchievedQuantities();
        if(Objects.isNull(achievedQuantities)) {
            achievedQuantities = new AchievedQuantities();
            consolidationDetails.setAchievedQuantities(achievedQuantities);
        }

        populateWeightAndVolumeFields(consolidationDetails.getTransportMode(), newShipmentWtVolResponse, achievedQuantities, oldShipmentWtVolResponse);
        populatePackageFields(newShipmentWtVolResponse, achievedQuantities, oldShipmentWtVolResponse);
        populateCountFields(newShipmentWtVolResponse, achievedQuantities, oldShipmentWtVolResponse);

        consolidationDetails.setContainersList(containersList);
        consolidationDetailsDao.updateV3(consolidationDetails, true);
    }

    private List<ShipmentDetails> getShipmentDetailsListFromConsoleMapping(Long consolidationId) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        if(!listIsNullOrEmpty(consoleShipmentMappings)) {
            shipmentDetailsList = shipmentDao.findShipmentsByIds(consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).collect(Collectors.toSet()));
        }
        return shipmentDetailsList;
    }

    private void populateWeightAndVolumeFields(String transportMode,
                                               ShipmentWtVolResponse newShipmentWtVolResponse,
                                               AchievedQuantities achievedQuantities,
                                               ShipmentWtVolResponse oldShipmentWtVolResponse) throws RunnerException {
        if(checkVolumeIsSame(oldShipmentWtVolResponse.getVolume(), oldShipmentWtVolResponse.getVolumeUnit(),
                achievedQuantities.getConsolidatedVolume(), achievedQuantities.getConsolidatedVolumeUnit())) {
            achievedQuantities.setConsolidatedVolume(newShipmentWtVolResponse.getVolume());
            achievedQuantities.setConsolidatedVolumeUnit(newShipmentWtVolResponse.getVolumeUnit());
        }
        if(checkWeightIsSame(oldShipmentWtVolResponse.getWeight(), oldShipmentWtVolResponse.getWeightUnit(),
                achievedQuantities.getConsolidatedWeight(), achievedQuantities.getConsolidatedWeightUnit())) {
            achievedQuantities.setConsolidatedWeight(newShipmentWtVolResponse.getWeight());
            achievedQuantities.setConsolidatedWeightUnit(newShipmentWtVolResponse.getWeightUnit());
        }
        if(TRANSPORT_MODE_AIR.equals(transportMode) ||
                TRANSPORT_MODE_ROA.equals(transportMode)) {
            if(checkWeightIsSame(oldShipmentWtVolResponse.getWeightVolume(), oldShipmentWtVolResponse.getWeightVolumeUnit(),
                    achievedQuantities.getWeightVolume(), achievedQuantities.getWeightVolumeUnit())) {
                achievedQuantities.setWeightVolume(newShipmentWtVolResponse.getWeightVolume());
                achievedQuantities.setWeightVolumeUnit(newShipmentWtVolResponse.getWeightVolumeUnit());
            }
            if(checkWeightIsSame(oldShipmentWtVolResponse.getChargable(), oldShipmentWtVolResponse.getChargeableUnit(),
                    achievedQuantities.getConsolidationChargeQuantity(), achievedQuantities.getConsolidationChargeQuantityUnit())) {
                achievedQuantities.setConsolidationChargeQuantity(newShipmentWtVolResponse.getChargable());
                achievedQuantities.setConsolidationChargeQuantityUnit(newShipmentWtVolResponse.getChargeableUnit());
            }
        } else {
            if(checkVolumeIsSame(oldShipmentWtVolResponse.getWeightVolume(), oldShipmentWtVolResponse.getWeightVolumeUnit(),
                    achievedQuantities.getWeightVolume(), achievedQuantities.getWeightVolumeUnit())) {
                achievedQuantities.setWeightVolume(newShipmentWtVolResponse.getWeightVolume());
                achievedQuantities.setWeightVolumeUnit(newShipmentWtVolResponse.getWeightVolumeUnit());
            }
            if(checkVolumeIsSame(oldShipmentWtVolResponse.getChargable(), oldShipmentWtVolResponse.getChargeableUnit(),
                    achievedQuantities.getConsolidationChargeQuantity(), achievedQuantities.getConsolidationChargeQuantityUnit())) {
                achievedQuantities.setConsolidationChargeQuantity(newShipmentWtVolResponse.getChargable());
                achievedQuantities.setConsolidationChargeQuantityUnit(newShipmentWtVolResponse.getChargeableUnit());
            }
        }
    }

    private void populatePackageFields(ShipmentWtVolResponse newShipmentWtVolResponse,
                                       AchievedQuantities achievedQuantities,
                                       ShipmentWtVolResponse oldShipmentWtVolResponse) {
        if(checkPackagesIsSame(oldShipmentWtVolResponse.getPacks(), oldShipmentWtVolResponse.getPacksType(),
                achievedQuantities.getPacks(), achievedQuantities.getPacksType())) {
            achievedQuantities.setPacks(Objects.equals(newShipmentWtVolResponse.getPacks(), 0) ? null : newShipmentWtVolResponse.getPacks());
            achievedQuantities.setPacksType(newShipmentWtVolResponse.getPacksType());
        }
        if(checkPackagesIsSame(oldShipmentWtVolResponse.getDgPacks(), oldShipmentWtVolResponse.getDgPacksType(),
                achievedQuantities.getDgPacks(), achievedQuantities.getDgPacksType())) {
            achievedQuantities.setDgPacks(Objects.equals(newShipmentWtVolResponse.getDgPacks(), 0) ? null : newShipmentWtVolResponse.getDgPacks());
            achievedQuantities.setDgPacksType(newShipmentWtVolResponse.getDgPacksType());
        }
    }

    private void populateCountFields(ShipmentWtVolResponse newShipmentWtVolResponse,
                                     AchievedQuantities achievedQuantities,
                                     ShipmentWtVolResponse oldShipmentWtVolResponse) {
        if(checkCountIsSame(oldShipmentWtVolResponse.getConsoleContainerCount(), achievedQuantities.getContainerCount())) {
            achievedQuantities.setContainerCount(newShipmentWtVolResponse.getConsoleContainerCount());
        }
        if(checkCountIsSame(oldShipmentWtVolResponse.getConsoleDgContainerCount(), achievedQuantities.getDgContainerCount())) {
            achievedQuantities.setDgContainerCount(newShipmentWtVolResponse.getConsoleDgContainerCount());
        }
        if(checkCountIsSame(oldShipmentWtVolResponse.getSlacCount(), achievedQuantities.getSlacCount())) {
            achievedQuantities.setSlacCount(newShipmentWtVolResponse.getSlacCount());
        }
        if(checkBigDecimalCountIsSame(oldShipmentWtVolResponse.getConsoleTeuCount(), achievedQuantities.getTeuCount())) {
            achievedQuantities.setTeuCount(newShipmentWtVolResponse.getConsoleTeuCount());
        }
    }

    private boolean checkWeightIsSame(BigDecimal weight1, String weightUnit1, BigDecimal weight2, String weightUnit2) throws RunnerException {
        if(!Objects.isNull(weight1) && !Objects.isNull(weight2) && !isStringNullOrEmpty(weightUnit1) && !isStringNullOrEmpty(weightUnit2)) {
            return weight2.compareTo(new BigDecimal(convertUnit(MASS, weight1, weightUnit1, weightUnit2).toString())) == 0;
        }
        return (Objects.isNull(weight1) || BigDecimal.ZERO.compareTo(weight1) == 0) &&
                (Objects.isNull(weight2) || BigDecimal.ZERO.compareTo(weight2) == 0);
    }

    private boolean checkVolumeIsSame(BigDecimal volume1, String volumeUnit1, BigDecimal volume2, String volumeUnit2) throws RunnerException {
        if(!Objects.isNull(volume1) && !Objects.isNull(volume2) && !isStringNullOrEmpty(volumeUnit1) && !isStringNullOrEmpty(volumeUnit2)) {
            return volume2.compareTo(new BigDecimal(convertUnit(VOLUME, volume1, volumeUnit1, volumeUnit2).toString())) == 0;
        }
        return (Objects.isNull(volume1) || BigDecimal.ZERO.compareTo(volume1) == 0) &&
                (Objects.isNull(volume2) || BigDecimal.ZERO.compareTo(volume2) == 0);
    }

    private boolean checkPackagesIsSame(Integer packages1, String packagesUnit1, Integer packages2, String packagesUnit2) {
        if(!Objects.isNull(packages1) && !Objects.isNull(packages2) && !isStringNullOrEmpty(packagesUnit1) && !isStringNullOrEmpty(packagesUnit2)) {
            return packages2.equals(packages1);
        }
        return (Objects.isNull(packages1) || Objects.equals(packages1, 0)) &&
                (Objects.isNull(packages2) || Objects.equals(packages2, 0));
    }

    private boolean checkCountIsSame(Integer count1, Integer count2) {
        if(!Objects.isNull(count1) && !Objects.isNull(count2)) {
            return count2.equals(count1);
        }
        return (Objects.isNull(count1) || Objects.equals(count1, 0)) &&
                (Objects.isNull(count2) || Objects.equals(count2, 0));
    }

    private boolean checkBigDecimalCountIsSame(BigDecimal count1, BigDecimal count2) {
        if(!Objects.isNull(count1) && !Objects.isNull(count2)) {
            return Objects.equals(count2.doubleValue(), count1.doubleValue());
        }
        return (Objects.isNull(count1) || BigDecimal.ZERO.compareTo(count1) == 0) &&
                (Objects.isNull(count2) || BigDecimal.ZERO.compareTo(count2) == 0);
    }


    private boolean checkForOceanNonDGConsolidation(ConsolidationDetails consolidationDetails) {
        return Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    private boolean checkForNonDGConsoleAndAirDGFlag(ConsolidationDetails consolidationDetails) {
        if (!checkForAirDGFlag(consolidationDetails)) {
            return false;
        }
        return !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    public boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag())) {
            return false;
        }
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    protected void processInterConsoleAttachShipment(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        try {
            if (!isValidRequest(console, shipments)) {
                return;
            }

            boolean isConsoleAcceptedCase = isConsoleAccepted(console);
            if (isConsoleAcceptedCase) {
                return;
            }

            List<Long> shipmentIds = getShipmentIds(shipments);

            Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap = getShipmentNetworkTransferMap(shipmentIds);

            Long consoleReceivingBranch = console.getReceivingBranch();
            List<ShipmentDetails> shipmentsForHiddenNte = new ArrayList<>();
            List<NetworkTransfer> nteToUpdate = new ArrayList<>();
            List<NetworkTransfer> nteToDelete = new ArrayList<>();

            if (consoleReceivingBranch == null && shipmentNetworkTransferMap != null) {
                List<NetworkTransfer> allNetworkTransfers = shipmentNetworkTransferMap.values().stream()
                        .flatMap(innerMap -> innerMap.values().stream()).toList();
                allNetworkTransfers.forEach(networkTransferService::deleteNetworkTransferEntity);
                return;
            }

            for (ShipmentDetails shipment : shipments) {
                processNTEConsoleShipment(consoleReceivingBranch, shipment, shipmentNetworkTransferMap,
                        shipmentsForHiddenNte, nteToUpdate, nteToDelete);
            }

            if (!shipmentsForHiddenNte.isEmpty()) {
                networkTransferService.bulkProcessInterConsoleNte(shipmentsForHiddenNte);
            }
            if (!nteToUpdate.isEmpty()) {
                networkTransferDao.saveAll(nteToUpdate);
            }
        } catch (Exception e) {
            log.error("Error in attach shipment process: ", e.getMessage());
        }
    }

    private void processDbNte(Map<Integer, NetworkTransfer> nteMap, Long receivingBranch, List<NetworkTransfer> nteToDelete) {
        NetworkTransfer dbNte = nteMap.values().stream().findFirst().orElse(null);
        if (dbNte != null && dbNte.getTenantId() != receivingBranch.intValue()) {
            nteToDelete.add(dbNte);
        }
    }

    private void processNTEConsoleShipment(Long consoleReceivingBranch, ShipmentDetails shipment,
            Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap,
            List<ShipmentDetails> shipmentsForHiddenNte, List<NetworkTransfer> nteToUpdate,
            List<NetworkTransfer> nteToDelete) {
        Long receivingBranch = shipment.getReceivingBranch();
        if (receivingBranch == null) {
            return;
        }
        NetworkTransfer networkTransfer = null;
        Map<Integer, NetworkTransfer> tenantMap = shipmentNetworkTransferMap != null ? shipmentNetworkTransferMap.get(shipment.getId()) : null;

        if (tenantMap != null) {
            processDbNte(tenantMap, receivingBranch, nteToDelete);
            networkTransfer = tenantMap.get(receivingBranch.intValue());
        }

        if (Objects.equals(receivingBranch, consoleReceivingBranch)) {
            if (networkTransfer == null) {
                shipmentsForHiddenNte.add(shipment);
            } else {
                networkTransfer.setIsHidden(Boolean.TRUE);
                nteToUpdate.add(networkTransfer);
            }
        } else if (networkTransfer == null) {
            networkTransferService.processNetworkTransferEntity(
                    receivingBranch, null, Constants.SHIPMENT,
                    shipment, null, Constants.DIRECTION_IMP, null, true);
        }
    }

    private Map<Long, Map<Integer, NetworkTransfer>> getShipmentNetworkTransferMap(List<Long> shipmentIds) {
        return networkTransferDao.getInterConsoleNTList(shipmentIds, Constants.SHIPMENT).stream()
                .collect(Collectors.groupingBy(
                        NetworkTransfer::getEntityId,
                        toMap(NetworkTransfer::getTenantId, transfer -> transfer)
                ));
    }

    private List<Long> getShipmentIds(List<ShipmentDetails> shipments) {
        return shipments.stream().map(ShipmentDetails::getId).filter(Objects::nonNull).toList();
    }

     boolean isConsoleAccepted(ConsolidationDetails console) {
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(console.getId()), CONSOLIDATION);
        if (networkTransferList != null && !networkTransferList.isEmpty()) {
            for (NetworkTransfer networkTransfer : networkTransferList) {
                if (Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS)) {
                    continue;
                }
                if (networkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isValidRequest(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isValidShipmentType = console.getShipmentType() != null && Constants.DIRECTION_EXP.equals(console.getShipmentType());
        return Boolean.TRUE.equals(console.getInterBranchConsole()) && !listIsNullOrEmpty(shipments) && Boolean.TRUE.equals(
                shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()) && isValidShipmentType;
    }

    /**
     * Determines whether the provided console should be processed by comparing it with a previous version.
     * <p>
     * Returns true if: - The console is new (oldEntity is null). - Any significant field has changed between the console and the oldEntity.
     * <p>
     * Fields compared include: general details, carrier details, routing information, parties, and triangulation partners.
     *
     * @param console   The new or updated `ConsolidationDetails` object.
     * @param oldEntity The existing `ConsolidationDetails` object.
     * @return true if the console should be processed; false otherwise.
     */
    protected boolean canProcessConsole(ConsolidationDetails console, ConsolidationDetails oldEntity, Map<Long, ShipmentDetails> dgStatusChangeInShipments) {
        return console != null && (oldEntity == null || !Objects.equals(console.getBol(), oldEntity.getBol()) ||
                !Objects.equals(console.getShipmentType(), oldEntity.getShipmentType()) ||
                !CollectionUtils.isEmpty(console.getRoutingsList()) ||
                !Objects.equals(console.getCarrierBookingRef(), oldEntity.getCarrierBookingRef()) ||
                !Objects.equals(console.getBookingNumber(), oldEntity.getBookingNumber()) ||
                !Objects.equals(console.getConsolidationType(), oldEntity.getConsolidationType()) ||
                !Objects.equals(console.getIncoterms(), oldEntity.getIncoterms()) ||
                (console.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                        (!Objects.equals(console.getCarrierDetails().getVoyage(), oldEntity.getCarrierDetails().getVoyage()) ||
                                !Objects.equals(console.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()) ||
                                !Objects.equals(console.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
                                !Objects.equals(console.getCarrierDetails().getAircraftType(), oldEntity.getCarrierDetails().getAircraftType()) ||
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
                                !Objects.equals(console.getCarrierDetails().getAta(), oldEntity.getCarrierDetails().getAta()) ||
                                canProcesscutOffFields(console, oldEntity)
                        ))  || !dgStatusChangeInShipments.isEmpty()  ||
                !CommonUtils.checkSameParties(console.getSendingAgent(), oldEntity.getSendingAgent()) ||
                !CommonUtils.checkSameParties(console.getReceivingAgent(), oldEntity.getReceivingAgent())
                || !Objects.equals(console.getReferenceNumber(), oldEntity.getReferenceNumber())
                || !Objects.equals(console.getCoLoadCarrierName(), oldEntity.getCoLoadCarrierName())
                || !Objects.equals(console.getPartner(), oldEntity.getPartner())
                || !Objects.equals(console.getBookingAgent(), oldEntity.getBookingAgent())
                || !Objects.equals(console.getCoLoadBookingReference(), oldEntity.getCoLoadBookingReference())
                || !Objects.equals(console.getCoLoadMBL(), oldEntity.getCoLoadMBL())
                || !Objects.equals(console.getDeliveryMode(), oldEntity.getDeliveryMode())
        );
    }

    /**
     * Updates all shipments linked to a given consolidation with new data.
     * <p>
     * This includes validating EFreight status for air transport, updating shipment fields, handling DG status changes, triggering relevant events, and syncing with external
     * systems (e.g., if shipment is attached via UI).
     *
     * @param console                   The latest consolidation details
     * @param oldConsolEntity           The previous state of the consolidation
     * @param fromAttachShipment        True if update is triggered from the attach shipment screen
     * @param dgStatusChangeInShipments Map containing shipment ID to updated DG status (if any)
     * @return List of updated shipments
     * @throws RunnerException If EFreight status is invalid or CFS cutoff validation fails
     */
    public List<ShipmentDetails> updateLinkedShipmentData(ConsolidationDetails console, ConsolidationDetails oldConsolEntity,
            Boolean fromAttachShipment, Map<Long, ShipmentDetails> dgStatusChangeInShipments, Boolean fromConsole) throws RunnerException {

        // This will ve true if console is/was interbranch enabled
        boolean isOrWasInterBranchConsole = Optional.ofNullable(console.getInterBranchConsole()).orElse(false) ||
                Optional.ofNullable(oldConsolEntity)
                        .map(e -> e.getInterBranchConsole())
                        .orElse(false);

        // Set interbranc context if Interbranch Flag is/was enabled
        if (Boolean.TRUE.equals(fromConsole) && isOrWasInterBranchConsole)
            commonUtils.setInterBranchContextForHub();

        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        List<ShipmentDetails> shipments = null;

        // Check if Air Messaging is enabled and consolidation is air with EAW (E-Freight)
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) &&
                Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) &&
                Objects.equals(console.getEfreightStatus(), Constants.EAW)) {

            shipments = consolidationV3Util.getShipmentsList(console.getId());

            // Validate that no shipment has NON as EFreight status when console is EAW
            var shipmentlist = shipments.stream()
                    .filter(shipmentDetails -> Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), Constants.NON)).toList();

            if (shipmentlist != null && !shipmentlist.isEmpty()) {
                throw new RunnerException("EFreight status can only be EAP or NON as one of the Shipment has EFreight status as NON");
            }
        }

        // Proceed only if changes are allowed on the consolidation
        if (canProcessConsole(console, oldConsolEntity, dgStatusChangeInShipments)) {
            if (shipments == null) {
                shipments = consolidationV3Util.getShipmentsList(console.getId());
            }

            List<EventsRequest> events = new ArrayList<>();
            // Update each linked shipment and collect relevant event triggers
            for (ShipmentDetails sd : shipments) {
                updateLinkedShipments(console, oldConsolEntity, fromAttachShipment, dgStatusChangeInShipments, sd, events);
            }
            updateShipmentDetailsIfConsolidationChanged(oldConsolEntity, console, shipments, fromAttachShipment);
            // Persist updated shipment details and event logs
            shipmentV3Service.saveAll(shipments);
            eventV3Service.saveAllEvent(events);
        }

        return shipments;
    }

    protected void setBookingNumberInShipment(ConsolidationDetails console, ConsolidationDetails oldEntity, ShipmentDetails shipmentDetails, Boolean fromAttachShipment) {
        if(Boolean.FALSE.equals(fromAttachShipment)){
            String oldBookingNumber = oldEntity.getBookingNumber();
            String newBookingNumber = console.getBookingNumber();

            if(isFieldChanged(oldBookingNumber, newBookingNumber)){
                shipmentDetails.setBookingNumber(console.getBookingNumber());
            }
        }else{
            shipmentDetails.setBookingNumber(console.getBookingNumber());
        }
    }

    /**
     * Updates carrier-related details in a linked shipment from the given consolidation.
     * <p>
     * This method handles both general and transport-mode-specific fields, such as voyage, vessel, flight number, ETA/ETD, etc. If the consolidation was attached from the shipment
     * screen (`fromAttachShipment` is true), additional fields like ETA, ETD, and flight number are copied.
     *
     * @param console         The consolidation from which carrier details are sourced
     * @param shipmentDetails The shipment to update
     */
    private void updateCarrierDetailsForLinkedShipments(ConsolidationDetails console, ShipmentDetails shipmentDetails) {
        if (console.getCarrierDetails() != null) {
            // Basic carrier detail updates from consolidation to shipment
            shipmentDetails.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(console.getCarrierDetails().getCfs());
            if(StringUtility.isNotEmpty(console.getCarrierDetails().getShippingLine())) {
                shipmentDetails.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
            }

            // If transport mode is air, update air-specific fields like flight number
            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                shipmentDetails.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
            }
        }
    }

    /**
     * Updates the shipment details linked to a consolidation. This includes: - Updating references like BOL, booking number, direction, and routing - Setting event for booking
     * number change (BOCO) - Syncing inter-branch console fields - Updating carrier and DG (hazardous) information - Performing CFS cut-off validation against shipment gate-in
     * date - Syncing main carriage routing conditionally - Updating inter-branch specific broker fields
     *
     * @param console                   New consolidation data
     * @param oldEntity                 Old consolidation entity (used for comparison)
     * @param fromAttachShipment        Flag to determine if update is from attachment
     * @param dgStatusChangeInShipments Map of shipment ID to updated DG status
     * @param shipmentDetails           The shipment to be updated
     * @param events                    List to collect any triggered events
     * @throws RunnerException If cut-off validation fails
     */
    private void updateLinkedShipments(ConsolidationDetails console, ConsolidationDetails oldEntity,
            Boolean fromAttachShipment, Map<Long, ShipmentDetails> dgStatusChangeInShipments,
            ShipmentDetails shipmentDetails,
            List<EventsRequest> events) throws RunnerException {

        String transportMode = console.getTransportMode();
        shipmentDetails.setConsolRef(console.getConsolidationNumber());
        shipmentDetails.setJobType(console.getConsolidationType());
        shipmentDetails.setBookingAgent(console.getBookingAgent());
        serviceTypeAutoPopulation(console, shipmentDetails);

        if(TRANSPORT_MODE_SEA.equalsIgnoreCase(transportMode)){
            //Non-Editable Fields
            shipmentDetails.setPartner(console.getPartner());

            shipmentDetails.setMasterBill(console.getBol());
            updateCarrierDetailsForLinkedShipments(console, shipmentDetails);
            setBookingNumberInShipment(console, oldEntity, shipmentDetails, fromAttachShipment);

            //Editable Fields
            setColoadBookingFields(console, oldEntity, shipmentDetails, fromAttachShipment);
            partnerRelatedFieldAutopopulation(console, oldEntity, shipmentDetails, fromAttachShipment);

        }else if(TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode)){
            //Non-Editable Fields
            shipmentDetails.setShipmentType(console.getContainerCategory());
            shipmentDetails.setPartner(console.getPartner());
            shipmentDetails.setMasterBill(console.getBol());
            updateCarrierDetailsForLinkedShipments(console, shipmentDetails);

            //Editable Fields
            setColoadBookingFields(console, oldEntity, shipmentDetails, fromAttachShipment);
            partnerRelatedFieldAutopopulation(console, oldEntity, shipmentDetails, fromAttachShipment);
            setIncoTerms(console, oldEntity, shipmentDetails, fromAttachShipment);
            setBookingNumberInShipment(console, oldEntity, shipmentDetails, fromAttachShipment);
        }

        if(dgStatusChangeInShipments.containsKey(shipmentDetails.getId())) {
            shipmentDetails.setContainsHazardous(dgStatusChangeInShipments.get(shipmentDetails.getId()).getContainsHazardous());
            shipmentDetails.setOceanDGStatus(dgStatusChangeInShipments.get(shipmentDetails.getId()).getOceanDGStatus());
        }
        // Update basic references in the shipment from the console
        // Set new booking number and create BOCO event if changed
        String oldBookingNumber = shipmentDetails.getBookingNumber();

        if (!Objects.equals(oldBookingNumber, shipmentDetails.getBookingNumber())
                && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            events.add(commonUtils.prepareEventRequest(shipmentDetails.getId(), EventConstants.BOCO, SHIPMENT, shipmentDetails.getBookingNumber()));
        }

        // If the console is marked as an inter-branch console, update shipment with:
        // - Triangulation partner list (copied as new list to avoid mutation)
        // - Triangulation partner
        // - Documentation partner
        // - Receiving branch (only if it hasn't already been added)
        if (Boolean.TRUE.equals(console.getInterBranchConsole())) {
            shipmentDetails.setTriangulationPartnerList(console.getTriangulationPartnerList() != null
                    ? new ArrayList<>(console.getTriangulationPartnerList()) : null);
            shipmentDetails.setTriangulationPartner(console.getTriangulationPartner());
            shipmentDetails.setDocumentationPartner(console.getDocumentationPartner());

            if (!Boolean.TRUE.equals(shipmentDetails.getIsReceivingBranchAdded())) {
                shipmentDetails.setReceivingBranch(console.getReceivingBranch());
            }
        }

        // Perform CFS cut-off date validation if enabled and required
        if (consolidationV3Util.checkConsolidationEligibleForCFSValidation(console)
                && consolidationValidationV3Util.checkIfShipmentDateGreaterThanConsole(shipmentDetails.getShipmentGateInDate(), console.getCfsCutOffDate())) {
            throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
        }

        // Sync main carriage routing from console to shipment
        syncMainCarriageRoutingToShipment(console.getRoutingsList(), shipmentDetails);

        // Update export/import brokers if inter-branch logic applies
        updateNonInterBranchConsoleData(console, shipmentDetails);
    }

    protected void setIncoTerms(ConsolidationDetails console, ConsolidationDetails oldEntity, ShipmentDetails shipmentDetails, Boolean fromAttachShipment) {
        if (Boolean.FALSE.equals(fromAttachShipment)) {
            String oldIncoTerms = oldEntity.getIncoterms();
            String newIncoTerms = console.getIncoterms();

            if(isFieldChanged(oldIncoTerms, newIncoTerms)){
                shipmentDetails.setIncoterms(console.getIncoterms());
            }
        }else{
            shipmentDetails.setIncoterms(console.getIncoterms());
        }
    }

    private void partnerRelatedFieldAutopopulation(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ShipmentDetails shipmentDetails,
        Boolean fromAttachShipment){
        if (Boolean.FALSE.equals(fromAttachShipment)) {
            String oldPartner = oldEntity.getPartner();
            String newPartner = consolidationDetails.getPartner();

            if(isFieldChanged(oldPartner, newPartner)){
                shipmentDetails.setCoLoadCarrierName(consolidationDetails.getCoLoadCarrierName());
                shipmentDetails.setCoLoadBlNumber(consolidationDetails.getCoLoadMBL());
                shipmentDetails.setCoLoadBkgNumber(consolidationDetails.getCoLoadBookingReference());
            }
        }
    }

    private void setColoadBookingFields(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ShipmentDetails shipmentDetails,
        Boolean fromAttachShipment){
        if (Boolean.FALSE.equals(fromAttachShipment)) {
                if(isFieldChanged(oldEntity.getCoLoadCarrierName(), consolidationDetails.getCoLoadCarrierName())){
                    shipmentDetails.setCoLoadCarrierName(consolidationDetails.getCoLoadCarrierName());
                }
                if(isFieldChanged(oldEntity.getCoLoadMBL(), consolidationDetails.getCoLoadMBL())){
                    shipmentDetails.setCoLoadBlNumber(consolidationDetails.getCoLoadMBL());
                }
                if(isFieldChanged(oldEntity.getCoLoadBookingReference(), consolidationDetails.getCoLoadBookingReference())){
                    shipmentDetails.setCoLoadBkgNumber(consolidationDetails.getCoLoadBookingReference());
                }
        } else{
            shipmentDetails.setCoLoadCarrierName(consolidationDetails.getCoLoadCarrierName());
            shipmentDetails.setCoLoadBlNumber(consolidationDetails.getCoLoadMBL());
            shipmentDetails.setCoLoadBkgNumber(consolidationDetails.getCoLoadBookingReference());
        }
    }


    private boolean isFieldChanged(String oldValue, String newValue) {
        return !Objects.equals(oldValue, newValue);
    }

    /**
     * Updates the export and import broker information in the shipment details based on the consolidation's sending and receiving agents, but only if the consolidation is **not**
     * marked as an inter-branch console.
     * <p>
     * Logic: - If inter-branch console is false or null: - Set export broker in shipment from sending agent if different - Set import broker in shipment from receiving agent if
     * different - If `AdditionalDetails` is null, initializes it before setting brokers
     *
     * @param console The consolidation object containing sending and receiving agents
     * @param sd      The shipment details to update with broker info
     */
    private void updateNonInterBranchConsoleData(ConsolidationDetails console, ShipmentDetails sd) {
        // Proceed only if it's NOT an inter-branch console
        if (!Boolean.TRUE.equals(console.getInterBranchConsole())) {

            // Check and update Export Broker from Sending Agent
            if (sd.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getSendingAgent(), sd.getAdditionalDetails().getExportBroker())) {
                // If export broker doesn't match, update it from sending agent
                sd.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            } else if (sd.getAdditionalDetails() == null) {
                // If no AdditionalDetails exist, initialize and set export broker
                sd.setAdditionalDetails(new AdditionalDetails());
                sd.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            }

            // Check and update Import Broker from Receiving Agent
            if (sd.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getReceivingAgent(), sd.getAdditionalDetails().getImportBroker())) {
                // If import broker doesn't match, update it from receiving agent
                sd.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
            } else if (sd.getAdditionalDetails() == null) {
                // If still null (shouldn't happen here), initialize and set import broker
                sd.setAdditionalDetails(new AdditionalDetails());
                sd.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
            }
        }
    }

    /**
     * Syncs MAIN_CARRIAGE routing legs from consolidation to the shipment.
     * <p>
     * - It creates copies of consolidation MAIN_CARRIAGE legs and assigns them to the shipment. - Existing MAIN_CARRIAGE legs in the shipment (not inherited from consolidation)
     * are preserved. - Reassigns leg sequence (1, 2, 3, ...) across all routings (PRE_CARRIAGE, MAIN_CARRIAGE, ON_CARRIAGE). - Optionally persists updated routing list to DB.
     *
     * @param consolidationRoutings List of routing legs from consolidation.
     * @param shipmentDetails       Shipment entity to which the routings should be applied.
     * @param saveRoutes            Flag to persist the routings to DB
     * @throws RunnerException If any failure occurs during processing.
     */
    @Override
    public void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails) throws RunnerException {
        if(consolidationRoutings == null) return;
        List<Routings> mainCarriageList = consolidationRoutings.stream()
                .filter(routing -> routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE)
                .toList();
        if (CollectionUtils.isEmpty(mainCarriageList)) {
            return;
        }
        List<Routings> shipmentMainCarriageRouting = new ArrayList<>();
        List<Routings> shipmentRoutingList = Optional.ofNullable(shipmentDetails.getRoutingsList()).orElse(new ArrayList<>());
        shipmentDetails.setRoutingsList(shipmentRoutingList);

        consolidationRoutings.stream()
                .filter(consolRoutings -> RoutingCarriage.MAIN_CARRIAGE.equals(consolRoutings.getCarriage()))
                .forEach(consolRoute -> {
                    // Deep copy of the routing object
                    var syncedRoute = jsonHelper.convertCreateValue(consolRoute, Routings.class);
                    syncedRoute.setConsolidationId(null);
                    syncedRoute.setShipmentId(shipmentDetails.getId());
                    syncedRoute.setBookingId(null);
                    syncedRoute.setInheritedFromConsolidation(true); // Mark as inherited
                    shipmentMainCarriageRouting.add(syncedRoute);
                });
        // Logic to regroup all shipment routings with updated leg sequence
        // Assumption -> order of routes is as follows; Otherwise legs will have a chaotic order for user
        // 1. PRE_CARRIAGE
        // 2. MAIN_CARRIAGE
        // 3. ON_CARRIAGE
        AtomicLong legCount = new AtomicLong(1);
        List<Routings> finalShipmentRouteList = new ArrayList<>();
        List<Routings> preCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream()
                .filter(routings -> RoutingCarriage.PRE_CARRIAGE.equals(routings.getCarriage())).toList();
        List<Routings> onCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream()
                .filter(routings -> RoutingCarriage.ON_CARRIAGE.equals(routings.getCarriage())).toList();

        // Merge routings list in proper order with updated leg sequence
        mergeRoutingList(preCarriageShipmentRoutes, finalShipmentRouteList, legCount);
        mergeRoutingList(shipmentMainCarriageRouting, finalShipmentRouteList, legCount);
        mergeRoutingList(onCarriageShipmentRoutes, finalShipmentRouteList, legCount);

        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setEntityId(shipmentDetails.getId());
        bulkUpdateRoutingsRequest.setRoutings(jsonHelper.convertValueToList(finalShipmentRouteList, RoutingsRequest.class));

        routingsV3Service.updateBulk(bulkUpdateRoutingsRequest, SHIPMENT);
        List<Routings> routings = routingsV3Service.getRoutingsByShipmentId(shipmentDetails.getId());
        shipmentDetails.setRoutingsList(routings);
    }

    /**
     * Merges the given carriage route into the shipment routing list, updating leg numbers sequentially.
     *
     * @param routingsList         the list of routings to be merged
     * @param shipmentRoutingsList the target list where routings are merged
     * @param legCount             an atomic counter used to set sequential leg numbers
     */
    private void mergeRoutingList(List<Routings> routingsList, List<Routings> shipmentRoutingsList, AtomicLong legCount) {
        // Do nothing if there are no routings to merge
        if (routingsList.isEmpty()) {
            return;
        }

        // Add each routing to the target list with incremented leg number
        routingsList.forEach(routing -> {
            routing.setLeg(legCount.get());
            legCount.incrementAndGet();
            shipmentRoutingsList.add(routing);
        });
    }

    /**
     * Checks and updates the SCI value for the MAWB (Master AWB) linked to the given console ID. If any attached shipment has SCI = 'T1', and the current MAWB SCI is not 'T1' and
     * not locked, the method updates the MAWB and consolidation SCI to 'T1'.
     *
     * @param consoleId the ID of the consolidation/console
     * @throws RunnerException in case of underlying data issues
     */
    @Override
    public void checkSciForAttachConsole(Long consoleId) throws RunnerException {
        // Get all shipment mappings linked to this console
        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        List<Long> shipIdList = consoleShipmentMappingList.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Fetch MAWBs associated with the console
        List<Awb> mawbs = awbDao.findByConsolidationId(consoleId);

        // Fetch consolidation details
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consoleId);

        if (consol.isPresent() && mawbs != null && !mawbs.isEmpty() && !shipIdList.isEmpty()) {
            Awb mawb = mawbs.get(0);

            // Check if SCI needs to be updated to T1
            boolean isMawbEligibleForUpdate = mawb.getAwbCargoInfo() != null &&
                    !Objects.equals(mawb.getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) &&
                    !Objects.equals(mawb.getAwbCargoInfo().getSci(), AwbConstants.T1);

            if (isMawbEligibleForUpdate) {
                // Fetch AWBs of all attached shipments
                List<Awb> awbs = awbDao.findByShipmentIdList(shipIdList);

                // If any shipment AWB has SCI = T1, update MAWB and consolidation SCI
                boolean anyShipmentHasSciT1 = awbs != null && awbs.stream()
                        .anyMatch(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1));

                if (anyShipmentHasSciT1) {
                    mawb.getAwbCargoInfo().setSci(AwbConstants.T1);
                    mawb.setAirMessageResubmitted(false);
                    awbDao.save(mawb);

                    consol.get().setSci(AwbConstants.T1);
                    consolidationDetailsDao.save(consol.get(), false);
                }
            }
        }
    }

    /**
     * Sets the sending and receiving agents in the consolidation if they are not already set. Only assigns them when exactly one party is present in origin or destination.
     *
     * @param consolidationDetails the consolidation entity being updated
     * @param originParties        set of origin parties collected from shipments
     * @param destinationParties   set of destination parties collected from shipments
     */
    private void processSendingAgentAndReceivingAgent(ConsolidationDetails consolidationDetails, Set<Parties> originParties, Set<Parties> destinationParties) {
        // Set sending agent if not already set and exactly one origin party exists
        if (!CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent()) && originParties.size() == 1) {
            Parties originAgent = originParties.iterator().next();
            consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(originAgent));
        }

        // Set receiving agent if not already set and exactly one destination party exists
        if (!CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent()) && destinationParties.size() == 1) {
            Parties destinationAgent = destinationParties.iterator().next();
            consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(destinationAgent));
        }
    }

    /**
     * Calculates how many agents (sending and/or receiving) are present in the consolidation.
     *
     * @param consolidationDetails the consolidation to check for agents
     * @return the count of non-null agents (0 to 2)
     */
    private Integer getAgentCount(ConsolidationDetails consolidationDetails) {
        Integer agentCount = 0;

        // Check if sending agent is present
        if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
            agentCount++;
        }

        // Check if receiving agent is present
        if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
            agentCount++;
        }

        return agentCount;
    }


    /**
     * Processes the given list of shipment details during consolidation.
     * <p>
     * - Filters only shipments that are attached and not inter-branch requested. - Updates containers with consolidation ID and persists them. - Processes packing events and
     * origin/destination parties. - Logs history for each shipment.
     *
     * @param consolidationId             the ID of the consolidation
     * @param shipmentDetailsList         list of shipment details to process
     * @param attachedShipmentIds         set of already attached shipment IDs
     * @param interBranchRequestedShipIds set of shipment IDs requested as inter-branch
     * @param consolidationDetails        details of the current consolidation
     * @param originParties               set to collect origin parties
     * @param destinationParties          set to collect destination parties
     */
    private void processShipmentDetailsList(Long consolidationId, List<ShipmentDetails> shipmentDetailsList, HashSet<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds, ConsolidationDetails consolidationDetails, Set<Parties> originParties, Set<Parties> destinationParties) {
        Integer agentCount = getAgentCount(consolidationDetails);

        containerV3Service.processContainersAfterShipmentAttachment(consolidationId,shipmentDetailsList, attachedShipmentIds, interBranchRequestedShipIds);

        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if (attachedShipmentIds.contains(shipmentDetails.getId()) && !interBranchRequestedShipIds.contains(shipmentDetails.getId())) {
                packingV3Service.processPacksAfterShipmentAttachment(consolidationId, shipmentDetails);
                eventV3Service.processEventsAfterShipmentAttachment(consolidationId, shipmentDetails);
                processOriginAndDestinationParties(consolidationDetails, agentCount, originParties, destinationParties, shipmentDetails);
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shipmentV3Service.createLogHistoryForShipment(shipmentDetails)), executorService);
            }
        }
    }

    /**
     * Creates a log history entry for the given shipment. Converts the shipment to JSON and sends it to the log history service. Logs error if the operation fails.
     *
     * @param shipmentDetails the shipment for which log history needs to be created
     */
    private void createLogHistoryForShipment(ShipmentDetails shipmentDetails) {
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

    /**
     * Populates origin and destination party sets with export/import brokers from shipment details, only if the console is not inter-branch and agent count is less than 2.
     *
     * @param consolidationDetails the consolidation entity for context
     * @param agentCount           number of agents already set
     * @param originParties        set to hold origin (export broker) parties
     * @param destinationParties   set to hold destination (import broker) parties
     * @param shipmentDetails      the shipment being processed
     */
    private void processOriginAndDestinationParties(ConsolidationDetails consolidationDetails, Integer agentCount, Set<Parties> originParties, Set<Parties> destinationParties,
            ShipmentDetails shipmentDetails) {
        if (!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole()) && agentCount < 2) {
            if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                originParties.add(shipmentDetails.getAdditionalDetails().getExportBroker());
            }
            if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                destinationParties.add(shipmentDetails.getAdditionalDetails().getImportBroker());
            }
        }
    }

    /**
     * Returns a map of inter-branch import shipments for the given consolidation. Only includes shipments with direction 'IMP' and a different tenant ID than the consolidation.
     *
     * @param shipmentDetailsList  list of all shipment details
     * @param consolidationDetails consolidation details for tenant comparison
     * @return map of shipment ID to corresponding inter-branch import ShipmentDetails
     */
    private Map<Long, ShipmentDetails> getInterBranchImportShipmentMap(List<ShipmentDetails> shipmentDetailsList, ConsolidationDetails consolidationDetails) {
        List<ShipmentDetails> interBranchShipmentDetailsList = shipmentDetailsList.stream()
                .filter(c -> !Objects.equals(c.getTenantId(), consolidationDetails.getTenantId())) // Filter inter-branch shipments
                .toList();

        return interBranchShipmentDetailsList.stream()
                .filter(shipment -> DIRECTION_IMP.equalsIgnoreCase(shipment.getDirection()))
                .collect(toMap(
                        ShipmentDetails::getId,   // Key: ID of the shipment
                        shipment -> shipment      // Value: ShipmentDetails object itself
                ));
    }

    /**
     * Sets inter-branch context if shipment type is not specified and the consolidation is an inter-branch console.
     *
     * @param shipmentRequestedType type of shipment request, can be null
     * @param consolidationDetails  consolidation details with inter-branch flag
     */
    private void setContextIfNeededForHub(ShipmentRequestedType shipmentRequestedType, ConsolidationDetails consolidationDetails) {
        if (shipmentRequestedType == null && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
        }
    }

    /**
     * Fetches the {@link ConsolidationDetails} entity from the database using the given consolidation ID.
     *
     * <p>If no consolidation is found with the given ID, a {@link DataRetrievalFailureException} is thrown
     * to indicate that the requested data could not be retrieved.</p>
     *
     * @param consolidationId the ID of the consolidation to fetch (must not be {@code null})
     * @return the {@link ConsolidationDetails} entity corresponding to the provided ID
     * @throws DataRetrievalFailureException if no consolidation exists for the given ID
     */
    @NotNull
    @Override
    public ConsolidationDetails fetchConsolidationDetails(Long consolidationId) {
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if (consol.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return consol.get();
    }

    public Optional<ConsolidationDetails> retrieveForNte(Long id) throws RunnerException, AuthenticationException {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(id);
        if (!consolidationDetails.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, id, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        List<TriangulationPartner> triangulationPartners = consolidationDetails.get().getTriangulationPartnerList();
        Long currentTenant = TenantContext.getCurrentTenant().longValue();
        if (
                (triangulationPartners == null && !Objects.equals(consolidationDetails.get().getTriangulationPartner(), TenantContext.getCurrentTenant().longValue())
                        && !Objects.equals(consolidationDetails.get().getReceivingBranch(), TenantContext.getCurrentTenant().longValue()))
                        ||
                        ((triangulationPartners == null || triangulationPartners.stream().filter(Objects::nonNull).noneMatch(tp -> Objects.equals(tp.getTriangulationPartner(), currentTenant)))
                                && !Objects.equals(consolidationDetails.get().getReceivingBranch(), currentTenant))
        ) {
            throw new AuthenticationException(Constants.NOT_ALLOWED_TO_VIEW_CONSOLIDATION_FOR_NTE);
        }
        return consolidationDetails;
    }

    @Override
    public ConsolidationDetailsV3Response retrieveById(CommonGetRequest request, String source) throws RunnerException, AuthenticationException {
        if (request.getId() == null && request.getGuid() == null) {
            log.error("Request Id and Guid are null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id and GUID can't be null. Please provide any one !");
        }
        Long id = request.getId();
        Optional<ConsolidationDetails> consolidationDetails;
        if(Objects.equals(source, NETWORK_TRANSFER)){
            consolidationDetails = retrieveForNte(request.getId());
        } else {
            if (id != null) {
                consolidationDetails = consolidationDetailsDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                consolidationDetails = consolidationDetailsDao.findByGuid(guid);
            }
        }
        if (!consolidationDetails.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ConsolidationDetails consoleDetails = consolidationDetails.get();
        log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, id, LoggerHelper.getRequestIdFromMDC());
        ConsolidationDetailsV3Response response = jsonHelper.convertValue(consoleDetails, ConsolidationDetailsV3Response.class);
        if(!Objects.equals(source, NETWORK_TRANSFER))
            setPendingActionCountInResponse(consoleDetails, response);
        createConsolidationPayload(consoleDetails, response);
        setMainCarriageFlag(consoleDetails, response);
        return response;
    }
    private void setMainCarriageFlag(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response response) {
        List<Routings> routingsList = consolidationDetails.getRoutingsList();
        if (!CollectionUtils.isEmpty(routingsList)) {
            List<Routings> mainCarriageRoutings = routingsList.stream().filter(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE).toList();
            response.setTransportInfoStatusMessage(CommonUtils.setTransportInfoStatusMessage(consolidationDetails.getCarrierDetails(), consolidationDetails.getTransportInfoStatus(), mainCarriageRoutings));
            if (!CollectionUtils.isEmpty(mainCarriageRoutings)) {
                response.setIsMainCarriageAvailable(true);
                response.setIsVesselVoyageOrCarrierFlightNumberAvailable(CommonUtils.isVesselVoyageOrCarrierFlightNumberAvailable(mainCarriageRoutings));
            }

        }
    }
    private void setPendingActionCountInResponse(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response response) {
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnConsolidation(Arrays.asList(consolidationDetails.getId()), ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(consolidationDetails.getId()), CONSOLIDATION);
        int pendingCount = map.getOrDefault(consolidationDetails.getId(), 0) + notificationMap.getOrDefault(consolidationDetails.getId(), 0);
        response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
    }

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        try {
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                consolidationDetailsV3Response.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            if(consolidationDetailsV3Response.getId() != null) {
                var awb = awbDao.findByConsolidationId(consolidationDetailsV3Response.getId());
                if (!listIsNullOrEmpty(awb)) {
                    if (awb.get(0).getAirMessageStatus() != null)
                        consolidationDetailsV3Response.setAwbStatus(awb.get(0).getAirMessageStatus());
                    else
                        consolidationDetailsV3Response.setAwbStatus(AwbStatus.AWB_GENERATED);
                    if(awb.get(0).getLinkedHawbAirMessageStatus() != null)
                        consolidationDetailsV3Response.setLinkedHawbStatus(awb.get(0).getLinkedHawbAirMessageStatus());
                }
            }
            // fetch NTE status
            this.fetchNTEstatusForReceivingBranch(consolidationDetailsV3Response);
            // calculate Shipment Wt Vol
            this.calculateShipmentWtVol(consolidationDetails, consolidationDetailsV3Response);
        }  catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, ex.getLocalizedMessage());
        }
    }

    private void fetchNTEstatusForReceivingBranch(ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        if(consolidationDetailsV3Response.getReceivingBranch() != null) {
            String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(consolidationDetailsV3Response.getId(), CONSOLIDATION, consolidationDetailsV3Response.getReceivingBranch().intValue());
            consolidationDetailsV3Response.setTransferStatus(transferStatus);
        }
    }

    private String determineWeightChargeableUnit(ShipmentSettingsDetails shipmentSettingsDetails){
        String weightChargeableUnit = Constants.WEIGHT_UNIT_KG;
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            weightChargeableUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        return weightChargeableUnit;
    }

    private String determineVolumeChargeableUnit(ShipmentSettingsDetails shipmentSettingsDetails){
        String volumeChargeableUnit = Constants.VOLUME_UNIT_M3;
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            volumeChargeableUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        return volumeChargeableUnit;
    }

    private void updateContainerMap(ShipmentDetails shipmentDetails, Map<Long, Containers> containersMap){
        if(!setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            for(Containers containers: shipmentDetails.getContainersList()) {
                if(!containersMap.containsKey(containers.getId()))
                    containersMap.put(containers.getId(), containers);
            }
        }
    }

    protected void calculateShipmentWtVol(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response consolidationDetailsV3Response) throws RunnerException {
        ShipmentWtVolResponse shipmentWtVolResponse = calculateShipmentWtVol(consolidationDetails);
        consolidationDetailsV3Response.setShipmentWtVolResponse(shipmentWtVolResponse);
        consolidationDetailsV3Response.setShipmentsCount(shipmentWtVolResponse.getShipmentsCount());
    }

    @Override
    public ShipmentWtVolResponse calculateShipmentWtVol(ConsolidationDetails consolidationDetails) throws RunnerException {
        return calculateShipmentWtVol(consolidationDetails.getTransportMode(), consolidationDetails.getShipmentsList() != null ?
                        consolidationDetails.getShipmentsList().stream().toList() : new ArrayList<>(),
                                        consolidationDetails.getContainersList());
    }

    public ShipmentWtVolResponse calculateShipmentWtVol(String transportMode, List<ShipmentDetails> shipmentDetailsList,
                                                        List<Containers> consoleContainersList) throws RunnerException {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        String weightChargeableUnit = determineWeightChargeableUnit(shipmentSettingsDetails);
        String volumeChargeableUnit = determineVolumeChargeableUnit(shipmentSettingsDetails);

        BigDecimal sumWeight = new BigDecimal(0);
        BigDecimal sumVolume = new BigDecimal(0);
        Integer slacCount = 0;
        Integer packs = 0;
        Integer dgPacks = 0;
        String packsType = null;
        String dgPacksType = null;
        Integer noOfCont = 0;
        Integer dgContCount = 0;
        BigDecimal teus = BigDecimal.ZERO;
        Integer consoleNoOfCont = 0;
        Integer consoleDgContCount = 0;
        BigDecimal consoleTeus = BigDecimal.ZERO;
        Long shipmentsCount = 0L;
        Map<Long, Containers> containersMap = new HashMap<>();
        if (!listIsNullOrEmpty(shipmentDetailsList)) {
            shipmentsCount = shipmentDetailsList.stream().count();
            for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                sumWeight = sumWeight.add(new BigDecimal(convertUnit(Constants.MASS, shipmentDetails.getWeight(), shipmentDetails.getWeightUnit(), weightChargeableUnit).toString()));
                sumVolume = sumVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit(), volumeChargeableUnit).toString()));
                packs = packs + (shipmentDetails.getNoOfPacks() != null ? shipmentDetails.getNoOfPacks() : 0);
                dgPacks = dgPacks + (shipmentDetails.getDgPacksCount() != null ? shipmentDetails.getDgPacksCount() : 0);
                slacCount = slacCount + (shipmentDetails.getSlac() != null ? shipmentDetails.getSlac() : 0);
                packsType = getPacksType(shipmentDetails, packsType);
                dgPacksType = getPacksType(shipmentDetails, dgPacksType, containersMap);
            }
        }

        Result result = getResult(containersMap, noOfCont, teus, dgContCount);

        ConsoleCount consoleCount = getConsoleCount(consoleContainersList, consoleNoOfCont, consoleTeus, consoleDgContCount);

        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, sumWeight, sumVolume);

        return ShipmentWtVolResponse.builder()
                .weight(sumWeight)
                .weightUnit(weightChargeableUnit)
                .volume(sumVolume)
                .volumeUnit(volumeChargeableUnit)
                .packs(Objects.equals(packs, 0) ? null : packs)
                .packsType(packsType)
                .chargable(vwOb.getChargeable())
                .chargeableUnit(vwOb.getChargeableUnit())
                .weightVolume(vwOb.getVolumeWeight())
                .weightVolumeUnit(vwOb.getVolumeWeightUnit())
                .containerCount(result.noOfCont())
                .teuCount(result.teus())
                .dgContainerCount(result.dgContCount())
                .shipmentsCount(shipmentsCount)
                .consoleTeuCount(consoleCount.consoleTeus())
                .consoleDgContainerCount(consoleCount.consoleDgContCount())
                .consoleContainerCount(consoleCount.consoleNoOfCont())
                .dgPacks(dgPacks)
                .dgPacksType(dgPacksType)
                .slacCount(slacCount)
                .build();
    }

    public String getPacksType(ShipmentDetails shipmentDetails, String packsType) {
        if(isStringNullOrEmpty(packsType))
            packsType = shipmentDetails.getPacksUnit();
        else if(!isStringNullOrEmpty(shipmentDetails.getPacksUnit()) && !Objects.equals(packsType, shipmentDetails.getPacksUnit()))
            packsType = PackingConstants.PKG;
        return packsType;
    }

    private String getPacksType(ShipmentDetails shipmentDetails, String dgPacksType, Map<Long, Containers> containersMap) {
        if(isStringNullOrEmpty(dgPacksType))
            dgPacksType = shipmentDetails.getDgPacksUnit();
        else if(!isStringNullOrEmpty(shipmentDetails.getDgPacksUnit()) && !Objects.equals(dgPacksType, shipmentDetails.getDgPacksUnit()))
            dgPacksType = PackingConstants.PKG;
        updateContainerMap(shipmentDetails, containersMap);
        return dgPacksType;
    }

    @NotNull
    private static ConsoleCount getConsoleCount(List<Containers> consoleContainersList, Integer consoleNoOfCont, BigDecimal consoleTeus, Integer consoleDgContCount) {
        if(!listIsNullOrEmpty(consoleContainersList)) {
            for(Containers containers: consoleContainersList) {
                consoleNoOfCont++;
                if(Objects.nonNull(containers.getTeu()))
                    consoleTeus = consoleTeus.add(containers.getTeu());
                if(Boolean.TRUE.equals(containers.getHazardous()))
                    consoleDgContCount++;
            }
        }
        return new ConsoleCount(consoleNoOfCont, consoleDgContCount, consoleTeus);
    }

    private record ConsoleCount(Integer consoleNoOfCont, Integer consoleDgContCount, BigDecimal consoleTeus) {
    }

    @NotNull
    private static Result getResult(Map<Long, Containers> containersMap, Integer noOfCont, BigDecimal teus, Integer dgContCount) {
        for(Containers containers: containersMap.values()) {
            noOfCont++;
            if(Objects.nonNull(containers.getTeu()))
                teus = teus.add(containers.getTeu());
            if(Boolean.TRUE.equals(containers.getHazardous()))
                dgContCount++;
        }
        return new Result(noOfCont, dgContCount, teus);
    }

    private record Result(Integer noOfCont, Integer dgContCount, BigDecimal teus) {
    }

    @Override
    public Map<String, Object> getAllMasterData(Long id, String source) throws RunnerException, AuthenticationException {
        Optional<ConsolidationDetails> consolidationDetailsOptional;
        if(Objects.equals(source, NETWORK_TRANSFER)){
            consolidationDetailsOptional = retrieveForNte(id);
        } else
            consolidationDetailsOptional = consolidationDetailsDao.findConsolidationByIdWithQuery(id);
        if(!consolidationDetailsOptional.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
        long start = System.currentTimeMillis();
        List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ConsolidationDetails.class, null), createFieldClassDto(ArrivalDepartureDetails.class, "arrivalDetails."), createFieldClassDto(ArrivalDepartureDetails.class, "departureDetails.")));
        includeColumns.addAll(FieldUtils.getTenantIdAnnotationFields(List.of(createFieldClassDto(ConsolidationDetails.class, null))));
        includeColumns.addAll(ConsolidationConstants.LIST_INCLUDE_COLUMNS_V3);
        ConsolidationDetailsV3Response consolidationDetailsV3Response = (ConsolidationDetailsV3Response) commonUtils.setIncludedFieldsToResponse(consolidationDetails, includeColumns.stream().collect(Collectors.toSet()), new ConsolidationDetailsV3Response());
        log.info("Total time taken in setting consol details response {}", (System.currentTimeMillis() - start));
        return fetchAllMasterDataByKey(consolidationDetailsV3Response);
    }

    public Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        Map<String, Object> response = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(response)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllVesselDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var organizationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllOrganizationDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, vesselDataFuture, organizationsFuture).join();

        return response;
    }

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsV3Response.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            if(masterDataResponse != null) {
                if (!Objects.isNull(consolidationDetailsV3Response.getArrivalDetails()))
                    locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsV3Response.getArrivalDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Arrival", cacheMap)));
                if (!Objects.isNull(consolidationDetailsV3Response.getDepartureDetails()))
                    locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsV3Response.getDepartureDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Departure", cacheMap)));
            }

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap) );
                if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                    consolidationDetailsV3Response.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap));
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

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(consolidationDetailsV3Response.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, cacheMap));
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

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> currencyList = new HashSet<>(masterDataUtils.createInBulkCurrencyRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferCurrency> v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES, currencyList, new EntityTransferCurrency(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.CURRENCIES, cacheMap));
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

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(
        Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if(masterDataResponse != null) {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> wareHouseTypes = new HashSet<>(masterDataUtils.createInBulkWareHouseRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, WareHouseResponse> v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES, wareHouseTypes, new WareHouseResponse(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.setTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.WAREHOUSES, cacheMap));
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

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    protected CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsV3Response.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            processMasterDataResponse(consolidationDetailsV3Response, masterDataResponse, listRequests, fieldNameKeyMap, cacheMap);

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
                if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                    consolidationDetailsV3Response.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap) );
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

    private void processMasterDataResponse(ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse, Set<MasterListRequest> listRequests, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if(masterDataResponse != null) {
            if (!Objects.isNull(consolidationDetailsV3Response.getAllocations()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsV3Response.getAllocations(), Allocations.class, fieldNameKeyMap, Allocations.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsV3Response.getAchievedQuantities()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsV3Response.getAchievedQuantities(), AchievedQuantities.class, fieldNameKeyMap, AchievedQuantities.class.getSimpleName(), cacheMap));
            if(!Objects.isNull(consolidationDetailsV3Response.getReferenceNumbersList()))
                consolidationDetailsV3Response.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId(), cacheMap)));
        }
    }

    protected CompletableFuture<Map<String, TenantModel>> addAllTenantDataInSingleCall(
        ConsolidationDetailsV3Response consolidationDetailsV3Response,
        Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.TENANTS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTenantDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    protected CompletableFuture<Map<String, EntityTransferVessels>> addAllVesselDataInSingleCall(ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsV3Response.getCarrierDetails()))
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(consolidationDetailsV3Response.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsV3Response.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(new HashMap<>());
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllOrganizationDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsV3Response, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> orgIds = new HashSet<>((masterDataUtils.createInBulkOrganizationRequest(consolidationDetailsV3Response, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap)));

            Map<String, EntityTransferOrganizations> keyMasterDataMap = masterDataUtils.fetchInOrganizations(orgIds, EntityTransferConstants.ID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.ORGANIZATIONS, orgIds, new EntityTransferOrganizations(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.ORGANIZATIONS, masterDataResponse, cacheMap);

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllOrganizationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public ConsolidationPendingNotificationResponse getPendingNotificationData(CommonGetRequest request){

        Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(request.getId());
        if (!optionalConsolidationDetails.isPresent()) {
            log.debug("Id missing", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        if (listIsNullOrEmpty(request.getIncludeColumns())) {
            request.setIncludeColumns(List.of(TRANSPORT_MODE, "hazardous", "guid", "receivingBranch", "triangulationPartnerList", TENANT_ID,
                    "consolidationNumber", "interBranchConsole"));
        }
        Set<String> includeColumns = new HashSet<>(request.getIncludeColumns());
        CommonUtils.includeRequiredColumns(includeColumns);

        ConsolidationPendingNotificationResponse consolidationPendingNotificationResponse = (ConsolidationPendingNotificationResponse) commonUtils.setIncludedFieldsToResponse(optionalConsolidationDetails.get(), includeColumns, new ConsolidationPendingNotificationResponse());

        Map<String, Object> response = new HashMap<>();
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationPendingNotificationResponse, response)), executorService);
        tenantDataFuture.join();
        if(response.get("Tenants") != null)
            consolidationPendingNotificationResponse.setTenantMasterData((Map<String, Object>) response.get("Tenants"));
        return consolidationPendingNotificationResponse;
    }

    @Override
    public ConsolidationListV3Response list(ListCommonRequest request, boolean getMasterData) {
        if (request == null) {
            log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
        }
        appendCriteriaIfNotificationFlagEnabled(request);
        checkBookingIdCriteria(request);
        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent(), getMasterData);
        log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        ConsolidationListV3Response response = new ConsolidationListV3Response();
        response.setTotalPages(consolidationDetailsPage.getTotalPages());
        response.setNumberOfRecords(consolidationDetailsPage.getTotalElements());
        List<ConsolidationListResponse> consolidationListResponses = new ArrayList<>();
        consoleResponse.forEach(consol -> consolidationListResponses.add((ConsolidationListResponse) consol));
        response.setConsolidationListResponses(consolidationListResponses);
        return response;
    }

    private void appendCriteriaIfNotificationFlagEnabled(ListCommonRequest request) {
        if(Boolean.TRUE.equals(request.getNotificationFlag())) {
            Page<Long> eligibleConsolId = consolidationDetailsDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED,
                    PageRequest.of(Math.max(0, request.getPageNo() - 1), request.getPageSize()));
            List<Long> consolidationIds = notificationDao.findEntityIdsByEntityType(CONSOLIDATION);
            Set<Long> uniqueConsolidationIds = new HashSet<>(eligibleConsolId.getContent());
            uniqueConsolidationIds.addAll(consolidationIds);
            List<Long> combinedConsolIds = new ArrayList<>(uniqueConsolidationIds);
            andCriteria("id", combinedConsolIds, "IN", request);
        }
    }

    private void checkBookingIdCriteria(ListCommonRequest request) {
        if(request != null && request.getFilterCriteria() != null && !request.getFilterCriteria().isEmpty())
        {
            checkForBookingIdFilter(request.getFilterCriteria());
        }
    }

    private void checkForBookingIdFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList) {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                filterCriteria.getCriteria().getFieldName().equals("bookingId") && filterCriteria.getCriteria().getValue() != null) {

                ConsoleBookingIdFilterRequest consoleBookingIdFilterRequest = new ConsoleBookingIdFilterRequest();
                consoleBookingIdFilterRequest.setIntraBookingId(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchBookingIdFilterGuids(consoleBookingIdFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && !filterCriteria.getInnerFilter().isEmpty()) {
                checkForBookingIdFilter(filterCriteria.getInnerFilter());
            }
        }
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
            containerCountUpdate(consolidationDetails, res, Boolean.TRUE.equals(shipmentSettingsDetails.getIsShipmentLevelContainer()));
            int pendingCount = map.getOrDefault(consolidationDetails.getId(), 0) + notificationMap.getOrDefault(consolidationDetails.getId(), 0);
            res.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            consolidationListResponses.add(res);
        });
        consolidationListResponses.forEach(responseList::add);
        this.getMasterDataForList(lst, responseList, getMasterData, true);
        return responseList;
    }

    private void getMasterDataForList(List<ConsolidationDetails> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData) {
        if(getMasterData || Boolean.TRUE.equals(includeMasterData)) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                var containerTeuData = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setConsolidationContainerTeuData(lst, responseList)), executorServiceMasterData);
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture =CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, containerTeuData, vesselDataFuture, tenantDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    private void updateHouseBillsShippingIds(ConsolidationDetails consol, ConsolidationListResponse consolidationRes) {
        var shipments = consol.getShipmentsList();
        List<String> shipmentIds = null;
        List<String> houseBills = null;
        if (shipments != null)
            shipmentIds = shipments.stream().map(ShipmentDetails::getShipmentId).toList();
        if (shipmentIds != null)
            houseBills = shipments.stream().map(ShipmentDetails::getHouseBill).filter(Objects::nonNull).toList();
        consolidationRes.setHouseBills(houseBills);
        consolidationRes.setShipmentIds(shipmentIds);
    }

    private record ContainerCounts(Long container20Count, Long container40Count, Long container20GPCount, Long container20RECount, Long container40GPCount, Long container40RECount, Long containerCount) {
    }

    private void containerCountUpdate(ConsolidationDetails consolidationDetails, ConsolidationListResponse response, boolean isShipmentLevelContainer) {
        Set<String> containerNumber = new HashSet<>();
        List<Containers> containersList = getContainersList(consolidationDetails, isShipmentLevelContainer);
        ContainerCounts containerCounts = getContainerCounts(containersList, containerNumber);
        if(containerCounts!=null) {
            response.setContainer20Count(containerCounts.container20Count());
            response.setContainer40Count(containerCounts.container40Count());
            response.setContainer20GPCount(containerCounts.container20GPCount());
            response.setContainer20RECount(containerCounts.container20RECount());
            response.setContainer40GPCount(containerCounts.container40GPCount());
            response.setContainer40RECount(containerCounts.container40RECount());
            response.setContainerCount(containerCounts.containerCount());
            response.setContainerNumbers(containerNumber);
        }
    }

    private List<Containers> getContainersList(ConsolidationDetails consolidationDetails, boolean isShipmentLevelContainer) {
        List<Containers> containersList = consolidationDetails.getContainersList();
        if(isShipmentLevelContainer) {
            Set<ShipmentDetails> shipmentDetails = consolidationDetails.getShipmentsList();
            if(!setIsNullOrEmpty(shipmentDetails)) {
                containersList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                    if(shipmentDetails1.getContainersList() != null && !shipmentDetails1.getContainersList().isEmpty()) {
                        containersList.addAll(shipmentDetails1.getContainersList());
                    }
                }
            }
        } else {
            containersList = consolidationDetails.getContainersList();
        }
        return containersList;
    }

    private ContainerCounts getContainerCounts(List<Containers> containersList, Set<String> containerNumber) {
        if (containersList == null || containersList.isEmpty()) {
            return null;
        }
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Long containerCount = 0L;

        for (Containers container : containersList) {
            if(container.getContainerCode() != null) {
                String containerCode = container.getContainerCode();
                if (containerCode.contains(Constants.CONT_20)) {
                    ++container20Count;
                }
                if (containerCode.contains(Constants.CONT_40)) {
                    ++container40Count;
                }

                switch (containerCode) {
                    case Constants.CONT_20_GP:
                        ++container20GPCount;
                        break;
                    case Constants.CONT_20_RE:
                        ++container20RECount;
                        break;
                    case Constants.CONT_40_GP:
                        ++container40GPCount;
                        break;
                    case Constants.CONT_40_RE:
                        ++container40RECount;
                        break;
                    default:
                        break;
                }
            }
            ++containerCount;
            addContainerNumber(container, containerNumber);
        }
        return new ContainerCounts(container20Count, container40Count, container20GPCount, container20RECount, container40GPCount, container40RECount, containerCount);
    }

    private void addContainerNumber(Containers container, Set<String> containerNumber) {
        if (StringUtility.isNotEmpty(container.getContainerNumber())) {
            containerNumber.add(container.getContainerNumber());
        }
    }

    public ConsolidationListV3Response getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel){
        AutoAttachConsolidationV3Request request = (AutoAttachConsolidationV3Request) commonRequestModel.getData();
        ListCommonRequest consolListRequest = request;
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }

        consolListRequest = CommonUtils.andCriteria(TRANSPORT_MODE, request.getTransportMode(), "=", consolListRequest);
        if (!Objects.isNull(request.getDirection()))
            consolListRequest = CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, request.getDirection(), "=", consolListRequest);
        if (request.getShipId() != null) {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentIdAll(request.getShipId());
            List<Long> excludeConsolidation = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            if (!listIsNullOrEmpty(excludeConsolidation))
                consolListRequest = CommonUtils.andCriteria("id", excludeConsolidation, "NOTIN", consolListRequest);
        }
        if (!ObjectUtils.isEmpty(request.getBranchIds())){
            consolListRequest = CommonUtils.andCriteria(TENANT_ID, request.getBranchIds(), "IN", consolListRequest);
        }

        return this.list(consolListRequest, true);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseEntity<IRunnerResponse> detachShipments(ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        if(setIsNullOrEmpty(request.getShipmentIds())){
            throw new RunnerException("Select atleast one shipment to detach");
        }
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(request.getConsolidationId());
        if(consol.isPresent())
            return detachShipmentsHelper(consol.get(), request.getShipmentIds(), request.getRemarks());
        return ResponseHelper.buildSuccessResponseWithWarning("Consol is null");
    }

    public ResponseEntity<IRunnerResponse> detachShipmentsHelper(ConsolidationDetails consol, Set<Long> shipmentIds, String remarks) throws RunnerException {
        List<ShipmentDetails> shipmentDetails = fetchAndValidateShipments(shipmentIds);
        Long consolidationId = consol.getId();
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);
        ResponseEntity<IRunnerResponse> validationsResponse = validateShipmentDetachment(shipmentDetails, consolidationDetails);
        if(validationsResponse != null){
            return validationsResponse;
        }

        if (Boolean.TRUE.equals(consol.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
        }

        List<Packing> packingList = new ArrayList<>();
        List<Long> shipmentIdList = new ArrayList<>(shipmentIds);

        ShipmentWtVolResponse oldShipmentWtVolResponse = calculateShipmentWtVol(consolidationDetails);

        if(consolidationId != null && shipmentIds!= null && !shipmentIds.isEmpty()) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIdList);
            Map<Long, ShipmentDetails> shipmentDetailsMap = getShipmentDetailsMap(shipmentDetails);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetail = shipmentDetailsMap.get(shipId);
                validateDetachedShipment(shipmentDetail);

                if(shipmentDetail != null) {
                    packingList = getPackingList(shipmentDetail, packingList);
                    setEventsList(shipmentDetail);
                    shipmentDetail.setMasterBill(null);
                    shipmentDetail.setConsolRef(null);
                    this.createLogHistoryForShipment(shipmentDetail);
                }
            }

            List<ShipmentDetails> shipmentDetailsToSave = shipmentDetailsMap.values().stream().toList();
            shipmentV3Service.updateShipmentFieldsAfterDetach(shipmentDetailsToSave);
            //delete routings from shipment which has isFromConsolidation as true
            routingsV3Service.deleteInheritedRoutingsFromShipment(shipmentDetailsToSave);
            shipmentDao.saveAll(shipmentDetailsToSave);
            if(shipmentDetailsToSave!=null){
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> updateShipmentDataInPlatform(shipmentIds)), executorService);
            }

            List<ShipmentDetails> shipmentDetailsList = getShipmentDetailsListFromConsoleMapping(consolidationDetails.getId());
            updateConsolidationCargoSummary(consolidationDetails, shipmentDetailsList, oldShipmentWtVolResponse);

            if(checkAttachDgAirShipments(consol, shipmentDetailsList)){
                consol.setHazardous(false);
                consolidationDetailsDao.updateV3(consol, true);
            }
        }

        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();

        String warning = null;

        if(remarks != null && !shipmentRequestedTypes.isEmpty()) {
            warning = "Mail Template not found, please inform the region users individually";
        }
        processInterConsoleDetachShipment(consol, shipmentIdList);

        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    protected void processInterConsoleDetachShipment(ConsolidationDetails console, List<Long> shipmentIds){
        try {
            if(console.getShipmentType()==null || !Constants.DIRECTION_EXP.equals(console.getShipmentType()))
                return;
            boolean isInterBranchConsole = console.getInterBranchConsole();
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            boolean isNetworkTransferEntityEnabled = Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
            if(!isInterBranchConsole || !isNetworkTransferEntityEnabled ||(shipmentIds==null || shipmentIds.isEmpty()))
                return;
            boolean isConsoleAcceptedCase = isConsoleAccepted(console);
            if(isConsoleAcceptedCase){
                return;
            }
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

    private boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetailsList) {
        if(!checkForAirDGFlag(consolidationDetails))
            return false;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return false;
        Boolean isDgShipmentAttached = shipmentDetailsList.stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
        return !isDgShipmentAttached;
    }

    protected void updateShipmentDataInPlatform(Set<Long> shipmentIds){
        try {
            List<ShipmentDetails> shipmentDetailsList = Optional.ofNullable(shipmentIds)
                .filter(ObjectUtils::isNotEmpty).map(ids -> shipmentDao.findShipmentsByIds(ids.stream().collect(Collectors.toSet())))
                .orElse(Collections.emptyList());
            if (shipmentDetailsList == null || shipmentDetailsList.isEmpty())
                return;
            for (ShipmentDetails shipmentDetail : shipmentDetailsList) {
                if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
                    bookingIntegrationsUtility.updateBookingInPlatformEmptyContainer(shipmentDetail);
            }
        } catch (Exception e) {
            log.error("Error while updating data in platform service for shipmentIds: {} Error: {}", shipmentIds.toString(), e.getMessage());
        }
    }

    private void setEventsList(ShipmentDetails shipmentDetail) {
        if(shipmentDetail != null && shipmentDetail.getEventsList() != null) {
            var eventsList = shipmentDetail.getEventsList();
            for(Events event : eventsList) {
                event.setConsolidationId(null);
            }
            eventDao.saveAll(eventsList);
        }
    }

    private List<Packing> getPackingList(ShipmentDetails shipmentDetail, List<Packing> packingList) {
        if (shipmentDetail != null && shipmentDetail.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetail.getPackingList() != null) {
            packingList = shipmentDetail.getPackingList();
            for (Packing packing : packingList) {
                packing.setConsolidationId(null);
            }
            packingList = packingDao.saveAll(packingList);
        }
        return packingList;
    }

    protected void validateDetachedShipment(ShipmentDetails shipmentDetail)  {
        if (shipmentDetail == null || shipmentDetail.getContainersList() == null) {
            return;
        }

        List<Containers> containersList = new ArrayList<>(shipmentDetail.getContainersList());
        Map<Long, Containers> containerIdMap = buildContainerIdMap(containersList);

        validatePacksAgainstContainers(shipmentDetail, containerIdMap);
        validateContainersAttachedToShipment(shipmentDetail, containersList);

    }

    private Map<Long, Containers> buildContainerIdMap(List<Containers> containersList) {
        return containersList.stream()
            .filter(c -> c.getId() != null)
            .collect(Collectors.toMap(
                Containers::getId,
                Function.identity()
            ));
    }

    private void validatePacksAgainstContainers(ShipmentDetails shipmentDetail, Map<Long, Containers> containersMap){
        List<Packing> packings = packingDao.findByShipmentId(shipmentDetail.getId());

        if (!isSeaPackingList(shipmentDetail, packings)) {
            return;
        }

        for (Packing packing : packings) {
            if (packing.getContainerId() != null) {
                Containers containers  = containersMap.get(packing.getContainerId());
                String containerNumber = packing.getPacksType();
                if(containers != null) {
                    containerNumber = containers.getContainerNumber() != null ? containers.getContainerNumber() : containers.getContainerCode();
                }
                String errorMsg = String.format(
                    "Selected Shipment - %s - %s packs is assigned to the container(s): %s. Please unassign to detach.",
                    shipmentDetail.getShipmentId(),
                    packing.getPacks(),
                    containerNumber
                );
                throw new ValidationException(errorMsg);
            }
        }
    }

    private void validateContainersAttachedToShipment(ShipmentDetails shipmentDetail, List<Containers> containersList) {
        for (Containers container : containersList) {
            if (container.getId() != null) {
                String containerNumber = container.getContainerNumber() != null ? container.getContainerNumber() : container.getContainerCode();
                String errorMsg = String.format(
                    "Selected Shipment - %s is assigned with mentioned container : %s, Please unassign to detach the same.",
                    shipmentDetail.getShipmentId(),
                    containerNumber
                );
                throw new ValidationException(errorMsg);
            }
        }
    }


    boolean isSeaPackingList(ShipmentDetails shipmentDetail, List<Packing> packingList) {
        return Constants.TRANSPORT_MODE_SEA.equals(shipmentDetail.getTransportMode()) && !listIsNullOrEmpty(packingList);
    }

    private Map<Long, ShipmentDetails> getShipmentDetailsMap(List<ShipmentDetails> shipmentDetails){
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
        }
        return shipmentDetailsMap;
    }

    private List<ShipmentDetails> fetchAndValidateShipments(Set<Long> shipmentIds) {
        return Optional.ofNullable(shipmentIds)
            .filter(ObjectUtils::isNotEmpty)
            .map(ids -> shipmentDao.findShipmentsByIds(ids.stream().collect(Collectors.toSet())))
            .orElse(Collections.emptyList());
    }

    private ResponseEntity<IRunnerResponse> validateShipmentDetachment(List<ShipmentDetails> shipmentDetails, ConsolidationDetails consolidationDetails) {
        consolidationValidationV3Util.validateAirDGPermissionsInDetach(shipmentDetails, consolidationDetails);
        return validateOutstandingDuesForShipments(shipmentDetails);
    }

    protected ResponseEntity<IRunnerResponse> validateOutstandingDuesForShipments(List<ShipmentDetails> shipmentDetails) {
        try {
            // Retrieve tenant settings
            V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();

            // Exit if shipment details are empty or if split billing is not enabled
            if (ObjectUtils.isEmpty(shipmentDetails) || !Boolean.TRUE.equals(tenantSettings.getEnableConsolSplitBillCharge())) {
                return null;
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
                return null;
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

            List<ShipmentDetachResponse> shipmentDetachResponseList = new ArrayList<>();


            if (ObjectUtils.isNotEmpty(summariesWithOutstandingDues)) {
                List<ShipmentDetails> shipmentWithDues = summariesWithOutstandingDues.stream()
                    .map(summary -> {
                        ShipmentDetails shipment = shipmentMapByGuid.get(summary.getModuleGuid());
                        return (shipment != null) ? shipment : null;
                    }).filter(Objects::nonNull).toList();

                if (!shipmentWithDues.isEmpty()) {

                    for(ShipmentDetails shipmentDetails1 : shipmentWithDues){
                        shipmentDetachResponseList.add(ShipmentDetachResponse
                            .builder()
                            .shipmentId(shipmentDetails1.getId())
                            .shipmentNumber(shipmentDetails1.getShipmentId())
                            .hbl(shipmentDetails1.getHouseBill())
                            .build());
                    }
                    return buildDependentServiceResponse(shipmentDetachResponseList, 0 , 0);
                }
            }
            return null;
        }  catch (Exception e) {
            throw new BillingException(e.getMessage(), e);
        }
    }

    BillingBulkSummaryBranchWiseRequest createBillingBulkSummaryBranchWiseRequest(
        List<ShipmentDetails> shipmentDetails) {
        BillingBulkSummaryBranchWiseRequest branchWiseRequest = new BillingBulkSummaryBranchWiseRequest();
        branchWiseRequest.setModuleType(Constants.SHIPMENT);

        branchWiseRequest.setModuleData(
            shipmentDetails.stream()
                .map(shp -> BillingBulkSummaryBranchWiseRequest.ModuleData.builder()
                    .branchId(shp.getTenantId().toString())
                    .moduleGuid(shp.getGuid().toString()).build())
                .toList()
        );
        return branchWiseRequest;
    }

    @Override
    public ConsolidationDetails getConsolidationById(Long consolidationId) {
        return consolidationDetailsDao.findConsolidationsById(consolidationId);
    }

    @Override
    public Optional<ConsolidationDetails> findById(Long consolidationId) {
        return consolidationDetailsDao.findById(consolidationId);
    }

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync) {
        return consolidationDetailsDao.save(consolidationDetails, fromV1Sync);
    }

    private boolean isValidCutoffCase(ConsolidationDetails oldConsolidation,
        ConsolidationDetails newConsolidation, List<ShipmentDetails> shipmentDetailsList, Boolean fromAttachShipment){

        return ((!Boolean.TRUE.equals(fromAttachShipment) && oldConsolidation == null)
            || newConsolidation == null || shipmentDetailsList == null);
    }


    protected void updateShipmentDetailsIfConsolidationChanged(ConsolidationDetails oldConsolidation,
        ConsolidationDetails newConsolidation, List<ShipmentDetails> shipmentDetailsList, Boolean fromAttachShipment) {

        if(isValidCutoffCase(oldConsolidation, newConsolidation, shipmentDetailsList, fromAttachShipment)) return;

        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            String transportMode = shipmentDetails.getTransportMode();

            if (TRANSPORT_MODE_SEA.equalsIgnoreCase(transportMode)) {
                applySeaCutoffs(oldConsolidation, newConsolidation, shipmentDetails, fromAttachShipment);
            } else if (TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode)) {
                applyAirCutOffs(oldConsolidation, newConsolidation, shipmentDetails, fromAttachShipment);
            }
        }
    }

    private void applyAirCutOffs(ConsolidationDetails oldConsolidation,
        ConsolidationDetails newConsolidation, ShipmentDetails shipmentDetails, Boolean fromAttachShipment){
        if (Boolean.TRUE.equals(fromAttachShipment)) {
            if (newConsolidation.getLatDate() != null) {
                shipmentDetails.setLatestArrivalTime(newConsolidation.getLatDate());
            }
        } else {
            updateIfChanged(oldConsolidation.getLatDate(), newConsolidation.getLatDate(),
                shipmentDetails::setLatestArrivalTime);
        }
    }

    private void applySeaCutoffs(ConsolidationDetails oldConsolidation,
        ConsolidationDetails newConsolidation, ShipmentDetails shipmentDetails, Boolean fromAttachShipment){
        if (Boolean.TRUE.equals(fromAttachShipment)) {
            applySeaCutoffsForAttachedShipment(newConsolidation, shipmentDetails);
        } else {
            applySeaCutoffsUpdate(oldConsolidation, newConsolidation, shipmentDetails);
        }
    }

    private void applySeaCutoffsForAttachedShipment(ConsolidationDetails newConsolidation, ShipmentDetails shipmentDetails) {
        if (newConsolidation.getTerminalCutoff() != null) {
            shipmentDetails.setTerminalCutoff(newConsolidation.getTerminalCutoff());
        }
        if (newConsolidation.getVerifiedGrossMassCutoff() != null) {
            shipmentDetails.setVerifiedGrossMassCutoff(newConsolidation.getVerifiedGrossMassCutoff());
        }
        if (newConsolidation.getShipInstructionCutoff() != null) {
            shipmentDetails.setShippingInstructionCutoff(newConsolidation.getShipInstructionCutoff());
        }
        if (newConsolidation.getHazardousBookingCutoff() != null) {
            shipmentDetails.setDgCutoff(newConsolidation.getHazardousBookingCutoff());
        }
        if (newConsolidation.getReeferCutoff() != null) {
            shipmentDetails.setReeferCutoff(newConsolidation.getReeferCutoff());
        }
        if (newConsolidation.getEarliestEmptyEquPickUp() != null) {
            shipmentDetails.setEarliestEmptyEquipmentPickUp(newConsolidation.getEarliestEmptyEquPickUp());
        }
        if (newConsolidation.getLatestFullEquDeliveredToCarrier() != null) {
            shipmentDetails.setLatestFullEquipmentDeliveredToCarrier(newConsolidation.getLatestFullEquDeliveredToCarrier());
        }
        if (newConsolidation.getEarliestDropOffFullEquToCarrier() != null) {
            shipmentDetails.setEarliestDropOffFullEquipmentToCarrier(newConsolidation.getEarliestDropOffFullEquToCarrier());
        }
    }

    private void applySeaCutoffsUpdate(ConsolidationDetails oldConsolidation, ConsolidationDetails newConsolidation,
        ShipmentDetails shipmentDetails) {
        updateIfChanged(oldConsolidation.getTerminalCutoff(), newConsolidation.getTerminalCutoff(),
            shipmentDetails::setTerminalCutoff);

        updateIfChanged(oldConsolidation.getVerifiedGrossMassCutoff(), newConsolidation.getVerifiedGrossMassCutoff(),
            shipmentDetails::setVerifiedGrossMassCutoff);

        updateIfChanged(oldConsolidation.getShipInstructionCutoff(), newConsolidation.getShipInstructionCutoff(),
            shipmentDetails::setShippingInstructionCutoff);

        updateIfChanged(oldConsolidation.getHazardousBookingCutoff(), newConsolidation.getHazardousBookingCutoff(),
            shipmentDetails::setDgCutoff);

        updateIfChanged(oldConsolidation.getReeferCutoff(), newConsolidation.getReeferCutoff(),
            shipmentDetails::setReeferCutoff);

        updateIfChanged(oldConsolidation.getEarliestEmptyEquPickUp(), newConsolidation.getEarliestEmptyEquPickUp(),
            shipmentDetails::setEarliestEmptyEquipmentPickUp);

        updateIfChanged(oldConsolidation.getLatestFullEquDeliveredToCarrier(), newConsolidation.getLatestFullEquDeliveredToCarrier(),
            shipmentDetails::setLatestFullEquipmentDeliveredToCarrier);

        updateIfChanged(oldConsolidation.getEarliestDropOffFullEquToCarrier(), newConsolidation.getEarliestDropOffFullEquToCarrier(),
            shipmentDetails::setEarliestDropOffFullEquipmentToCarrier);
    }

    private void updateIfChanged(LocalDateTime oldValue, LocalDateTime newValue, Consumer<LocalDateTime> setter) {
        if (!Objects.equals(oldValue, newValue)) {
            setter.accept(newValue);
        }
    }

  @Override
  public String getBookingNumberFromConsol(Long consolidationId) {
    return consolidationDetailsDao.getBookingNumberFromConsol(consolidationId);
  }

    @Override
    public void updateConsolidationAttachmentFlag(Boolean enableFlag, Long consolidationId) {
        try {
            if (enableFlag == null) {
                throw new IllegalArgumentException("enableFlag cannot be null");
            }
            consolidationDetailsDao.updateConsolidationAttachmentFlag(enableFlag, consolidationId);
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public ConsolidationSailingScheduleResponse updateSailingScheduleDataToShipment(
        ConsolidationSailingScheduleRequest request) throws RunnerException {
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        Optional<RoutingsRequest> firstRouting = request.getRoutings().stream().findFirst();
        if (firstRouting.isEmpty()) {
            return new ConsolidationSailingScheduleResponse();
        }
        Long consolidationId = firstRouting.get().getConsolidationId();
        List<Routings> routingsList = routingsV3Service.getRoutingsByConsolidationId(consolidationId);
        if (!CollectionUtils.isEmpty(routingsList)) {
            routingsList.removeIf(routing -> routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE);
        }
        List<RoutingsRequest> finalShipmentRouteList = new ArrayList<>();
        finalShipmentRouteList.addAll(request.getRoutings());
        finalShipmentRouteList.addAll(jsonHelper.convertValueToList(routingsList, RoutingsRequest.class));

        bulkUpdateRoutingsRequest.setRoutings(finalShipmentRouteList);
        bulkUpdateRoutingsRequest.setEntityId(consolidationId);
        routingsV3Service.updateBulk(bulkUpdateRoutingsRequest, CONSOLIDATION);

        //update shipment fields
        Optional<ConsolidationDetails> consolidationDetailsEntity = consolidationDetailsDao.findById(consolidationId);
        if (consolidationDetailsEntity.isEmpty())
            return new ConsolidationSailingScheduleResponse();
        ConsolidationDetails consolidationDetails = consolidationDetailsEntity.get();


        String carrierNameFromMasterData = masterDataUtils.getCarrierNameFromMasterDataUsingScacCodeFromIntraa(request.getScacCode());
        if(consolidationDetails.getShipmentsList() != null) {
            if (isInterBranchContextNeeded(consolidationDetails))
                commonUtils.setInterBranchContextForHub();

            for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                shipmentV3Service.updateCutoffDetailsToShipment(jsonHelper.convertValue(request, ShipmentSailingScheduleRequest.class), shipmentDetails);
                CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();
                carrierDetails.setShippingLine(carrierNameFromMasterData);
                carrierDetailsDao.update(carrierDetails);
            }
        }
        updateCutoffDetailsToConsolidations(request, consolidationDetails);
        CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();
        carrierDetails.setShippingLine(carrierNameFromMasterData);
        carrierDetailsDao.update(carrierDetails);
        return new ConsolidationSailingScheduleResponse();
    }



    private void updateCutoffDetailsToConsolidations(ConsolidationSailingScheduleRequest request, ConsolidationDetails consolidationDetails){
        String transportMode = consolidationDetails.getTransportMode();
        if(TRANSPORT_MODE_SEA.equalsIgnoreCase(transportMode)){
            consolidationDetailsDao.updateSailingScheduleRelatedInfo(request, consolidationDetails.getId());
        }else if(TRANSPORT_MODE_AIR.equalsIgnoreCase(transportMode)){
            consolidationDetailsDao.updateSailingScheduleRelatedInfoForAir(request, consolidationDetails.getId());
        }
    }

    protected boolean canProcesscutOffFields(ConsolidationDetails consol, ConsolidationDetails oldEntity) {
        return !Objects.equals(consol.getTerminalCutoff(), oldEntity.getTerminalCutoff()) ||
            !Objects.equals(consol.getVerifiedGrossMassCutoff(), oldEntity.getVerifiedGrossMassCutoff()) ||
            !Objects.equals(consol.getShipInstructionCutoff(), oldEntity.getShipInstructionCutoff()) ||
            !Objects.equals(consol.getHazardousBookingCutoff(), oldEntity.getHazardousBookingCutoff()) ||
            !Objects.equals(consol.getReeferCutoff(), oldEntity.getReeferCutoff()) ||
            !Objects.equals(consol.getEarliestEmptyEquPickUp(), oldEntity.getEarliestEmptyEquPickUp()) ||
            !Objects.equals(consol.getLatestFullEquDeliveredToCarrier(), oldEntity.getLatestFullEquDeliveredToCarrier()) ||
            !Objects.equals(consol.getEarliestDropOffFullEquToCarrier(), oldEntity.getEarliestDropOffFullEquToCarrier()) ||
            !Objects.equals(consol.getLatDate(), oldEntity.getLatDate());
    }

    private void serviceTypeAutoPopulation(ConsolidationDetails consolidationDetails, ShipmentDetails shipmentDetails) {
            String deliveryMode = consolidationDetails.getDeliveryMode();
            String serviceType = getIdentifierFromHBLDeliveryModeMasterData(deliveryMode);

            if (!Objects.equals(serviceType, shipmentDetails.getServiceType()) && isServiceTypeValid(serviceType)) {
                //Validate new deliveryCode
                    shipmentDetails.setServiceType(serviceType);
        }
    }
    protected String getIdentifierFromHBLDeliveryModeMasterData(String deliveryMode) {
        if (StringUtility.isEmpty(deliveryMode)) {
            return null;
        }
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> subCriteria1 = Arrays.asList(
            Arrays.asList(MasterDataConstants.ITEM_TYPE),
            "=",
            MasterDataType.HBL_DELIVERY_MODE.getId()
        );
        List<Object> subCriteria2 = Arrays.asList(
            Arrays.asList(MasterDataConstants.ITEM_VALUE),
            "=",
            deliveryMode
        );
        criteria.addAll(List.of(subCriteria1, "and", subCriteria2));
        request.setCriteriaRequests(criteria);
        try {
            V1DataResponse response = v1Service.fetchMasterData(request);
            List<EntityTransferMasterLists> responseList = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            if (responseList != null && !responseList.isEmpty())
                return responseList.get(0).getIdentifier1();
        } catch (Exception ignored) {
            log.info(Constants.IGNORED_ERROR_MSG);
        }
        return null;
    }

    protected boolean isServiceTypeValid(String serviceType){
        if (StringUtility.isEmpty(serviceType)) {
            return false;
        }
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> subCriteria1 = Arrays.asList(
            Arrays.asList(MasterDataConstants.ITEM_TYPE),
            "=",
            MasterDataType.SERVICE_MODE.getId()
        );
        List<Object> subCriteria2 = Arrays.asList(
            Arrays.asList(MasterDataConstants.ITEM_VALUE),
            "=",
            serviceType
        );
        criteria.addAll(List.of(subCriteria1, "and", subCriteria2));
        request.setCriteriaRequests(criteria);
        try {
            V1DataResponse response = v1Service.fetchMasterData(request);
            List<EntityTransferMasterLists> responseList = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            if (responseList != null && !responseList.isEmpty())
                return true;
        } catch (Exception ignored) {
            log.info(Constants.IGNORED_ERROR_MSG);
        }
        return false;
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
                    processNonEmptyShipIdList(shipIdList, mawb, consol.get());
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

    private void processNonEmptyShipIdList(List<Long> shipIdList, Awb mawb, ConsolidationDetails consol) throws RunnerException {
        List<Awb> awbs = awbDao.findByShipmentIdList(shipIdList);
        if (awbs != null && !awbs.isEmpty()) {
            var isShipmentSciT1 = awbs.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1)).findAny();
            if (isShipmentSciT1.isEmpty()) {
                mawb.setAirMessageResubmitted(false);
                mawb.getAwbCargoInfo().setSci(null);
                awbDao.save(mawb);
                consol.setSci(null);
                consolidationDetailsDao.save(consol, false);
            }
        }
    }



    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> aibAction(AibActionConsolidation aibActionRequest) throws RunnerException {
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(aibActionRequest.getConsoleId());
        if (consolidationDetails.isPresent()) {
            if (Boolean.TRUE.equals(consolidationDetails.get().getInterBranchConsole())) {
                commonUtils.setInterBranchContextForHub();
            }
            if (ShipmentRequestedType.APPROVE.equals(aibActionRequest.getShipmentRequestedType())) { // one console multiple shipments
                try {
                    this.attachShipments(
                            ShipmentConsoleAttachDetachV3Request.builder()
                                    .shipmentRequestedType(aibActionRequest.getShipmentRequestedType())
                                    .consolidationId(aibActionRequest.getConsoleId())
                                    .shipmentIds(new HashSet<>(aibActionRequest.getListOfShipments()))
                                    .isFromConsolidation(true)
                                    .build());
                } catch (RunnerException e) {
                    log.error("{} | Error while attaching shipments during AIB Action for Console: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage(), e);
                    throw new ValidationException(e.getMessage());
                }
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, aibActionRequest.getListOfShipments(), Constants.IN);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(), ConsoleShipmentMapping.class);

                consoleShipmentMappingDao.deletePendingStateByShipmentIds(aibActionRequest.getListOfShipments());
                // one console and list of approved shipments for shipment push accepted from console
                // for each shipment pending multiple consolidation auto rejections (shipment push and shipment pull both got rejected)
                sendEmailsForPushRequestAccept(consolidationDetails.get(), aibActionRequest.getListOfShipments(), shipmentRequestedTypes, consoleShipmentMappingsForEmails);
            } else if (ShipmentRequestedType.REJECT.equals(aibActionRequest.getShipmentRequestedType()) || ShipmentRequestedType.WITHDRAW.equals(aibActionRequest.getShipmentRequestedType())) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, aibActionRequest.getListOfShipments(), Constants.IN, null);
                listCommonRequest = andCriteria(Constants.CONSOLIDATION_ID, aibActionRequest.getConsoleId(), Constants.EQ, listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair2 = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
                List<ConsoleShipmentMapping> consoleShipmentMappings = jsonHelper.convertValueToList(consoleShipmentMappingDao.findAll(pair2.getLeft(), pair2.getRight()).getContent(), ConsoleShipmentMapping.class);
                aibActionRequest.getListOfShipments().stream().forEach(shipmentId -> consoleShipmentMappingDao.deletePendingStateByConsoleIdAndShipmentId(aibActionRequest.getConsoleId(), shipmentId));
                // one console and multiple shipments (shipment push rejected)
                sendRejectionEmails(aibActionRequest, shipmentRequestedTypes, consolidationDetails.get(), consoleShipmentMappings);
            }
        } else {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        String warning = getWarningMsg(shipmentRequestedTypes);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

    public void sendEmailsForPushRequestAccept(ConsolidationDetails console, List<Long> shipmentIds, Set<ShipmentRequestedType> requestedTypes, List<ConsoleShipmentMapping> consoleShipMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> locationsMap = new HashMap<>();
        Map<String, CarrierMasterData> carriersMap = new HashMap<>();
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> userNames = new HashSet<>();
        List<ShipmentDetails> shipmentDetails = new ArrayList<>();
        List<ConsolidationDetails> otherConsoles = new ArrayList<>();
        Map<Long, String> requestedUserNamesMap = new HashMap<>();

        // fetching data from db
        fetchShipmentsAndConsolidationsForPushRequestEmails(tenantIds, userNames, shipmentIds, console, shipmentDetails, otherConsoles, consoleShipMappings, requestedUserNamesMap);

        // making v1 calls for master data
        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(Stream.of(console.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carriersMap)), executorService);
        var locationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(Stream.of(console.getCarrierDetails().getOriginPort(), console.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), locationsMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, tenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(userNames, userEmailsMap)), executorService);

        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, locationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        // constructing and sending emails
        constructAndSendEmailsForPushRequestAccept(shipmentDetails, console, otherConsoles, consoleShipMappings, requestedTypes,
                locationsMap, carriersMap, userEmailsMap, tenantSettingsMap, emailTemplatesMap, requestedUserNamesMap);
    }

    private void fetchShipmentsAndConsolidationsForPushRequestEmails(Set<Integer> tenantIds, Set<String> userNames, List<Long> shipmentIds, ConsolidationDetails console,
                                                                     List<ShipmentDetails> shipments, List<ConsolidationDetails> otherConsoles,
                                                                     List<ConsoleShipmentMapping> consoleShipMappings, Map<Long, String> requestedUsernameMap) {
        // fetching shipments
        ListCommonRequest listCommonRequest = constructListCommonRequest(ID, shipmentIds, Constants.IN);
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        for(ShipmentDetails shipmentDetails : shipmentDetailsPage.getContent()) {
            tenantIds.add(shipmentDetails.getTenantId());
            userNames.add(shipmentDetails.getCreatedBy());
            userNames.add(shipmentDetails.getAssignedTo());
            shipments.add(shipmentDetails);
        }

        // fetching other consolidations
        List<Long> otherConsoleIds = new ArrayList<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipMappings) {
            if(!Boolean.TRUE.equals(consoleShipmentMapping.getIsAttachmentDone())) {
                otherConsoleIds.add(consoleShipmentMapping.getConsolidationId());
            } else if(shipmentIds.contains(consoleShipmentMapping.getShipmentId()) && Objects.equals(consoleShipmentMapping.getConsolidationId(), console.getId())){
                requestedUsernameMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            }
            userNames.add(consoleShipmentMapping.getCreatedBy());
        }
        Page<ConsolidationDetails> consolidationDetailsPage = null;
        if(!otherConsoleIds.isEmpty()) {
            listCommonRequest = constructListCommonRequest(ID, otherConsoleIds, Constants.IN);
            Pair<Specification<ConsolidationDetails>, Pageable> pair3 = fetchData(listCommonRequest, ConsolidationDetails.class);
            consolidationDetailsPage = consolidationDetailsDao.findAll(pair3.getLeft(), pair3.getRight());
            for(ConsolidationDetails consolidationDetails1 : consolidationDetailsPage.getContent()) {
                tenantIds.add(consolidationDetails1.getTenantId());
                userNames.add(consolidationDetails1.getCreatedBy());
                otherConsoles.add(consolidationDetails1);
            }
        }
    }

    private void constructAndSendEmailsForPushRequestAccept(List<ShipmentDetails> shipments, ConsolidationDetails console,
                                                            List<ConsolidationDetails> otherConsoles, List<ConsoleShipmentMapping> consoleShipMapping,
                                                            Set<ShipmentRequestedType> requestedTypes,
                                                            Map<String, UnlocationsResponse> locationsMap, Map<String, CarrierMasterData> carriersMap,
                                                            Map<String, String> userEmailsMap, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap,
                                                            Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap, Map<Long, String> requestedUsernameMap) {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        if(shipments != null && !shipments.isEmpty()) {
            shipmentDetailsMap = shipments.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));
            shipments.forEach(shipment -> {
                try {
                    commonUtils.sendEmailForPullPushRequestStatus(shipment, console, SHIPMENT_PUSH_ACCEPTED, null, emailTemplatesMap, requestedTypes, locationsMap, carriersMap, userEmailsMap, tenantSettingsMap, requestedUsernameMap.get(shipment.getId()), null);
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
        if(!otherConsoles.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsoles.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetailsMap;
            consoleShipMapping.stream().filter(e -> !Boolean.TRUE.equals(e.getIsAttachmentDone())).forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        sendPullPushRequestStatusEmail(requestedTypes, locationsMap, carriersMap, userEmailsMap, tenantSettingsMap, emailTemplatesMap, consoleShipmentMapping, finalShipmentDetailsMap, finalConsolidationDetailsMap);
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
    }

    private void sendPullPushRequestStatusEmail(Set<ShipmentRequestedType> requestedTypes,
                                                Map<String, UnlocationsResponse> locationsMap, Map<String, CarrierMasterData> carriersMap,
                                                Map<String, String> userEmailMap, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap,
                                                Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap,
                                                ConsoleShipmentMapping consoleShipMapping, Map<Long, ShipmentDetails> finalShipMap,
                                                Map<Long, ConsolidationDetails> finalConsoleMap) throws Exception {
        if(consoleShipMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
            commonUtils.sendEmailForPullPushRequestStatus(finalShipMap.get(consoleShipMapping.getShipmentId()), finalConsoleMap.get(consoleShipMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesMap, requestedTypes, locationsMap, carriersMap, userEmailMap, tenantSettingsMap, consoleShipMapping.getCreatedBy(), null);
        else
            commonUtils.sendEmailForPullPushRequestStatus(finalShipMap.get(consoleShipMapping.getShipmentId()), finalConsoleMap.get(consoleShipMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesMap, requestedTypes, locationsMap, carriersMap, userEmailMap, tenantSettingsMap, consoleShipMapping.getCreatedBy(), null);
    }

    private void sendRejectionEmails(AibActionConsolidation aibActionRequest, Set<ShipmentRequestedType> requestedTypes, ConsolidationDetails console, List<ConsoleShipmentMapping> consoleShipMapping) {
        if(ShipmentRequestedType.REJECT.equals(aibActionRequest.getShipmentRequestedType())) {
            sendEmailForPushRequestReject(console, aibActionRequest.getListOfShipments(), requestedTypes, aibActionRequest.getRejectRemarks(), consoleShipMapping);
        } else if(ShipmentRequestedType.WITHDRAW.equals(aibActionRequest.getShipmentRequestedType())) {
            sendEmailForPullRequestWithdrawal(console, aibActionRequest.getListOfShipments(), requestedTypes, aibActionRequest.getRejectRemarks());
        }
    }

    public void sendEmailForPushRequestReject(ConsolidationDetails console, List<Long> shipmentIds, Set<ShipmentRequestedType> requestedTypes, String rejectRemarks, List<ConsoleShipmentMapping> consoleShipMappings) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.ID, shipmentIds, Constants.IN);
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for(var shipmentDetail : shipmentDetails.getContent()) {
            shipmentDetailsMap.put(shipmentDetail.getId(), shipmentDetail);
            tenantIds.add(shipmentDetail.getTenantId());
            usernamesList.add(shipmentDetail.getCreatedBy());
            usernamesList.add(shipmentDetail.getAssignedTo());
        }
        if(shipmentDetails != null && !shipmentDetails.getContent().isEmpty())
            shipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e -> e));

        Map<Long, String> shipmentRequestUserMap = new HashMap<>();
        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipMappings) {
            shipmentRequestUserMap.put(consoleShipmentMapping.getShipmentId(), consoleShipmentMapping.getCreatedBy());
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, tenantSettingsMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, userEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for(Long shipId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipId), console, SHIPMENT_PUSH_REJECTED, rejectRemarks, emailTemplatesMap, requestedTypes, null, null, userEmailsMap, tenantSettingsMap, shipmentRequestUserMap.get(shipId), null);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    public void sendEmailForPullRequestWithdrawal(ConsolidationDetails console, List<Long> shipmentIds, Set<ShipmentRequestedType> requestedTypes, String withdrawRemarks) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesMap =  new EnumMap<>(ShipmentRequestedType.class);
        Map<String, String> userEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> tenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> userNames = new HashSet<>();
        Map<Integer, TenantModel> tenantsMap = new HashMap<>();

        List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        if(!listIsNullOrEmpty(shipments)) {
            for(var shipment : shipments) {
                shipmentDetailsMap.put(shipment.getId(), shipment);
                tenantIds.add(shipment.getTenantId());
                userNames.add(shipment.getCreatedBy());
                userNames.add(shipment.getAssignedTo());
            }
        }
        userNames.add(console.getCreatedBy());
        tenantIds.add(console.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesMap)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(userNames, userEmailsMap)), executorService);
        var tenantsDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettingsAndTenantsData(tenantIds, tenantSettingsMap, tenantsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, tenantsDataFuture, userEmailsFuture).join();

        for(Long shipmentId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipmentId), console, SHIPMENT_PULL_WITHDRAW, withdrawRemarks, emailTemplatesMap, requestedTypes, null, null, userEmailsMap, tenantSettingsMap, null, tenantsMap);
            } catch (Exception e) {
                log.error(ERROR_WHILE_SENDING_EMAIL);
            }
        }
    }

    private String getWarningMsg(Set<ShipmentRequestedType> requestedTypes) {
        return !requestedTypes.isEmpty() ? TEMPLATE_NOT_FOUND_MESSAGE : null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> aibPendingNotification(CommonRequestModel commonRequestModel) {
        AibNotificationRequest request = (AibNotificationRequest) commonRequestModel.getData();
        PendingNotificationResponse<PendingConsolidationActionResponse> response = new PendingNotificationResponse<>();
        if (request.getId() == null) {
            log.info("Received empty request for pending notification in consolidation", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(response);
        }
        var notificationMap = getNotificationMap(request);
        response.setNotificationMap(notificationMap);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private Map<Long, List<PendingConsolidationActionResponse>> getNotificationMap(AibNotificationRequest request) {
        // Get data of all shipments pushing to be attached to this consol
        Map<Long, List<PendingConsolidationActionResponse>> notificationResultMap = new HashMap<>();

        if(commonUtils.getCurrentTenantSettings() == null || !(Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) &&
                Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsColoadingMAWBStationEnabled()))) {
            return notificationResultMap;
        }

        try {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, request.getId(), Constants.EQ);
            listRequest = andCriteria(Constants.IS_ATTACHMENT_DONE, false, Constants.EQ, listRequest);
            Pair<Specification<ConsoleShipmentMapping>, Pageable> consoleShipMappingPair = fetchData(listRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> mappingPage = consoleShipmentMappingDao.findAll(consoleShipMappingPair.getLeft(), consoleShipMappingPair.getRight());

            List<Long> shipmentIds = mappingPage.getContent().stream().map(ConsoleShipmentMapping::getShipmentId).toList();
            final var consoleShipmentsMap = mappingPage.getContent().stream().collect(Collectors.toMap(
                    ConsoleShipmentMapping::getShipmentId, Function.identity(), (oldVal, newVal) -> oldVal)
            );

            commonUtils.setInterBranchContextForHub();

            listRequest = constructListCommonRequest(ID, shipmentIds, Constants.IN);
            listRequest.setContainsText(request.getContainsText());
            listRequest.setSortRequest(request.getSortRequest());
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class, ShipmentService.tableNames);
            Page<ShipmentDetails> shipmentsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            var tenantIds = new HashSet<String>();
            var locations =  new HashSet<String>();
            final CarrierDetails nullCarrierDetails = new CarrierDetails();
            shipmentsPage.getContent().stream().forEach(i -> {
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
                .requestedType(Objects.nonNull(consoleShipmentsMap.get(shipment.getId()).getRequestedType()) ? consoleShipmentsMap.get(shipment.getId()).getRequestedType().getV3Description() : null)
                .build();
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

    public AchievedQuantitiesResponse getConsoleSyncAchievedData(Long consolidationId) throws RunnerException, JsonMappingException {
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);
        if(Objects.isNull(consolidationDetails.getAchievedQuantities()))
            consolidationDetails.setAchievedQuantities(new AchievedQuantities());
        AchievedQuantitiesResponse achievedQuantitiesResponse = jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class);
        ShipmentWtVolResponse shipmentWtVolResponse = calculateShipmentWtVol(consolidationDetails);
        achievedQuantitiesResponse.setWeightVolume(shipmentWtVolResponse.getWeightVolume());
        achievedQuantitiesResponse.setWeightVolumeUnit(shipmentWtVolResponse.getWeightVolumeUnit());
        achievedQuantitiesResponse.setPacks(shipmentWtVolResponse.getPacks());
        achievedQuantitiesResponse.setPacksType(shipmentWtVolResponse.getPacksType());
        achievedQuantitiesResponse.setDgPacks(shipmentWtVolResponse.getDgPacks());
        achievedQuantitiesResponse.setDgPacksType(shipmentWtVolResponse.getDgPacksType());
        achievedQuantitiesResponse.setSlacCount(shipmentWtVolResponse.getSlacCount());
        achievedQuantitiesResponse.setConsolidatedWeight(shipmentWtVolResponse.getWeight());
        achievedQuantitiesResponse.setConsolidatedWeightUnit(shipmentWtVolResponse.getWeightUnit());
        achievedQuantitiesResponse.setConsolidatedVolume(shipmentWtVolResponse.getVolume());
        achievedQuantitiesResponse.setConsolidatedVolumeUnit(shipmentWtVolResponse.getVolumeUnit());
        achievedQuantitiesResponse.setConsolidationChargeQuantity(shipmentWtVolResponse.getChargable());
        achievedQuantitiesResponse.setConsolidationChargeQuantityUnit(shipmentWtVolResponse.getChargeableUnit());
        achievedQuantitiesResponse.setContainerCount(shipmentWtVolResponse.getConsoleContainerCount());
        achievedQuantitiesResponse.setTeuCount(shipmentWtVolResponse.getConsoleTeuCount());
        achievedQuantitiesResponse.setDgContainerCount(shipmentWtVolResponse.getConsoleDgContainerCount());
        return achievedQuantitiesResponse;
    }

    private boolean isInterBranchContextNeeded(ConsolidationDetails consolidationDetails) {

        if (Objects.isNull(consolidationDetails) || Objects.isNull(consolidationDetails.getShipmentsList()))
            return false;

        var interBranchShipments = consolidationDetails.getShipmentsList().stream()
                .filter(s -> !Objects.equals(s.getTenantId(), consolidationDetails.getTenantId())).findFirst();

        return interBranchShipments.isPresent() && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
    }

    public ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationEtV3Request request) {
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails, true);

            consolidationV3Util.afterSaveForET(consolidationDetails, null, request, true, shipmentSettingsDetails, false, true);
            syncShipmentDataInPlatform(consolidationDetails);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
    }

    public ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationEtV3Request consolidationDetailsRequest) throws RunnerException {
        Optional<ConsolidationDetails> oldEntity =  getConsolidationDetails(consolidationDetailsRequest.getId(), consolidationDetailsRequest.getGuid());

        consolidationDetailsRequest.setShipmentsList(null);

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            ConsolidationDetails entity = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            setInterBranchContext(entity.getInterBranchConsole());
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            ConsolidationDetails oldConvertedConsolidation = jsonHelper.convertValue(oldEntity.get(), ConsolidationDetails.class);

            beforeSave(entity, oldEntity.get(), false);

            entity = consolidationDetailsDao.updateV3(entity, true);
            ConsolidationDetails prevEntity = jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class);

            addAuditLogConsolidation(entity, prevEntity, DBOperationType.UPDATE.name());

            consolidationV3Util.afterSaveForET(entity, oldConvertedConsolidation, consolidationDetailsRequest, false, shipmentSettingsDetails, false, false);
            ConsolidationDetails finalEntity1 = entity;
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(finalEntity1)), executorService);
            return jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e.getMessage());
        }
    }
    private void addAuditLogConsolidation(ConsolidationDetails entity, ConsolidationDetails prevEntity, String operationName) {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(entity)
                            .prevData(prevEntity)
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operationName).build()
            );
        }
        catch (Exception e) {
            log.error("Error writing audit service log", e);
        }
    }

    /**
     * This method is used to get the count of attached self branch shipments | cross branch shipments | pending requests for any given console.
     * This is used for validation whether we can turn off the Open For Attachment Flag any console.
     * @param request
     * @return AllShipmentCountResponse containing attached self/inter branch shipments & pending for attachment shipments.
     */
    @Override
    public ResponseEntity<IRunnerResponse> aibAttachedPendingShipmentCount(@NotNull CommonGetRequest request, String xSource) throws AuthenticationException, RunnerException {
        Long attachedShipSelfBranch = 0L;
        Long attachedShipInterBranch = 0L;
        Long pendingForAttachment = 0L;
        Optional<ConsolidationDetails> consoleDetails;

        if(Objects.equals(xSource, NETWORK_TRANSFER)){
            consoleDetails = retrieveForNte(request.getId());
        }else {
            consoleDetails = consolidationDetailsDao.findById(request.getId());
        }

        if (consoleDetails.isEmpty()) {
            log.error(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        var consoleShipMappings = consoleShipmentMappingDao.findByConsolidationIdAll(request.getId());

        for(var mapping: consoleShipMappings) {
            if(Objects.isNull(mapping.getRequestedType()))
                attachedShipSelfBranch++;
            else if(Objects.equals(mapping.getRequestedType(), ShipmentRequestedType.APPROVE))
                attachedShipInterBranch++;
            else if(Objects.equals(mapping.getRequestedType(), ShipmentRequestedType.SHIPMENT_PULL_REQUESTED) || Objects.equals(mapping.getRequestedType(), ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED))
                pendingForAttachment++;
        }

        return ResponseHelper.buildSuccessResponse(
                AllShipmentCountResponse.builder()
                        .attachedShipmentCurrentBranchCount(attachedShipSelfBranch)
                        .attachedShipmentInterBranchCount(attachedShipInterBranch)
                        .pendingAttachmentCount(pendingForAttachment)
                        .build()
        );
    }

    @Override
    public CheckDGShipmentV3 getDGShipment(Long consolidationId) {
        if (consolidationId == null) {
            throw new ValidationException("Consolidation Id is required");
        }

        var console = consolidationDetailsDao.findById(consolidationId)
                .orElseThrow(() -> new ValidationException("No Consolidation found for the Id: " + consolidationId));

        boolean isAnyContainerDG = hasHazardousContainer(console);
        boolean isDgShipmentPresent = hasHazardousShipment(console);

        return CheckDGShipmentV3.builder()
                .isAnyContainerDG(isAnyContainerDG)
                .isDGShipmentPresent(isDgShipmentPresent)
                .build();
    }

    private boolean hasHazardousContainer(ConsolidationDetails console) {
        var containers = console.getContainersList();
        return containers != null && containers.stream()
                .anyMatch(container -> Boolean.TRUE.equals(container.getHazardous()));
    }

    private boolean hasHazardousShipment(ConsolidationDetails console) {
        var shipments = console.getShipmentsList();
        return shipments != null && shipments.stream()
                .anyMatch(shipment -> Boolean.TRUE.equals(shipment.getContainsHazardous()));
    }
}
