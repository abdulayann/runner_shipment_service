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
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.ConsolidationMapper;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest.ModuleData;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.request.ConsoleBookingIdFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import java.util.function.Function;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_DETACH;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
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
    private IPackingsSync packingsADSync;

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
    private IRoutingsDao routingsDao;

    @Autowired
    private IEventsV3Service eventV3Service;

    @Autowired
    private IShipmentServiceV3 shipmentV3Service;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IPackingService packingService;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private INotificationDao notificationDao;

    @Autowired
    private IShipmentSync shipmentSync;

    @Autowired
    private IConsolidationSync consolidationSync;

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

    @Autowired
    private ModelMapper modelMapper;

    @Value("${include.master.data}")
    private Boolean includeMasterData;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Override
    @Transactional
    public ConsolidationDetailsResponse create(ConsolidationDetailsV3Request request) {
        return this.createConsolidation(request, false);
    }

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
        Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
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
        Map.entry("hazardous", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build()),
        Map.entry("shipShipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).fieldName("shipmentType").build()),
        Map.entry("tenantId", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Integer.class).fieldName("tenantId").build()),
        Map.entry(Constants.INTER_BRANCH_CONSOLE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.INTER_BRANCH_CONSOLE).build()),
        Map.entry(Constants.OPEN_FOR_ATTACHMENT, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).fieldName(Constants.OPEN_FOR_ATTACHMENT).build()),
        Map.entry("requestedOn", RunnerEntityMapping.builder().tableName("consoleShipmentMappings").dataType(LocalDateTime.class).fieldName(Constants.CREATED_AT).build()),
        Map.entry(Constants.LAT_DATE, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName(Constants.LAT_DATE).build())
    );

    private ConsolidationDetailsResponse createConsolidation(ConsolidationDetailsV3Request request, boolean isFromET) {
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, false, isFromET);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
    }

    @Transactional
    @Override
    public ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel){
        ConsolidationDetailsV3Request request = (ConsolidationDetailsV3Request) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request is null for Consolidation Create");
        }

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);
            populateOriginDestinationAgentDetailsForBookingConsolidation(consolidationDetails);
            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, true, false);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(consolidationDetails)), executorService);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
    }

    @Override
    @Transactional
    public ConsolidationDetailsResponse completeUpdate(ConsolidationDetailsV3Request consolidationDetailsRequest) throws RunnerException {
        return this.completeUpdateConsolidation(consolidationDetailsRequest, false);
    }

    private ConsolidationDetailsResponse completeUpdateConsolidation(ConsolidationDetailsV3Request consolidationDetailsRequest, boolean isFromET) throws RunnerException {
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

            afterSave(entity, oldConvertedConsolidation, consolidationDetailsRequest, false, shipmentSettingsDetails, false, isFromET);
            ConsolidationDetails finalEntity1 = entity;
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForConsole(finalEntity1)), executorService);
            return jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RuntimeException(e.getMessage());
        }
    }

    public Optional<ConsolidationDetails> retrieveByIdOrGuid(ConsolidationDetailsV3Request request) throws RunnerException {
        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ConsolidationDetails> oldEntity;

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

    private void beforeSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isCreate) throws Exception {
        if(Objects.isNull(consolidationDetails.getInterBranchConsole()))
            consolidationDetails.setInterBranchConsole(false);
        /* Future to populate unloc code in consoliation child entities*/
        var populateUnlocCodeFuture = getPopulateUnlocCodeFuture(consolidationDetails, oldEntity);

        if (Objects.isNull(consolidationDetails.getSourceTenantId()))
            consolidationDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
        log.info("Executing consolidation before save");
        List<ShipmentDetails> shipmentDetails = null;
        consolidationValidationV3Util.checkCFSValidation(consolidationDetails, isCreate, shipmentDetails);

        if(consolidationDetails.getCarrierDetails() != null) {
            if (consolidationDetails.getTransportMode() != null && consolidationDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setVoyage(null);
            } else {
                consolidationDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
        if(consolidationDetails.getDocumentationPartner() != null && consolidationDetails.getDocumentationPartner() == 0)
            consolidationDetails.setDocumentationPartner(null);
        setReceivingAndTriangulationBranch(consolidationDetails);
        saveAwb(consolidationDetails, oldEntity);
        if (Boolean.TRUE.equals(isCreate))
            consolidationDetails.setOpenForAttachment(true);

        consolidationValidationV3Util.checkInterBranchPermission(consolidationDetails, oldEntity);

        populateUnlocCodeFuture.join();
    }

    void getConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException{
        generateConsolidationNumber(consolidationDetails);
        consolidationDetails = consolidationDetailsDao.saveV3(consolidationDetails);

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

    private void afterSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ConsolidationDetailsV3Request consolidationDetailsRequest, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean isFromET) throws RunnerException{
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

        Long id = consolidationDetails.getId();

        if (Boolean.FALSE.equals(isCreate) && (checkForAwbUpdate(consolidationDetails, oldEntity))) {
                awbDao.updatedAwbInformationEvent(consolidationDetails, oldEntity);
        }

        processRequestLists(consolidationDetails, isCreate, isFromBooking, referenceNumbersRequestList, id, consolidationAddressRequest);

        if(!isFromET) {
            pushShipmentDataToDependentService(consolidationDetails, isCreate, oldEntity);
            this.pushAllShipmentDataToDependentService(consolidationDetails);
        }
        try {
            if (Boolean.FALSE.equals(isFromBooking))
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

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consolidationDetails, oldEntity)), executorService);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(consolidationDetails, oldEntity, false)), executorService);
    }

    private boolean checkForAwbUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(consolidationDetails.getSci(), oldEntity.getSci())) return true;
        return !Objects.equals(consolidationDetails.getEfreightStatus(), oldEntity.getEfreightStatus());
    }

    private void processRequestLists(ConsolidationDetails consolidationDetails, Boolean isCreate, Boolean isFromBooking, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id,
                                     List<PartiesRequest> consolidationAddressRequest) throws RunnerException {
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (consolidationAddressRequest != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedParties);
        }
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

    private void createLogHistoryForConsole(ConsolidationDetails consolidationDetails){
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
    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, Set<ShipmentDetails> shipmentDetailsList)
            throws RunnerException {

        // Skip processing if consolidation is set to override mode
        if (Boolean.TRUE.equals(consolidationDetails.getOverride())) {
            return;
        }

        // Retrieve shipment settings and tenant settings from the context
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        // Set default units for weight and volume chargeable quantities
        String weightChargeableUnit = Constants.WEIGHT_UNIT_KG;
        if (ObjectUtils.isNotEmpty(shipmentSettingsDetails.getWeightChargeableUnit())) {
            weightChargeableUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        }

        String volumeChargeableUnit = Constants.VOLUME_UNIT_M3;
        if (ObjectUtils.isNotEmpty(shipmentSettingsDetails.getVolumeChargeableUnit())) {
            volumeChargeableUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        }

        // Initialize total weight and volume as zero
        BigDecimal sumWeight = new BigDecimal(0);
        BigDecimal sumVolume = new BigDecimal(0);

        // Accumulate weight and volume from the shipment details list
        if (ObjectUtils.isNotEmpty(shipmentDetailsList)) {
            for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                // Convert weight to the chargeable unit and add to the sum
                sumWeight = sumWeight.add(new BigDecimal(convertUnit(Constants.MASS, shipmentDetails.getWeight(),
                        shipmentDetails.getWeightUnit(), weightChargeableUnit).toString()));

                // Convert volume to the chargeable unit and add to the sum
                sumVolume = sumVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, shipmentDetails.getVolume(),
                        shipmentDetails.getVolumeUnit(), volumeChargeableUnit).toString()));
            }
            // Update the response with the count of processed shipments
            response.setSummaryShipmentsCount(shipmentDetailsList.size());
        } else {
            // Set the shipment count to zero if the list is empty
            response.setSummaryShipmentsCount(0);
        }

        // Calculate the TEU count for the consolidation and update the response
        calculateConsoleShipmentTeuCount(consolidationDetails, response, v1TenantSettingsResponse);

        // Initialize achieved quantities if not already set
        if (consolidationDetails.getAchievedQuantities() == null) {
            consolidationDetails.setAchievedQuantities(new AchievedQuantities());
        }

        // Update the consolidation details with calculated weight and volume
        consolidationDetails.getAchievedQuantities().setConsolidatedWeight(sumWeight);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(weightChargeableUnit);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolume(sumVolume);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(volumeChargeableUnit);

        // Calculate utilization for the consolidation
        consolidationDetails = calculateConsolUtilization(consolidationDetails);

        // Retrieve the transport mode from consolidation details
        String transportMode = consolidationDetails.getTransportMode();

        // Initialize allocations if not already set
        if (consolidationDetails.getAllocations() == null) {
            consolidationDetails.setAllocations(new Allocations());
        }

        // Calculate the volume-weight-based chargeable quantity
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, sumWeight, sumVolume);

        // Update the achieved quantities with chargeable weight and unit
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());

        // Handle special case for SEA transport mode with LCL container category
        if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())) {

            // Convert weight to KG and volume to M3
            BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(),
                    consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
            BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(),
                    consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());

            // Set the charge quantity as the maximum of weight (in tonnes) or volume (in M3)
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
        }

        // Update the weight-volume ratio in the achieved quantities
        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());

        // Update the response object with allocation and achieved quantities data
        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        response.setSummaryWeight(IReport.convertToWeightNumberFormat(sumWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.convertToVolumeNumberFormat(sumVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);

        // Calculate and set the chargeable weight if applicable
        if (canSetChargableWeight(consolidationDetails)) {
            double volInM3 = convertUnit(Constants.VOLUME, sumVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, sumWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    private void calculateConsoleShipmentTeuCount(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, V1TenantSettingsResponse v1TenantSettingsResponse) {
        // Initialize the TEU (Twenty-foot Equivalent Unit) and container counters for console and shipment.
        Double consoleTeu = 0.0;
        Double shipmentTeu = 0.0;
        long consoleCont = 0L;
        long shipmentCont = 0L;

        // Check if the containers list is not null or empty before proceeding.
        if (consolidationDetails.getContainersList() != null && !consolidationDetails.getContainersList().isEmpty()) {

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

                    // Calculate TEU values if the cached entityTransferContainerType is present and has a valid TEU value.
                    if (entityTransferContainerType != null && entityTransferContainerType.getTeu() != null) {

                        // Update the console TEU count by multiplying container count with the TEU value.
                        consoleTeu += (containers.getContainerCount() * entityTransferContainerType.getTeu());

                        // Update the shipment TEU count using a separate method.
                        shipmentTeu = getShipmentTeu(containers, shipmentTeu, entityTransferContainerType);
                    }
                }
            }
        }

        // Format and set the calculated TEU and container counts in the response.
        response.setSummaryConsoleTEU(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse));
        response.setSummaryConsolContainer(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));
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

        // Check if the container category is LCL (Less than Container Load).
        if (Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())) {

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
                    UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(
                            consolidationDetails, shipmentDetails);
                    List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                    if (_utPayload != null) {
                        trackingPayloads.add(_utPayload);
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
            containerService.pushContainersToDependentServices(consolidationDetails.getContainersList(), oldEntity != null ? oldEntity.getContainersList() : null);
        }
        catch (Exception e) {
            log.error("Error producing message due to " + e.getMessage());
        }
    }

    public List<ShipmentDetails> findShipmentForTrackingService(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        if(oldEntity == null) return null;

        // check MBL / MAWB / Carrier Scac change
        if(isMasterDataChange(consolidationDetails, oldEntity)){
            return new ArrayList<>(consolidationDetails.getShipmentsList());
        }

        // check Container Number change
        return findContainerNumberChangeShipment(consolidationDetails, oldEntity);
    }

    private List<ShipmentDetails> findContainerNumberChangeShipment(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        List<Containers> containersList = consolidationDetails.getContainersList();
        List<Containers> oldContainerList = oldEntity.getContainersList();

        Map<Long, String> oldContainerMap = oldContainerList.stream()
                .collect(toMap(Containers::getId, Containers::getContainerNumber));

        Map<Long, String> containerMap = containersList.stream()
                .collect(toMap(Containers::getId, Containers::getContainerNumber));

        List<Long> containerIds = new ArrayList<>();
        for(Long id : oldContainerMap.keySet()){
            String oldContainerNumber = oldContainerMap.get(id);
            String newContainerNumber = containerMap.getOrDefault(id, null);

            if(newContainerNumber == null) continue;

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

    private void setReceivingAndTriangulationBranch(ConsolidationDetails consolidationDetails) {
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

    private void saveAwb(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) throws RunnerException {
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

    private void setInterBranchContext(Boolean isInterBranchConsole) {
        if (Boolean.TRUE.equals(isInterBranchConsole))
            commonUtils.setInterBranchContextForHub();
    }

    /**
     * Calculates achieved weight and volume values for all shipments under a consolidation, computes chargeable weight, updates allocations, and prepares summary response.
     *
     * @param consolidationId the ID of the consolidation to calculate for
     * @return {@link ShipmentGridChangeResponse} containing updated shipment summary
     * @throws RunnerException in case of data fetching or computation issues
     */
    @Override
    public ShipmentGridChangeResponse calculateAchievedValues(Long consolidationId) throws RunnerException {
        ShipmentGridChangeResponse response = new ShipmentGridChangeResponse();
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);

        // Perform the main logic for calculating achieved values and update the response
        calculateAchievedValues(consolidationDetails, response, consolidationDetails.getShipmentsList());

        return response;
    }

    /**
     * Populates the response summary fields: weight, volume, allocations, and chargeable weight.
     */
    private void updateResponseSummary(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, BigDecimal totalWeight,
            V1TenantSettingsResponse v1TenantSettingsResponse, String weightChargeableUnit, BigDecimal totalVolume, String volumeChargeableUnit) throws RunnerException {

        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        // Format and set total weight and volume
        response.setSummaryWeight(IReport.convertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.convertToVolumeNumberFormat(totalVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);

        // For LCL Sea shipments, calculate chargeable weight (max of volume and weight)
        if (canSetChargableWeight(consolidationDetails)) {
            double volInM3 = convertUnit(Constants.VOLUME, totalVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, totalWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3); // Compare in tons
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    /**
     * Computes chargeable weight/volume and updates achieved quantities and allocation units.
     */
    private void updateAllocationsAndChargeables(ConsolidationDetails consolidationDetails, String weightChargeableUnit, String volumeChargeableUnit,
            BigDecimal totalWeight, BigDecimal totalVolume) throws RunnerException {

        String transportMode = consolidationDetails.getTransportMode();

        if (consolidationDetails.getAllocations() == null) {
            consolidationDetails.setAllocations(new Allocations());
        }

        // Compute chargeable weight based on mode
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, totalWeight, totalVolume);

        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());

        // Special case for LCL sea shipments
        if (transportMode.equals(Constants.TRANSPORT_MODE_SEA)
                && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())) {

            BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(),
                    consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());

            BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(),
                    consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());

            // Max of weight in tons or volume in m3
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
        }

        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
    }

    /**
     * Sets weight and volume into AchievedQuantities section of the consolidation.
     */
    private void setAchievedQuantities(ConsolidationDetails details, BigDecimal weight, String weightUnit, BigDecimal volume, String volumeUnit) {
        if (details.getAchievedQuantities() == null) {
            details.setAchievedQuantities(new AchievedQuantities());
        }
        details.getAchievedQuantities().setConsolidatedWeight(weight);
        details.getAchievedQuantities().setConsolidatedWeightUnit(weightUnit);
        details.getAchievedQuantities().setConsolidatedVolume(volume);
        details.getAchievedQuantities().setConsolidatedVolumeUnit(volumeUnit);
    }

    /**
     * Calculates the total gross weight from all containers in the provided shipments,
     * converting each container's gross weight to the specified target unit.
     *
     * @param shipments  Set of shipment details containing containers.
     * @param targetUnit The unit to which all gross weights should be converted.
     * @return The total gross weight in the specified target unit.
     * @throws RunnerException If unit conversion fails for any container.
     */
    private BigDecimal calculateTotalGrossWeight(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        // Return zero if there are no shipments
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalGrossWeight = BigDecimal.ZERO;

        // Iterate through each shipment
        for (ShipmentDetails shipment : shipments) {
            Set<Containers> containersList = shipment.getContainersList();

            // Skip if there are no containers in the shipment
            if (containersList == null || containersList.isEmpty()) {
                continue;
            }

            // Iterate through each container in the shipment
            for (Containers container : containersList) {
                try {
                    // Convert container gross weight to target unit and add to total
                    BigDecimal convertedWeight = new BigDecimal(convertUnit(
                            Constants.MASS,
                            container.getGrossWeight(),
                            container.getGrossWeightUnit(),
                            targetUnit
                    ).toString());

                    totalGrossWeight = totalGrossWeight.add(convertedWeight);
                } catch (RunnerException e) {
                    // Log and rethrow the exception with shipment context
                    log.error("Unit conversion failed for shipmentId={}, containerUnit={}, targetUnit={}: {}",
                            shipment.getId(), container.getGrossWeightUnit(), targetUnit, e.getMessage(), e);
                    throw new RunnerException("Failed gross weight conversion for shipment ID: " + shipment.getId(), e);
                }
            }
        }

        return totalGrossWeight;
    }

    /**
     * Calculates the total gross volume from all containers in the provided shipments,
     * converting each container's gross volume to the specified target unit.
     *
     * @param shipments  Set of shipment details containing containers.
     * @param targetUnit The unit to which all gross volumes should be converted.
     * @return The total gross volume in the specified target unit.
     * @throws RunnerException If unit conversion fails for any container.
     */
    private BigDecimal calculateTotalGrossVolume(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        // Return zero if there are no shipments
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalGrossVolume = BigDecimal.ZERO;

        // Iterate through each shipment
        for (ShipmentDetails shipment : shipments) {
            Set<Containers> containersList = shipment.getContainersList();

            // Skip if there are no containers in the shipment
            if (containersList == null || containersList.isEmpty()) {
                continue;
            }

            // Iterate through each container in the shipment
            for (Containers container : containersList) {
                try {
                    // Convert container gross volume to target unit and add to total
                    BigDecimal convertedVolume = new BigDecimal(convertUnit(
                            Constants.VOLUME,
                            container.getGrossVolume(),
                            container.getGrossVolumeUnit(),
                            targetUnit
                    ).toString());

                    totalGrossVolume = totalGrossVolume.add(convertedVolume);
                } catch (RunnerException e) {
                    // Log and rethrow the exception with shipment context
                    log.error("Unit conversion failed for shipmentId={}, containerVolumeUnit={}, targetUnit={}: {}",
                            shipment.getId(), container.getGrossVolumeUnit(), targetUnit, e.getMessage(), e);
                    throw new RunnerException("Failed gross volume conversion for shipment ID: " + shipment.getId(), e);
                }
            }
        }

        return totalGrossVolume;
    }


    /**
     * Calculates total weight from all shipments converted to target unit.
     */
    private BigDecimal calculateTotalWeight(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalWeight = BigDecimal.ZERO;
        for (ShipmentDetails shipment : shipments) {
            try {
                BigDecimal weight = new BigDecimal(convertUnit(
                        Constants.MASS,
                        shipment.getWeight(),
                        shipment.getWeightUnit(),
                        targetUnit
                ).toString());
                totalWeight = totalWeight.add(weight);
            } catch (RunnerException e) {
                log.error("Failed to convert weight unit for shipment ID: {}. Source unit: {}, Target unit: {}. Reason: {}",
                        shipment.getId(), shipment.getWeightUnit(), targetUnit, e.getMessage(), e);
                throw new RunnerException("Error converting weight unit for shipment ID: " + shipment.getId(), e);
            }
        }
        return totalWeight;
    }

    /**
     * Calculates total volume from all shipments converted to target unit.
     */
    private BigDecimal calculateTotalVolume(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalVolume = BigDecimal.ZERO;
        for (ShipmentDetails shipment : shipments) {
            try {
                BigDecimal volume = new BigDecimal(convertUnit(
                        Constants.VOLUME,
                        shipment.getVolume(),
                        shipment.getVolumeUnit(),
                        targetUnit
                ).toString());
                totalVolume = totalVolume.add(volume);
            } catch (RunnerException e) {
                log.error("Failed to convert volume unit for shipment ID: {}. Source unit: {}, Target unit: {}. Reason: {}",
                        shipment.getId(), shipment.getVolumeUnit(), targetUnit, e.getMessage(), e);
                throw new RunnerException("Error converting volume unit for shipment ID: " + shipment.getId(), e);
            }
        }
        return totalVolume;
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

        // Valid if LCL + SEA or any AIR transport
        return (Constants.SHIPMENT_TYPE_LCL.equals(containerCategory)
                && Constants.TRANSPORT_MODE_SEA.equals(transportMode))
                || Constants.TRANSPORT_MODE_AIR.equals(transportMode);
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
    private Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
        if (cacheMap.isEmpty()) {
            var cached = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)
                    .get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
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

    @Override
    @Transactional
    public String attachShipments(ShipmentConsoleAttachDetachV3Request request) throws RunnerException {

        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        HashSet<Long> attachedShipmentIds = new HashSet<>();
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
        setContextIfNeeded(shipmentRequestedType, consolidationDetails);

        // Validate messaging logic for air consoles
        awbDao.validateAirMessaging(consolidationId);
        log.info("Air messaging validated for consolidationId: {}", consolidationId);

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
                listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
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
        attachedShipmentIds = consoleShipmentMappingDao.assignShipments(shipmentRequestedType, consolidationId, shipmentIds,
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
        updateLinkedShipmentData(consolidationDetails, null, true);

        // If any inter-console linkage needs to be handled, process it now
        processInterConsoleAttachShipment(consolidationDetails, shipmentDetailsList);

        // Update pack utilisation if user accepts any pull or push request
        if (ShipmentRequestedType.APPROVE.equals(shipmentRequestedType)) {
            packingService.savePackUtilisationCalculationInConsole(CalculatePackUtilizationRequest.builder()
                    .consolidationId(consolidationId)
                    .shipmentIdList(shipmentIds)
                    .build()
            );
        }

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

        try {
            consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), false);
        } catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        String warning = null;
        if (!shipmentRequestedTypes.isEmpty()) {
            warning = "Template not found, please inform the region users manually";
        }
        return warning;
    }

    @NotNull
    private List<ConsoleShipmentMapping> getConsoleShipmentMappingsFromShipmentIds(List<Long> shipmentIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
        Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent();
        return consoleShipmentMappings;
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

    private ResponseEntity<IRunnerResponse> sendImportShipmentPullAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
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
                if (!checkConsolidationTypeValidation(consolidationDetails)) {
                    throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
                }
                consolidationDetailsDao.update(consolidationDetails, false, true);
            }
        }
    }

    private boolean checkConsolidationTypeValidation(ConsolidationDetails consolidationDetails) {
        return !(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && Boolean.TRUE.equals(consolidationDetails.getHazardous())
                && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && !StringUtility.isEmpty(consolidationDetails.getConsolidationType()) && !Constants.CONSOLIDATION_TYPE_AGT.equals(consolidationDetails.getConsolidationType())
                && !Constants.CONSOLIDATION_TYPE_CLD.equals(consolidationDetails.getConsolidationType()));
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

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag())) {
            return false;
        }
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    private void processInterConsoleAttachShipment(ConsolidationDetails console, List<ShipmentDetails> shipments) {
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

    private boolean isConsoleAccepted(ConsolidationDetails console) {
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
        return Boolean.TRUE.equals(console.getInterBranchConsole()) && shipments != null && !shipments.isEmpty() && Boolean.TRUE.equals(
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
    private boolean canProcessConsole(ConsolidationDetails console, ConsolidationDetails oldEntity) {
        return console != null && (oldEntity == null || !Objects.equals(console.getBol(), oldEntity.getBol()) ||
                !Objects.equals(console.getShipmentType(), oldEntity.getShipmentType()) ||
                !CollectionUtils.isEmpty(console.getRoutingsList()) ||
                !Objects.equals(console.getCarrierBookingRef(), oldEntity.getCarrierBookingRef()) ||
                !Objects.equals(console.getBookingNumber(), oldEntity.getBookingNumber()) ||
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
                                !Objects.equals(console.getCarrierDetails().getAta(), oldEntity.getCarrierDetails().getAta())
                        )) ||
                !CommonUtils.checkSameParties(console.getSendingAgent(), oldEntity.getSendingAgent()) ||
                !CommonUtils.checkSameParties(console.getReceivingAgent(), oldEntity.getReceivingAgent()));
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
            Boolean fromAttachShipment) throws RunnerException {

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
        if (canProcessConsole(console, oldConsolEntity)) {
            if (shipments == null) {
                shipments = consolidationV3Util.getShipmentsList(console.getId());
            }

            List<EventsRequest> events = new ArrayList<>();

            // Update each linked shipment and collect relevant event triggers
            for (ShipmentDetails sd : shipments) {
                updateLinkedShipments(console, oldConsolEntity, fromAttachShipment, sd, events);
            }

            // Persist updated shipment details and event logs
            shipmentV3Service.saveAll(shipments);
            eventV3Service.saveAllEvent(events);

            // Sync to external systems if triggered via attach UI
            if (Boolean.TRUE.equals(fromAttachShipment)) {
                shipmentV3Service.syncShipmentsList(shipments, StringUtility.convertToString(console.getGuid()));
            }
        }

        return shipments;
    }

    private void setBookingNumberInShipment(ConsolidationDetails console, ConsolidationDetails oldConsolEntity, Boolean fromAttachShipment, ShipmentDetails sd) {
        if (fromAttachShipment != null && fromAttachShipment) {
            if (!CommonUtils.isStringNullOrEmpty(console.getBookingNumber())) {
                sd.setBookingNumber(console.getBookingNumber());
            } else {
                sd.setBookingNumber(console.getCarrierBookingRef());
            }
        } else {
            if (CommonUtils.isStringNullOrEmpty(oldConsolEntity.getBookingNumber())) {
                sd.setBookingNumber(console.getCarrierBookingRef());
            }
        }
    }

    /**
     * Updates carrier-related details in a linked shipment from the given consolidation.
     * <p>
     * This method handles both general and transport-mode-specific fields, such as voyage, vessel, flight number, ETA/ETD, etc. If the consolidation was attached from the shipment
     * screen (`fromAttachShipment` is true), additional fields like ETA, ETD, and flight number are copied.
     *
     * @param console            The consolidation from which carrier details are sourced
     * @param shipmentDetails    The shipment to update
     */
    private void updateCarrierDetailsForLinkedShipments(ConsolidationDetails console, ShipmentDetails shipmentDetails) {
        if (console.getCarrierDetails() != null) {
            // Basic carrier detail updates from consolidation to shipment
            shipmentDetails.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(console.getCarrierDetails().getCfs());
            shipmentDetails.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAtd(console.getCarrierDetails().getAtd());
            shipmentDetails.getCarrierDetails().setAta(console.getCarrierDetails().getAta());
            shipmentDetails.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
            shipmentDetails.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
            shipmentDetails.getCarrierDetails().setOrigin(console.getCarrierDetails().getOrigin());
            shipmentDetails.getCarrierDetails().setDestination(console.getCarrierDetails().getDestination());

            // If transport mode is air, update air-specific fields like flight number
            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
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
            Boolean fromAttachShipment,
            ShipmentDetails shipmentDetails,
            List<EventsRequest> events) throws RunnerException {

        // Update basic references in the shipment from the console
        shipmentDetails.setConsolRef(console.getReferenceNumber());
        shipmentDetails.setMasterBill(console.getBol());
        shipmentDetails.setDirection(console.getShipmentType());
        shipmentDetails.setTransportMode(console.getTransportMode());
        shipmentDetails.setCoLoadCarrierName(console.getCoLoadCarrierName());
        shipmentDetails.setPartner(console.getPartner());
        shipmentDetails.setBookingAgent(console.getBookingAgent());
        shipmentDetails.setCoLoadBkgNumber(console.getCoLoadBookingReference());
        shipmentDetails.setCoLoadBlNumber(console.getCoLoadMBL());
        shipmentDetails.setShipmentType(console.getContainerCategory());
        shipmentDetails.getAdditionalDetails().setDeliveryMode(console.getDeliveryMode());

        // Set new booking number and create BOCO event if changed
        String oldBookingNumber = shipmentDetails.getBookingNumber();
        setBookingNumberInShipment(console, oldEntity, fromAttachShipment, shipmentDetails);

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

        // Update carrier details if required
        updateCarrierDetailsForLinkedShipments(console, shipmentDetails);

        // Perform CFS cut-off date validation if enabled and required
        if (consolidationV3Util.checkConsolidationEligibleForCFSValidation(console)
                && consolidationValidationV3Util.checkIfShipmentDateGreaterThanConsole(shipmentDetails.getShipmentGateInDate(), console.getCfsCutOffDate())) {
            throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
        }

        // Sync main carriage routing from console to shipment
        syncMainCarriageRoutingToShipment(console.getRoutingsList(), shipmentDetails, true);

        // Update export/import brokers if inter-branch logic applies
        updateInterBranchConsoleData(console, shipmentDetails);
    }

    private boolean isDesiredShipmenTypeForReverseSyncFromConsol(ShipmentDetails shipmentDetails) {
        boolean isDesiredShipmenTypeForReverseSyncFromConsol = false;
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled())
                && Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())
                && ((shipmentDetails.getShipmentType().equals("HSE") && Boolean.FALSE.equals(shipmentDetails.getB2b()))
                || shipmentDetails.getShipmentType().equals("BCN")
                || shipmentDetails.getShipmentType().equals("SCN"))) {
            isDesiredShipmenTypeForReverseSyncFromConsol = true;
        }
        return isDesiredShipmenTypeForReverseSyncFromConsol;
    }

    /**
     * Updates the export and import broker information in the shipment details based on the consolidation's sending and receiving agents, but only if the consolidation is **not**
     * marked as an inter-branch console.
     * <p>
     * Logic: - If inter-branch console is false or null: - Set export broker in shipment from sending agent if different - Set import broker in shipment from receiving agent if
     * different - If `AdditionalDetails` is null, initializes it before setting brokers
     *
     * @param console The consolidation object containing sending and receiving agents
     * @param sd       The shipment details to update with broker info
     */
    private void updateInterBranchConsoleData(ConsolidationDetails console, ShipmentDetails sd) {
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
     * @param consolidationRoutings           List of routing legs from consolidation.
     * @param shipmentDetails                 Shipment entity to which the routings should be applied.
     * @param saveRoutes                      Flag to persist the routings to DB
     * @throws RunnerException If any failure occurs during processing.
     */
    @Override
    public void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes) throws RunnerException {

        // Exit early if no consolidation routings or route master feature is disabled
        if (CollectionUtils.isEmpty(consolidationRoutings)
                || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            return;
        }

        List<Routings> shipmentMainCarriageRouting = new ArrayList<>();
        List<Routings> shipmentRoutingList = Optional.ofNullable(shipmentDetails.getRoutingsList()).orElse(new ArrayList<>());
        shipmentDetails.setRoutingsList(shipmentRoutingList);
        List<Routings> existingOriginalShipmentMainCarriageRoutings = new ArrayList<>();

        // Determine if routing sync is required based on shipment type and settings
        boolean isDesiredShipmenTypeForReverseSyncFromConsol = isDesiredShipmenTypeForReverseSyncFromConsol(shipmentDetails);
        if (isDesiredShipmenTypeForReverseSyncFromConsol) {
            // Preserve original MAIN_CARRIAGE legs from shipment that were not inherited from consolidation
            shipmentRoutingList.stream()
                    .filter(shipmentRouting -> RoutingCarriage.MAIN_CARRIAGE.equals(shipmentRouting.getCarriage()) && Boolean.FALSE.equals(
                            shipmentRouting.getInheritedFromConsolidation()))
                    .forEach(existingOriginalShipmentMainCarriageRoutings::add);
        }

        consolidationRoutings.stream()
                .filter(consolRoutings -> RoutingCarriage.MAIN_CARRIAGE.equals(consolRoutings.getCarriage()))
                .forEach(consolRoute -> {
                    // Look for this POL POD main carriage routing in shipment routings list
                    // update/create

                    // Deep copy of the routing object
                    var syncedRoute = jsonHelper.convertCreateValue(consolRoute, Routings.class);
                    syncedRoute.setConsolidationId(null);
                    syncedRoute.setShipmentId(shipmentDetails.getId());
                    syncedRoute.setBookingId(null);
                    syncedRoute.setInheritedFromConsolidation(true); // Mark as inherited
                    shipmentMainCarriageRouting.add(syncedRoute);
                });

        // Add back the original MAIN_CARRIAGE entries that weren't inherited
        shipmentMainCarriageRouting.addAll(existingOriginalShipmentMainCarriageRoutings);

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

        if (saveRoutes) {
            // Persist updated routings to database
            routingsDao.updateEntityFromShipment(finalShipmentRouteList, shipmentDetails.getId());
        }

        // Assign routing list to shipment routing
        shipmentDetails.setRoutingsList(finalShipmentRouteList);
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
     * Updates packing and event records with consolidation ID if applicable.
     * <p>
     * - For AIR mode shipments, sets consolidation ID on all packing entries. - Sets consolidation ID on qualifying events based on transport mode.
     *
     * @param consolidationId the ID of the consolidation
     * @param shipmentDetails the shipment whose packing and events are to be processed
     */
    private void processConsolePackingAndEvents(Long consolidationId, ShipmentDetails shipmentDetails) {

        packingV3Service.processPacksAfterShipmentAttachment(consolidationId, shipmentDetails);
        eventV3Service.processEventsAfterShipmentAttachment(consolidationId, shipmentDetails);
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
    private void setContextIfNeeded(ShipmentRequestedType shipmentRequestedType, ConsolidationDetails consolidationDetails) {
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
    private ConsolidationDetails fetchConsolidationDetails(Long consolidationId) {
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if (consol.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ConsolidationDetails consolidationDetails = consol.get();
        return consolidationDetails;
    }

    private Optional<ConsolidationDetails> retrieveForNte(Long id) throws RunnerException, AuthenticationException {
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
        calculateAchievedValuesForRetrieve(consoleDetails);
        ConsolidationDetailsV3Response response = jsonHelper.convertValue(consoleDetails, ConsolidationDetailsV3Response.class);
        if(!Objects.equals(source, NETWORK_TRANSFER))
            setPendingActionCountInResponse(consoleDetails, response);
        createConsolidationPayload(consoleDetails, response);

        return response;
    }

    private void setPendingActionCountInResponse(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response response) {
        var map = consoleShipmentMappingDao.pendingStateCountBasedOnConsolidation(Arrays.asList(consolidationDetails.getId()), ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED.ordinal());
        var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(Arrays.asList(consolidationDetails.getId()), CONSOLIDATION);
        int pendingCount = map.getOrDefault(consolidationDetails.getId(), 0) + notificationMap.getOrDefault(consolidationDetails.getId(), 0);
        response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
    }

    private void calculateAchievedValuesForRetrieve(ConsolidationDetails consolidationDetails) {
        try {
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), consolidationDetails.getShipmentsList());
        } catch (Exception e) {
            log.error("Error while calculating achieved values for Consolidation with Id " + consolidationDetails.getId());
        }
    }

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsV3Response consolidationDetailsResponse) {
        try {
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                consolidationDetailsResponse.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
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
            // fetch NTE status
            this.fetchNTEstatusForReceivingBranch(consolidationDetailsResponse);
        }  catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, ex.getLocalizedMessage());
        }
    }

    private void fetchNTEstatusForReceivingBranch(ConsolidationDetailsV3Response consolidationDetailsResponse) {
        if(consolidationDetailsResponse.getReceivingBranch() != null) {
            String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(consolidationDetailsResponse.getId(), CONSOLIDATION, consolidationDetailsResponse.getReceivingBranch().intValue());
            consolidationDetailsResponse.setTransferStatus(transferStatus);
        }
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
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllVesselDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        var organizationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllOrganizationDataInSingleCall(consolidationDetailsV3Response, response)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, vesselDataFuture, organizationsFuture).join();

        return response;
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            if(masterDataResponse != null) {
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

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
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

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> currencyList = new HashSet<>(masterDataUtils.createInBulkCurrencyRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferCurrency> v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
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

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> wareHouseTypes = new HashSet<>(masterDataUtils.createInBulkWareHouseRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, WareHouseResponse> v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
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

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));

            processMasterDataResponse(consolidationDetailsResponse, masterDataResponse, listRequests, fieldNameKeyMap, cacheMap);

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

    private void processMasterDataResponse(ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse, Set<MasterListRequest> listRequests, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        if(masterDataResponse != null) {
            if (!Objects.isNull(consolidationDetailsResponse.getAllocations()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAllocations(), Allocations.class, fieldNameKeyMap, Allocations.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(consolidationDetailsResponse.getAchievedQuantities()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAchievedQuantities(), AchievedQuantities.class, fieldNameKeyMap, AchievedQuantities.class.getSimpleName(), cacheMap));
            if(!Objects.isNull(consolidationDetailsResponse.getReferenceNumbersList()))
                consolidationDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId(), cacheMap)));
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
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

    private CompletableFuture<Map<String, TenantModel>> addAllTenantDataInSingleCall (ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName(), cacheMap));

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.TENANTS, cacheMap));
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

    private CompletableFuture<Map<String, EntityTransferVessels>> addAllVesselDataInSingleCall(ConsolidationDetailsV3Response consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, cacheMap));
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

        if (request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty()) {
            request.setIncludeColumns(List.of("transportMode", "hazardous", "guid", "receivingBranch", "triangulationPartnerList", "tenantId",
                    "consolidationNumber", "interBranchConsole"));
        }
        Set<String> includeColumns = new HashSet<>(request.getIncludeColumns());
        CommonUtils.includeRequiredColumns(includeColumns);

        ConsolidationPendingNotificationResponse consolidationDetailsResponse = (ConsolidationPendingNotificationResponse) commonUtils.setIncludedFieldsToResponse(optionalConsolidationDetails.get(), includeColumns, new ConsolidationPendingNotificationResponse());

        Map<String, Object> response = new HashMap<>();
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetailsResponse, null)), executorService);
        tenantDataFuture.join();
        if(response.get("Tenants") != null)
            consolidationDetailsResponse.setTenantMasterData((Map<String, Object>) response.get("Tenants"));
        return consolidationDetailsResponse;
    }

    @Override
    public ConsolidationListV3Response list(ListCommonRequest request, boolean getMasterData) {
        if (request == null) {
            log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
        }
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
            containerCountUpdate(consolidationDetails, res, shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer());
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
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture =CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, containerTeuData, vesselDataFuture, tenantDataFuture).join();
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
            if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
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

        consolListRequest = CommonUtils.andCriteria("transportMode", request.getTransportMode(), "=", consolListRequest);
        if (!Objects.isNull(request.getDirection()))
            consolListRequest = CommonUtils.andCriteria(Constants.SHIPMENT_TYPE, request.getDirection(), "=", consolListRequest);
        if (request.getShipId() != null) {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentIdAll(request.getShipId());
            List<Long> excludeConsolidation = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getConsolidationId).toList();
            if (excludeConsolidation != null && !excludeConsolidation.isEmpty())
                consolListRequest = CommonUtils.andCriteria("id", excludeConsolidation, "NOTIN", consolListRequest);
        }
        if (request.getBranchIds() != null) {
            consolListRequest = CommonUtils.andCriteria("tenantId", request.getBranchIds(), "IN", consolListRequest);
        }

        return this.list(consolListRequest, true);
    }

    @Override
    public String detachShipments(ShipmentConsoleAttachDetachV3Request request) throws RunnerException {
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(request.getConsolidationId());
        if(consol.isPresent())
            return detachShipmentsHelper(consol.get(), request.getShipmentIds(), request.getRemarks());
        return "Consol is null";
    }

    public String detachShipmentsHelper(ConsolidationDetails consol, Set<Long> shipmentIds, String remarks) throws RunnerException {
        List<ShipmentDetails> shipmentDetails = fetchAndValidateShipments(shipmentIds);
        validateShipmentDetachment(shipmentDetails);

        if (Boolean.TRUE.equals(consol.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
        }

        boolean saveSeaPacks = false;
        Long consolidationId = consol.getId();
        List<Packing> packingList = new ArrayList<>();
        List<Long> shipmentIdList = new ArrayList<>(shipmentIds);
        List<ShipmentDetails> shipmentDetailsToSave = new ArrayList<>();
        if(consolidationId != null && shipmentIds!= null && !shipmentIds.isEmpty()) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIdList);
            Map<Long, ShipmentDetails> shipmentDetailsMap = getShipmentDetailsMap(shipmentDetails);
            List<Containers> allContainersList = new ArrayList<>();
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetail = shipmentDetailsMap.get(shipId);
                saveSeaPacks = isSaveSeaPacks(shipmentDetail, packingList, saveSeaPacks, allContainersList);
                shipmentsContainersMappingDao.detachListShipments(allContainersList.stream().map(Containers::getId).toList(), removedShipmentIds, false);
                containerDao.saveAll(allContainersList);
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerService.afterSaveList(allContainersList, false)), executorService);

                packingList = getPackingList(shipmentDetail, packingList);
                setEventsList(shipmentDetail);
                shipmentDetail.setConsolRef(null);
                shipmentDetail.setMasterBill(null);
                this.createLogHistoryForShipment(shipmentDetail);
            }
            if(saveSeaPacks)
                packingList = packingDao.saveAll(packingList);
            shipmentDetailsToSave = shipmentDetailsMap.values().stream().toList();
            shipmentDao.saveAll(shipmentDetailsToSave);
            if(shipmentDetailsToSave!=null){
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> updateShipmentDataInPlatform(shipmentIds)), executorService);
            }
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
        syncPackingConsoleAndShipment(consol, packingList, shipmentDetailsToSave);
        String warning = null;
        if(!shipmentRequestedTypes.isEmpty()) {
            warning = "Mail Template not found, please inform the region users individually";
        }
        processInterConsoleDetachShipment(consol, shipmentIdList);
        return warning;
    }

    private void processInterConsoleDetachShipment(ConsolidationDetails console, List<Long> shipmentIds){
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

    private void syncPackingConsoleAndShipment(ConsolidationDetails consol, List<Packing> packingList, List<ShipmentDetails> shipmentDetailsToSave) {
        String transactionId = consol.getGuid().toString();
        if(packingList != null) {
            packingADSync(packingList, transactionId);
        }
        try {
            consolidationSync.sync(consol, transactionId, false);
        } catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        syncShipmentsList(shipmentDetailsToSave, transactionId);
    }

    private void packingADSync(List<Packing> packingList, String transactionId) {
        try {
            packingsADSync.sync(packingList, transactionId);
        } catch (Exception e) {
            log.error(SyncingConstants.ERROR_SYNCING_PACKS);
        }
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
    private void updateShipmentDataInPlatform(Set<Long> shipmentIds){
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
        if(shipmentDetail.getEventsList() != null) {
            var eventsList = shipmentDetail.getEventsList();
            for(Events event : eventsList) {
                event.setConsolidationId(null);
            }
            eventDao.saveAll(eventsList);
        }
    }

    private List<Packing> getPackingList(ShipmentDetails shipmentDetail, List<Packing> packingList) {
        if (shipmentDetail.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetail.getPackingList() != null) {
            packingList = shipmentDetail.getPackingList();
            for (Packing packing : packingList) {
                packing.setConsolidationId(null);
            }
            packingList = packingDao.saveAll(packingList);
        }
        return packingList;
    }

    private boolean isSaveSeaPacks(ShipmentDetails shipmentDetail, List<Packing> packingList, boolean saveSeaPacks, List<Containers> allContainersList) throws RunnerException {
        if(shipmentDetail.getContainersList() != null) {
            List<Containers> containersList = new ArrayList<>(shipmentDetail.getContainersList());
            Map<Long, List<Packing>> containerPacksMap = new HashMap<>();
            if(isSeaPackingList(shipmentDetail)) {
                //TODO : Unassign packing logic clarfication
                for(Packing packing: shipmentDetail.getPackingList()) {
                    if(packing.getContainerId() != null) {
                        updateContainerPacksMap(packing, containerPacksMap);
                        packing.setContainerId(null);
                        packingList.add(packing);
                        saveSeaPacks = true;
                    }
                }
            }
            for(Containers container : containersList) {
                processContainersListForShipment(container, shipmentDetail, containerPacksMap);
            }
            allContainersList.addAll(containersList);
        }
        return saveSeaPacks;
    }

    private void processContainersListForShipment(Containers container, ShipmentDetails shipmentDetail, Map<Long, List<Packing>> containerPacksMap) throws RunnerException {
        //TODO : verify logic
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

    private void updateContainerPacksMap(Packing packing, Map<Long, List<Packing>> containerPacksMap) {
        if(containerPacksMap.containsKey(packing.getContainerId()))
            containerPacksMap.get(packing.getContainerId()).add(packing);
        else
            containerPacksMap.put(packing.getContainerId(), new ArrayList<>(Collections.singletonList(packing)));
    }

    private boolean isSeaPackingList(ShipmentDetails shipmentDetail) {
        return Constants.TRANSPORT_MODE_SEA.equals(shipmentDetail.getTransportMode()) && !listIsNullOrEmpty(shipmentDetail.getPackingList());
    }

    private Map<Long, ShipmentDetails> getShipmentDetailsMap(List<ShipmentDetails> shipmentDetails) throws RunnerException {
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            if(Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails1.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails1.getContainsHazardous()) &&
                (OceanDGStatus.OCEAN_DG_REQUESTED.equals(shipmentDetails1.getOceanDGStatus()) || OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED.equals(shipmentDetails1.getOceanDGStatus())))
                throw new RunnerException("Shipment " + shipmentDetails1.getShipmentId() + " is in " + shipmentDetails1.getOceanDGStatus() + " state, first get the required approval");
        }
        return shipmentDetailsMap;
    }

    private List<ShipmentDetails> fetchAndValidateShipments(Set<Long> shipmentIds) {
        return Optional.ofNullable(shipmentIds)
            .filter(ObjectUtils::isNotEmpty)
            .map(ids -> shipmentDao.findShipmentsByIds(ids.stream().collect(Collectors.toSet())))
            .orElse(Collections.emptyList());
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
}
