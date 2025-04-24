package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ESTIMATED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentServiceImplV3 implements IShipmentServiceV3 {

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private INotificationDao notificationDao;
    private CommonUtils commonUtils;
    private IShipmentRepository shipmentRepository;
    private IShipmentDao shipmentDao;
    private ShipmentMasterDataHelperV3 shipmentMasterDataHelper;
    private JsonHelper jsonHelper;
    private MasterDataUtils masterDataUtils;
    private ExecutorService executorService;
    private IAuditLogService auditLogService;
    private ILogsHistoryService logsHistoryService;
    private IHblDao hblDao;
    private IDateTimeChangeLogService dateTimeChangeLogService;
    private IConsolidationDetailsDao consolidationDetailsDao;
    private IConsolidationService consolidationService;
    private IPartiesDao partiesDao;
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    private IPackingDao packingDao;
    private IPackingService packingService;
    private DependentServiceHelper dependentServiceHelper;
    private IEventDao eventDao;
    private IEventService eventService;
    private IAwbDao awbDao;
    private IShipmentSync shipmentSync;
    private IConsolidationSync consolidationSync;
    private IShipmentSettingsDao shipmentSettingsDao;
    private GetNextNumberHelper getNextNumberHelper;
    private IV1Service v1Service;
    private ProductIdentifierUtility productEngine;
    private NetworkTransferV3Util networkTransferV3Util;
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    private IReferenceNumbersDao referenceNumbersDao;

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
            MasterDataUtils masterDataUtils,
            ExecutorService executorService,
            IAuditLogService auditLogService,
            ILogsHistoryService logsHistoryService,
            IDateTimeChangeLogService dateTimeChangeLogService,
            IConsolidationDetailsDao consolidationDetailsDao,
            IConsolidationService consolidationService,
            IPartiesDao partiesDao,
            BookingIntegrationsUtility bookingIntegrationsUtility,
            IPackingDao packingDao,
            IPackingService packingService,
            DependentServiceHelper dependentServiceHelper,
            IEventDao eventDao,
            IEventService eventService,
            IAwbDao awbDao,
            IShipmentSync shipmentSync,
            IConsolidationSync consolidationSync,
            IShipmentSettingsDao shipmentSettingsDao,
            GetNextNumberHelper getNextNumberHelper,
            IV1Service v1Service,
            ProductIdentifierUtility productEngine,
            NetworkTransferV3Util networkTransferV3Util,
            ITruckDriverDetailsDao truckDriverDetailsDao,
            IReferenceNumbersDao referenceNumbersDao
    ) {
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.notificationDao = notificationDao;
        this.commonUtils = commonUtils;
        this.shipmentRepository = shipmentRepository;
        this.shipmentDao = shipmentDao;
        this.shipmentMasterDataHelper = shipmentMasterDataHelper;
        this.jsonHelper = jsonHelper;
        this.hblDao = hblDao;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.auditLogService = auditLogService;
        this.logsHistoryService = logsHistoryService;
        this.dateTimeChangeLogService = dateTimeChangeLogService;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.consolidationService = consolidationService;
        this.partiesDao = partiesDao;
        this.bookingIntegrationsUtility = bookingIntegrationsUtility;
        this.packingDao = packingDao;
        this.packingService = packingService;
        this.dependentServiceHelper = dependentServiceHelper;
        this.eventDao = eventDao;
        this.eventService = eventService;
        this.awbDao = awbDao;
        this.shipmentSync = shipmentSync;
        this.consolidationSync = consolidationSync;
        this.shipmentSettingsDao = shipmentSettingsDao;
        this.getNextNumberHelper = getNextNumberHelper;
        this.v1Service = v1Service;
        this.productEngine = productEngine;
        this.networkTransferV3Util = networkTransferV3Util;
        this.truckDriverDetailsDao = truckDriverDetailsDao;
        this.referenceNumbersDao = referenceNumbersDao;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {

        Integer tenantId = TenantContext.getCurrentTenant();
        Integer count = consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal(), tenantId);
        count += notificationDao.findAllPendingNotificationCount(SHIPMENT, tenantId);

        return ResponseHelper.buildSuccessResponse(NotificationCount.builder().count(count).build());
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
            if(Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                        PageRequest.of(Math.max(0,listCommonRequest.getPageNo()-1), listCommonRequest.getPageSize()));

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

            if(!Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
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
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) {
        return null;
    }

    @Override
    @Transactional
    public ShipmentDetailsResponse create(CommonRequestModel commonRequestModel) {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(request);
        return this.createShipment(request, false, false);
    }

    private ShipmentDetailsResponse createShipment(ShipmentRequest request, boolean includeGuid, boolean isFromET) {
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

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentId() == null){
            shipmentDetails.setShipmentId(generateShipmentId(shipmentDetails));
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
    }

    private String generateShipmentId(ShipmentDetails shipmentDetails) {
        Optional<ShipmentSettingsDetails> shipmentSettingsOptional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
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
                if(shipmentSettingsOptional.isPresent() && Boolean.TRUE.equals(shipmentSettingsOptional.get().getCustomisedSequence())) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        log.error("Execption during common sequence {}", ignored.getMessage());
                        log.error("Execption occurred for common sequence {}", ignored.getStackTrace());
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                shipmentId = getShipmentId(shipmentId, shipmentSettingsOptional);
            }
        }
        return shipmentId;
    }

    private String getCustomizedShipmentProcessNumber(ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts();
        // to check the commmon sequence
        var sequenceNumber = productEngine.getCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.identifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null) {
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null) {
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null) {
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

    private String getShipmentId(String shipmentId, Optional<ShipmentSettingsDetails> shipmentSettingsOptional) {
        if(StringUtility.isEmpty(shipmentId)) {
            if(shipmentSettingsOptional.isPresent()) {
                log.info("CR-ID {} || no common sequence found and shipment settings data is: {}",
                        LoggerHelper.getRequestIdFromMDC(),
                        jsonHelper.convertToJson(shipmentSettingsOptional.get()));
            }
            log.info("CR-ID {} || no common sequence found", LoggerHelper.getRequestIdFromMDC());
            shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
        }
        return shipmentId;
    }

    @Override
    @Transactional
    public ShipmentDetailsResponse completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {
        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();
        this.setColoadingStation(shipmentRequest);
        return completeUpdateShipment(shipmentRequest, false);
    }

    private ShipmentDetailsResponse completeUpdateShipment(ShipmentRequest shipmentRequest, boolean isFromET) throws RunnerException {
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
            // TODO: check how to skip many to many fields
            ShipmentDetails oldConvertedShipment = jsonHelper.convertValue(oldEntity.get(), ShipmentDetails.class);
            log.info("{} | completeUpdateShipment object mapper old entity.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);

            if(Objects.equals(Constants.SHIPMENT_TYPE_DRT, entity.getJobType()) && !Objects.equals(oldEntity.get().getJobType(), entity.getJobType()) &&  checkIfAlreadyPushRequested(oldEntity.get())) {
                throw new ValidationException(ErrorConstants.VALIDATE_JOB_TYPE_CHANGE);
            }
            mid = System.currentTimeMillis();
            boolean syncConsole = false;
            beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, false);
            log.info("{} | completeUpdateShipment before save.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - mid);
            // TODO: need to remove many to many fields here before update
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
            ShipmentDetails finalEntity1 = entity;
            String entityPayload = jsonHelper.convertToJson(finalEntity1);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.createLogHistoryForShipment(entityPayload, finalEntity1.getId(), finalEntity1.getGuid())), executorServiceMasterData);
            log.info("end completeUpdateShipment.... {} ms", System.currentTimeMillis() - start);
            return jsonHelper.convertValue(entity, ShipmentDetailsResponse.class);
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

    public void createLogHistoryForShipment(String entityPayload, Long id, UUID guid){
        try {
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(id)
                    .entityType(Constants.SHIPMENT).entityGuid(guid).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Shipment: " + ex.getMessage());
        }
    }

    private void setColoadingStation(ShipmentRequest request) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request) throws RunnerException {
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
    private void beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean isImportFile) throws RunnerException{
        validateStaleShipmentUpdateError(shipmentDetails, isCreate);

        /* Future to populate unloc code in shipment child entities*/
        CompletableFuture<Void> populateUnlocCodeFuture = getPopulateUnlocCodeFuture(shipmentDetails, oldEntity);

        processVoyageAndFlightNumber(shipmentDetails);

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        Set<ConsolidationDetails> consolidationDetails = oldEntity != null ? oldEntity.getConsolidationList() : new HashSet<>();
        processDGValidations(shipmentDetails, oldEntity, consolidationDetails);

        if(Boolean.TRUE.equals(shipmentRequest.getIsChargableEditable())) {
            shipmentDetails.setChargable(shipmentRequest.getChargable());
        }
        validateBeforeSave(shipmentDetails);

        processBranchesAndPartner(shipmentDetails);

        if(Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getDraftPrinted())
                && Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentDetails.getId());
            if(!hbls.isEmpty()) {
                hblDao.delete(hbls.get(0));
            }
            shipmentDetails.getAdditionalDetails().setDraftPrinted(false);
        }
        if(checkOriginalPrintedForJobTypeChange(shipmentDetails, oldEntity)) {
            throw new ValidationException("Consolidation type cannot be changed as the original BL has been generated for this shipment.");
        }
        updateAwbForDisableFetchConditionForAwb(shipmentDetails, oldEntity, shipmentSettingsDetails);

        var tenantSettings = Optional.ofNullable(commonUtils.getCurrentTenantSettings()).orElse(V1TenantSettingsResponse.builder().build());
        // If TransportModeConfig flag is ON, this block will check for the valid transport mode
        validTransportModeForTrasnportModeConfig(shipmentDetails, oldEntity, isCreate, isImportFile, tenantSettings);

        // Ignore events payload to avoid transaction issues bypassing shipmentDetailsDao.update(...);
        // Update happens in after save from request body
        shipmentDetails.setEventsList(null);

        populateUnlocCodeFuture.join();
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, boolean isFromET) throws RunnerException {
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
        }
        log.info("shipment afterSave isCreate .... ");
        shipmentRequest.setId(id);
        dateTimeChangeLogService.createEntryFromShipment(shipmentRequest, oldEntity);
        log.info("shipment afterSave dateTimeChangeLogService .... ");
        ConsolidationDetails consolidationDetails = updateLinkedShipmentData(shipmentDetails, oldEntity, shipmentRequest);
        log.info("shipment afterSave updateLinkedShipmentData.... ");
        if(!Objects.isNull(consolidationDetails)) {
            shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails)));
            syncConsole = true;
        }
        // TODO: Need to check if it can be moved to EventsUtils (update request to entity)
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

        pushShipmentDataToDependentService(shipmentDetails, oldEntity, isCreate, shipmentRequest, isFromET);

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()){
            consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        }

        // Delete the shipment pending pull/push request tasks when the shipment got cancelled
        deletePendingStateAfterCancellation(shipmentDetails, oldEntity);
        log.info("shipment afterSave consoleShipmentMappingDao.deletePendingStateByShipmentId..... ");
        processSyncV1AndAsyncFunctions(shipmentDetails, oldEntity, shipmentSettingsDetails, syncConsole, consolidationDetails);
        log.info("shipment afterSave end..... ");
    }

    private void processSyncV1AndAsyncFunctions(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole, ConsolidationDetails consolidationDetails) {
        // Syncing shipment to V1
        syncShipment(shipmentDetails, consolidationDetails, syncConsole);
        log.info("shipment afterSave syncShipment..... ");
        if (commonUtils.getCurrentTenantSettings().getP100Branch() != null && commonUtils.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity)), executorService);
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails, oldEntity, false)), executorService);
    }

    private void syncShipment(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, boolean syncConsole) {
        String transactionId = shipmentDetails.getGuid().toString();
        try {
            shipmentSync.sync(shipmentDetails, null, null, transactionId, false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        if(syncConsole && consolidationDetails != null) {
            try {
                consolidationSync.sync(consolidationDetails, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
    }

    private void deletePendingStateAfterCancellation(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()) && Objects.nonNull(oldEntity)
                && !Objects.equals(oldEntity.getStatus(), shipmentDetails.getStatus()) && Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            log.info("Request: {} | Deleting console_shipment_mapping due to shipment cancelled for shipment: {}", LoggerHelper.getRequestIdFromMDC(), shipmentDetails.getShipmentId());
            consoleShipmentMappingDao.deletePendingStateByShipmentId(shipmentDetails.getId());
        }
    }

    private void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentRequest shipmentRequest, boolean isFromET) {
        if(!isFromET) {
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, isCreate, Boolean.TRUE.equals(shipmentRequest.getIsAutoSellRequired()), Optional.ofNullable(oldEntity).map(ShipmentDetails::getContainersList).orElse(null));
            log.info("shipment afterSave pushShipmentDataToDependentService..... ");
        }
    }

    private void storeMblAudit(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (StringUtils.isNotBlank(shipmentDetails.getMasterBill())) {
            List<ConsolidationDetailsProjection> consolidations = consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill());

            consolidations.forEach(consolidation -> {
                try {
                    if(ObjectUtils.isEmpty(oldEntity) || ObjectUtils.notEqual(oldEntity.getMasterBill(), shipmentDetails.getMasterBill())) {
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

    private ConsolidationDetails changeConsolidationDGValues(boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, Set<ConsolidationDetails> consolidationList, ShipmentDetails shipment) {
        if(!setIsNullOrEmpty(consolidationList)) {
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList.iterator().next().getId(), shipment, null);
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
            List<ShipmentDetails> shipmentDetails = shipmentDao.findByShipmentIdInAndContainsHazardous(shipmentIdList, true);
            if(!CollectionUtils.isEmpty(shipmentDetails))
                return false;
        }
        return true;
    }

    public ConsolidationDetails saveConsolidationDGValue(boolean dgFlag, ConsolidationDetails consolidationDetails) {
        if( (!Boolean.TRUE.equals(consolidationDetails.getHazardous()) && dgFlag)
                || (!dgFlag && Boolean.TRUE.equals(consolidationDetails.getHazardous())) ) {
            consolidationDetails.setHazardous(dgFlag);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, dgFlag);
            return consolidationDetails;
        }
        return null;
    }

    private void processEventsInAfterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, List<Events> eventsList, Long id, Integer previousStatus) throws RunnerException {
        if (eventsList != null) {
            eventsList = setEventDetails(eventsList, shipmentDetails);
            eventsList = createOrUpdateEvents(shipmentDetails, oldEntity, eventsList, isCreate);
            if (eventsList != null) {
                commonUtils.updateEventWithMasterData(eventsList);
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(eventsList, id, Constants.SHIPMENT);
                shipmentDetails.setEventsList(updatedEvents);
                eventService.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
            }
        }
        log.info("shipment afterSave eventDao.updateEntityFromOtherEntity.... ");

        // create Shipment event on the bases of auto create event flag
        if(isCreate && Boolean.TRUE.equals(shipmentSettingsDetails.getAutoEventCreate()))
            autoGenerateCreateEvent(shipmentDetails);
        log.info("shipment afterSave autoGenerateCreateEvent.... ");

        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails);
        log.info("shipment afterSave generateEvents.... ");
    }

    @SuppressWarnings({"java:S1066", "java:S2583"})
    private void autoGenerateEvents(ShipmentDetails shipmentDetails) {
        Events response = null;
        if(shipmentDetails.getStatus() != null) {
            // LATER : remove this
            if(response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    private List<Events> setEventDetails(List<Events> eventsList, ShipmentDetails shipmentDetails) {
        if(eventsList != null && !eventsList.isEmpty()) {
            for (Events events : eventsList) {
                events.setShipmentNumber(shipmentDetails.getShipmentId());
            }
        }
        return eventsList;
    }

    public List<Events> createOrUpdateEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> updatedEvents, Boolean isNewShipment) {
        List<Events> newUpdatedEvents = (updatedEvents != null) ? new ArrayList<>(updatedEvents) : new ArrayList<>();

        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            // we need db events now instead of the eventslist in shipment
            newUpdatedEvents = Optional.ofNullable(oldEntity).map(ShipmentDetails::getEventsList).orElse(new ArrayList<>());
        }

        // Update the direction if blank
        newUpdatedEvents.forEach(events -> events.setDirection(events.getDirection() == null ?
                shipmentDetails.getDirection() : events.getDirection()));

        createUpdateEvent(shipmentDetails, oldEntity, newUpdatedEvents, isNewShipment);

        // Update event fields for runner events generated
        // linking specific events to consol and populating other fields
        eventDao.updateFieldsForShipmentGeneratedEvents(newUpdatedEvents, shipmentDetails);

        return newUpdatedEvents;
    }

    private void createUpdateEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment) {
        commonUtils.removeDuplicateTrackingEvents(events);
        Map<String, List<Events>> cargoesRunnerDbEvents = groupCargoesRunnerEventsByCode(events);
        oldEntity = Optional.ofNullable(oldEntity).orElse(new ShipmentDetails());
        oldEntity.setAdditionalDetails(Optional.ofNullable(oldEntity.getAdditionalDetails()).orElse(new AdditionalDetails()));

        processLclOrFclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processLclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processEMCREvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processECCCEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processBLRSEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processFNMUEvent(shipmentDetails, oldEntity, isNewShipment, cargoesRunnerDbEvents);

        processCOODEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

    }

    private Map<String, List<Events>> groupCargoesRunnerEventsByCode(List<Events> events) {
        Map<String, List<Events>> eventMap = new HashMap<>();

        for (Events event : events) {
            String key = event.getEventCode();
            if(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER.equalsIgnoreCase(event.getSource())){
                if (eventMap.containsKey(key)) {
                    // Append the event to the existing list
                    eventMap.get(key).add(event);
                } else {
                    // Create a new list and add the event
                    List<Events> eventList = new ArrayList<>();
                    eventList.add(event);
                    eventMap.put(key, eventList);
                }
            }
        }
        return eventMap;
    }

    private void processLclOrFclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrFclOrAir(shipmentDetails)) {

            processBOCOEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCADEEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCACOEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCUREEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processDOTPEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processPRDEEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processSEPUEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processBOCOEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getBookingNumber(), oldEntity.getBookingNumber(), isNewShipment) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            boolean shouldCreateBOCO = true;
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BOCO))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BOCO);
                for (Events event : dbEvents) {
                    if (Objects.equals(shipmentDetails.getBookingNumber(), event.getContainerNumber())) {
                        event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                        eventDao.updateUserFieldsInEvent(event, true);
                        shouldCreateBOCO = false;
                    }
                }
            }
            createBOCOEvent(shipmentDetails, events, shouldCreateBOCO);
        }
    }

    private void createBOCOEvent(ShipmentDetails shipmentDetails, List<Events> events, boolean shouldCreateBOCO) {
        if(Boolean.TRUE.equals(shouldCreateBOCO)) {
            Events bocoEvent = initializeAutomatedEvents(shipmentDetails, EventConstants.BOCO, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
            if (!CommonUtils.isStringNullOrEmpty(shipmentDetails.getBookingNumber()))
                bocoEvent.setContainerNumber(shipmentDetails.getBookingNumber());
            events.add(bocoEvent);
        }
    }

    private void processCADEEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate(),
                        oldEntity.getAdditionalDetails().getCargoDeliveredDate(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CADE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CADE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CADE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoDeliveredDate()), null));
            }
        }
    }

    private void processCACOEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getPickupDate(),
                        oldEntity.getAdditionalDetails().getPickupDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CACO))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CACO);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getPickupDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CACO,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getPickupDate()), null));
            }
        }
    }

    private void processCUREEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCustomReleaseDate(),
                        oldEntity.getAdditionalDetails().getCustomReleaseDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CURE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CURE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCustomReleaseDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CURE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCustomReleaseDate()), null));
            }
        }
    }

    private void processDOTPEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer(),
                        oldEntity.getAdditionalDetails().getDocTurnedOverToCustomer(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.DOTP))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.DOTP);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.DOTP,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processPRDEEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate(),
                        oldEntity.getAdditionalDetails().getProofOfDeliveryDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.PRDE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.PRDE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.PRDE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()), null));
            }
        }
    }

    private void processSEPUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getPickupByConsigneeCompleted(),
                        oldEntity.getAdditionalDetails().getPickupByConsigneeCompleted(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.SEPU))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.SEPU);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.SEPU,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processLclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrAir(shipmentDetails)) {
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate(),
                            oldEntity.getAdditionalDetails().getWarehouseCargoArrivalDate(), isNewShipment)) {
                if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAFS))){
                    List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAFS);
                    for(Events event: dbEvents){
                        event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()));
                        eventDao.updateUserFieldsInEvent(event, true);
                    }
                }else{
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAFS,
                            commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()), null));
                }
            }

            processCAAWEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processCAAWEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getShipmentGateInDate(), oldEntity.getShipmentGateInDate(), isNewShipment) &&
                shipmentDetails.getDateType()!=null) {
            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAAW))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAAW);
                processDbCaawEvent(shipmentDetails, dbEvents);
            }else{
                if(shipmentDetails.getDateType() == ACTUAL){
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()), null));
                } else if (shipmentDetails.getDateType() == ESTIMATED) {
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW, null,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate())));
                }
            }
        }
    }

    private void processDbCaawEvent(ShipmentDetails shipmentDetails, List<Events> dbEvents) {
        for(Events event: dbEvents){
            if(ACTUAL.equals(shipmentDetails.getDateType())) {
                event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }else if (ESTIMATED.equals(shipmentDetails.getDateType() )){
                event.setEstimated(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }
            eventDao.updateUserFieldsInEvent(event, true);
        }
    }

    private void processEMCREvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isFcl(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getEmptyContainerReturned(),
                        oldEntity.getAdditionalDetails().getEmptyContainerReturned(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.EMCR))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.EMCR);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.EMCR,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processECCCEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getIsExportCustomClearanceCompleted(),
                        oldEntity.getAdditionalDetails().getIsExportCustomClearanceCompleted(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.ECCC))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.ECCC);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.ECCC,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processBLRSEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getBlInstructionReceived(),
                        oldEntity.getAdditionalDetails().getBlInstructionReceived(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BLRS))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BLRS);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.BLRS,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()), null));
            }
        }
    }

    private void processFNMUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getMasterBill(), oldEntity.getMasterBill(), isNewShipment) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.FNMU))) {
            List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.FNMU);
            for (Events event : dbEvents) {
                event.setContainerNumber(shipmentDetails.getMasterBill());
                eventDao.updateUserFieldsInEvent(event, true);
            }
        }
    }

    private void processCOODEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery(),
                        oldEntity.getAdditionalDetails().getCargoOutForDelivery(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.COOD))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.COOD);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.COOD,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()), null));
            }
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

    private boolean isEventChanged(Object newValue, Object oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(isNewShipment) ? ObjectUtils.isNotEmpty(newValue) : !Objects.equals(newValue, oldValue);
    }

    private boolean isEventBooleanChanged(Boolean newValue, Boolean oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(newValue) && (Boolean.TRUE.equals(isNewShipment) || !Boolean.TRUE.equals(oldValue));
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

    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest) throws RunnerException {
        Set<ConsolidationDetails> consolidationList = oldEntity != null? oldEntity.getConsolidationList() : new HashSet<>();
        ConsolidationDetails consolidationDetails;
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var linkedConsol = (!setIsNullOrEmpty(consolidationList)) ? consolidationList.iterator().next() : null;
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)) {
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if (consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)) {
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        // TODO check if need to update utilisation -> Check with Mayank
        processPackUtilisationCalculationInConsole(shipment, oldEntity, shipmentRequest, linkedConsol);
        boolean makeConsoleDG = checkForDGShipmentAndAirDgFlag(shipment) || checkForOceanDGShipment(shipment);
        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(checkForNonDGShipmentAndAirDgFlag(shipment));
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(shipment.getAdditionalDetails() != null && Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1));
        if(linkedConsol != null && isDiffPresentInOldNewShipment(shipment, oldEntity)) {
            consolidationDetails = processLinkedConsolidationDetails(shipment, oldEntity, consolidationList, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, Boolean.TRUE.equals(shipment.getContainsHazardous()));
            return consolidationDetails;
        }
        else // only execute when above logic execution not required (i.e. saving all shipments not required)
            return changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, consolidationList, shipment);
    }

    private boolean isDiffPresentInOldNewShipment(ShipmentDetails shipment, ShipmentDetails oldEntity) {
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

    private ConsolidationDetails processLinkedConsolidationDetails(ShipmentDetails shipment, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationList, boolean makeConsoleDG, AtomicBoolean makeConsoleNonDG, AtomicBoolean makeConsoleSciT1) throws RunnerException {
        ConsolidationDetails consolidationDetails;
        consolidationDetails = consolidationDetailsDao.findById(consolidationList.iterator().next().getId()).get();
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
        setSendindAndReceivingAgentForNonInterConsole(shipment, consolidationDetails);
        Boolean interBranchConsole = consolidationDetails.getInterBranchConsole();
        List<Long> shipmentIdList = getShipmentIdsExceptCurrentShipment(consolidationList.iterator().next().getId(), shipment);
        if (!shipmentIdList.isEmpty()) {
            processShipmentIdList(shipment, shipmentIdList, interBranchConsole, makeConsoleNonDG, makeConsoleSciT1);
        }
        if(makeConsoleNonDG.get())
            consolidationDetails.setHazardous(false);
        if(makeConsoleSciT1.get() && checkConsoleSciUpdateT1(shipment, oldEntity))
            consolidationDetails.setSci(AwbConstants.T1);
        else if(Objects.equals(consolidationDetails.getSci(), AwbConstants.T1) && !makeConsoleSciT1.get() && oldEntity != null && !Objects.equals(shipment.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci()))
            consolidationDetails.setSci(null);
        return consolidationDetails;
    }

    private boolean checkConsoleSciUpdateT1(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        if(shipment.getAdditionalDetails() == null) return false;
        if(Strings.isNullOrEmpty(shipment.getAdditionalDetails().getSci())) return false;
        if(!Objects.equals(shipment.getAdditionalDetails().getSci(), AwbConstants.T1)) return false;
        return oldEntity == null || !Objects.equals(shipment.getAdditionalDetails().getSci(),oldEntity.getAdditionalDetails().getSci());
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
                    if(!Boolean.TRUE.equals(interBranchConsole)) {
                        if (shipment.getAdditionalDetails() != null && isExportOrImportBrokerPresent(shipment)) {
                            addAdditionalDetailsForShipment(shipment, i);
                        } else if(shipment.getAdditionalDetails() == null && i.getAdditionalDetails() != null) {
                            i.getAdditionalDetails().setExportBroker(null);
                            i.getAdditionalDetails().setImportBroker(null);
                        }
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

    private boolean isExportOrImportBrokerPresent(ShipmentDetails shipment) {
        return CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getExportBroker()) || CommonUtils.checkPartyNotNull(shipment.getAdditionalDetails().getImportBroker());
    }

    private void addAdditionalDetailsForShipment(ShipmentDetails shipment, ShipmentDetails i) {
        if(i.getAdditionalDetails() == null) {
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
        if(!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (shipment.getAdditionalDetails() != null) {
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getExportBroker(), consolidationDetails.getSendingAgent())) {
                    consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getExportBroker()));
                }
                if(!CommonUtils.checkSameParties(shipment.getAdditionalDetails().getImportBroker(), consolidationDetails.getReceivingAgent())) {
                    consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(shipment.getAdditionalDetails().getImportBroker()));
                }
            } else {
                consolidationDetails.setSendingAgent(null);
                consolidationDetails.setReceivingAgent(null);
            }
        }
    }

    private void processPackUtilisationCalculationInConsole(ShipmentDetails shipment, ShipmentDetails oldEntity, ShipmentRequest shipmentRequest, ConsolidationDetails linkedConsol) {
        if(linkedConsol != null && shipmentRequest != null) {
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(linkedConsol.getId())
                    .saveConsol(true)
                    .shipmentRequest(shipmentRequest).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
        else if(oldEntity != null && oldEntity.getConsolidationList() != null && !oldEntity.getConsolidationList().isEmpty()) {
            var oldConsolId = oldEntity.getConsolidationList().iterator().next().getId();
            CalculatePackUtilizationRequest utilizationRequest = CalculatePackUtilizationRequest.builder()
                    .consolidationId(oldConsolId)
                    .saveConsol(true)
                    .shipmentRequest(ShipmentRequest.builder().id(shipment.getId()).build()).build();
            packingService.savePackUtilisationCalculationInConsole(utilizationRequest);
        }
    }

    private void updateAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) throws RunnerException {
        if(checkForAwbUpdate(shipmentDetails, oldEntity)) {
            awbDao.updatedAwbInformationEvent(shipmentDetails, oldEntity);
        }
    }

    private boolean checkForAwbUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldEntity.getAdditionalDetails().getSci())) return true;
        if(!Objects.equals(shipmentDetails.getSecurityStatus(), oldEntity.getSecurityStatus())) return true;
        return !Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldEntity.getAdditionalDetails().getEfreightStatus());
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

    private void updateAwbForDisableFetchConditionForAwb(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
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

    private void processBranchesAndPartner(ShipmentDetails shipmentDetails) {
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
        if(shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);
    }

    private void validateBeforeSave(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null && shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
            throw new ValidationException("Consignor & Consignee parties can't be selected as same.");

        if(!isStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            }
            else if(!isStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }
    }

    private void validTransportModeForTrasnportModeConfig(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, boolean isCreate, boolean isImportFile, V1TenantSettingsResponse tenantSettings) {
        if (Boolean.TRUE.equals(tenantSettings.getTransportModeConfig()) && Boolean.FALSE.equals(isImportFile) && (isCreate || !Objects.equals(oldEntity.getTransportMode(), shipmentDetails.getTransportMode()))
                && Boolean.FALSE.equals(commonUtils.isTransportModeValid(shipmentDetails.getTransportMode(), Constants.SHIPMENT_DETAILS, tenantSettings))) {
            throw new ValidationException(String.format(ErrorConstants.INVALID_TRANSPORT_MODE, shipmentDetails.getTransportMode()));
        }
    }

    private void validateStaleShipmentUpdateError(ShipmentDetails shipmentDetails, boolean isCreate) {
        if(!isCreate) {
            // Check the shipment for attached consolidation, if the user is updating stale shipment and causing shipment to detach
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            if (!CollectionUtils.isEmpty(consoleShipmentMappings)) {
                consoleShipmentMappings = consoleShipmentMappings.stream().filter(i -> Boolean.TRUE.equals(i.getIsAttachmentDone())).toList();
                if (CollectionUtils.isEmpty(shipmentDetails.getConsolidationList()) && !consoleShipmentMappings.isEmpty()
                        && !Objects.isNull(consoleShipmentMappings.get(0).getRequestedType())) {
                    throw new ValidationException(ShipmentConstants.STALE_SHIPMENT_UPDATE_ERROR);
                }
            }
        }
    }

    private CompletableFuture<Void> getPopulateUnlocCodeFuture(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        CarrierDetails finalOldCarrierDetails = Optional.ofNullable(oldEntity).map(ShipmentDetails::getCarrierDetails).orElse(null);
        List<Routings> finalOldRoutings = Optional.ofNullable(oldEntity).map(ShipmentDetails::getRoutingsList).orElse(Collections.emptyList());

        /* Set to extract the unlocations from entities whose unloc code needs to be saved */
        Set<String> unlocationsSet = Collections.synchronizedSet(new HashSet<>());
        Map<String, EntityTransferUnLocations> unLocationsMap = new ConcurrentHashMap<>();

        CompletableFuture.allOf(
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(shipmentDetails.getCarrierDetails(), finalOldCarrierDetails, unlocationsSet), executorService),
                CompletableFuture.runAsync(() -> commonUtils.getChangedUnLocationFields(shipmentDetails.getRoutingsList(), finalOldRoutings, unlocationsSet), executorService)
        ).join();

        return CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(unlocationsSet, unLocationsMap)), executorService)
                .thenCompose(v -> CompletableFuture.allOf(
                        CompletableFuture.runAsync(() -> commonUtils.updateCarrierUnLocData(shipmentDetails.getCarrierDetails(), unLocationsMap), executorService),
                        CompletableFuture.runAsync(() -> commonUtils.updateRoutingUnLocData(shipmentDetails.getRoutingsList(), unLocationsMap), executorService)
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

    private void processDGValidations(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()))
            airDGValidations(shipmentDetails, consolidationDetails);
        if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (Objects.isNull(oldEntity) || !Boolean.TRUE.equals(oldEntity.getContainsHazardous())) && !setIsNullOrEmpty(consolidationDetails)) {
            ConsolidationDetails consolidationDetails1 = consolidationDetails.iterator().next();
            dgValidations(shipmentDetails, consolidationDetails1, 0);
        }
    }

    public void airDGValidations(ShipmentDetails shipmentDetails, Set<ConsolidationDetails> consolidationDetails) throws RunnerException {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && !isAirDgUser()
                && !Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()) && (!setIsNullOrEmpty(consolidationDetails) && Boolean.TRUE.equals(consolidationDetails.iterator().next().getHazardous()))) {
            throw new RunnerException("You do not have Air DG permissions to edit this as it is a part of DG Consol");
        }
    }

    // TODO: doubt check with Mayank
    public void dgValidations(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if( ((Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()) && SHIPMENT_TYPE_LCL.equals(consolidationDetails1.getContainerCategory()))
                || checkForAirDGFlag(consolidationDetails1))
                && (Boolean.TRUE.equals(consolidationDetails1.getHazardous()) || Boolean.TRUE.equals(shipmentDetails.getContainsHazardous()))) {
            List<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails1.getId());
            if(!listIsNullOrEmpty(consoleShipmentMapping) && consoleShipmentMapping.size() + isNewConsoleAttached > 1) {
                throwErrorMaxOneShipmentAllowedInDgConsolidation(consolidationDetails1, isNewConsoleAttached);
            }
        }
    }

    private void throwErrorMaxOneShipmentAllowedInDgConsolidation(ConsolidationDetails consolidationDetails1, int isNewConsoleAttached) throws RunnerException {
        if(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails1.getTransportMode()))
            throw new RunnerException("For Ocean DG Consolidation LCL Cargo Type, and can have only 1 shipment");
        else {
            if(isNewConsoleAttached == 1)
                throw new RunnerException(String.format(CAN_NOT_ATTACH_MORE_SHIPMENTS_IN_DG_CONSOL, consolidationDetails1.getConsolidationNumber()));
            else
                throw new RunnerException(CAN_NOT_UPDATE_DG_SHIPMENTS_CONSOLE_CONSISTS_MULTIPLE_SHIPMENTS);
        }
    }

    private boolean isAirDgUser() {
        return  LicenseContext.isDgAirLicense();
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()))
            return false;
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
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

    private boolean checkForOceanDGShipment(ShipmentDetails shipmentDetails) {
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) && Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDgFlag(ShipmentDetails shipment) {
        if(checkForNonAirDGFlag(shipment, commonUtils.getShipmentSettingFromContext()))
            return false;
        return !Boolean.TRUE.equals(shipment.getContainsHazardous());
    }

    private void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation)
    {
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
        }
        catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst, boolean getMasterData, List<ShipmentListResponse> shipmentListResponses, Set<String> includeColumns, Boolean notificationFlag) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        Map<Long, ShipmentDetails> shipmentDetailsMap = lst.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
        // Pending Notification Count
        if(Boolean.TRUE.equals(notificationFlag)) {
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
            if(includeColumns.contains(SHIPPER_REFERENCE))
                setShipperReferenceNumber(response, ship);
            if (includeColumns.contains(SHIPMENT_STATUS_FIELDS) && ship.getStatus() != null && ship.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[ship.getStatus()].toString());
            if(includeColumns.contains(ORDERS_COUNT) && ObjectUtils.isNotEmpty(ship.getShipmentOrders()))
                response.setOrdersCount(ship.getShipmentOrders().size());
            responseList.add(response);
        });
        shipmentMasterDataHelper.getMasterDataForList(lst, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    private void setShipperReferenceNumber(ShipmentListResponse response, ShipmentDetails ship){
        if(ship.getReferenceNumbersList() != null && !ship.getReferenceNumbersList().isEmpty()){
            Optional<String> srnReferenceNumber = ship.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equalsIgnoreCase(SRN))
                    .findFirst()
                    .map(a -> a.getReferenceNumber());

            if(srnReferenceNumber.isPresent() && response.getPickupDetails() != null){
                response.getPickupDetails().setShipperRef(srnReferenceNumber.get());
            }
        }
    }



}
