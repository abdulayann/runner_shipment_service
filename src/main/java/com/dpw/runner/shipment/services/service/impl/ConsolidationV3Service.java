package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
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
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationV3Service implements IConsolidationV3Service {

    @Autowired
    ExecutorService executorService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private ConsolidationValidationUtil consolidationValidationUtil;

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
    private IPackingDao packingDao;

    @Autowired
    private IEventDao eventDao;

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
    CacheManager cacheManager;

    @Autowired
    CustomKeyGenerator keyGenerator;

    @Override
    @Transactional
    public ConsolidationDetailsResponse create(ConsolidationDetailsRequest request) {
        return this.createConsolidation(request, false, false);
    }

    private ConsolidationDetailsResponse createConsolidation(ConsolidationDetailsRequest request, boolean includeGuid, boolean isFromET) {
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
    public ConsolidationDetailsResponse completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        ConsolidationDetailsResponse response = this.completeUpdateConsolidation(consolidationDetailsRequest, false);

        return response;
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

            afterSave(entity, oldConvertedConsolidation, consolidationDetailsRequest, false, shipmentSettingsDetails, false, isFromET);
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
    public ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException {
        return this.completeUpdateConsolidation(consolidationDetailsRequest, true);
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
        List<ShipmentDetails> shipmentDetails = null;
        if(!isCreate){
            // This method will only work for non air transport modes , validation check moved inside the method
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), oldEntity.getShipmentsList());
            shipmentDetails = updateLinkedShipmentData(consolidationDetails, oldEntity, false);
        }
        checkCFSValidation(consolidationDetails, isCreate, shipmentDetails);

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

        this.checkInterBranchPermission(consolidationDetails, oldEntity);

        populateUnlocCodeFuture.join();
    }

    void getConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException{
        generateConsolidationNumber(consolidationDetails);
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false, false);

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

    private void afterSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ConsolidationDetailsRequest consolidationDetailsRequest, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean isFromET) throws RunnerException{
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

        processRequestLists(consolidationDetails, isCreate, isFromBooking, referenceNumbersRequestList, id, truckDriverDetailsRequestList, routingsRequestList);
        if (consolidationAddressRequest != null) {
            List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedFileRepos);
        }

        if(!isFromET) {
            pushShipmentDataToDependentService(consolidationDetails, isCreate, oldEntity);
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

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consolidationDetails, oldEntity)), executorService);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(consolidationDetails, oldEntity, false)), executorService);
    }

    private boolean checkForAwbUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) return false;
        if(!Objects.equals(consolidationDetails.getSci(), oldEntity.getSci())) return true;
        return !Objects.equals(consolidationDetails.getEfreightStatus(), oldEntity.getEfreightStatus());
    }

    private void processRequestLists(ConsolidationDetails consolidationDetails, Boolean isCreate, Boolean isFromBooking, List<ReferenceNumbersRequest> referenceNumbersRequestList, Long id, List<TruckDriverDetailsRequest> truckDriverDetailsRequestList, List<RoutingsRequest> routingsRequestList) throws RunnerException {
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
        String maxId = v1Service.getMaxConsolidationId();
//        Long maxId = consolidationDetailsDao.findMaxId();
//        if(maxId == null)
//            maxId = 0L;
//        maxId += 1;
        return maxId;
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

    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, Set<ShipmentDetails> shipmentDetailsList) throws Exception {
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
        calculateConsoleShipmentTeuCount(consolidationDetails, response, v1TenantSettingsResponse);

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
        if(canSetChargableWeight(consolidationDetails)) {
            double volInM3 = convertUnit(Constants.VOLUME, sumVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, sumWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    private void calculateConsoleShipmentTeuCount(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, V1TenantSettingsResponse v1TenantSettingsResponse) {
        Double consoleTeu = 0.0;
        Double shipmentTeu = 0.0;
        long consoleCont = 0l;
        long shipmentCont = 0l;
        if(consolidationDetails.getContainersList() != null && consolidationDetails.getContainersList().size() > 0) {
            Map<String, Object> cacheMap = new HashMap<String, Object>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            processCacheAndContainerResponseList(consolidationDetails, containerTypes, fieldNameKeyMap, cacheMap);

            for(Containers containers : consolidationDetails.getContainersList()) {
                Object cache = getEntityTransferObjectCache(containers, cacheMap);
                EntityTransferContainerType object = (EntityTransferContainerType) cache;
                if(containers.getContainerCount() != null) {
                    consoleCont = consoleCont + containers.getContainerCount();
                    shipmentCont = getShipmentCont(containers, shipmentCont);
                    if(object != null && object.getTeu() != null) {
                        consoleTeu = consoleTeu + (containers.getContainerCount() * object.getTeu());
                        shipmentTeu = getShipmentTeu(containers, shipmentTeu, object);
                    }
                }
            }
        }
        response.setSummaryConsoleTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse));
        response.setSummaryConsolContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));
    }

    private Double getShipmentTeu(Containers containers, Double shipmentTeu, EntityTransferContainerType object) {
        if(isShipmentListPresent(containers)) {
            shipmentTeu = shipmentTeu + (containers.getContainerCount() * object.getTeu());
        }
        return shipmentTeu;
    }

    private long getShipmentCont(Containers containers, long shipmentCont) {
        if(isShipmentListPresent(containers)) {
            shipmentCont = shipmentCont + containers.getContainerCount();
        }
        return shipmentCont;
    }

    private boolean isShipmentListPresent(Containers containers) {
        return containers.getShipmentsList() != null && containers.getShipmentsList().size() > 0;
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
                .collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));

        Map<Long, String> containerMap = containersList.stream()
                .collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));

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

    private Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
        Object cache = null;
        if(cacheMap.isEmpty()) {
            var resp = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
            if(!Objects.isNull(resp)) cache = resp.get();
        } else {
            cache = cacheMap.get(containers.getContainerCode());
        }
        return cache;
    }

    private void processCacheAndContainerResponseList(ConsolidationDetails consolidationDetails, Set<String> containerTypes, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
        List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(consolidationDetails.getContainersList(), ContainerResponse.class);
        if (!Objects.isNull(containerResponseList))
            containerResponseList.forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));
        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
    }

    private void checkCFSValidation(ConsolidationDetails consolidationDetails, Boolean isCreate, List<ShipmentDetails> shipmentDetails) throws RunnerException {
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

    private void checkInterBranchPermission(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if(((Objects.isNull(oldEntity) && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) || (!Objects.isNull(oldEntity)
                && !Objects.equals(consolidationDetails.getInterBranchConsole(), oldEntity.getInterBranchConsole())))
                && (!UserContext.getUser().getPermissions().containsKey(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)
                || Boolean.FALSE.equals(UserContext.getUser().getPermissions().get(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH)))) {
            throw new ValidationException("User don't have InterBranch Consolidation Permission to change InterBranch Flag");
        }
    }

    private boolean canSetChargableWeight(ConsolidationDetails consolidationDetails) {
        return !IsStringNullOrEmpty(consolidationDetails.getContainerCategory()) && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)
                && !IsStringNullOrEmpty(consolidationDetails.getTransportMode()) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA);
    }

    private void setInterBranchContext(Boolean isInterBranchConsole) {
        if (Boolean.TRUE.equals(isInterBranchConsole))
            commonUtils.setInterBranchContextForHub();
    }

    @Override
    @Transactional
    public String attachShipments(ShipmentAttachDetachV3Request request) throws RunnerException {

        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        HashSet<Long> attachedShipmentIds = new HashSet<>();
        boolean isConsolePullCall = false;

        // Extract request details
        List<Long> shipmentIds = request.getShipmentIds().stream().toList();
        Long consolidationId = request.getConsolidationId();
        ShipmentRequestedType shipmentRequestedType = request.getShipmentRequestedType();
        boolean fromConsolidation = request.isFromConsolidation();

        // Ensure request is valid and has required IDs
        consolidationValidationUtil.validateConsolidationIdAndShipmentIds(consolidationId, shipmentIds);

        // Fetch the corresponding consolidation record from the database
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);

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
        consolidationValidationUtil.validationsBeforeAttachShipments(consolidationDetails, consoleShipmentMappings, shipmentIds, consolidationId, shipmentDetailsList,
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

        // Detach entities from persistence context to prevent unintended lazy loading or caching
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
                        Collectors.toMap(NetworkTransfer::getTenantId, transfer -> transfer)
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
     * Retrieves a list of shipments associated with a given consolidation ID.
     * <p>
     * This method fetches mappings from the `ConsoleShipmentMapping` table, extracts shipment IDs, constructs a dynamic list query, and returns the corresponding
     * `ShipmentDetails`.
     *
     * @param consoleId The ID of the consolidation whose shipments need to be fetched
     * @return List of linked `ShipmentDetails`
     */
    private List<ShipmentDetails> getShipmentsList(Long consoleId) {
        // Fetch mapping records between the consolidation and its linked shipments
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consoleId);

        // Extract shipment IDs from the mapping
        List<Long> shipmentIdList = consoleShipmentMappings.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Build a dynamic list request using "IN" operation on shipment IDs
        ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");

        // Create JPA Specification and pagination info from the request
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        // Execute the paginated query using the specification
        Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

        // Return the list of shipment details
        return new ArrayList<>(page.getContent());
    }

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

            shipments = getShipmentsList(console.getId());

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
                shipments = getShipmentsList(console.getId());
            }

            List<EventsRequest> events = new ArrayList<>();

            // Update each linked shipment and collect relevant event triggers
            for (ShipmentDetails sd : shipments) {
                updateLinkedShipments(console, oldConsolEntity, fromAttachShipment, sd, events);
            }

            // Persist updated shipment details and event logs
            shipmentDao.saveAll(shipments);
            eventService.saveAllEvent(events);

            // Sync to external systems if triggered via attach UI
            if (Boolean.TRUE.equals(fromAttachShipment)) {
                syncShipmentsList(shipments, StringUtility.convertToString(console.getGuid()));
            }
        }

        return shipments;
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

    private void setBookingNumberInShipment(ConsolidationDetails console, ConsolidationDetails oldEntity, Boolean fromAttachShipment, ShipmentDetails i) {
        if (fromAttachShipment != null && fromAttachShipment) {
            if (!CommonUtils.IsStringNullOrEmpty(console.getBookingNumber())) {
                i.setBookingNumber(console.getBookingNumber());
            } else {
                i.setBookingNumber(console.getCarrierBookingRef());
            }
        } else {
            if (CommonUtils.IsStringNullOrEmpty(oldEntity.getBookingNumber())) {
                i.setBookingNumber(console.getCarrierBookingRef());
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
     * @param fromAttachShipment Boolean flag indicating if shipment was attached via UI
     * @param shipmentDetails    The shipment to update
     */
    private void updateCarrierDetailsForLinkedShipments(ConsolidationDetails console, Boolean fromAttachShipment, ShipmentDetails shipmentDetails) {
        if (console.getCarrierDetails() != null) {
            // Basic carrier detail updates from consolidation to shipment
            shipmentDetails.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(console.getCarrierDetails().getCfs());

            // Only update ETA, ETD, and flight number if attached from shipment screen
            if (fromAttachShipment != null && fromAttachShipment) {
                shipmentDetails.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
                shipmentDetails.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
            }

            // If transport mode is air, update air-specific fields like ATD, ATA, flight number
            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                shipmentDetails.getCarrierDetails().setAtd(console.getCarrierDetails().getAtd());
                shipmentDetails.getCarrierDetails().setAta(console.getCarrierDetails().getAta());
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
        updateCarrierDetailsForLinkedShipments(console, fromAttachShipment, shipmentDetails);

        // Perform CFS cut-off date validation if enabled and required
        if (checkConsolidationEligibleForCFSValidation(console)
                && checkIfShipmentDateGreaterThanConsole(shipmentDetails.getShipmentGateInDate(), console.getCfsCutOffDate())) {
            throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
        }

        // Determine if routing sync is required based on shipment type and settings
        boolean isDesiredShipmenTypeForReverseSyncFromConsol = false;
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled())
                && Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())
                && ((shipmentDetails.getShipmentType().equals("HSE") && Boolean.FALSE.equals(shipmentDetails.getB2b()))
                || shipmentDetails.getShipmentType().equals("BCN")
                || shipmentDetails.getShipmentType().equals("SCN"))) {
            isDesiredShipmenTypeForReverseSyncFromConsol = true;
        }

        // Sync main carriage routing from console to shipment
        syncMainCarriageRoutingToShipment(console.getRoutingsList(), shipmentDetails, true, isDesiredShipmenTypeForReverseSyncFromConsol);

        // Update export/import brokers if inter-branch logic applies
        updateInterBranchConsoleData(console, shipmentDetails);
    }

    /**
     * Updates the export and import broker information in the shipment details based on the consolidation's sending and receiving agents, but only if the consolidation is **not**
     * marked as an inter-branch console.
     * <p>
     * Logic: - If inter-branch console is false or null: - Set export broker in shipment from sending agent if different - Set import broker in shipment from receiving agent if
     * different - If `AdditionalDetails` is null, initializes it before setting brokers
     *
     * @param console The consolidation object containing sending and receiving agents
     * @param i       The shipment details to update with broker info
     */
    private void updateInterBranchConsoleData(ConsolidationDetails console, ShipmentDetails i) {
        // Proceed only if it's NOT an inter-branch console
        if (!Boolean.TRUE.equals(console.getInterBranchConsole())) {

            // Check and update Export Broker from Sending Agent
            if (i.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getSendingAgent(), i.getAdditionalDetails().getExportBroker())) {
                // If export broker doesn't match, update it from sending agent
                i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            } else if (i.getAdditionalDetails() == null) {
                // If no AdditionalDetails exist, initialize and set export broker
                i.setAdditionalDetails(new AdditionalDetails());
                i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            }

            // Check and update Import Broker from Receiving Agent
            if (i.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getReceivingAgent(), i.getAdditionalDetails().getImportBroker())) {
                // If import broker doesn't match, update it from receiving agent
                i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
            } else if (i.getAdditionalDetails() == null) {
                // If still null (shouldn't happen here), initialize and set import broker
                i.setAdditionalDetails(new AdditionalDetails());
                i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
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
     * @param saveRoutes                      Flag to persist the routings to DB.
     * @param reverseSyncFromConsolToShipment If true, retain existing non-inherited MAIN_CARRIAGE routes.
     * @throws RunnerException If any failure occurs during processing.
     */
    @Override
    public void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes,
            boolean reverseSyncFromConsolToShipment) throws RunnerException {

        // Exit early if no consolidation routings or route master feature is disabled
        if (CollectionUtils.isEmpty(consolidationRoutings)
                || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            return;
        }

        List<Routings> shipmentMainCarriageRouting = new ArrayList<>();
        List<Routings> shipmentRoutingList = Optional.ofNullable(shipmentDetails.getRoutingsList()).orElse(new ArrayList<>());
        shipmentDetails.setRoutingsList(shipmentRoutingList);
        List<Routings> existingOriginalShipmentMainCarriageRoutings = new ArrayList<>();

        if (reverseSyncFromConsolToShipment) {
            // Preserve original MAIN_CARRIAGE legs from shipment that were not inherited from consolidation
            shipmentRoutingList.stream()
                    .filter(r -> RoutingCarriage.MAIN_CARRIAGE.equals(r.getCarriage()) && Boolean.FALSE.equals(r.getInheritedFromConsolidation()))
                    .forEach(existingOriginalShipmentMainCarriageRoutings::add);
        }

        consolidationRoutings.stream()
                .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage()))
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
     * Checks if the shipment date is after the consolidation date.
     *
     * @param shipmentDate      the date of the shipment
     * @param consolidationDate the date of the consolidation
     * @return true if shipment date is after consolidation date, false otherwise
     */
    public boolean checkIfShipmentDateGreaterThanConsole(LocalDateTime shipmentDate, LocalDateTime consolidationDate) {
        // If either date is null, comparison can't be made
        if (shipmentDate == null || consolidationDate == null) {
            return false;
        }

        // Return true if shipmentDate is after consolidationDate
        return shipmentDate.isAfter(consolidationDate);
    }

    /**
     * Checks if a consolidation is eligible for CFS (Container Freight Station) validation. Criteria: - Transport mode must be SEA - Shipment type must be EXPORT - LCL
     * consolidation setting must be enabled in context
     *
     * @param consolidationDetails the consolidation to validate
     * @return true if eligible for CFS validation, false otherwise
     */
    public boolean checkConsolidationEligibleForCFSValidation(ConsolidationDetails consolidationDetails) {
        // Must be SEA transport mode
        if (!Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())) {
            return false;
        }

        // Must be an Export shipment
        if (!Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType())) {
            return false;
        }

        // LCL consolidation setting must be enabled
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
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
        // Handle packing consolidation only for AIR shipments
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipmentDetails.getPackingList() != null) {
            List<Packing> packingList = shipmentDetails.getPackingList();
            for (Packing packing : packingList) {
                packing.setConsolidationId(consolidationId); // Tag packing with consolidation ID
            }
            packingDao.saveAll(packingList); // Persist updated packings
        }

        // Handle event consolidation if event list is present
        if (shipmentDetails.getEventsList() != null) {
            List<Events> eventsList = shipmentDetails.getEventsList();
            for (Events event : eventsList) {
                // Update only if event qualifies for shipment-to-consolidation transfer
                if (eventDao.shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                    event.setConsolidationId(consolidationId); // Tag event with consolidation ID
                }
            }
            eventDao.saveAll(eventsList); // Persist updated events
        }
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
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if (attachedShipmentIds.contains(shipmentDetails.getId()) && !interBranchRequestedShipIds.contains(shipmentDetails.getId())) {
                if (shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = new ArrayList<>(shipmentDetails.getContainersList());
                    for (Containers container : containersList) {
                        container.setConsolidationId(consolidationId);
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
                processConsolePackingAndEvents(consolidationId, shipmentDetails);
                processOriginAndDestinationParties(consolidationDetails, agentCount, originParties, destinationParties, shipmentDetails);
                this.createLogHistoryForShipment(shipmentDetails);
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
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(shipmentDetails.getId())
                    .entityType(Constants.SHIPMENT).entityGuid(shipmentDetails.getGuid()).entityPayload(entityPayload).tenantId(shipmentDetails.getTenantId()).build());
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
                .collect(Collectors.toMap(
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

    @Override
    public ConsolidationDetailsResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if (request.getId() == null && request.getGuid() == null) {
            log.error("Request Id and Guid are null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id and GUID can't be null. Please provide any one !");
        }
        Long id = request.getId();
        Optional<ConsolidationDetails> consolidationDetails;
        if (id != null) {
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

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, boolean getMasterData) {
        try {
            double _start = System.currentTimeMillis();
            setMasterDataForCreateConsolePayload(consolidationDetailsResponse, getMasterData);
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
            // fetch NTE status
            this.fetchNTEstatusForReceivingBranch(consolidationDetailsResponse);
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

    private void fetchNTEstatusForReceivingBranch(ConsolidationDetailsResponse consolidationDetailsResponse) {
        if(consolidationDetailsResponse.getReceivingBranch() != null) {
            String transferStatus = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(consolidationDetailsResponse.getId(), CONSOLIDATION, consolidationDetailsResponse.getReceivingBranch().intValue());
            consolidationDetailsResponse.setTransferStatus(transferStatus);
        }
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
                setContData = getCountDataForAutoUpdateAndLcl(shipmentDetails, autoUpdate, lcl, descOfGoodsMap, handlingInfoMap, hblNumberMap, setContData);
                updateDescGoodsAndhandlingInfoMap(shipmentDetails, setContData, containerSelfDataAdded, lcl, descOfGoodsMap, handlingInfoMap, hblNumberMap);

            }
        }

        updateContainerResponseForContainerList(consolidationDetailsResponse, descOfGoodsMap, handlingInfoMap, hblNumberMap);

        if(!isAutoUpdate && consolidationDetailsResponse.getPackingList() != null && !consolidationDetailsResponse.getPackingList().isEmpty()) {
            for (PackingResponse packingResponse : consolidationDetailsResponse.getPackingList()) {
                if(packingResponse.getShipmentId() != null && shipmentNumberMap.containsKey(packingResponse.getShipmentId())) {
                    packingResponse.setShipmentNumber(shipmentNumberMap.get(packingResponse.getShipmentId()).getShipmentId());
                    packingResponse.setShipmentHazardous(shipmentNumberMap.get(packingResponse.getShipmentId()).getContainsHazardous());
                }
            }
        }
    }

    private void updateContainerResponseForContainerList(ConsolidationDetailsResponse consolidationDetailsResponse, Map<Long, String> descOfGoodsMap, Map<Long, String> handlingInfoMap, Map<Long, String> hblNumberMap) {
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
    }

    private void updateDescGoodsAndhandlingInfoMap(ShipmentDetails shipmentDetails, boolean setContData, Set<Long> containerSelfDataAdded, boolean lcl, Map<Long, String> descOfGoodsMap, Map<Long, String> handlingInfoMap, Map<Long, String> hblNumberMap) {
        if((setContData || !IsStringNullOrEmpty(shipmentDetails.getHouseBill())) && shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
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

    private boolean getCountDataForAutoUpdateAndLcl(ShipmentDetails shipmentDetails, boolean autoUpdate, boolean lcl, Map<Long, String> descOfGoodsMap, Map<Long, String> handlingInfoMap, Map<Long, String> hblNumberMap, boolean setContData) {
        if(autoUpdate && lcl && shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate()) {
            if(!listIsNullOrEmpty(shipmentDetails.getPackingList())) {
                for (Packing packing : shipmentDetails.getPackingList()) {
                    if(packing.getContainerId() != null) {
                        setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, packing.getContainerId(), packing.getGoodsDescription(), packing.getHandlingInfo(), null);
                    }
                }
            }
            setContData = false;
        }

        return setContData;
    }

    private ConsoleCalculationsResponse calculateChargeable(CommonRequestModel commonRequestModel) throws Exception{
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
                charge = getCharge(charge, half, floor);
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
        return getConsoleCalculationsResponse(consolidationDetails);
    }

    private BigDecimal getCharge(BigDecimal charge, BigDecimal half, BigDecimal floor) {
        if (charge.subtract(half).compareTo(floor) <= 0 && charge.compareTo(floor) != 0) {
            charge = floor.add(half);
        } else {
            charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
        }
        return charge;
    }

    private ConsoleCalculationsResponse getConsoleCalculationsResponse(ConsolidationDetails consolidationDetails) {
        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        return response;
    }

    private ConsolidationDetails getConsoleForCalculations(ConsoleCalculationsRequest request) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getId()).orElse(new ConsolidationDetails());
        consolidationDetails.setAllocations(jsonHelper.convertValue(request.getAllocations(), Allocations.class));
        consolidationDetails.setAchievedQuantities(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class));
        consolidationDetails.setTransportMode(request.getTransportMode());
        consolidationDetails.setContainerCategory(request.getContainerCategory());
        return consolidationDetails;
    }

    @Override
    public Map<String, Object> getAllMasterData(CommonRequestModel commonRequestModel) {
        Long id = commonRequestModel.getId();
        Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationDetailsDao.findConsolidationByIdWithQuery(id);
        if(!consolidationDetailsOptional.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
        long start = System.currentTimeMillis();
        List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ConsolidationDetails.class, null), createFieldClassDto(ArrivalDepartureDetails.class, "arrivalDetails."), createFieldClassDto(ArrivalDepartureDetails.class, "departureDetails.")));
        includeColumns.addAll(FieldUtils.getTenantIdAnnotationFields(List.of(createFieldClassDto(ConsolidationDetails.class, null))));
        includeColumns.addAll(ConsolidationConstants.LIST_INCLUDE_COLUMNS);
        ConsolidationDetailsResponse consolidationDetailsResponse = (ConsolidationDetailsResponse) commonUtils.setIncludedFieldsToResponse(consolidationDetails, includeColumns, new ConsolidationDetailsResponse());
        log.info("Total time taken in setting consol details response {}", (System.currentTimeMillis() - start));
        return fetchAllMasterDataByKey(consolidationDetails, consolidationDetailsResponse);
    }

    public Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Object> response = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetailsResponse, response)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllVesselDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllDGSubstanceDataInSingleCall(consolidationDetailsResponse, response)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(consolidationDetailsResponse, response)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, vesselDataFuture, dgSubstanceFuture, containerTypeFuture).join();
        return response;
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    private void setMasterDataForCreateConsolePayload(ConsolidationDetailsResponse consolidationDetailsResponse, boolean getMasterData) {
        if(getMasterData) {
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetailsResponse, null)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetailsResponse, null)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture).join();
        }
    }

    private CompletableFuture<Map<String, EntityTransferMasterLists>> addAllMasterDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private void processMasterDataResponse(ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse, Set<MasterListRequest> listRequests, Map<String, Map<String, String>> fieldNameKeyMap, Map<String, Object> cacheMap) {
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
    }

    private CompletableFuture<Map<String, EntityTransferUnLocations>> addAllUnlocationDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, EntityTransferCarrier>> addAllCarrierDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carrierList, new EntityTransferCarrier(), cacheMap);

            if(masterDataResponse == null) {
                consolidationDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, EntityTransferCurrency>> addAllCurrencyDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCurrencyDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, EntityTransferCommodityType>> addAllCommodityTypesInSingleCall(ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            if (masterDataResponse != null && !Objects.isNull(consolidationDetailsResponse.getPackingList()))
                consolidationDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if(masterDataResponse == null) {
                if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                    consolidationDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY, cacheMap)));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, TenantModel>> addAllTenantDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

    private CompletableFuture<Map<String, WareHouseResponse>> addAllWarehouseDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllWarehouseDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, EntityTransferVessels>> addAllVesselDataInSingleCall(ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> vesselList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));
            if (!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                consolidationDetailsResponse.getRoutingsList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));

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

    private CompletableFuture<Map<String, EntityTransferDGSubstance>> addAllDGSubstanceDataInSingleCall (ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> dgSubstanceIdList = new HashSet<>();
            if (!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                consolidationDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferDGSubstance> v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES, dgSubstanceIdList, new EntityTransferDGSubstance(), cacheMap);

            if(masterDataResponse == null) { }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllDGSubstanceDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<Map<String, EntityTransferContainerType>> addAllContainerTypesInSingleCall(ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
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

            return CompletableFuture.completedFuture(v1Data);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ConsolidationService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

}
