package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
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

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

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
    private IShipmentSync shipmentSync;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private MasterDataUtils masterDataUtils;
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
    private IConsolidationService consolidationService;

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

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            consolidationDetails.setShipmentsList(null);
            populateOriginDestinationAgentDetailsForBookingConsolidation(consolidationDetails);
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

    @Override
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
        Map<Long, ShipmentDetails> dgStatusChangeInShipments = new HashMap<>();
        dgOceanFlowsAndValidations(consolidationDetails, oldEntity, dgStatusChangeInShipments);
        List<ShipmentDetails> shipmentDetails = null;
        if(!isCreate){
            // This method will only work for non air transport modes , validation check moved inside the method
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), oldEntity.getShipmentsList());
            shipmentDetails = updateLinkedShipmentData(consolidationDetails, oldEntity, false, dgStatusChangeInShipments);
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

        // Ignore events payload to avoid transaction issues bypassing consolidationDao.update(...);
        // Update happens in after save from request body
        consolidationDetails.setEventsList(null);
        populateUnlocCodeFuture.join();
    }

    void getConsolidation(ConsolidationDetails consolidationDetails, boolean creatingFromDgShipment) throws RunnerException{
        consolidationService.generateConsolidationNumber(consolidationDetails);
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

        setContainerAndPackingList(consolidationDetails, isCreate, shipmentSettingsDetails, isFromBooking, includeGuid, containerRequestList, id, packingRequestList);

        // Events section
        setEventsForConsole(consolidationDetails, isCreate, shipmentSettingsDetails, isFromBooking, eventsRequestList, id);

        processRequestLists(consolidationDetails, isCreate, isFromBooking, referenceNumbersRequestList, id, truckDriverDetailsRequestList, routingsRequestList);
        if (consolidationAddressRequest != null) {
            List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedFileRepos);
        }

        if(!isFromET) {
            consolidationService.pushShipmentDataToDependentService(consolidationDetails, isCreate, oldEntity);
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

    private void setContainerAndPackingList(ConsolidationDetails consolidationDetails, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, boolean includeGuid, List<ContainerRequest> containerRequestList, Long id, List<PackingRequest> packingRequestList) throws RunnerException {
        if(containerRequestList != null && (shipmentSettingsDetails.getMergeContainers() == null || !shipmentSettingsDetails.getMergeContainers())
                && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
            List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class, (!isFromBooking && !includeGuid) && isCreate), id, (Long) null, true);
            consolidationDetails.setContainersList(updatedContainers);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class, (!isFromBooking && !includeGuid) && isCreate), id);
            consolidationDetails.setPackingList(updatedPackings);
        }
    }

    private void setEventsForConsole(ConsolidationDetails consolidationDetails, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking, List<EventsRequest> eventsRequestList, Long id) throws RunnerException {
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
    }

    private List<EventsRequest> setEventDetails(List<EventsRequest> eventsRequestList, ConsolidationDetails consolidationDetails) {
        if(eventsRequestList != null && !eventsRequestList.isEmpty()) {
            for (EventsRequest req : eventsRequestList) {
                req.setConsolidationId(consolidationDetails.getId());
            }
        }
        return eventsRequestList;
    }

    public void generateEvents(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getEventsList() == null) {
            consolidationDetails.setEventsList(new ArrayList<>());
        }
        consolidationDetails.getEventsList().add(createEvent(consolidationDetails, EventConstants.COCR));
    }

    private Events createEvent(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
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
        if(!checkConsolidationTypeValidation(consolidationDetails))
            throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
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
        updateLinkedShipmentData(consolidationDetails, null, true, new HashMap<>());

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

    private boolean canProcessConsole(ConsolidationDetails console, ConsolidationDetails oldEntity, Map<Long, ShipmentDetails> dgStatusChangeInShipments) {
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
                        )) || !dgStatusChangeInShipments.isEmpty() ||
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
            Boolean fromAttachShipment, Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {

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
        if (canProcessConsole(console, oldConsolEntity, dgStatusChangeInShipments)) {
            if (shipments == null) {
                shipments = getShipmentsList(console.getId());
            }

            List<EventsRequest> events = new ArrayList<>();

            // Update each linked shipment and collect relevant event triggers
            for (ShipmentDetails sd : shipments) {
                updateLinkedShipments(console, oldConsolEntity, fromAttachShipment, dgStatusChangeInShipments, sd, events);
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
            Map<Long, ShipmentDetails> dgStatusChangeInShipments,
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

        // Update DG status if there’s a change detected for this shipment
        if (dgStatusChangeInShipments.containsKey(shipmentDetails.getId())) {
            shipmentDetails.setContainsHazardous(dgStatusChangeInShipments.get(shipmentDetails.getId()).getContainsHazardous());
            shipmentDetails.setOceanDGStatus(dgStatusChangeInShipments.get(shipmentDetails.getId()).getOceanDGStatus());
        }

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
}
