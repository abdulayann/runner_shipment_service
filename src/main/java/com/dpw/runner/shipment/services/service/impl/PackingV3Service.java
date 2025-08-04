package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import com.dpw.runner.shipment.services.utils.v3.PackingValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.google.common.annotations.VisibleForTesting;
import com.nimbusds.jose.util.Pair;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;


@SuppressWarnings("java:S4165")
@Service
@Slf4j
public class PackingV3Service implements IPackingV3Service {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private PackingValidationV3Util packingValidationV3Util;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private PackingV3Util packingV3Util;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentService;

    @Lazy
    @Autowired
    private ICustomerBookingV3Service customerBookingV3Service;

    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Autowired
    @Lazy
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Lazy
    @Autowired
    private IContainerV3Service containerV3Service;

    @Autowired
    private ShipmentValidationV3Util shipmentValidationV3Util;

    private List<String> defaultIncludeColumns = new ArrayList<>();

    @VisibleForTesting
    PackingContext createPackingContextForTest(Long consolidationId, String xSource) throws AuthenticationException, RunnerException {
        return createConsolidationContext(consolidationId, xSource);
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ParentResult {
        private String parent;
        private Long parentId;
    }

    @Override
    @Transactional
    public PackingResponse create(PackingV3Request packingRequest, String module) throws RunnerException {
        String requestId = LoggerHelper.getRequestIdFromMDC();
        updatePackingRequestOnDgAndTemperatureFlag(List.of(packingRequest));

        log.info("Starting packing creation | Request ID: {} | Request Body: {}", requestId, packingRequest);
        if (packingRequest.getContainerId() != null) {
            throw new ValidationException("Package can be assigned to a container only after creation.");
        }
        Object entity = packingValidationV3Util.validateModule(packingRequest, module);
        // Convert DTO to Entity
        Packing packing = jsonHelper.convertValue(packingRequest, Packing.class);
        log.debug("Converted packing request to entity | Entity: {}", packing);
        ShipmentDetails shipmentDetails = null;
        Long consolidationId = null;
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = (ShipmentDetails) entity;
            consolidationId = packingV3Util.updateConsolidationIdInPackings(shipmentDetails, List.of(packing));
            packingV3Util.setColoadingStation(shipmentDetails);
        }

        if(consolidationId == null && packingRequest.getShipmentId() != null) {
            consolidationId = packingV3Util.getConsolidationId(packingRequest.getShipmentId());
        }
        ConsolidationDetails consolidationDetails = null;
        ShipmentWtVolResponse oldShipmentWtVolResponse = null;
        if(consolidationId != null) {
            consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            oldShipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails);
        }

        // Save to DB
        Packing savedPacking = packingDao.save(packing);
        log.info("Saved packing entity to DB | Packing ID: {} | Request ID: {}", savedPacking.getId(), requestId);

        ParentResult parentResult = getParentDetails(List.of(savedPacking), module);
        if(Objects.equals(module, BOOKING)) {
            customerBookingV3Service.updatePackingInfoInBooking(packingRequest.getBookingId());
        }
        // Audit logging
        recordAuditLogs(null, List.of(savedPacking), DBOperationType.CREATE, parentResult);
        log.info("Audit log recorded for packing creation | Packing ID: {}", savedPacking.getId());

        PackingResponse response = jsonHelper.convertValue(savedPacking, PackingResponse.class);
        log.info("Returning packing response | Packing ID: {} | Response: {}", savedPacking.getId(), response);
        afterSave(List.of(savedPacking), module, shipmentDetails, consolidationDetails, oldShipmentWtVolResponse);
        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(List.of(savedPacking), true, module);
        return response;
    }

    private void afterSave(List<Packing> packings, String module, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                           ShipmentWtVolResponse oldShipmentWtVolResponse) throws RunnerException {
        if (!CommonUtils.listIsNullOrEmpty(packings) && Constants.SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = getShipment(shipmentDetails, packings.get(0).getShipmentId());
            List<Packing> finalPackings = updateCargoDetailsInShipment(packings, shipmentDetails);
            packingValidationV3Util.validatePackageAfterSave(shipmentDetails, finalPackings);
            if (checkIfLCLConsolidationEligible(shipmentDetails)) {
                updateShipmentGateInDateAndStatusFromPacks(shipmentDetails, finalPackings);
            }
            updateOceanDGStatus(shipmentDetails, finalPackings);
            if(consolidationDetails != null)
                consolidationV3Service.updateConsolidationCargoSummary(consolidationDetails, oldShipmentWtVolResponse);
            boolean isSeaFCLOrRoadFTL = commonUtils.isSeaFCLOrRoadFTL(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType());
            if (!isSeaFCLOrRoadFTL)
                updateAttachedContainersData(packings, shipmentDetails);
        }
    }

    protected void updateOceanDGStatus(ShipmentDetails shipmentDetails, List<Packing> packingList) throws RunnerException {
        if(shipmentDetails == null || CommonUtils.listIsNullOrEmpty(packingList)
                || !TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode())) return;

        boolean isDG = false;
        boolean isDGClass1Added = false;
        for (Packing packing : packingList) {
            if (Boolean.TRUE.equals(packing.getHazardous())) {
                isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(packing.getDGClass());
                isDG = true;
            }
        }

        if(isDG){
            boolean saveShipment = commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1Added);
            if(saveShipment) {
                shipmentDetails.setContainsHazardous(true);
                shipmentValidationV3Util.processDGValidations(shipmentDetails, null, shipmentDetails.getConsolidationList());
                String oceanDGStatus = shipmentDetails.getOceanDGStatus() != null ? shipmentDetails.getOceanDGStatus().name() : null;
                shipmentDao.updateDgStatusInShipment(true, oceanDGStatus, shipmentDetails.getId());
            }
        }
    }

    private List<Packing> updateCargoDetailsInShipment(List<Packing> updatedPackings, ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails == null)
            return Collections.emptyList();
        List<Packing> packings = packingDao.findByShipmentId(shipmentDetails.getId());
        boolean isSeaFCLOrRoadFTL = commonUtils.isSeaFCLOrRoadFTL(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType());
        if (isSeaFCLOrRoadFTL) {
            updateAttachedContainersData(updatedPackings, shipmentDetails);
            CargoDetailsResponse cargoDetailsResponse = shipmentService.calculateShipmentSummary(shipmentDetails.getTransportMode(), packings, shipmentDetails.getContainersList());
            if (cargoDetailsResponse != null)
                shipmentService.updateCargoDetailsInShipment(shipmentDetails, cargoDetailsResponse);
        } else if (!CollectionUtils.isEmpty(packings)) {
            CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
            cargoDetailsResponse.setWeight(shipmentDetails.getWeight());
            cargoDetailsResponse.setWeightUnit(shipmentDetails.getWeightUnit());
            cargoDetailsResponse.setTransportMode(shipmentDetails.getTransportMode());
            cargoDetailsResponse.setShipmentType(shipmentDetails.getShipmentType());
            cargoDetailsResponse = calculateCargoDetails(packings, cargoDetailsResponse);
            shipmentService.updateCargoDetailsInShipment(shipmentDetails, cargoDetailsResponse);
        }
        return packings;
    }

    @Override
    @Transactional
    public PackingResponse update(PackingV3Request packingRequest, String module) throws RunnerException {
        updatePackingRequestOnDgAndTemperatureFlag(List.of(packingRequest));
        packingValidationV3Util.validateUpdateRequest(packingRequest);
        Optional<Packing> optionalPacking = packingDao.findById(packingRequest.getId());
        if (optionalPacking.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Object entity = packingValidationV3Util.validateModule(packingRequest, module);
        Packing oldPacking = optionalPacking.get();
        if (!Objects.equals(packingRequest.getContainerId(), oldPacking.getContainerId())) {
            throw new ValidationException("Changes are available for Package section, Please refresh for latest updates.");
        }
        Packing oldConvertedPacking = jsonHelper.convertValue(oldPacking, Packing.class);
        Packing newPacking = jsonHelper.convertValue(packingRequest, Packing.class);

        ShipmentDetails shipmentDetails = null;
        Long consolidationId = null;
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = (ShipmentDetails) entity;
            consolidationId = packingV3Util.updateConsolidationIdInPackings(shipmentDetails, List.of(newPacking));
        }

        if(consolidationId == null && packingRequest.getShipmentId() != null) {
            consolidationId = packingV3Util.getConsolidationId(packingRequest.getShipmentId());
        }
        ConsolidationDetails consolidationDetails = null;
        ShipmentWtVolResponse oldShipmentWtVolResponse = null;
        if(consolidationId != null) {
            consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            oldShipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails);
        }

        Packing updatedPacking = packingDao.save(newPacking);

        ParentResult parentResult = getParentDetails(List.of(updatedPacking), module);
        if(Objects.equals(module, BOOKING)) {
            customerBookingV3Service.updatePackingInfoInBooking(packingRequest.getBookingId());
        }

        recordAuditLogs(List.of(oldConvertedPacking), List.of(updatedPacking), DBOperationType.UPDATE, parentResult);
        afterSave(List.of(updatedPacking), module, shipmentDetails, consolidationDetails, oldShipmentWtVolResponse);
        boolean isAutoSell = false;
        if (!Objects.equals(oldConvertedPacking.getPacksType(), updatedPacking.getPacksType()) || !Objects.equals(oldConvertedPacking.getPacks(), updatedPacking.getPacks())) {
            isAutoSell = true;
        }
        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(List.of(updatedPacking), isAutoSell, module);
        return convertEntityToDto(updatedPacking);
    }

    @Override
    @Transactional
    public String delete(Long id, String module) throws RunnerException {
        if (id == null) {
            throw new IllegalArgumentException("Packing Id cannot be null or empty.");
        }
        Optional<Packing> optionalPacking = packingDao.findById(id);
        if (optionalPacking.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Packing packing = optionalPacking.get();

        Long consolidationId = packing.getConsolidationId();
        if(consolidationId == null && packing.getShipmentId() != null) {
            consolidationId = packingV3Util.getConsolidationId(packing.getShipmentId());
        }
        ConsolidationDetails consolidationDetails = null;
        ShipmentWtVolResponse oldShipmentWtVolResponse = null;
        if(consolidationId != null) {
            consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            oldShipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails);
        }

        if (packing.getContainerId() != null) {
            throw new ValidationException("Assigned packages cannot be deleted, Please Unassign to delete.");
        }
        packingDao.delete(packing);

        ParentResult parentResult = getParentDetails(List.of(packing), module);
        if(Objects.equals(module, BOOKING)) {
            customerBookingV3Service.updatePackingInfoInBooking(packing.getBookingId());
        }

        recordAuditLogs(List.of(packing), null, DBOperationType.DELETE, parentResult);

        String packs = packing.getPacks();
        String packsType = packing.getPacksType();
        afterSave(List.of(packing), module, null, consolidationDetails, oldShipmentWtVolResponse);

        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(List.of(packing), true, module);
        return packsType != null
                ? String.format("Packing %s - %s deleted successfully!", packs, packsType)
                : String.format("Packing %s deleted successfully!", packs);
    }

    @Override
    @Transactional
    public BulkPackingResponse updateBulk(List<PackingV3Request> packingRequestList, String module) throws RunnerException {
        updatePackingRequestOnDgAndTemperatureFlag(packingRequestList);
        packingValidationV3Util.validateSameParentId(packingRequestList, module);
        // Separate IDs and determine existing packings
        List<Long> incomingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        Object entity = packingValidationV3Util.validateModule(packingRequestList.get(0), module);

        List<Packing> existingPackings = fetchExistingPackings(incomingIds);

        // Validate incoming request
        packingValidationV3Util.validateUpdateBulkRequest(packingRequestList, existingPackings);

        // Separate into create and update requests
        List<PackingV3Request> updateRequests = new ArrayList<>();
        List<PackingV3Request> createRequests = new ArrayList<>();

        for (PackingV3Request request : packingRequestList) {
            if (request.getId() != null && incomingIds.contains(request.getId())) {
                updateRequests.add(request);
            } else {
                createRequests.add(request);
            }
        }

        if(!module.equals(BOOKING)) {
            packingValidationV3Util.validateContainerIds(createRequests, updateRequests, existingPackings);
        }

        // Convert and process updates
        List<Packing> oldConvertedPackings = jsonHelper.convertValueToList(existingPackings, Packing.class);
        List<Packing> updatedPackings = jsonHelper.convertValueToList(updateRequests, Packing.class);

        // Convert and process creates
        List<Packing> newPackings = jsonHelper.convertValueToList(createRequests, Packing.class);

        ShipmentDetails shipmentDetails = null;
        Long consolidationId = null;
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = (ShipmentDetails) entity;
            consolidationId = packingV3Util.updateConsolidationIdInPackings(shipmentDetails, updatedPackings);
            setConsolidationId(shipmentDetails, newPackings, consolidationId);
        }

        if(consolidationId == null && packingRequestList.get(0).getShipmentId() != null) {
            consolidationId = packingV3Util.getConsolidationId(packingRequestList.get(0).getShipmentId());
        }
        ConsolidationDetails consolidationDetails = null;
        ShipmentWtVolResponse oldShipmentWtVolResponse = null;
        if(consolidationId != null) {
            consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            oldShipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails);
        }

        List<Packing> savedUpdatedPackings = saveIfNotEmpty(updatedPackings);
        List<Packing> savedNewPackings = saveIfNotEmpty(newPackings);

        // Combine results for parent calculation and auditing
        List<Packing> allSavedPackings = new ArrayList<>();
        allSavedPackings.addAll(savedNewPackings);
        allSavedPackings.addAll(savedUpdatedPackings);

        ParentResult parentResult = getParentDetails(allSavedPackings, module);

        // Audit logs
        auditLogsChanges(oldConvertedPackings, savedUpdatedPackings, savedNewPackings, parentResult);

        boolean isAutoSell = false;
        Map<UUID, Packing> oldPackings = oldConvertedPackings.stream().collect(Collectors.toMap(Packing::getGuid, packing -> packing, (packing1, packing2) -> packing1));

        for (Packing packing : updatedPackings) {
            Packing packing1 = oldPackings.get(packing.getGuid());
            if (!Objects.equals(packing1.getPacksType(), packing.getPacksType()) || !Objects.equals(packing1.getPacks(), packing.getPacks())) {
                isAutoSell = true;
                break;
            }
        }
        if(Objects.equals(module, BOOKING)) {
            customerBookingV3Service.updatePackingInfoInBooking(packingRequestList.iterator().next().getBookingId());
        }

        // Convert to response
        List<PackingResponse> packingResponses = jsonHelper.convertValueToList(allSavedPackings, PackingResponse.class);
        afterSave(allSavedPackings, module, shipmentDetails, consolidationDetails, oldShipmentWtVolResponse);

        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(allSavedPackings, isAutoSell, module);

        return BulkPackingResponse.builder()
                .packingResponseList(packingResponses)
                .message(prepareBulkUpdateMessage(packingResponses))
                .build();
    }

    private List<Packing> saveIfNotEmpty(List<Packing> packings) {
        return CommonUtils.listIsNullOrEmpty(packings) ? Collections.emptyList() : packingDao.saveAll(packings);
    }

    private List<Packing> fetchExistingPackings(List<Long> ids) {
        return CommonUtils.listIsNullOrEmpty(ids) ? new ArrayList<>() : packingDao.findByIdIn(ids);
    }

    private void auditLogsChanges(List<Packing> oldPackings, List<Packing> updatedPackings,
                                  List<Packing> newPackings, ParentResult parentResult) {
        recordAuditLogs(oldPackings, updatedPackings, DBOperationType.UPDATE, parentResult);
        recordAuditLogs(null, newPackings, DBOperationType.CREATE, parentResult);
    }

    private void pushToDependentServices(List<Packing> packings, boolean isAutoSell, String module) {
        if (Objects.equals(module, SHIPMENT)) {
            Long shipId = packings.get(0).getShipmentId();
            Long consoleId = packings.stream().map(Packing::getConsolidationId).filter(Objects::nonNull).findFirst().orElse(null);
            triggerPushToDownStreamForShipment(shipId, consoleId, isAutoSell);
        }
    }

    private void triggerPushToDownStreamForShipment(Long shipmentId, Long consoleId, boolean isAutoSell) {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(shipmentId)
                .parentEntityName(SHIPMENT)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .isAutoSellRequired(isAutoSell)
                        .build())
                .build();
        if (consoleId != null) {
            PushToDownstreamEventDto.Triggers triggers = PushToDownstreamEventDto.Triggers.builder()
                    .entityName(Constants.CONSOLIDATION)
                    .entityId(consoleId)
                    .build();
            pushToDownstreamEventDto.setTriggers(new ArrayList<>(Collections.singletonList(triggers)));
        }
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, shipmentId.toString());
    }

    private void setConsolidationId(ShipmentDetails shipmentDetails, List<Packing> packings, Long consolidationId) {
        if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode()) && consolidationId != null && !CommonUtils.listIsNullOrEmpty(packings)) {
            for (Packing packing : packings) {
                packing.setConsolidationId(consolidationId);
            }
        }
    }

    @Override
    @Transactional
    public BulkPackingResponse deleteBulk(List<PackingV3Request> packingRequestList, String module) throws RunnerException {
        packingValidationV3Util.validateDeleteBulkRequest(packingRequestList);
        packingValidationV3Util.validateSameParentId(packingRequestList, module);
        // Extract unique packing IDs from the request
        List<Long> packingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .distinct()
                .toList();

        // Fetch packings from DB to ensure they exist before deletion
        List<Packing> packingsToDelete = packingDao.findByIdIn(packingIds);

        Long consolidationId = null;
        if(!listIsNullOrEmpty(packingsToDelete)) {
            consolidationId = packingsToDelete.get(0).getConsolidationId();
            if(consolidationId == null && packingsToDelete.get(0).getShipmentId() != null)
                consolidationId = packingV3Util.getConsolidationId(packingsToDelete.get(0).getShipmentId());
        }
        ConsolidationDetails consolidationDetails = null;
        ShipmentWtVolResponse oldShipmentWtVolResponse = null;
        if(consolidationId != null) {
            consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            oldShipmentWtVolResponse = consolidationV3Service.calculateShipmentWtVol(consolidationDetails);
        }

        if (packingsToDelete.isEmpty()) {
            throw new DataRetrievalFailureException("No packing found for the given Ids.");
        }
        for (Packing packing : packingsToDelete) {
            if (packing.getContainerId() != null) {
                throw new ValidationException("Assigned packages cannot be deleted, Please Unassign to delete.");
            }
        }

        // Validate that all necessary packing IDs are present in the request
        packingValidationV3Util.validateUpdateBulkRequest(packingRequestList, packingsToDelete);

        ParentResult parentResult = getParentDetails(packingsToDelete, module);

        // Delete packings from DB
        packingDao.deleteByIdIn(packingIds);
        if(Objects.equals(module, BOOKING)) {
            customerBookingV3Service.updatePackingInfoInBooking(packingRequestList.iterator().next().getBookingId());
        }
        // Record audit logs for the deletion operation
        recordAuditLogs(packingsToDelete, null, DBOperationType.DELETE, parentResult);

        afterSave(packingsToDelete, module, null, consolidationDetails, oldShipmentWtVolResponse);

        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(packingsToDelete, true, module);
        // Return the response with status message
        return BulkPackingResponse.builder()
                .message(prepareBulkDeleteMessage(packingsToDelete))
                .build();
    }

    private void recordAuditLogs(List<Packing> oldPackings, List<Packing> newPackings, DBOperationType operationType, ParentResult parentResult) {
        Map<Long, Packing> oldPackingMap = Optional.ofNullable(oldPackings).orElse(List.of()).stream()
                .filter(packing -> packing.getId() != null)
                .collect(Collectors.toMap(Packing::getId, Function.identity()));

        Map<Long, Packing> newPackingMap = Optional.ofNullable(newPackings).orElse(List.of()).stream()
                .filter(packing -> packing.getId() != null)
                .collect(Collectors.toMap(Packing::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newPackingMap.keySet();
            case DELETE -> oldPackingMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldPackingMap.keySet());
                ids.retainAll(newPackingMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                Packing oldData = oldPackingMap.get(id);
                Packing newData = newPackingMap.get(id);

                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().getUsername())
                                .prevData(oldData)
                                .newData(newData)
                                .parent(parentResult.getParent())
                                .parentId(parentResult.getParentId())
                                .operation(operationType.name())
                                .build()
                );
            } catch (Exception ex) {
                log.error("Failed to add audit log for packing ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    private String prepareBulkUpdateMessage(List<PackingResponse> packingResponses) {
        String message;

        // If more than one packing was updated, return a generic bulk success message
        if (packingResponses.size() > 1) {
            message = "Bulk edit success! All selected packs have been updated.";
        } else {
            message = "Packing saved successfully.";
        }

        return message;
    }

    private String prepareBulkDeleteMessage(List<Packing> packings) {
        String message;

        // If more than one packing was deleted, return a generic bulk success message
        if (packings.size() > 1) {
            message = "Packings deleted successfully!";
        } else {
            message = "Packing deleted successfully!";
        }

        return message;
    }

    @Override
    public void downloadPacking(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException {
        packingV3Util.downloadPacking(response, request);
    }

    private Optional<Packing> retrieveForNte(Long id, String guid) {
        Optional<Packing> packing;
        if (id != null) {
            packing = packingDao.findByIdWithQuery(id);
        } else {
            packing = packingDao.findByGuidWithQuery(UUID.fromString(guid));
        }
        return packing;
    }

    @Override
    public PackingResponse retrieveById(Long id, String guid, String source) {
        String responseMsg;
        try {
            if (id == null && isStringNullOrEmpty(guid)) {
                log.error("Id and Guid are null for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Optional<Packing> packing;
            if (Objects.equals(source, NETWORK_TRANSFER)) {
                packing = retrieveForNte(id, guid);
            } else {
                if (id != null) {
                    packing = packingDao.findById(id);
                } else {
                    packing = packingDao.findByGuid(UUID.fromString(guid));
                }
            }
            if (packing.isEmpty()) {
                log.debug(PackingConstants.PACKING_RETRIEVE_BY_ID_ERROR, id, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Packing fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return convertEntityToDto(packing.get());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new ValidationException(responseMsg);
        }
    }

    @Override
    public PackingListResponse list(ListCommonRequest request, boolean getMasterData, String source) {
        if (request == null) {
            log.error("Request is empty for Packing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request cannot be null for list request.");
        }
        // construct specifications for filter request
        Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
        Page<Packing> packingPage;
        if (Objects.equals(source, NETWORK_TRANSFER))
            packingPage = packingDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
        else
            packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        PackingListResponse packingListResponse = new PackingListResponse();
        if (packingPage != null) {
            List<PackingResponse> responseList = convertEntityListToDtoList(packingPage.getContent());
            Map<String, Object> masterDataResponse = this.getMasterDataForList(responseList, getMasterData);
            packingListResponse.setPackings(responseList);
            packingListResponse.setTotalPages(packingPage.getTotalPages());
            packingListResponse.setTotalCount(packingPage.getTotalElements());
            packingListResponse.setMasterData(masterDataResponse);
        }
        return packingListResponse;
    }

    @Override
    public List<PackingResponse> fetchPacksAttachedToContainers(List<Long> containerIds) {
        List<Packing> packingList = packingDao.findByContainerIdIn(containerIds);
        return convertEntityListToDtoList(packingList);
    }

    @Override
    public void removeContainersFromPacking(List<Long> containerIds) {
        packingDao.removeContainersFromPacking(containerIds);
    }

    private Map<String, Object> getMasterDataForList(List<PackingResponse> responseList, boolean getMasterData) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllUnlocationInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllMasterDataInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                var commodityTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllCommodityTypesInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, masterDataFuture, commodityTypeFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.PACKING_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
                return masterDataResponse;
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_PACKING_LIST, ex.getLocalizedMessage());
            }
        }
        return masterDataResponse;
    }

    @Override
    public PackingListResponse fetchShipmentPackages(ListCommonRequest request, String xSource) {
        if (StringUtility.isEmpty(request.getEntityId()) || Long.valueOf(request.getEntityId()) <= 0) {
            throw new ValidationException("Entity id is empty");
        }
        ListCommonRequest listCommonRequest;
        if (CollectionUtils.isEmpty(request.getFilterCriteria())) {
            listCommonRequest = CommonUtils.constructListCommonRequest(SHIPMENT_ID, Long.valueOf(request.getEntityId()), Constants.EQ);
        } else {
            listCommonRequest = CommonUtils.andCriteria(Constants.SHIPMENT_ID, Long.valueOf(request.getEntityId()), Constants.EQ, request);
        }
        listCommonRequest.setSortRequest(request.getSortRequest());
        listCommonRequest.setPageNo(request.getPageNo());
        listCommonRequest.setPageSize(request.getPageSize());
        listCommonRequest.setContainsText(request.getContainsText());
        PackingListResponse packingListResponse = list(listCommonRequest, true, xSource);
        log.info("Packing list retrieved successfully for shipment with Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        PackingAssignmentProjection assignedPackages;
        if (StringUtility.isEmpty(xSource)) {
            assignedPackages = packingDao.getPackingAssignmentCountByShipmentAndTenant(Long.valueOf(request.getEntityId()), TenantContext.getCurrentTenant());
        } else {
            assignedPackages = packingDao.getPackingAssignmentCountByShipment(Long.valueOf(request.getEntityId()));
        }
        packingListResponse.setAssignedPackageCount(assignedPackages.getAssignedCount());
        packingListResponse.setUnassignedPackageCount(assignedPackages.getUnassignedCount());
        Optional<ShipmentDetails> shipmentDetailsEntity = shipmentService.findById(Long.valueOf(request.getEntityId()));
        if (shipmentDetailsEntity.isPresent()) {
            ShipmentDetails shipmentDetails = shipmentDetailsEntity.get();
            packingListResponse.getPackings().forEach(packingResponse -> {
                packingResponse.setShipmentNumber(shipmentDetails.getShipmentId());
                if(Objects.nonNull(shipmentDetails.getContainerAssignedToShipmentCargo()))
                    packingResponse.setShipmentContainerAssignedToShipmentCargo(shipmentDetails.getContainerAssignedToShipmentCargo());
            });
        }
        processAfterList(packingListResponse);
        return packingListResponse;
    }

    public void processPackingListResponse(PackingListResponse packingListResponse, Map<Long, ContainerInfoProjection> containerIdContainerNumberMap) {
        for (PackingResponse item : packingListResponse.getPackings()) {
            Long containerId = item.getContainerId();
            if (containerId != null && containerIdContainerNumberMap.containsKey(containerId)) {
                item.setContainerNumber(containerIdContainerNumberMap.get(containerId).getContainerNumber());
                item.setContainerCode(containerIdContainerNumberMap.get(containerId).getContainerCode());
            }
            containerId = item.getShipmentContainerAssignedToShipmentCargo();
            if (containerId != null && containerIdContainerNumberMap.containsKey(containerId)) {
                item.setShipmentContainerNumberAssignedToShipmentCargo(containerIdContainerNumberMap.get(containerId).getContainerNumber());
                item.setShipmentContainerCodeAssignedToShipmentCargo(containerIdContainerNumberMap.get(containerId).getContainerCode());
            }
        }
    }

    @Override
    public PackingListResponse fetchConsolidationPackages(ListCommonRequest request, String xSource) {
        if (StringUtility.isEmpty(request.getEntityId()) || Long.parseLong(request.getEntityId()) <= 0) {
            throw new ValidationException("Entity id is empty");
        }
        List<ConsoleShipmentMapping> consolidationDetailsEntity = consoleShipmentMappingDao.findByConsolidationId(Long.valueOf(request.getEntityId()));
        if (ObjectUtils.isEmpty(consolidationDetailsEntity)) {
            return new PackingListResponse();
        }
        List<Long> shipmentIds = consolidationDetailsEntity.stream().filter(ConsoleShipmentMapping::getIsAttachmentDone).map(ConsoleShipmentMapping::getShipmentId).toList();
        ListCommonRequest listCommonRequest;
        if (CollectionUtils.isEmpty(request.getFilterCriteria())) {
            listCommonRequest = CommonUtils.constructListCommonRequest(SHIPMENT_ID, shipmentIds, Constants.IN);
        } else {
            listCommonRequest = CommonUtils.andCriteria(Constants.SHIPMENT_ID, shipmentIds, Constants.IN, request);
        }
        listCommonRequest.setSortRequest(request.getSortRequest());
        listCommonRequest.setPageNo(request.getPageNo());
        listCommonRequest.setPageSize(request.getPageSize());
        listCommonRequest.setContainsText(request.getContainsText());
        PackingListResponse packingListResponse = list(listCommonRequest, true, xSource);
        log.info("Packing list retrieved successfully for consolidation with Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        PackingAssignmentProjection assignedPackages;
        if (StringUtility.isEmpty(xSource)) {
            assignedPackages = packingDao.getPackingAssignmentCountByShipmentInAndTenant(shipmentIds, TenantContext.getCurrentTenant());
        } else {
            assignedPackages = packingDao.getPackingAssignmentCountByShipmentIn(shipmentIds);
        }
        packingListResponse.setAssignedPackageCount(assignedPackages.getAssignedCount());
        packingListResponse.setUnassignedPackageCount(assignedPackages.getUnassignedCount());
        //get shipment details and consolidation details
        List<ShipmentDetails> shipmentDetailsList = shipmentService.findByIdIn(shipmentIds);
        Map<Long, ShipmentDetails> shipmentIdMap = shipmentDetailsList.stream()
                .collect(Collectors.toMap(
                        ShipmentDetails::getId,
                        Function.identity()
                ));
        if (!CollectionUtils.isEmpty(packingListResponse.getPackings())) {
            packingListResponse.getPackings().forEach(packingResponse -> {
                ShipmentDetails shipmentDetails = shipmentIdMap.get(packingResponse.getShipmentId());
                packingResponse.setShipmentNumber(shipmentDetails.getShipmentId());
                if(Objects.nonNull(shipmentDetails.getContainerAssignedToShipmentCargo()))
                   packingResponse.setShipmentContainerAssignedToShipmentCargo(shipmentDetails.getContainerAssignedToShipmentCargo());
            });
        }
        processAfterList(packingListResponse);
        return packingListResponse;

    }

    private void processAfterList(PackingListResponse packingListResponse) {
        if (!CollectionUtils.isEmpty(packingListResponse.getPackings())) {
            Set<Long> containerIds = packingListResponse.getPackings().stream()
                    .flatMap(packing -> Stream.of(packing.getContainerId(), packing.getShipmentContainerAssignedToShipmentCargo()))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
            if (!CollectionUtils.isEmpty(containerIds)) {
                Map<Long, ContainerInfoProjection> containerIdContainerNumberMap = getContainerIdNumberMap(containerIds);
                processPackingListResponse(packingListResponse, containerIdContainerNumberMap);
            }
        }
    }

    @Data
    @Builder
    private static class PackingContext {
        private List<Packing> packingList;
        private String transportMode;
        private String module;
        private Long entityId;
    }

    private PackingContext extractPackingContext(CalculatePackSummaryRequest request, String xSource) throws AuthenticationException, RunnerException {
        Long consolidationId = request.getConsolidationId();
        Long shipmentId = request.getShipmentEntityId();

        if (ObjectUtils.isNotEmpty(consolidationId)) {
            return createConsolidationContext(consolidationId, xSource);
        } else if (ObjectUtils.isNotEmpty(shipmentId)) {
            return createShipmentContext(shipmentId, xSource);
        } else {
            throw new IllegalArgumentException("Either Consolidation Id or Shipment Id must be provided.");
        }
    }

    private PackingContext createConsolidationContext(Long consolidationId, String xSource) throws AuthenticationException, RunnerException {
        ConsolidationDetails consolidation = getConsolidationDetails(consolidationId, xSource);

        List<Packing> packingList = new ArrayList<>();

        for (ShipmentDetails shipmentDetails : consolidation.getShipmentsList()) {
            packingList.addAll(shipmentDetails.getPackingList());
        }

        return PackingContext.builder()
                .packingList(packingList)
                .transportMode(consolidation.getTransportMode())
                .module(Constants.CONSOLIDATION)
                .entityId(consolidationId)
                .build();
    }

    public ConsolidationDetails getConsolidationDetails(Long consolidationId, String xSource) throws RunnerException, AuthenticationException {
        Optional<ConsolidationDetails> optionalConsolidationDetails;
        if (Objects.equals(xSource, NETWORK_TRANSFER)){
            optionalConsolidationDetails = consolidationV3Service.retrieveForNte(consolidationId);
        }else {
            optionalConsolidationDetails = consolidationV3Service.findById(consolidationId);
        }
        if(optionalConsolidationDetails.isEmpty()){
            throw new IllegalArgumentException("No Consolidation found with Id: " + consolidationId);
        }
        return optionalConsolidationDetails.get();
    }

    private PackingContext createShipmentContext(Long shipmentId, String xSource) throws AuthenticationException, RunnerException {
        ShipmentDetails shipment = getShipmentDetails(shipmentId, xSource);

        return PackingContext.builder()
            .packingList(shipment.getPackingList())
            .transportMode(shipment.getTransportMode())
            .module(Constants.SHIPMENT)
            .entityId(shipmentId)
            .build();
    }

    private ShipmentDetails getShipmentDetails(Long shipmentId, String xSource) throws RunnerException, AuthenticationException {
        Optional<ShipmentDetails> optionalShipmentDetails;
        if (Objects.equals(xSource, NETWORK_TRANSFER)){
            optionalShipmentDetails = shipmentService.retrieveForNte(CommonGetRequest.builder().id(shipmentId).build());
        }else {
            optionalShipmentDetails = shipmentService.findById(shipmentId);
        }
        if(optionalShipmentDetails.isEmpty()){
            throw new IllegalArgumentException("No Shipment found with Id: " + shipmentId);
        }
        return optionalShipmentDetails.get();
    }

    @Override
    public PackSummaryV3Response calculatePackSummary(CalculatePackSummaryRequest request, String xSource) throws AuthenticationException, RunnerException {
        PackingContext packingContext = extractPackingContext(request, xSource);
        List<Packing> packingList = packingContext.getPackingList();
        String transportMode = packingContext.getTransportMode();
        String module = packingContext.getModule();

        Long consolidationId = request.getConsolidationId();
        Long shipmentId = request.getShipmentEntityId();

        try {
            PackSummaryV3Response response = new PackSummaryV3Response();

            // Initialize totals
            double totalWeight = 0;
            double totalVolume = 0;
            double volumetricWeight;
            double chargeableWeight;

            // Initialize pack tracking
            int totalPacks = 0;
            int dgPacks = 0;
            int totalInnerPacks = 0;
            StringBuilder packsCount = new StringBuilder();
            String packsUnit = null;
            String innerPacksUnit = null;
            Map<String, Long> unitCountMap = new HashMap<>();

            // Determine units from settings
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();

            String toWeightUnit = !isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()) ?
                    shipmentSettingsDetails.getWeightChargeableUnit() : Constants.WEIGHT_UNIT_KG;

            String toVolumeUnit = !isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()) ?
                    shipmentSettingsDetails.getVolumeChargeableUnit() : Constants.VOLUME_UNIT_M3;

            // Loop over each packing entry
            if (packingList != null) {
                for (Packing packing : packingList) {
                    double convertedWeight = convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), toWeightUnit).doubleValue();
                    double convertedVolume = convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), toVolumeUnit).doubleValue();

                    totalWeight += convertedWeight;
                    totalVolume += convertedVolume;

                    packsUnit = getPacksUnit(packing, packsUnit, unitCountMap);
                    if (!isStringNullOrEmpty(packing.getPacks())) {
                        int packs = Integer.parseInt(packing.getPacks());
                        totalPacks += packs;
                        dgPacks = getDgPacks(packing, unitCountMap, packs, dgPacks);
                    }

                    totalInnerPacks = getTotalInnerPacks(packing, totalInnerPacks);
                    innerPacksUnit = getInnerPacksUnit(packing, innerPacksUnit);
                }
            }

            // Convert total volume and weight to standard units for further calculations
            double totalVolumeInM3 = convertUnit(Constants.VOLUME, BigDecimal.valueOf(totalVolume), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double totalWeightInKG = convertUnit(Constants.MASS, BigDecimal.valueOf(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();

            // Calculate volumetric and chargeable weight
            if (Constants.TRANSPORT_MODE_SEA.equals(transportMode)) {
                volumetricWeight = totalWeightInKG / 1000;
                chargeableWeight = Math.max(volumetricWeight, totalVolumeInM3);
            } else {
                double factor = Constants.AIR_FACTOR_FOR_VOL_WT;
                if (Constants.TRANSPORT_MODE_ROA.equals(transportMode)) {
                    factor = Constants.ROAD_FACTOR_FOR_VOL_WT;
                }
                volumetricWeight = totalVolumeInM3 * factor;
                chargeableWeight = Math.max(volumetricWeight, totalWeightInKG);
            }

            // Prepare packs count string
            List<String> sortedUnits = new ArrayList<>(unitCountMap.keySet());
            Collections.sort(sortedUnits);
            updatePacksCount(sortedUnits, unitCountMap, packsCount, tenantSettings);
            PackingAssignmentProjection assignedPackages = getAssignedPackages(module, consolidationId, shipmentId);

            // Fill response
            response.setDgPacks(dgPacks);
            response.setTotalPacksWithUnit(totalPacks + " " + commonUtils.getPacksUnit(packsUnit));
            response.setTotalPacks(packsCount.toString());
            response.setTotalPacksWeight(
                    String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), tenantSettings), toWeightUnit));
            response.setTotalPacksVolume(
                    String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), tenantSettings), toVolumeUnit));
            response.setPacksVolume(BigDecimal.valueOf(totalVolume));
            response.setPacksVolumeUnit(toVolumeUnit);
            response.setAchievedWeight(BigDecimal.valueOf(totalWeight));
            response.setAchievedVolume(BigDecimal.valueOf(totalVolume));
            response.setWeightUnit(toWeightUnit);
            response.setVolumeUnit(toVolumeUnit);

            // Set volumetric and chargeable weights
            setPacksVolumetricWeightInResponse(transportMode, response, volumetricWeight, tenantSettings);
            setChargeableWeightAndUnit(transportMode, chargeableWeight, totalVolume, toVolumeUnit, totalWeight, toWeightUnit, response, tenantSettings);

            // Set assigned and unassigned packages
            setPackageCount(assignedPackages, response);

            return response;
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }

    private PackingAssignmentProjection getAssignedPackages(String module, Long consolidationId, Long shipmentId) {
        PackingAssignmentProjection assignedPackages = null;
        if (module.equals(Constants.CONSOLIDATION)) {
            List<ConsoleShipmentMapping> consolidationDetailsEntity = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
            if (!ObjectUtils.isEmpty(consolidationDetailsEntity)) {
                List<Long> shipmentIds = consolidationDetailsEntity.stream().filter(ConsoleShipmentMapping::getIsAttachmentDone).map(ConsoleShipmentMapping::getShipmentId).toList();
                assignedPackages = packingDao.getPackingAssignmentCountByShipmentIn(shipmentIds);
            }
        } else {
            assignedPackages = packingDao.getPackingAssignmentCountByShipment(shipmentId);
        }

        return assignedPackages;
    }

    private void setPackageCount(PackingAssignmentProjection assignedPackages, PackSummaryV3Response response) {
        response.setAssignedPackageCount(assignedPackages != null && assignedPackages.getAssignedCount() != null ? assignedPackages.getAssignedCount() : 0);
        response.setUnassignedPackageCount(assignedPackages != null && assignedPackages.getUnassignedCount() != null ? assignedPackages.getUnassignedCount() : 0);
    }

    private void setChargeableWeightAndUnit(String transportMode, double chargeableWeight, double totalVolume, String toVolumeUnit,
                                            double totalWeight, String toWeightUnit, PackSummaryResponse response, V1TenantSettingsResponse v1TenantSettingsResponse) throws RunnerException {

        // Default unit for chargeable weight is kg, unless overridden by transport mode logic
        String packChargeableWeightUnit = Constants.WEIGHT_UNIT_KG;

        // Identify transport mode for conditional logic
        boolean isAir = Constants.TRANSPORT_MODE_AIR.equals(transportMode);
        boolean isSea = Constants.TRANSPORT_MODE_SEA.equals(transportMode);

        // For air shipments, chargeable weight is usually rounded to match standard billing practices (e.g., nearest 0.5kg or 1kg)
        if (isAir) {
            chargeableWeight = roundOffAirShipment(chargeableWeight);
        }

        // Sea freight uses different chargeable weight logic — based on actual volume in cubic meters vs weight in tons
        if (isSea) {
            // Convert total volume to cubic meters for comparison
            double volInM3 = convertUnit(VOLUME, BigDecimal.valueOf(totalVolume), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();

            // Convert total weight to kilograms, then derive tonnage (assumes 1 ton = 100 kg for this billing logic)
            double wtInKg = convertUnit(Constants.MASS, BigDecimal.valueOf(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();

            // Chargeable weight for sea is the greater of volume (in m³) vs weight in tons (derived by dividing kg by 100)
            chargeableWeight = Math.max(wtInKg / 1000, volInM3);

            // Unit is now volume-based (m³) instead of weight-based
            packChargeableWeightUnit = Constants.VOLUME_UNIT_M3;
        }

        // Round the final chargeable weight to 2 decimal places to align with currency/precision expectations
        chargeableWeight = BigDecimal.valueOf(chargeableWeight)
                .setScale(2, RoundingMode.HALF_UP)
                .doubleValue();

        // Format and set the user-facing field that includes the chargeable weight and its corresponding unit
        response.setPacksChargeableWeight(
                String.format(Constants.STRING_FORMAT,
                        IReport.convertToWeightNumberFormat(BigDecimal.valueOf(chargeableWeight), v1TenantSettingsResponse),
                        packChargeableWeightUnit));

        // Also set raw values to be used for any further internal computation or API responses
        response.setChargeableWeight(BigDecimal.valueOf(chargeableWeight));
        response.setPacksChargeableWeightUnit(packChargeableWeightUnit);
    }

    private void setPacksVolumetricWeightInResponse(String transportMode, PackSummaryResponse response,
                                                    double volumetricWeight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        // Sea freight typically treats volumetric weight as volume (in m³), not mass — this aligns with sea freight billing logic
        if (Objects.equals(transportMode, TRANSPORT_MODE_SEA)) {
            response.setPacksVolumetricWeight(
                    String.format(Constants.STRING_FORMAT,
                            IReport.convertToWeightNumberFormat(BigDecimal.valueOf(volumetricWeight), v1TenantSettingsResponse),
                            VOLUME_UNIT_M3));
        } else {
            // For air, road, and other modes, volumetric weight is represented as mass (usually in kg)
            response.setPacksVolumetricWeight(
                    String.format(Constants.STRING_FORMAT,
                            IReport.convertToWeightNumberFormat(BigDecimal.valueOf(volumetricWeight), v1TenantSettingsResponse),
                            WEIGHT_UNIT_KG));
        }
    }

    private void updatePacksCount(List<String> sortedKeys,
                                  Map<String, Long> packTypeToCountMap,
                                  StringBuilder packsCountBuilder,
                                  V1TenantSettingsResponse tenantSettings) {

        List<String> formattedPackCounts = new ArrayList<>();

        for (String packType : sortedKeys) {
            Long count = packTypeToCountMap.getOrDefault(packType, 0L);

            // Format the count according to tenant-specific rules (locale, grouping, etc.)
            String formattedCount = IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(count), 0, tenantSettings);

            // Combine formatted count and pack type (e.g., "10 CTN", "2 PALLET")
            formattedPackCounts.add(formattedCount + " " + packType);
        }

        // Join all formatted entries using comma separator for a clean, human-readable output
        packsCountBuilder.append(String.join(", ", formattedPackCounts));
    }

    private String getInnerPacksUnit(Packing packing, String currentInnerPacksUnit) {
        String packageType = packing.getInnerPackageType();

        // Proceed only if the package type is present
        if (!isStringNullOrEmpty(packageType)) {

            // If no unit has been set yet, assign the first non-null unit
            if (currentInnerPacksUnit == null) {
                currentInnerPacksUnit = packageType;
            }
            // If a different unit is found later, switch to generic 'MPK' (Mixed Pack)
            else if (!currentInnerPacksUnit.equals(packageType)) {
                currentInnerPacksUnit = MPK;
            }
        }

        return currentInnerPacksUnit;
    }

    private int getTotalInnerPacks(Packing packing, int totalInnerPacks) {
        // Check if inner package number is available and non-empty before processing
        if (!isStringNullOrEmpty(packing.getInnerPackageNumber())) {
            try {
                // Parse the inner package number and add to the running total
                int innerPacks = Integer.parseInt(packing.getInnerPackageNumber());
                totalInnerPacks += innerPacks;
            } catch (NumberFormatException e) {
                log.error("Error in getTotalInnerPacks to convert {} to integer for pack:{}", packing.getInnerPackageNumber(), packing.getId());
            }
        }
        return totalInnerPacks;
    }


    private int getDgPacks(Packing packing, Map<String, Long> map, int packs, int dgPacks) {
        // If packing type is not empty, accumulate the pack count per type
        if (!isStringNullOrEmpty(packing.getPacksType())) {
            map.put(packing.getPacksType(), map.getOrDefault(packing.getPacksType(), 0L) + packs);
        }

        // If the packing is hazardous, increment the dangerous goods (DG) packs
        if (Boolean.TRUE.equals(packing.getHazardous())) {
            dgPacks += packs;
        }
        return dgPacks;
    }

    private String getPacksUnit(Packing packing, String packsUnit, Map<String, Long> map) {
        // If the pack type is not empty, update the packs unit and initialize map entry if absent
        if (!isStringNullOrEmpty(packing.getPacksType())) {
            // Call to determine packs unit based on packing type
            packsUnit = commonUtils.getPacksUnit(packsUnit, packing.getPacksType());

            // If this packing type is not already in the map, initialize its count to 0
            map.putIfAbsent(packing.getPacksType(), 0L);
        }
        return packsUnit;
    }


    @Override
    public Map<String, Object> getAllMasterData(Long id, String source) {
        try {
            Optional<Packing> packingOptional;
            if (Objects.equals(source, NETWORK_TRANSFER))
                packingOptional = packingDao.findByIdWithQuery(id);
            else
                packingOptional = packingDao.findById(id);
            if (packingOptional.isEmpty()) {
                log.debug(PackingConstants.PACKING_RETRIEVE_BY_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            Packing packing = packingOptional.get();
            PackingResponse packingResponse = convertEntityToDto(packing);
            long start = System.currentTimeMillis();
            log.info("Total time taken in fetching Packing MasterData response {}", (System.currentTimeMillis() - start));
            return fetchAllMasterDataByKey(packingResponse);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return new HashMap<>();
        }
    }

    @Override
    public Map<String, Object> fetchAllMasterDataByKey(PackingResponse packingResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllMasterDataInSingleCall(packingResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllUnlocationDataInSingleCall(packingResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllCommodityTypesInSingleCall(packingResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, commodityTypesFuture).join();

        return masterDataResponse;
    }

    @Override
    public List<Long> filterContainerIdsAttachedToPacking(List<Long> containerIds) {
        return List.of();
    }

    private PackingResponse convertEntityToDto(Packing packing) {
        return jsonHelper.convertValue(packing, PackingResponse.class);
    }

    private List<PackingResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<PackingResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> responseList.add(convertEntityToDto(containers, defaultIncludeColumns)));
        return responseList;
    }

    private PackingResponse convertEntityToDto(Packing packing, List<String> includesFields) {
        return (PackingResponse) commonUtils.setIncludedFieldsToResponse(packing, new HashSet<>(includesFields), new PackingResponse());
    }

    public ParentResult getParentDetails(List<Packing> packingList, String moduleType) {
        Packing firstPacking = packingList.get(0);

        return switch (moduleType) {
            case Constants.SHIPMENT ->
                    new ParentResult(ShipmentDetails.class.getSimpleName(), firstPacking.getShipmentId());
            case Constants.CONSOLIDATION ->
                    new ParentResult(ConsolidationDetails.class.getSimpleName(), firstPacking.getConsolidationId());
            case Constants.BOOKING ->
                    new ParentResult(CustomerBooking.class.getSimpleName(), firstPacking.getBookingId());
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        };
    }

    private CargoDetailsResponse calculateCargoDetails(List<Packing> packings, CargoDetailsResponse response) throws RunnerException {
        Integer totalPacks = 0;
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        response.setVolume(totalVolume);
        response.setNoOfPacks(totalPacks);
        response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        response.setPacksUnit(Constants.PACKAGES);
        response.setDgPacksUnit(Constants.PACKAGES);
        Set<String> uniquePacksUnits = new HashSet<>();
        if (!CollectionUtils.isEmpty(packings)) {
            populateSummaryDetails(packings, response, uniquePacksUnits, totalWeight, totalVolume, totalPacks);
            response = calculateVW(response);
        }
        return response;
    }

    private void populateSummaryDetails(List<Packing> packings, CargoDetailsResponse response, Set<String> uniquePacksUnits, BigDecimal totalWeight, BigDecimal totalVolume, Integer totalPacks) throws RunnerException {
        Set<String> dgPacksUnitSet = new HashSet<>();
        int dgPacksCount = 0;
        boolean skipWeightInCalculation = false;
        if (TRANSPORT_MODE_AIR.equals(response.getTransportMode())) {
            skipWeightInCalculation = packings.stream()
                    .anyMatch(packing -> packing.getWeight() == null);
        }
        for (Packing packing : packings) {
            setUniquePacksUnit(uniquePacksUnits, dgPacksUnitSet, packing);
            if (!skipWeightInCalculation && packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit())) {
                totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
            }
            if (packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
                totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
            }
            if (!isStringNullOrEmpty(packing.getPacks())) {
                if (packing.getHazardous() != null && packing.getHazardous()) {
                    dgPacksCount = dgPacksCount + Integer.parseInt(packing.getPacks());
                }
                totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
            }
        }
        response.setNoOfPacks(totalPacks);
        if (!BigDecimal.ZERO.equals(totalWeight)) {
            response.setWeight(totalWeight);
        }
        response.setVolume(totalVolume);
        response.setDgPacks(dgPacksCount);
        setPacksUnits(response, uniquePacksUnits, dgPacksUnitSet);
    }

    private static void setUniquePacksUnit(Set<String> uniquePacksUnits, Set<String> dgPacksUnitSet, Packing packing) {
        if (!StringUtility.isEmpty(packing.getPacksType())) {
            if (packing.getHazardous() != null && packing.getHazardous()) {
                dgPacksUnitSet.add(packing.getPacksType());
                uniquePacksUnits.add(packing.getPacksType());
            } else {
                uniquePacksUnits.add(packing.getPacksType());
            }
        }
    }

    public void setPacksUnits(CargoDetailsResponse response, Set<String> uniquePacksUnits, Set<String> dgPacksUnitSet) {
        if (uniquePacksUnits.size() == 1) {
            response.setPacksUnit(uniquePacksUnits.iterator().next());
        }
        if (dgPacksUnitSet.size() == 1) {
            response.setDgPacksUnit(dgPacksUnitSet.iterator().next());
        }
    }

    private CargoDetailsResponse calculateVW(CargoDetailsResponse response) throws RunnerException {
        if (isStringNullOrEmpty(response.getTransportMode()))
            return response;
        if (!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable weightChargeable = consolidationV3Service.calculateVolumeWeight(response.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(weightChargeable.getChargeable());
            response.setChargeableUnit(weightChargeable.getChargeableUnit());
            response.setVolumetricWeight(weightChargeable.getVolumeWeight());
            response.setVolumetricWeightUnit(weightChargeable.getVolumeWeightUnit());
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

    @Override
    public void processPacksAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipmentDetails.getPackingList() != null) {
            List<Packing> packingList = shipmentDetails.getPackingList();
            for (Packing packing : packingList) {
                packing.setConsolidationId(consolidationId);
            }
            packingDao.saveAll(packingList);
        }
    }


    @PostConstruct
    private void setDefaultIncludeColumns() {
        defaultIncludeColumns = FieldUtils.getNonRelationshipFields(Packing.class);
        defaultIncludeColumns.addAll(List.of("id", "guid", "tenantId"));
    }

    private ShipmentDetails getShipment(ShipmentDetails shipmentDetails, Long shipmentId) {
        if (shipmentDetails != null)
            return shipmentDetails;
        Optional<ShipmentDetails> shipmentEntity = shipmentService.findById(shipmentId);
        return shipmentEntity.orElse(null);
    }

    public boolean checkIfLCLConsolidationEligible(ShipmentDetails shipmentDetails) {
        if (!Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()))
            return false;
        if (!Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
            return false;
        if (!Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType()))
            return false;
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    public void updateShipmentGateInDateAndStatusFromPacks(ShipmentDetails shipmentDetails, List<Packing> packings) throws RunnerException {
        ShipmentDetails tempShipment = new ShipmentDetails();
        tempShipment.setId(shipmentDetails.getId());
        tempShipment.setShipmentPackStatus(null);
        tempShipment.setCarrierDetails(shipmentDetails.getCarrierDetails());
        tempShipment.setShipmentGateInDate(shipmentDetails.getShipmentGateInDate());
        tempShipment.setDateType(shipmentDetails.getDateType());
        if (!CommonUtils.listIsNullOrEmpty(packings)) {
            tempShipment.setShipmentPackStatus(ShipmentPackStatus.BOOKED);
            packingV3Util.processPackingRequests(packings, tempShipment);
        }
        packingV3Util.setShipmentPackStatusSailed(tempShipment);
        packingValidationV3Util.validateShipmentGateInDate(tempShipment);
        shipmentService.updateShipmentDetailsFromPacks(
                shipmentDetails.getId(), tempShipment.getDateType(),
                tempShipment.getShipmentGateInDate(), tempShipment.getShipmentPackStatus()
        );
    }

    private void updateAttachedContainersData(List<Packing> packings, ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails == null ||
                !(TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode()) ||
                        TRANSPORT_MODE_ROA.equalsIgnoreCase(shipmentDetails.getTransportMode())))
            return;
        Set<Long> containerIdsToUpdate = new HashSet<>();
        packings.forEach(e -> {
            if (Objects.nonNull(e.getContainerId()))
                containerIdsToUpdate.add(e.getContainerId());
        });
        if (Objects.nonNull(shipmentDetails.getContainerAssignedToShipmentCargo()))
            containerIdsToUpdate.add(shipmentDetails.getContainerAssignedToShipmentCargo());
        containerV3Service.updateAttachedContainersData(containerIdsToUpdate.stream().toList());
    }

    @Override
    public Map<Long, ContainerInfoProjection> getContainerIdNumberMap(Set<Long> containerIds) {
        List<ContainerInfoProjection> containerInfoProjections = containerV3Service.getContainers(containerIds.stream().toList());
        return containerInfoProjections.stream()
                .collect(Collectors.toMap(
                        ContainerInfoProjection::getId,
                        Function.identity(),
                        (existing, replacement) -> existing
                ));
    }

    @Override
    public ContainerResponse assignPackagesContainers(AssignContainerRequest request) throws RunnerException {
        if(request.getShipmentPackIds().size() > 1) {
            throw new ValidationException("Please select Packages of single shipment only for assignment.");
        }
        return containerV3Service.assignContainers(request, Constants.CONSOLIDATION_PACKING);
    }

    @Override
    public ContainerResponse assignShipmentPackagesContainers(ShipmentPackAssignmentRequest request) throws RunnerException {
        if(listIsNullOrEmpty(request.getPackingIds())) {
            throw new ValidationException("No Packing Ids provided.");
        }
        List<Packing> packingList = packingDao.findByIdIn(request.getPackingIds());
        if(listIsNullOrEmpty(packingList)) {
            throw new ValidationException("No Packing found with Ids: " + request.getPackingIds());
        }
        if(packingList.stream().map(Packing::getShipmentId).distinct().count() > 1) {
            throw new ValidationException("Please select Packages of single shipment only for assignment.");
        }
        Long shipmentId = packingList.get(0).getShipmentId();
        ShipmentDetails shipmentDetails = shipmentService.findById(shipmentId).orElseThrow(() -> new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        if(!Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType()) && !Constants.CARGO_TYPE_FTL.equalsIgnoreCase(shipmentDetails.getShipmentType())) {
            throw new ValidationException("Shipment level package assignment is only allowed for FCL/FTL shipments.");
        }
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(request.getContainerId());
        assignContainerRequest.setShipmentPackIds(Map.of(shipmentId, request.getPackingIds()));
        return containerV3Service.assignContainers(assignContainerRequest, SHIPMENT_PACKING);
    }

    @Override
    public void unAssignPackageContainers(UnAssignPackageContainerRequest request, String module) throws RunnerException {
        List<Packing> packingList = packingDao.findByIdIn(request.getPackingIds());
        List<UnAssignContainerRequest> unAssignContainerRequests = new ArrayList<>();
        Map<Long, List<Packing>> containerIdPacksIdMap = new HashMap<>();
        Set<Long> shipmentIds = new HashSet<>();
        for(Packing packing: packingList) {
            if(packing.getContainerId() == null) {
                throw new ValidationException("Container Id is null for packing with Id: " + packing.getId());
            }
            containerIdPacksIdMap.computeIfAbsent(packing.getContainerId(), k -> new ArrayList<>());
            containerIdPacksIdMap.get(packing.getContainerId()).add(packing);
            shipmentIds.add(packing.getShipmentId());
        }
        if(shipmentIds.size() > 1)
            throw new ValidationException("Please select Packages of only one shipment for unAssign action");
        for(Map.Entry<Long, List<Packing>> entry: containerIdPacksIdMap.entrySet()) {
            UnAssignContainerRequest unAssignContainerRequest = new UnAssignContainerRequest();
            unAssignContainerRequest.setContainerId(entry.getKey());
            Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
            for(Packing packing: entry.getValue()) {
                shipmentPackIds.computeIfAbsent(packing.getShipmentId(), k -> new ArrayList<>());
                shipmentPackIds.get(packing.getShipmentId()).add(packing.getId());
            }
            unAssignContainerRequest.setShipmentPackIds(shipmentPackIds);
            unAssignContainerRequests.add(unAssignContainerRequest);
        }
        UnAssignContainerParams unAssignContainerParams = new UnAssignContainerParams();
        for(UnAssignContainerRequest unAssignContainerRequest: unAssignContainerRequests) {
            containerV3Service.unAssignContainers(unAssignContainerRequest, module, unAssignContainerParams);
        }
        // update shipments and consolidations data only for FCL/FTL shipments
        updateShipmentAndContainerDataForFCLAndFTLShipments(unAssignContainerParams);
    }

    public void updateShipmentAndContainerDataForFCLAndFTLShipments(UnAssignContainerParams unAssignContainerParams) throws RunnerException {
        if(!setIsNullOrEmpty(unAssignContainerParams.getFclOrFtlShipmentIds())) {
            for(Long shipmentId: unAssignContainerParams.getFclOrFtlShipmentIds()) {
                ShipmentDetails shipmentDetails = unAssignContainerParams.getShipmentDetailsMap().get(shipmentId);
                shipmentService.calculateAndUpdateShipmentCargoSummary(shipmentDetails);
            }
            consolidationV3Service.updateConsolidationCargoSummary(unAssignContainerParams.getConsolidationDetails(), unAssignContainerParams.getOldShipmentWtVolResponse());
        }
    }

    private void updatePackingRequestOnDgAndTemperatureFlag(List<PackingV3Request> packingV3RequestList) {
        for(PackingV3Request packingRequest: packingV3RequestList) {
            if(Boolean.FALSE.equals(packingRequest.getHazardous())) {
                updatePackingRequestWithDgFalse(packingRequest);
            }
            if(Boolean.FALSE.equals(packingRequest.getIsTemperatureControlled())) {
                updatePackingRequestWithTemperatureFalse(packingRequest);
            }
        }
    }

    private void updatePackingRequestWithDgFalse(PackingV3Request packingV3Request) {
        packingV3Request.setDgClassAir(null);
        packingV3Request.setDgClassAirDescription(null);
        packingV3Request.setUnNumberAir(null);
        packingV3Request.setDGClass(null);
        packingV3Request.setUnNumber(null);
        packingV3Request.setProperShippingName(null);
        packingV3Request.setPackingGroup(null);
        packingV3Request.setMinimumFlashPoint(null);
        packingV3Request.setMarinePollutant(null);
    }

    private void updatePackingRequestWithTemperatureFalse(PackingV3Request packingV3Request) {
        packingV3Request.setMinTemp(null);
        packingV3Request.setMaxTemp(null);
    }
}
