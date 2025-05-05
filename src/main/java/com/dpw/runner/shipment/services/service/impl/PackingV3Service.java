package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import com.dpw.runner.shipment.services.utils.v3.PackingValidationV3Util;
import com.nimbusds.jose.util.Pair;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;


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
    private IShipmentServiceV3 shipmentService;

    @Autowired
    private IConsolidationService consolidationService;

    private List<String> defaultIncludeColumns = new ArrayList<>();

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

        log.info("Starting packing creation | Request ID: {} | Request Body: {}", requestId, packingRequest);
        packingValidationV3Util.validateModule(packingRequest, module);
        // Convert DTO to Entity
        Packing packing = jsonHelper.convertValue(packingRequest, Packing.class);
        log.debug("Converted packing request to entity | Entity: {}", packing);

        // Save to DB
        Packing savedPacking = packingDao.save(packing);
        log.info("Saved packing entity to DB | Packing ID: {} | Request ID: {}", savedPacking.getId(), requestId);

        ParentResult parentResult = getParentDetails(List.of(savedPacking), module);
        // Audit logging
        recordAuditLogs(null, List.of(savedPacking), DBOperationType.CREATE, parentResult);
        log.info("Audit log recorded for packing creation | Packing ID: {}", savedPacking.getId());

        PackingResponse response = jsonHelper.convertValue(savedPacking, PackingResponse.class);
        log.info("Returning packing response | Packing ID: {} | Response: {}", savedPacking.getId(), response);
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            updateCargoDetailsInShipment(packingRequest.getShipmentId());
        }
        return response;
    }

    private void updateCargoDetailsInShipment(Long shipmentId) throws RunnerException {
        List<Packing> packings = packingDao.findByShipmentId(shipmentId);
        if (!CollectionUtils.isEmpty(packings)) {
            Optional<ShipmentDetails> shipmentEntity = shipmentService.findById(shipmentId);
            if (shipmentEntity.isPresent()) {
                ShipmentDetails shipmentDetails = shipmentEntity.get();
                CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();
                cargoDetailsResponse.setTransportMode(shipmentDetails.getTransportMode());
                cargoDetailsResponse.setShipmentType(shipmentDetails.getShipmentType());
                cargoDetailsResponse = calculateCargoDetails(packings, cargoDetailsResponse);
                shipmentService.updateCargoDetailsInShipment(shipmentId, cargoDetailsResponse);
            }
        }
    }



    @Override
    @Transactional
    public PackingResponse update(PackingV3Request packingRequest, String module) throws RunnerException {
        packingValidationV3Util.validateUpdateRequest(packingRequest);
        Optional<Packing> optionalPacking = packingDao.findById(packingRequest.getId());
        if (optionalPacking.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        packingValidationV3Util.validateModule(packingRequest, module);
        Packing oldPacking = optionalPacking.get();
        Packing oldConvertedPacking = jsonHelper.convertValue(oldPacking, Packing.class);
        Packing newPacking = jsonHelper.convertValue(packingRequest, Packing.class);

        Packing updatedPacking = packingDao.save(newPacking);

        ParentResult parentResult = getParentDetails(List.of(updatedPacking), module);

        recordAuditLogs(List.of(oldConvertedPacking), List.of(updatedPacking), DBOperationType.UPDATE, parentResult);
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            updateCargoDetailsInShipment(packingRequest.getShipmentId());
        }
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
        packingDao.delete(packing);

        ParentResult parentResult = getParentDetails(List.of(packing), module);

        recordAuditLogs(List.of(packing), null, DBOperationType.DELETE, parentResult);

        String packs = packing.getPacks();
        String packsType = packing.getPacksType();
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            updateCargoDetailsInShipment(packing.getShipmentId());
        }
        return packsType != null
                ? String.format("Packing %s - %s deleted successfully!", packs, packsType)
                : String.format("Packing %s deleted successfully!", packs);
    }

    @Override
    @Transactional
    public BulkPackingResponse updateBulk(List<PackingV3Request> packingRequestList, String module) throws RunnerException {
        // Separate IDs and determine existing packings
        List<Long> incomingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        List<Packing> existingPackings = packingDao.findByIdIn(incomingIds);

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

        // Convert and process updates
        List<Packing> oldConvertedPackings = jsonHelper.convertValueToList(existingPackings, Packing.class);
        List<Packing> updatedPackings = jsonHelper.convertValueToList(updateRequests, Packing.class);
        List<Packing> savedUpdatedPackings = packingDao.saveAll(updatedPackings);

        // Convert and process creates
        List<Packing> newPackings = jsonHelper.convertValueToList(createRequests, Packing.class);
        List<Packing> savedNewPackings = packingDao.saveAll(newPackings);

        // Combine results for parent calculation and auditing
        List<Packing> allSavedPackings = new ArrayList<>();
        allSavedPackings.addAll(savedNewPackings);
        allSavedPackings.addAll(savedUpdatedPackings);

        ParentResult parentResult = getParentDetails(allSavedPackings, module);

        // Audit logs
        recordAuditLogs(oldConvertedPackings, savedUpdatedPackings, DBOperationType.UPDATE, parentResult);
        recordAuditLogs(null, savedNewPackings, DBOperationType.CREATE, parentResult);

        // Convert to response
        List<PackingResponse> packingResponses = jsonHelper.convertValueToList(allSavedPackings, PackingResponse.class);
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            updateCargoDetailsInShipment(existingPackings.get(0).getShipmentId());
        }
        return BulkPackingResponse.builder()
                .packingResponseList(packingResponses)
                .message(prepareBulkUpdateMessage(packingResponses))
                .build();
    }

    @Override
    @Transactional
    public BulkPackingResponse deleteBulk(List<PackingV3Request> packingRequestList, String module) {
        packingValidationV3Util.validateDeleteBulkRequest(packingRequestList);
        // Extract unique packing IDs from the request
        List<Long> packingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .distinct()
                .toList();

        // Fetch packings from DB to ensure they exist before deletion
        List<Packing> packingsToDelete = packingDao.findByIdIn(packingIds);

        if (packingsToDelete.isEmpty()) {
            throw new DataRetrievalFailureException("No packing found for the given Ids.");
        }

        // Validate that all necessary packing IDs are present in the request
        packingValidationV3Util.validateUpdateBulkRequest(packingRequestList, packingsToDelete);

        ParentResult parentResult = getParentDetails(packingsToDelete, module);

        // Delete packings from DB
        packingDao.deleteByIdIn(packingIds);

        // Record audit logs for the deletion operation
        recordAuditLogs(packingsToDelete, null, DBOperationType.DELETE, parentResult);

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

    @Override
    public PackingResponse retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<Packing> packing;
            if (id != null) {
                packing = packingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                packing = packingDao.findByGuid(guid);
            }
            if (packing.isEmpty()) {
                log.debug(PackingConstants.PACKING_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
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
    public PackingListResponse list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Packing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        // construct specifications for filter request
        Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
        Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        List<PackingResponse> responseList = convertEntityListToDtoList(packingPage.getContent());
        this.getMasterDataForList(responseList, getMasterData);
        PackingListResponse packingListResponse = new PackingListResponse();
        packingListResponse.setPackings(responseList);
        packingListResponse.setTotalPages(packingPage.getTotalPages());
        packingListResponse.setTotalCount(packingPage.getTotalElements());
        return packingListResponse;
    }

    private void getMasterDataForList(List<PackingResponse> responseList, boolean getMasterData) {
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllUnlocationInSingleCallList(responseList)), executorServiceMasterData);
                var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllMasterDataInSingleCallList(responseList)), executorServiceMasterData);
                var commodityTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> packingV3Util.addAllCommodityTypesInSingleCall(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, masterDataFuture, commodityTypeFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.PACKING_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_PACKING_LIST, ex.getLocalizedMessage());
            }
        }
    }

    @Override
    public PackingListResponse fetchShipmentPackages(CommonRequestModel commonRequestModel) {
        PackingListResponse packingListResponse = list(commonRequestModel, true);
        log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        if (!CollectionUtils.isEmpty(packingListResponse.getPackings())) {
            //get assigned packages
            Long shipmentId = null;
            for (PackingResponse packingResponse : packingListResponse.getPackings()) {
                if (packingResponse.getShipmentId() != null) {
                    shipmentId = packingResponse.getShipmentId();
                    break;
                }
            }
            if (shipmentId != null) {
                PackingAssignmentProjection assignedPackages = packingDao.getPackingAssignmentCountByShipment(shipmentId);
                packingListResponse.setAssignedPackageCount(assignedPackages.getAssignedCount());
                packingListResponse.setUnassignedPackageCount(assignedPackages.getUnassignedCount());
            }

        }
        return packingListResponse;
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
        response.setWeight(totalWeight);
        response.setVolume(totalVolume);
        response.setNoOfPacks(totalPacks);
        response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        response.setPacksUnit(Constants.PACKAGES);
        if (!CollectionUtils.isEmpty(packings)) {
            for (Packing packing : packings) {
                if (packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if (packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
                if (!isStringNullOrEmpty(packing.getPacks())) {
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                }
            }
            response.setNoOfPacks(totalPacks);
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(response);
        }
        return response;
    }

    private CargoDetailsResponse calculateVW(CargoDetailsResponse response) throws RunnerException {
        if (isStringNullOrEmpty(response.getTransportMode()))
            return response;
        if (!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if (Constants.TRANSPORT_MODE_AIR.equals(response.getTransportMode())) {
                response.setChargable(BigDecimal.valueOf(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if (Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && !isStringNullOrEmpty(response.getShipmentType()) && Constants.SHIPMENT_TYPE_LCL.equals(response.getShipmentType())) {
                double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                response.setChargable(BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3)));
                response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
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
}
