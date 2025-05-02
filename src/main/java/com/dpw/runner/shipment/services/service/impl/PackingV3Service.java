package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import com.dpw.runner.shipment.services.utils.v3.PackingValidationV3Util;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;


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

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ParentResult {
        private String parent;
        private Long parentId;
    }

    @Override
    @Transactional
    public PackingResponse create(PackingV3Request packingRequest, String module) {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting packing creation | Request ID: {} | Request Body: {}", requestId, packingRequest);

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

        return response;
    }

    @Override
    @Transactional
    public PackingResponse update(PackingV3Request packingRequest, String module) throws RunnerException {
        packingValidationV3Util.validateUpdateRequest(packingRequest);
        Optional<Packing> optionalPacking = packingDao.findById(packingRequest.getId());
        if (optionalPacking.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Packing oldPacking = optionalPacking.get();
        Packing oldConvertedPacking = jsonHelper.convertValue(oldPacking, Packing.class);
        Packing newPacking = jsonHelper.convertValue(packingRequest, Packing.class);

        Packing updatedPacking = packingDao.save(newPacking);

        ParentResult parentResult = getParentDetails(List.of(updatedPacking), module);

        recordAuditLogs(List.of(oldConvertedPacking), List.of(updatedPacking), DBOperationType.UPDATE, parentResult);

        return convertEntityToDto(updatedPacking);
    }

    @Override
    @Transactional
    public String delete(Long id, String module) {
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

        return packsType != null
                ? String.format("Packing %s - %s deleted successfully!", packs, packsType)
                : String.format("Packing %s deleted successfully!", packs);
    }

    @Override
    @Transactional
    public BulkPackingResponse updateBulk(List<PackingV3Request> packingRequestList, String module) {
        // Validate the incoming request to ensure all mandatory fields are present
        packingValidationV3Util.validateUpdateBulkRequest(packingRequestList);

        List<Long> packingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .distinct()
                .toList();

        List<Packing> oldPackings = packingDao.findByIdIn(packingIds);

        List<Packing> oldConvertedPackings = jsonHelper.convertValueToList(oldPackings, Packing.class);
        // Convert the request DTOs to entity models for persistence
        List<Packing> newPackings = jsonHelper.convertValueToList(packingRequestList, Packing.class);

        // Save the updated packings to the database
        List<Packing> updatedPackings = packingDao.saveAll(newPackings);

        ParentResult parentResult = getParentDetails(updatedPackings, module);

        // Add audit logs for all updated packings
        recordAuditLogs(oldConvertedPackings, updatedPackings, DBOperationType.UPDATE, parentResult);

        // Convert saved entities into response DTOs
        List<PackingResponse> packingResponses = jsonHelper.convertValueToList(updatedPackings, PackingResponse.class);

        // Build and return the response
        return BulkPackingResponse.builder()
                .packingResponseList(packingResponses)
                .message(prepareBulkUpdateMessage(packingResponses))
                .build();
    }

    @Override
    @Transactional
    public BulkPackingResponse deleteBulk(List<PackingV3Request> packingRequestList, String module) {
        // Validate that all necessary packing IDs are present in the request
        packingValidationV3Util.validateUpdateBulkRequest(packingRequestList);

        // Extract unique packing IDs from the request
        List<Long> packingIds = packingRequestList.stream()
                .map(PackingV3Request::getId)
                .distinct()
                .toList();

        // Fetch packings from DB to ensure they exist before deletion
        List<Packing> packingsToDelete = packingDao.findByIdIn(packingIds);

        if (packingsToDelete.isEmpty()) {
            throw new IllegalArgumentException("No packing found for the given IDs.");
        }

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
            // For a single packing update, include packs and optionally its packsType
            PackingResponse response = packingResponses.get(0);
            String packs = response.getPacks();
            String packsType = response.getPacksType();

            message = packsType != null
                    ? String.format("Packing %s - %s updated successfully!", packs, packsType)
                    : String.format("Packing %s updated successfully!", packs);
        }

        return message;
    }

    private String prepareBulkDeleteMessage(List<Packing> packings) {
        String message;

        // If more than one packing was deleted, return a generic bulk success message
        if (packings.size() > 1) {
            message = "Packings deleted successfully!";
        } else {
            // For a single packing delete, include packs and optionally its packsType
            Packing response = packings.get(0);
            String packs = response.getPacks();
            String packsType = response.getPacksType();

            message = packsType != null
                    ? String.format("Packing %s - %s deleted successfully!", packs, packsType)
                    : String.format("Packing %s deleted successfully!", packs);
        }

        return message;
    }

    @Override
    public void downloadPacking(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException {
        packingV3Util.downloadPacking(response, request);
    }

    @Override
    public PackingResponse retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<Packing> packing;
            if(id != null) {
                packing = packingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                packing = packingDao.findByGuid(guid);
            }
            if(packing.isEmpty()) {
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

    private PackingResponse convertEntityToDto(Packing packing) {
        return jsonHelper.convertValue(packing, PackingResponse.class);
    }

    public ParentResult getParentDetails(List<Packing> packingList, String moduleType) {
        Packing firstPacking = packingList.get(0);

        return switch (moduleType) {
            case Constants.SHIPMENT -> new ParentResult(ShipmentDetails.class.getSimpleName(), firstPacking.getShipmentId());
            case Constants.CONSOLIDATION ->
                    new ParentResult(ConsolidationDetails.class.getSimpleName(), firstPacking.getConsolidationId());
            case Constants.BOOKING -> new ParentResult(CustomerBooking.class.getSimpleName(), firstPacking.getBookingId());
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        };
    }

}
