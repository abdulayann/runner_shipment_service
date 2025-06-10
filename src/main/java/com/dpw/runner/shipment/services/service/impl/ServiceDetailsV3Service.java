package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.constants.ServiceDetailsConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IServiceDetailsV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.ServiceDetailsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ServiceDetailsValidationV3Util;
import com.nimbusds.jose.util.Pair;
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

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_ID;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;


@Service
@Slf4j
public class ServiceDetailsV3Service implements IServiceDetailsV3Service {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ServiceDetailsValidationV3Util serviceDetailsValidationV3Util;

    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private ServiceDetailsV3Util serviceDetailsV3Util;

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
    public ServiceDetailsResponse create(ServiceDetailsRequest serviceDetailsRequest, String module) throws RunnerException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Service details creation | Request ID: {} | Request Body: {}", requestId, serviceDetailsRequest);
        serviceDetailsValidationV3Util.validateModule(serviceDetailsRequest, module);
        // Convert DTO to Entity
        ServiceDetails serviceDetails = jsonHelper.convertValue(serviceDetailsRequest, ServiceDetails.class);
        log.debug("Converted service details request to entity | Entity: {}", serviceDetails);

        // Save to DB
        ServiceDetails savedServiceDetails = serviceDetailsDao.save(serviceDetails);
        log.info("Saved service details entity to DB | Service details ID: {} | Request ID: {}", savedServiceDetails.getId(), requestId);

        ParentResult parentResult = getParentDetails(List.of(savedServiceDetails), module);
        // Audit logging
        recordAuditLogs(null, List.of(savedServiceDetails), DBOperationType.CREATE, parentResult);
        log.info("Audit log recorded for service details creation | Service details ID: {}", savedServiceDetails.getId());

        ServiceDetailsResponse response = jsonHelper.convertValue(savedServiceDetails, ServiceDetailsResponse.class);
        log.info("Returning service details response | Service details ID: {} | Response: {}", savedServiceDetails.getId(), response);
        return response;
    }

    @Override
    @Transactional
    public String delete(Long id, String module) throws RunnerException {
        if (id == null) {
            throw new IllegalArgumentException("Id cannot be null or empty.");
        }
        Optional<ServiceDetails> optionalServiceDetails = serviceDetailsDao.findById(id);
        if (optionalServiceDetails.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ServiceDetails serviceDetails = optionalServiceDetails.get();
        serviceDetailsDao.delete(serviceDetails);

        ParentResult parentResult = getParentDetails(List.of(serviceDetails), module);

        recordAuditLogs(List.of(serviceDetails), null, DBOperationType.DELETE, parentResult);

        String serviceType = serviceDetails.getServiceType();
        Long serviceCount = serviceDetails.getServiceCount();
        return serviceCount != null
                ? String.format("Service Details %s - %s deleted successfully!", serviceType, serviceCount)
                : String.format("Service Details %s deleted successfully!", serviceType);
    }

    @Override
    @Transactional
    public BulkServiceDetailsResponse updateBulk(List<ServiceDetailsRequest> serviceDetailsRequestList, String module) throws RunnerException {
        serviceDetailsValidationV3Util.validateSameParentId(serviceDetailsRequestList, module);
        // Separate IDs and determine existing service details
        List<Long> incomingIds = serviceDetailsRequestList.stream()
                .map(ServiceDetailsRequest::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        serviceDetailsValidationV3Util.validateModule(serviceDetailsRequestList.get(0), module);

        List<ServiceDetails> existingServiceDetails = new ArrayList<>();
        if (!CommonUtils.listIsNullOrEmpty(incomingIds)) {
            existingServiceDetails = serviceDetailsDao.findByIdIn(incomingIds);
        }

        // Validate incoming request
        serviceDetailsValidationV3Util.validateUpdateBulkRequest(serviceDetailsRequestList, existingServiceDetails);

        // Separate into create and update requests
        List<ServiceDetailsRequest> updateRequests = new ArrayList<>();
        List<ServiceDetailsRequest> createRequests = new ArrayList<>();

        for (ServiceDetailsRequest request : serviceDetailsRequestList) {
            if (request.getId() != null && incomingIds.contains(request.getId())) {
                updateRequests.add(request);
            } else {
                createRequests.add(request);
            }
        }

        // Convert and process updates
        List<ServiceDetails> oldConvertedServiceDetails = jsonHelper.convertValueToList(existingServiceDetails, ServiceDetails.class);
        List<ServiceDetails> updatedServiceDetails = jsonHelper.convertValueToList(updateRequests, ServiceDetails.class);

        // Convert and process creates
        List<ServiceDetails> newServiceDetails = jsonHelper.convertValueToList(createRequests, ServiceDetails.class);

        List<ServiceDetails> savedUpdatedServiceDetails = CommonUtils.listIsNullOrEmpty(updatedServiceDetails) ? Collections.emptyList() : serviceDetailsDao.saveAll(updatedServiceDetails);
        List<ServiceDetails> savedNewServiceDetails = CommonUtils.listIsNullOrEmpty(newServiceDetails) ? Collections.emptyList() : serviceDetailsDao.saveAll(newServiceDetails);

        // Combine results for parent calculation and auditing
        List<ServiceDetails> allSavedServiceDetails = new ArrayList<>();
        allSavedServiceDetails.addAll(savedNewServiceDetails);
        allSavedServiceDetails.addAll(savedUpdatedServiceDetails);

        ParentResult parentResult = getParentDetails(allSavedServiceDetails, module);

        // Audit logs
        recordAuditLogs(oldConvertedServiceDetails, savedUpdatedServiceDetails, DBOperationType.UPDATE, parentResult);
        recordAuditLogs(null, savedNewServiceDetails, DBOperationType.CREATE, parentResult);

        // Convert to response
        List<ServiceDetailsResponse> serviceDetailsResponses = jsonHelper.convertValueToList(allSavedServiceDetails, ServiceDetailsResponse.class);

        return BulkServiceDetailsResponse.builder()
                .serviceDetailsResponseList(serviceDetailsResponses)
                .message(prepareBulkUpdateMessage(serviceDetailsResponses))
                .build();
    }

    @Override
    @Transactional
    public BulkServiceDetailsResponse deleteBulk(List<ServiceDetailsRequest> serviceDetailsRequestList, String module) throws RunnerException {
        serviceDetailsValidationV3Util.validateDeleteBulkRequest(serviceDetailsRequestList);
        serviceDetailsValidationV3Util.validateSameParentId(serviceDetailsRequestList, module);
        // Extract unique service details IDs from the request
        List<Long> serviceDetailsIds = serviceDetailsRequestList.stream()
                .map(ServiceDetailsRequest::getId)
                .distinct()
                .toList();

        // Fetch service details from DB to ensure they exist before deletion
        List<ServiceDetails> serviceDetailsToDelete = serviceDetailsDao.findByIdIn(serviceDetailsIds);

        if (serviceDetailsToDelete.isEmpty()) {
            throw new DataRetrievalFailureException("No service details found for the given Ids.");
        }

        // Validate that all necessary service details IDs are present in the request
        serviceDetailsValidationV3Util.validateUpdateBulkRequest(serviceDetailsRequestList, serviceDetailsToDelete);

        ParentResult parentResult = getParentDetails(serviceDetailsToDelete, module);

        // Delete service details from DB
        serviceDetailsDao.deleteByIdIn(serviceDetailsIds);

        // Record audit logs for the deletion operation
        recordAuditLogs(serviceDetailsToDelete, null, DBOperationType.DELETE, parentResult);

        // Return the response with status message
        return BulkServiceDetailsResponse.builder()
                .message(prepareBulkDeleteMessage(serviceDetailsToDelete))
                .build();
    }

    private void recordAuditLogs(List<ServiceDetails> oldServiceDetails, List<ServiceDetails> newServiceDetails, DBOperationType operationType, ParentResult parentResult) {
        Map<Long, ServiceDetails> oldServiceDetailsMap = Optional.ofNullable(oldServiceDetails).orElse(List.of()).stream()
                .filter(serviceDetails -> serviceDetails.getId() != null)
                .collect(Collectors.toMap(ServiceDetails::getId, Function.identity()));

        Map<Long, ServiceDetails> newServiceDetailsMap = Optional.ofNullable(newServiceDetails).orElse(List.of()).stream()
                .filter(serviceDetails -> serviceDetails.getId() != null)
                .collect(Collectors.toMap(ServiceDetails::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newServiceDetailsMap.keySet();
            case DELETE -> oldServiceDetailsMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldServiceDetailsMap.keySet());
                ids.retainAll(newServiceDetailsMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                ServiceDetails oldData = oldServiceDetailsMap.get(id);
                ServiceDetails newData = newServiceDetailsMap.get(id);

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
                log.error("Failed to add audit log for service details ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    private String prepareBulkUpdateMessage(List<ServiceDetailsResponse> serviceDetailsResponses) {
        String message;

        // If more than one service details was updated, return a generic bulk success message
        if (serviceDetailsResponses.size() > 1) {
            message = "Bulk edit success! All selected service details have been updated.";
        } else {
            message = "Service Details saved successfully.";
        }

        return message;
    }

    private String prepareBulkDeleteMessage(List<ServiceDetails> serviceDetails) {
        String message;

        // If more than one service details was deleted, return a generic bulk success message
        if (serviceDetails.size() > 1) {
            message = "All selected service details deleted successfully!";
        } else {
            message = "Service Details deleted successfully!";
        }

        return message;
    }

    private Optional<ServiceDetails> retrieveForNte(Long id, String guid) {
        Optional<ServiceDetails> serviceDetails;
        if (id != null) {
            serviceDetails = serviceDetailsDao.findByIdWithQuery(id);
        } else {
            serviceDetails = serviceDetailsDao.findByGuidWithQuery(UUID.fromString(guid));
        }
        return serviceDetails;
    }

    @Override
    public ServiceDetailsResponse retrieveById(Long id, String guid, String source) {
        String responseMsg;
        try {
            if (id == null && isStringNullOrEmpty(guid)) {
                log.error("Id and Guid are null for Service Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Optional<ServiceDetails> serviceDetailsOptional;
            if (Objects.equals(source, NETWORK_TRANSFER)) {
                serviceDetailsOptional = retrieveForNte(id, guid);
            } else {
                if (id != null) {
                    serviceDetailsOptional = serviceDetailsDao.findById(id);
                } else {
                    serviceDetailsOptional = serviceDetailsDao.findByGuid(UUID.fromString(guid));
                }
            }
            if (serviceDetailsOptional.isEmpty()) {
                log.debug(ServiceDetailsConstants.SERVICE_DETAILS_RETRIEVE_BY_ID_ERROR, id, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Service Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return convertEntityToDto(serviceDetailsOptional.get());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new ValidationException(responseMsg);
        }
    }

    @Override
    public ServiceDetailsListResponse list(ListCommonRequest request, boolean getMasterData, String source) {
        if (request == null) {
            log.error("Request is empty for Service list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request cannot be null for list request.");
        }
        // construct specifications for filter request
        Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
        Page<ServiceDetails> serviceDetailsPage;
        if (Objects.equals(source, NETWORK_TRANSFER))
            serviceDetailsPage = serviceDetailsDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
        else
            serviceDetailsPage = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Service Details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        ServiceDetailsListResponse serviceDetailsListResponse = new ServiceDetailsListResponse();
        if (serviceDetailsPage != null) {
            List<ServiceDetailsResponse> responseList = convertEntityListToDtoList(serviceDetailsPage.getContent());
            Map<String, Object> masterDataResponse = this.getMasterDataForList(responseList, getMasterData);
            serviceDetailsListResponse.setServiceDetailsResponses(responseList);
            serviceDetailsListResponse.setTotalPages(serviceDetailsPage.getTotalPages());
            serviceDetailsListResponse.setTotalCount(serviceDetailsPage.getTotalElements());
            serviceDetailsListResponse.setMasterData(masterDataResponse);
        }
        return serviceDetailsListResponse;
    }

    private Map<String, Object> getMasterDataForList(List<ServiceDetailsResponse> responseList, boolean getMasterData) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> serviceDetailsV3Util.addAllMasterDataInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                var unlocationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> serviceDetailsV3Util.addAllUnlocationInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                CompletableFuture.allOf(masterDataFuture, unlocationFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SERVICE_DETAILS_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
                return masterDataResponse;
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SERVICE_DETAILS_LIST, ex.getLocalizedMessage());
            }
        }
        return masterDataResponse;
    }

    @Override
    public ServiceDetailsListResponse fetchShipmentServices(ListCommonRequest request, String xSource) {
        if (StringUtility.isEmpty(request.getEntityId()) || Long.parseLong(request.getEntityId()) <= 0) {
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
        ServiceDetailsListResponse serviceDetailsListResponse = list(listCommonRequest, true, xSource);
        log.info("Service Details list retrieved successfully for shipment with Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        return serviceDetailsListResponse;
    }

    @Override
    public Map<String, Object> getAllMasterData(Long id, String source) {
        try {
            Optional<ServiceDetails> serviceDetailsOptional;
            if (Objects.equals(source, NETWORK_TRANSFER))
                serviceDetailsOptional = serviceDetailsDao.findByIdWithQuery(id);
            else
                serviceDetailsOptional = serviceDetailsDao.findById(id);
            if (serviceDetailsOptional.isEmpty()) {
                log.debug(PackingConstants.PACKING_RETRIEVE_BY_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ServiceDetails serviceDetails = serviceDetailsOptional.get();
            ServiceDetailsResponse serviceDetailsResponse = convertEntityToDto(serviceDetails);
            long start = System.currentTimeMillis();
            log.info("Total time taken in fetching Service Details MasterData response {}", (System.currentTimeMillis() - start));
            return fetchAllMasterDataByKey(serviceDetailsResponse);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return new HashMap<>();
        }
    }

    @Override
    public Map<String, Object> fetchAllMasterDataByKey(ServiceDetailsResponse serviceDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> serviceDetailsV3Util.addAllMasterDataInSingleCall(serviceDetailsResponse, masterDataResponse)), executorServiceMasterData);
        var unlocationFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> serviceDetailsV3Util.addAllUnlocationDataInSingleCall(serviceDetailsResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unlocationFuture).join();

        return masterDataResponse;
    }

    private ServiceDetailsResponse convertEntityToDto(ServiceDetails serviceDetails) {
        return jsonHelper.convertValue(serviceDetails, ServiceDetailsResponse.class);
    }

    private List<ServiceDetailsResponse> convertEntityListToDtoList(List<ServiceDetails> lst) {
        List<ServiceDetailsResponse> responseList = new ArrayList<>();
        lst.forEach(serviceDetails -> responseList.add(convertEntityToDto(serviceDetails, defaultIncludeColumns)));
        return responseList;
    }

    private ServiceDetailsResponse convertEntityToDto(ServiceDetails serviceDetails, List<String> includesFields) {
        return (ServiceDetailsResponse) commonUtils.setIncludedFieldsToResponse(serviceDetails, new HashSet<>(includesFields), new ServiceDetailsResponse());
    }

    public ParentResult getParentDetails(List<ServiceDetails> serviceDetailsList, String moduleType) {
        ServiceDetails firstServiceDetails = serviceDetailsList.get(0);

        return switch (moduleType) {
            case Constants.SHIPMENT ->
                    new ParentResult(ShipmentDetails.class.getSimpleName(), firstServiceDetails.getShipmentId());
            case Constants.CONSOLIDATION ->
                    new ParentResult(ConsolidationDetails.class.getSimpleName(), firstServiceDetails.getConsolidationId());
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        };
    }

    @PostConstruct
    private void setDefaultIncludeColumns() {
        defaultIncludeColumns = FieldUtils.getNonRelationshipFields(ServiceDetails.class);
        defaultIncludeColumns.addAll(List.of("id", "guid", "tenantId", "contractor"));
    }

}
