package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.RoutingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.RoutingValidationUtil;
import com.dpw.runner.shipment.services.utils.v3.RoutingV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class RoutingsV3Service implements IRoutingsV3Service {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IRoutingsDao routingsDao;
    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    private RoutingValidationUtil routingValidationUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private RoutingV3Util routingV3Util;
    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.ROUTINGS).dataType(NetworkTransferStatus.class).fieldName("status").build())
//            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.ROUTINGS).dataType(NetworkTransferStatus.class).fieldName("status").build())

    );

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ParentResult {
        private String parent;
        private Long parentId;
    }


    @Transactional
    @Override
    public RoutingsResponse create(CommonRequestModel commonRequestModel,  String module) throws RunnerException {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        if(request == null){
            String resp = "Request is null for Routing Create";
            log.error("Request is null for Routing Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(resp);
        }

        Routings routings = convertRequestToEntity(request);
        try {
            routings = routingsDao.save(routings);

            // Create Audit logs for route
            createAuditLogs(routings, null, DBOperationType.CREATE.name());
            log.info("Routing created successfully for Id {} with Request Id {}", routings.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
        return convertEntityToDto(routings);
    }

    private void createAuditLogs(Routings routings, Routings oldRoutings, String operation) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                        .newData(routings)
                        .prevData(oldRoutings)
                        .parent(Routings.class.getSimpleName())
                        .parentId(routings.getId())
                        .operation(operation).build()
        );
    }

    @Override
    public RoutingsResponse update(CommonRequestModel commonRequestModel,  String module) throws RunnerException {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        routingValidationUtil.validateUpdateRequest(request);
        Optional<Routings> oldEntity = routingsDao.findById(request.getId());
        if(oldEntity.isEmpty()){
            log.debug(RoutingConstants.ROUTING_NULL_FOR_ID_ERROR, request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Routings oldEntityData = jsonHelper.convertValue(oldEntity.get(), Routings.class);
        Routings routings = convertRequestToEntity(request);
        try {
            routings = routingsDao.save(routings);

            // Create Audit logs for route
            createAuditLogs(routings, oldEntityData, DBOperationType.CREATE.name());
            log.info("Routing created successfully for Id {} with Request Id {}", routings.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
        return convertEntityToDto(routings);
    }


    @Override
    public RoutingListResponse list(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Routing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> routingsPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Routing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            List<IRunnerResponse> response = convertEntityListToDtoList(routingsPage.getContent());
            return RoutingListResponse.builder().routingsResponseList(response).totalCount(routingsPage.getTotalElements())
                    .totalPages(routingsPage.getTotalPages()).build();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    void getMasterDataForList(List<RoutingsResponse> response) {
        try {
            double startTime = System.currentTimeMillis();
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllUnlocationInSingleCallList(response)), executorServiceMasterData);
            var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllMasterDataInSingleCallList(response)), executorServiceMasterData);
            var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllVesselInSingleCallList(response)), executorServiceMasterData);
            CompletableFuture.allOf(locationDataFuture, masterDataFuture, vesselDataFuture).join();
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.ROUTING_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_ROUTING_LIST, ex.getLocalizedMessage());
        }
    }

    @Override
    public void delete(CommonRequestModel commonRequestModel, String module) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        routingValidationUtil.validateDeleteRequest(request);
        Optional<Routings> routing = routingsDao.findById(request.getId());
        if(routing.isEmpty()){
            log.debug(RoutingConstants.ROUTING_NULL_FOR_ID_ERROR, request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Routings oldEntityData = jsonHelper.convertValue(routing.get(), Routings.class);
        try {
            routingsDao.delete(routing.get());

            // Create Audit logs for route
            createAuditLogs(null, oldEntityData, DBOperationType.DELETE.name());
            log.info("Routing deleted successfully for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @Override
    public RoutingsResponse retrieveById(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null) {
            log.error(RoutingConstants.ROUTING_ID_NULL_FOR_RETRIEVE, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(RoutingConstants.ID_GUID_NULL_ERROR);
        }
        Optional<Routings> routings = routingsDao.findById(request.getId());
        if(routings.isEmpty()){
            log.debug("Routing is null for the input id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return modelMapper.map(routings.get(), RoutingsResponse.class);
    }

    private Routings convertRequestToEntity(RoutingsRequest request) {
        return jsonHelper.convertValue(request, Routings.class);
    }

    private RoutingsResponse convertEntityToDto(Routings routings) {
        return jsonHelper.convertValue(routings, RoutingsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Routings> lst) {
        List<RoutingsResponse> routingsListResponses = new ArrayList<>();
        lst.forEach(route -> {
            var response = modelMapper.map(route, RoutingsResponse.class);
            routingsListResponses.add(response);
        });
        // Add master data if required
        this.getMasterDataForList(routingsListResponses);

        List<IRunnerResponse> responseList = new ArrayList<>(routingsListResponses);
        return responseList;
    }

    @Override
    @Transactional
    public BulkRoutingResponse updateBulk(List<RoutingsRequest> routingListRequest, String module) {
        // Separate IDs and determine existing routing
        List<Long> incomingIds = routingListRequest.stream()
                .map(RoutingsRequest::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        List<Routings> existingRoutings = routingsDao.findByIdIn(incomingIds);

        // Validate incoming request
        routingValidationUtil.validateUpdateBulkRequest(routingListRequest, existingRoutings);

        // Separate into create and update requests
        List<RoutingsRequest> updateRequests = new ArrayList<>();
        List<RoutingsRequest> createRequests = new ArrayList<>();

        for (RoutingsRequest request : routingListRequest) {
            if (request.getId() != null && incomingIds.contains(request.getId())) {
                updateRequests.add(request);
            } else {
                createRequests.add(request);
            }
        }

        // Convert and process updates
        List<Routings> oldConvertedRouting = jsonHelper.convertValueToList(existingRoutings, Routings.class);
        List<Routings> updatedRouting = jsonHelper.convertValueToList(updateRequests, Routings.class);
        List<Routings> savedUpdatedRouting = routingsDao.saveAll(updatedRouting);

        // Convert and process creates
        List<Routings> newRouting = jsonHelper.convertValueToList(createRequests, Routings.class);
        List<Routings> savedNewRouting = routingsDao.saveAll(newRouting);

        // Combine results for parent calculation and auditing
        List<Routings> allSavedRouting = new ArrayList<>();
        allSavedRouting.addAll(savedNewRouting);
        allSavedRouting.addAll(savedUpdatedRouting);

        ParentResult parentResult = getParentDetails(allSavedRouting, module);

        // Audit logs
        recordAuditLogs(oldConvertedRouting, savedUpdatedRouting, DBOperationType.UPDATE, parentResult);
        recordAuditLogs(null, savedNewRouting, DBOperationType.CREATE, parentResult);

        // Convert to response
        List<RoutingsResponse> routingResponses = jsonHelper.convertValueToList(allSavedRouting, RoutingsResponse.class);

        return BulkRoutingResponse.builder()
                .routingsResponseList(routingResponses)
                .message(prepareBulkUpdateMessage(routingResponses))
                .build();
    }

    private void recordAuditLogs(List<Routings> oldRouting, List<Routings> newRouting, DBOperationType operationType, ParentResult parentResult) {
        Map<Long, Routings> oldRoutingMap = Optional.ofNullable(oldRouting).orElse(List.of()).stream()
                .filter(route -> route.getId() != null)
                .collect(Collectors.toMap(Routings::getId, Function.identity()));

        Map<Long, Routings> newRoutingMap = Optional.ofNullable(newRouting).orElse(List.of()).stream()
                .filter(route -> route.getId() != null)
                .collect(Collectors.toMap(Routings::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newRoutingMap.keySet();
            case DELETE -> oldRoutingMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldRoutingMap.keySet());
                ids.retainAll(newRoutingMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                Routings oldData = oldRoutingMap.get(id);
                Routings newData = newRoutingMap.get(id);

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
                log.error("Failed to add audit log for Routing ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    public ParentResult getParentDetails(List<Routings> routingList, String moduleType) {
        Routings firstRouting = routingList.get(0);

        return switch (moduleType) {
            case Constants.SHIPMENT ->
                    new ParentResult(ShipmentDetails.class.getSimpleName(), firstRouting.getShipmentId());
            case Constants.CONSOLIDATION ->
                    new ParentResult(ConsolidationDetails.class.getSimpleName(), firstRouting.getConsolidationId());
            case Constants.BOOKING ->
                    new ParentResult(CustomerBooking.class.getSimpleName(), firstRouting.getBookingId());
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        };
    }

    private String prepareBulkUpdateMessage(List<RoutingsResponse> routingResponses) {
        String message;

        // If more than one Route was updated, return a generic bulk success message
        if (routingResponses.size() > 1) {
            message = "Bulk edit success! All selected routes have been updated.";
        } else {
            message = "Routing saved successfully.";
        }

        return message;
    }
    @Override
    @Transactional
    public BulkRoutingResponse deleteBulk(List<RoutingsRequest> routingListRequest,  String module) throws RunnerException {
        routingValidationUtil.validateDeleteBulkRequest(routingListRequest);
        // Extract unique routing IDs from the request
        List<Long> routingIds = routingListRequest.stream()
                .map(RoutingsRequest::getId)
                .distinct()
                .toList();

        // Fetch routing from DB to ensure they exist before deletion
        List<Routings> routingToDelete = routingsDao.findByIdIn(routingIds);

        if (routingToDelete.isEmpty()) {
            throw new DataRetrievalFailureException("No routing found for the given Ids.");
        }

        // Validate that all necessary routing IDs are present in the request
        routingValidationUtil.validateUpdateBulkRequest(routingListRequest, routingToDelete);

        ParentResult parentResult = getParentDetails(routingToDelete, module);

        // Delete routing from DB
        routingsDao.deleteByIdIn(routingIds);

        // Record audit logs for the deletion operation
        recordAuditLogs(routingToDelete, null, DBOperationType.DELETE, parentResult);

        // Return the response with status message
        return BulkRoutingResponse.builder()
                .message(prepareBulkDeleteMessage(routingToDelete))
                .build();
    }

    private String prepareBulkDeleteMessage(List<Routings> routings) {
        String message;

        // If more than one route was deleted, return a generic bulk success message
        if (routings.size() > 1) {
            message = "Routings deleted successfully!";
        } else {
            message = "Routing deleted successfully!";
        }

        return message;
    }
}
