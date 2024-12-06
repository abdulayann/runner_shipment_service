package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.NetworkTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Page;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.transaction.annotation.Transactional;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class NetworkTransferService implements INetworkTransferService {

    private final ModelMapper modelMapper;

    private final JsonHelper jsonHelper;

    private final INetworkTransferDao networkTransferDao;

    private final MasterDataUtils masterDataUtils;

    ExecutorService executorService;

    private final CommonUtils commonUtils;

    private final MasterDataKeyUtils masterDataKeyUtils;

    private final INotificationDao notificationDao;

    @Autowired
    public NetworkTransferService(ModelMapper modelMapper, JsonHelper jsonHelper, INetworkTransferDao networkTransferDao,
                                  MasterDataUtils masterDataUtils, ExecutorService executorService,
                                  CommonUtils commonUtils, MasterDataKeyUtils masterDataKeyUtils, INotificationDao notificationDao) {
        this.modelMapper = modelMapper;
        this.jsonHelper = jsonHelper;
        this.networkTransferDao = networkTransferDao;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.commonUtils = commonUtils;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.notificationDao = notificationDao;
    }


    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(NetworkTransferStatus.class).fieldName("status").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("entityType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("sourceBranchId", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(Integer.class).fieldName("sourceBranchId").isContainsText(true).build()),
            Map.entry("entityNumber", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build())
    );


    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for NetworkTransfer list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<NetworkTransfer>, Pageable> tuple = fetchData(request, NetworkTransfer.class, tableNames);
            Page<NetworkTransfer> networkTransferPage = networkTransferDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("NetworkTransfer list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(networkTransferPage.getContent()),
                    networkTransferPage.getTotalPages(), networkTransferPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<NetworkTransfer> lst) {
        List<NetworkTransferListResponse> networkTransferListResponses = new ArrayList<>();
        lst.forEach(networkTransfer -> {
            var response = modelMapper.map(networkTransfer, NetworkTransferListResponse.class);
            networkTransferListResponses.add(response);
        });

        List<IRunnerResponse> responseList = new ArrayList<>(networkTransferListResponses);
        try{
            if(ObjectUtils.isNotEmpty(networkTransferListResponses)){
                var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCallList(networkTransferListResponses)), executorService);
                var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorService);
                CompletableFuture.allOf(masterListFuture, tenantDataFuture).join();
            }
        } catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_NETWORK_TRANSFER_LIST, ex.getLocalizedMessage());
        }

        return responseList;
    }

    private void addAllMasterDataInSingleCallList(List<NetworkTransferListResponse> networkTransferListResponses) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            networkTransferListResponses.forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, NetworkTransfer.class, fieldNameKeyMap, NetworkTransfer.class.getSimpleName(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            networkTransferListResponses.forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(NetworkTransfer.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), NetworkTransfer.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for NetworkTransfer retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<NetworkTransfer> networkTransfer;
            if(id != null) {
                networkTransfer = networkTransferDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                networkTransfer = networkTransferDao.findByGuid(guid);
            }

            if (networkTransfer.isEmpty()) {
                log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            NetworkTransferResponse networkTransferResponse = jsonHelper.convertValue(networkTransfer.get(), NetworkTransferResponse.class);
            addDependantServiceData(networkTransferResponse);
            return ResponseHelper.buildSuccessResponse(networkTransferResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public void addDependantServiceData(NetworkTransferResponse networkTransferResponse){
        try{
            Map<String, Object> response = null;
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(networkTransferResponse, response)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(networkTransferResponse, response)), executorService);

            CompletableFuture.allOf(tenantDataFuture, masterListFuture).join();
        } catch (Exception ex) {
            log.error("Exception during fetching master data in retrieve API for guid: {} with exception: {}", networkTransferResponse.getGuid(), ex.getMessage());
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (NetworkTransferResponse networkTransferResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(networkTransferResponse, NetworkTransfer.class, fieldNameKeyMap, NetworkTransfer.class.getSimpleName(), cacheMap));

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            if(masterDataResponse == null) {
                networkTransferResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(NetworkTransfer.class.getSimpleName()), CacheConstants.TENANTS, cacheMap));
            }
            else{
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTenantDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), NetworkTransferResponse.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(NetworkTransferResponse networkTransferResponse, Map<String, Object> masterDataResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();

            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(networkTransferResponse, NetworkTransfer.class, fieldNameKeyMap, NetworkTransfer.class.getSimpleName(), cacheMap));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            // fetching from V1 in single call
            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            // Postprocessing
            if(masterDataResponse == null)
                networkTransferResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(NetworkTransfer.class.getSimpleName()), CacheConstants.MASTER_LIST, false, cacheMap));
            else
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), NetworkTransfer.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public NetworkTransfer getNetworkTransferEntityFromShipment(ShipmentDetails shipmentDetails, Integer tenantId, String jobType){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(tenantId);
        networkTransfer.setEntityId(shipmentDetails.getId());
        networkTransfer.setEntityType(Constants.SHIPMENT);
        networkTransfer.setEntityNumber(shipmentDetails.getShipmentId());
        networkTransfer.setTransportMode(shipmentDetails.getTransportMode());
        networkTransfer.setSourceBranchId(shipmentDetails.getTenantId());
        networkTransfer.setJobType(jobType);
        return networkTransfer;
    }

    public NetworkTransfer getNetworkTransferEntityFromConsolidation(ConsolidationDetails consolidationDetails, Integer tenantId, String jobType){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(tenantId);
        networkTransfer.setEntityId(consolidationDetails.getId());
        networkTransfer.setEntityType(Constants.CONSOLIDATION);
        networkTransfer.setEntityNumber(consolidationDetails.getConsolidationNumber());
        networkTransfer.setTransportMode(consolidationDetails.getTransportMode());
        networkTransfer.setSourceBranchId(consolidationDetails.getTenantId());
        networkTransfer.setJobType(jobType);
        return networkTransfer;
    }

    @Transactional
    public void processNetworkTransferEntity(Long newTenantId, Long oldTenantId, String entityType,
                                             ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                             String jobType, Map<String, Object> entityPayload){
            NetworkTransfer networkTransfer = null;

            if(Objects.equals(newTenantId, Long.valueOf(TenantContext.getCurrentTenant())))
                return; // Skip processing if entry is getting created for existing branch

            if (Objects.equals(oldTenantId, newTenantId)) {
                return;  // Skip processing if tenant IDs are identical
            }
            var intTenantId = (newTenantId!=null) ? Math.toIntExact(newTenantId) : null;
            if (Objects.equals(entityType, Constants.SHIPMENT) && ObjectUtils.isNotEmpty(shipmentDetails)) {
                networkTransfer = getNetworkTransferEntityFromShipment(shipmentDetails, intTenantId, jobType);
            } else if (Objects.equals(entityType, Constants.CONSOLIDATION) && ObjectUtils.isNotEmpty(consolidationDetails)) {
                networkTransfer = getNetworkTransferEntityFromConsolidation(consolidationDetails, intTenantId, jobType);
            }

            if (oldTenantId != null && networkTransfer != null)
            {
                Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                        Math.toIntExact(oldTenantId), networkTransfer.getEntityId(), entityType);
                String auditLogEntityType = getAuditLogEntityType(entityType);
                optionalNetworkTransfer.ifPresent(dbNetworkTransfer -> {
                    if (dbNetworkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED) {
                        return; // Return if the status is ACCEPTED
                    }
                    networkTransferDao.deleteAndLog(dbNetworkTransfer, auditLogEntityType);
                });
            }

            if (!Objects.isNull(networkTransfer) && networkTransfer.getTenantId() != null ) // Won't processing if tenant ID is null
                createNetworkTransfer(networkTransfer, entityPayload);
        }

    private String getAuditLogEntityType(String entityType) {
        return Objects.equals(entityType, Constants.SHIPMENT) ?
                ShipmentDetails.class.getSimpleName() : ConsolidationDetails.class.getSimpleName();
    }

    @Override
    public void deleteValidNetworkTransferEntity(Long oldTenantId, Long entityId, String entityType) {
        try{
            String auditLogEntityType = getAuditLogEntityType(entityType);
            networkTransferDao.findByTenantAndEntity(Math.toIntExact(oldTenantId), entityId, entityType)
                    .ifPresent(networkTransfer -> {
                        if (networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED) {
                            networkTransferDao.deleteAndLog(networkTransfer, auditLogEntityType);
                        }
                    });
        } catch (Exception e) {
            log.error("Error while deleting the Network Transfer Delete Request: {} for entityId: {} entityType: {}",
                    e.getMessage(), entityId, entityType);
        }
    }

    private void createNetworkTransfer(NetworkTransfer networkTransfer, Map<String, Object> entityPayload){
        networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
        if(entityPayload!=null){
            networkTransfer.setEntityPayload(entityPayload);
        }
        networkTransferDao.save(networkTransfer);
    }

    public void updateNetworkTransferTransferred(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) {
        networkTransfer.setEntityPayload(entityPayload);
        networkTransfer.setStatus(NetworkTransferStatus.TRANSFERRED);
        networkTransferDao.save(networkTransfer);
    }


    @Override
    public ResponseEntity<IRunnerResponse> requestForTransfer(CommonRequestModel commonRequestModel) {
        RequestForTransferRequest requestForTransferRequest = (RequestForTransferRequest) commonRequestModel.getData();
        var networkTransfer = networkTransferDao.findById(requestForTransferRequest.getId());
        if(networkTransfer.isEmpty()){
            log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, requestForTransferRequest.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(Objects.equals(networkTransfer.get().getStatus(), NetworkTransferStatus.REQUESTED_TO_TRANSFER)) {
            throw new DataRetrievalFailureException("Network Transfer is already in Request to Transfer state.");
        }
        networkTransfer.get().setStatus(NetworkTransferStatus.REQUESTED_TO_TRANSFER);
        Notification notification = getNotificationEntity(networkTransfer.get(), NotificationRequestType.REQUEST_TRANSFER, requestForTransferRequest.getRemarks(), null);
        notificationDao.save(notification);
        networkTransferDao.save(networkTransfer.get());
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<IRunnerResponse> requestForReassign(CommonRequestModel commonRequestModel) {
        ReassignRequest reassignRequest = (ReassignRequest) commonRequestModel.getData();
        var networkTransfer = networkTransferDao.findById(reassignRequest.getId());
        if(networkTransfer.isEmpty()){
            log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, reassignRequest.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(Objects.equals(networkTransfer.get().getStatus(), NetworkTransferStatus.REASSIGNED)) {
            throw new DataRetrievalFailureException("Network Transfer is already in Reassigned state.");
        }
        networkTransfer.get().setStatus(NetworkTransferStatus.REASSIGNED);
        Notification notification = getNotificationEntity(networkTransfer.get(), NotificationRequestType.REASSIGN, reassignRequest.getRemarks(), reassignRequest.getBranchId());
        notificationDao.save(notification);
        networkTransferDao.save(networkTransfer.get());
        return ResponseHelper.buildSuccessResponse();
    }

    public Notification getNotificationEntity(NetworkTransfer networkTransfer, NotificationRequestType notificationRequestType, String reason, Integer reassignBranchId) {
        Notification notification = new Notification();
        notification.setEntityId(networkTransfer.getEntityId());
        notification.setEntityType(networkTransfer.getEntityType());
        notification.setRequestedBranchId(TenantContext.getCurrentTenant());
        notification.setRequestedUser(UserContext.getUser().getUsername());
        notification.setRequestedOn(LocalDateTime.now(ZoneOffset.UTC));
        notification.setNotificationRequestType(notificationRequestType);
        notification.setReason(reason);
        if (Objects.equals(notificationRequestType, NotificationRequestType.REASSIGN)) {
            notification.setReassignedToBranchId(reassignBranchId);
        }
        notification.setTenantId(networkTransfer.getSourceBranchId());
        return notification;
    }
}
