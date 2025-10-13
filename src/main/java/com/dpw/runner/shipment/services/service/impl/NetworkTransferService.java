package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.NetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferExternalResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
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

import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class NetworkTransferService implements INetworkTransferService {

    private final ModelMapper modelMapper;

    private final JsonHelper jsonHelper;

    private final INetworkTransferDao networkTransferDao;

    private final MasterDataUtils masterDataUtils;
    private final IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private final IShipmentDao shipmentDao;
    private final IConsolidationDetailsDao consolidationDao;

    ExecutorService executorService;

    private final CommonUtils commonUtils;

    private final MasterDataKeyUtils masterDataKeyUtils;

    private final INotificationDao notificationDao;

    private final V1ServiceUtil v1ServiceUtil;

    @Autowired
    public NetworkTransferService(ModelMapper modelMapper, JsonHelper jsonHelper, INetworkTransferDao networkTransferDao,
                                  MasterDataUtils masterDataUtils, ExecutorService executorService,
                                  CommonUtils commonUtils, MasterDataKeyUtils masterDataKeyUtils, INotificationDao notificationDao,
                                  IConsoleShipmentMappingDao consoleShipmentMappingDao, IShipmentDao shipmentDao, IConsolidationDetailsDao consolidationDao, V1ServiceUtil v1ServiceUtil) {
        this.modelMapper = modelMapper;
        this.jsonHelper = jsonHelper;
        this.networkTransferDao = networkTransferDao;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.commonUtils = commonUtils;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.notificationDao = notificationDao;
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.consolidationDao = consolidationDao;
        this.shipmentDao = shipmentDao;
        this.v1ServiceUtil = v1ServiceUtil;
    }


    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(NetworkTransferStatus.class).fieldName("status").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("entityType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("sourceBranchId", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(Integer.class).fieldName("sourceBranchId").build()),
            Map.entry("entityNumber", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("isHidden", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(Boolean.class).fieldName("isHidden").build()),
            Map.entry("transferredDate", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(LocalDateTime.class).fieldName("transferredDate").build())
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
            String shipmentNumber = extractShipmentNumber(request.getFilterCriteria());
            removeShipmentNumberFilter(request.getFilterCriteria());

            Specification<NetworkTransfer> shipmentNumberSpec = buildShipmentNumberFilterSpec(shipmentNumber);
            Pair<Specification<NetworkTransfer>, Pageable> tuple = fetchData(request, NetworkTransfer.class, tableNames);
            Specification<NetworkTransfer> finalSpec = shipmentNumberSpec;
            if (tuple.getLeft() != null) {
                finalSpec = finalSpec == null ? tuple.getLeft() : finalSpec.and(tuple.getLeft());
            }
            Page<NetworkTransfer> networkTransferPage = networkTransferDao.findAll(finalSpec, tuple.getRight());
            log.info("NetworkTransfer list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(networkTransferPage.getContent()),
                    networkTransferPage.getTotalPages(), networkTransferPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Specification<NetworkTransfer> buildShipmentNumberFilterSpec(String shipmentNumber) {
        if (shipmentNumber == null || shipmentNumber.isEmpty()) {
            return null;
        }
        return (root, query, cb) -> {
            Subquery<Long> subquery = query.subquery(Long.class);
            Root<NetworkTransferShipmentsMapping> subRoot = subquery.from(NetworkTransferShipmentsMapping.class);

            subquery.select(subRoot.get("networkTransferId"))
                    .where(cb.equal(subRoot.get(NetworkTransferConstants.SHIPMENT_NUMBER), shipmentNumber.trim().toUpperCase()));

            return root.get("id").in(subquery);
        };
    }

    private String extractShipmentNumber(List<FilterCriteria> filters) {
        if (filters == null) return null;
        for (FilterCriteria filter : filters) {
            if (filter.getCriteria() != null && NetworkTransferConstants.SHIPMENT_NUMBER.equalsIgnoreCase(filter.getCriteria().getFieldName())) {
                Object val = filter.getCriteria().getValue();
                return val != null ? val.toString() : null;
            }
            String nested = extractShipmentNumber(filter.getInnerFilter());
            if (nested != null) return nested;
        }
        return null;
    }

    private void removeShipmentNumberFilter(List<FilterCriteria> filters) {
        if (filters == null) return;

        Iterator<FilterCriteria> iterator = filters.iterator();
        while (iterator.hasNext()) {
            FilterCriteria filter = iterator.next();
            if (filter.getCriteria() != null &&
                    NetworkTransferConstants.SHIPMENT_NUMBER.equalsIgnoreCase(filter.getCriteria().getFieldName())) {
                iterator.remove();
                continue;
            }
            removeShipmentNumberFilter(filter.getInnerFilter());
            if ((filter.getInnerFilter() == null || filter.getInnerFilter().isEmpty()) && filter.getCriteria() == null) {
                iterator.remove();
            }
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<NetworkTransfer> lst) {
        List<NetworkTransferListResponse> networkTransferListResponses = new ArrayList<>();
        lst.forEach(networkTransfer -> {
            var response = modelMapper.map(networkTransfer, NetworkTransferListResponse.class);
            if (networkTransfer.getNetworkTransferShipmentsMappings() != null) {
                List<String> shipmentNumbers = networkTransfer.getNetworkTransferShipmentsMappings()
                        .stream()
                        .map(NetworkTransferShipmentsMapping::getShipmentNumber)
                        .collect(Collectors.toList());
                response.setShipmentNumbers(shipmentNumbers);
            } else {
                response.setShipmentNumbers(Collections.emptyList());
            }

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

            networkTransferListResponses.forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, NetworkTransfer.class, fieldNameKeyMap, NetworkTransfer.class.getSimpleName()+r.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            networkTransferListResponses.forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(NetworkTransfer.class.getSimpleName()+r.getId()), CacheConstants.MASTER_LIST, cacheMap)));

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

    public NetworkTransfer getNetworkTransferEntityFromShipment(ShipmentDetails shipmentDetails, Integer tenantId, String jobType, Boolean isInterBranchEntity){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(tenantId);
        networkTransfer.setEntityId(shipmentDetails.getId());
        networkTransfer.setEntityType(Constants.SHIPMENT);
        networkTransfer.setEntityNumber(shipmentDetails.getShipmentId());
        networkTransfer.setTransportMode(shipmentDetails.getTransportMode());
        networkTransfer.setSourceBranchId(shipmentDetails.getTenantId());
        networkTransfer.setJobType(jobType);
        networkTransfer.setIsInterBranchEntity(isInterBranchEntity);
        networkTransfer.setEntityGuid(shipmentDetails.getGuid());
        networkTransfer.setIsHidden(Boolean.FALSE);
        return networkTransfer;
    }

    public NetworkTransfer getNetworkTransferEntityFromConsolidation(ConsolidationDetails consolidationDetails, Integer tenantId, String jobType, Boolean isInterBranchEntity){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(tenantId);
        networkTransfer.setEntityId(consolidationDetails.getId());
        networkTransfer.setEntityType(Constants.CONSOLIDATION);
        networkTransfer.setEntityNumber(consolidationDetails.getConsolidationNumber());
        networkTransfer.setTransportMode(consolidationDetails.getTransportMode());
        networkTransfer.setSourceBranchId(consolidationDetails.getTenantId());
        networkTransfer.setJobType(jobType);
        networkTransfer.setIsInterBranchEntity(isInterBranchEntity);
        networkTransfer.setEntityGuid(consolidationDetails.getGuid());
        networkTransfer.setIsHidden(Boolean.FALSE);
        return networkTransfer;
    }

    @Transactional
    public void processNetworkTransferEntity(Long newTenantId, Long oldTenantId, String entityType,
                                             ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                             String jobType, Map<String, Object> entityPayload, Boolean isInterBranchEntity){
            NetworkTransfer networkTransfer = null;

            if(Objects.equals(newTenantId, Long.valueOf(TenantContext.getCurrentTenant())))
                return; // Skip processing if entry is getting created for existing branch

            if (Objects.equals(oldTenantId, newTenantId)) {
                return;  // Skip processing if tenant IDs are identical
            }
            var intTenantId = (newTenantId!=null) ? Math.toIntExact(newTenantId) : null;
            if (Objects.equals(entityType, Constants.SHIPMENT) && ObjectUtils.isNotEmpty(shipmentDetails)) {
                networkTransfer = getNetworkTransferEntityFromShipment(shipmentDetails, intTenantId, jobType, isInterBranchEntity);
            } else if (Objects.equals(entityType, Constants.CONSOLIDATION) && ObjectUtils.isNotEmpty(consolidationDetails)) {
                networkTransfer = getNetworkTransferEntityFromConsolidation(consolidationDetails, intTenantId, jobType, isInterBranchEntity);
            }

            if (oldTenantId != null && networkTransfer != null)
            {
                Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntityAndJobType(
                        Math.toIntExact(oldTenantId), networkTransfer.getEntityId(), entityType, jobType);
                String auditLogEntityType = getAuditLogEntityType(entityType);
                optionalNetworkTransfer.ifPresent(dbNetworkTransfer -> {
                    if (dbNetworkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED) {
                        return;
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


    @Override
    public void deleteNetworkTransferEntity(NetworkTransfer networkTransfer) {
        try{
            String auditLogEntityType = getAuditLogEntityType(networkTransfer.getEntityType());
            if (networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED) {
                networkTransferDao.deleteAndLog(networkTransfer, auditLogEntityType);
            }
        } catch (Exception e) {
            log.error("Error while deleting the NTE Delete Request: {} for entityId: {} entityType: {}",
                    e.getMessage(), networkTransfer.getEntityId(), networkTransfer.getEntityType());
        }
    }

    private void createNetworkTransfer(NetworkTransfer networkTransfer, Map<String, Object> entityPayload){
        networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
        if(entityPayload!=null){
            networkTransfer.setStatus(NetworkTransferStatus.TRANSFERRED);
            networkTransfer.setEntityPayload(entityPayload);
            networkTransfer.setTransferredDate(LocalDateTime.now());
        }
        networkTransferDao.save(networkTransfer);
    }

    public void updateNetworkTransferTransferred(NetworkTransfer networkTransfer, Map<String, Object> entityPayload) {
        networkTransfer.setEntityPayload(entityPayload);
        networkTransfer.setStatus(NetworkTransferStatus.TRANSFERRED);
        networkTransfer.setTransferredDate(LocalDateTime.now());
        networkTransferDao.save(networkTransfer);
    }


    @Transactional
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
        var entityId = networkTransfer.get().getEntityId();
        var entityType = networkTransfer.get().getEntityType();
        Integer tenantId = networkTransfer.get().getSourceBranchId();

        List<NetworkTransfer> networkTransferList = new ArrayList<>();
        // For Overarching Shipment
        if(Boolean.TRUE.equals(networkTransfer.get().getIsInterBranchEntity()) && Objects.equals(networkTransfer.get().getEntityType(), Constants.SHIPMENT)) {
            var consoleShipmentMapping = consoleShipmentMappingDao.findByShipmentId(networkTransfer.get().getEntityId());
            if(!CommonUtils.listIsNullOrEmpty(consoleShipmentMapping)) {
                entityId = consoleShipmentMapping.get(0).getConsolidationId();
                entityType = Constants.CONSOLIDATION;
                var consoleNTE = networkTransferDao.findByEntityIdAndEntityTypeAndIsInterBranchEntity(List.of(entityId), Constants.CONSOLIDATION, true, List.of(NetworkTransferStatus.SCHEDULED.name()), Constants.DIRECTION_CTS);
                if (!CommonUtils.listIsNullOrEmpty(consoleNTE)) {
                    consoleNTE.get(0).setStatus(NetworkTransferStatus.REQUESTED_TO_TRANSFER);
                    networkTransferList.add(consoleNTE.get(0));
                    tenantId = consoleNTE.get(0).getSourceBranchId();
                }
                this.fetchOverarchingConsoleAndShipmentNTE(networkTransferList, entityId, networkTransfer.get().getEntityId());
            }
        } else if(Boolean.TRUE.equals(networkTransfer.get().getIsInterBranchEntity()) && Objects.equals(networkTransfer.get().getEntityType(), Constants.CONSOLIDATION)) {
            this.fetchOverarchingConsoleAndShipmentNTE(networkTransferList, entityId, null);
        }
        Notification notification = getNotificationEntity(tenantId, NotificationRequestType.REQUEST_TRANSFER, requestForTransferRequest.getRemarks(), null, entityType, entityId, null, requestForTransferRequest.getId());
        notificationDao.save(notification);
        networkTransferList.add(networkTransfer.get());
        networkTransferDao.saveAll(networkTransferList);

        if(isNteAdditionalEmailFlagEnabled())
            triggerRequestTransferEmail(entityId, entityType, requestForTransferRequest.getRemarks());

        return ResponseHelper.buildSuccessResponse();
    }

    void fetchOverarchingConsoleAndShipmentNTE(List<NetworkTransfer> networkTransferList, Long consoleId, Long shipId) {
        var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        if(!consoleShipMappingList.isEmpty()){
            List<Long> shipmentIds = consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).filter(shipmentId ->!Objects.equals(shipmentId, shipId)).toList();
            List<NetworkTransfer> shipmentsNte = new ArrayList<>();
            if(!CommonUtils.listIsNullOrEmpty(shipmentIds))
                shipmentsNte = networkTransferDao.findByEntityIdAndEntityTypeAndIsInterBranchEntity(shipmentIds, Constants.SHIPMENT, true, List.of(NetworkTransferStatus.SCHEDULED.name()), Constants.DIRECTION_CTS);

            if(!CommonUtils.listIsNullOrEmpty(shipmentsNte)){
                shipmentsNte.forEach(nte -> {
                    nte.setStatus(NetworkTransferStatus.REQUESTED_TO_TRANSFER);
                    networkTransferList.add(nte);
                });
            }
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> requestForReassign(CommonRequestModel commonRequestModel) {
        ReassignRequest reassignRequest = (ReassignRequest) commonRequestModel.getData();
        var shipmentGuidReassignBranch = reassignRequest.getShipmentGuidReassignBranch();
        if(reassignRequest.getBranchId() == null && (shipmentGuidReassignBranch == null || shipmentGuidReassignBranch.isEmpty())){
            throw new ValidationException("At least one branch is mandatory to reassign.");
        }
        var networkTransfer = networkTransferDao.findById(reassignRequest.getId());
        if(networkTransfer.isEmpty()){
            log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, reassignRequest.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(Objects.equals(networkTransfer.get().getStatus(), NetworkTransferStatus.REASSIGNED)) {
            throw new DataRetrievalFailureException("Network Transfer is already in Reassigned state.");
        }
        List<NetworkTransfer> networkTransferList = new ArrayList<>();
        Integer receivingBranch = TenantContext.getCurrentTenant();
        createShipmentNotificationForConsole(networkTransfer.get(), shipmentGuidReassignBranch, reassignRequest, networkTransferList);
        if(reassignRequest.getBranchId() != null) {
            networkTransfer.get().setStatus(NetworkTransferStatus.REASSIGNED);
            Notification notification = getNotificationEntity(networkTransfer.get().getSourceBranchId(), NotificationRequestType.REASSIGN, reassignRequest.getRemarks(), reassignRequest.getBranchId(), networkTransfer.get().getEntityType(), networkTransfer.get().getEntityId(), receivingBranch, reassignRequest.getId());
            notificationDao.save(notification);
            networkTransferList.add(networkTransfer.get());
        }
        if(!networkTransferList.isEmpty()){
            networkTransferDao.saveAll(networkTransferList);
        }

        if(isNteAdditionalEmailFlagEnabled()) {
            for (NetworkTransfer nte : networkTransferList) {
                triggerReassignmentEmail(nte, reassignRequest);
            }
        }
        return ResponseHelper.buildSuccessResponse();
    }

    private void createShipmentNotificationForConsole(NetworkTransfer networkTransfer, Map<String, Integer> shipmentGuidReassignBranch, ReassignRequest reassignRequest, List<NetworkTransfer> networkTransferList) {
        if(Objects.equals(networkTransfer.getEntityType(), Constants.CONSOLIDATION) && shipmentGuidReassignBranch != null && !shipmentGuidReassignBranch.isEmpty() &&
                !Objects.equals(networkTransfer.getJobType(), Constants.DIRECTION_CTS)) {
            var consolidation = consolidationDao.findConsolidationByIdWithQuery(networkTransfer.getEntityId());
            if(consolidation.isPresent() && Boolean.TRUE.equals(consolidation.get().getInterBranchConsole())){
                for(var ship: consolidation.get().getShipmentsList()) {
                    if(shipmentGuidReassignBranch.containsKey(ship.getGuid().toString()) && shipmentGuidReassignBranch.get(ship.getGuid().toString()) != null) {
                        this.createShipmentNotification(reassignRequest, ship.getId(), shipmentGuidReassignBranch.get(ship.getGuid().toString()), ship.getTenantId(), ship.getReceivingBranch().intValue(), networkTransferList);
                    }
                }
            }
        }
    }

    private void createShipmentNotification(ReassignRequest reassignRequest, Long entityId, Integer reassignBranchId, Integer sourceBranchId, Integer receivingBranchId, List<NetworkTransfer> networkTransferList) {
        var networkTransfer  = networkTransferDao.findByTenantAndEntity(receivingBranchId, entityId, Constants.SHIPMENT);
        Long networkId = networkTransfer.map(BaseEntity::getId).orElse(null);
        Notification notification = getNotificationEntity(sourceBranchId, NotificationRequestType.REASSIGN, reassignRequest.getRemarks(), reassignBranchId, Constants.SHIPMENT, entityId, receivingBranchId, networkId);
        if(networkTransfer.isPresent()) {
            networkTransfer.get().setStatus(NetworkTransferStatus.REASSIGNED);
            networkTransferList.add(networkTransfer.get());
        }
        notificationDao.save(notification);
    }

    public Notification getNotificationEntity(Integer sourceBranchId, NotificationRequestType notificationRequestType, String reason, Integer reassignBranchId, String entityType, Long entityId, Integer reassignFromBranchId, Long networkId) {
        Notification notification = new Notification();
        notification.setEntityId(entityId);
        notification.setEntityType(entityType);
        notification.setRequestedBranchId(TenantContext.getCurrentTenant());
        notification.setRequestedUser(UserContext.getUser().getUsername());
        notification.setRequestedOn(LocalDateTime.now(ZoneOffset.UTC));
        notification.setNotificationRequestType(notificationRequestType);
        notification.setReason(reason);
        notification.setNetworkTransferId(networkId);
        if (Objects.equals(notificationRequestType, NotificationRequestType.REASSIGN)) {
            notification.setReassignedToBranchId(reassignBranchId);
            notification.setReassignedFromBranchId(reassignFromBranchId);
        }
        notification.setTenantId(sourceBranchId);
        return notification;
    }

    @Override
    public void updateStatusAndCreatedEntityId(Long id, String status, Long createdEntityId) {
        networkTransferDao.updateStatusAndCreatedEntityId(id, status, createdEntityId);
    }

    @Transactional
    @Override
    public void bulkProcessInterConsoleNte(List<ShipmentDetails> shipmentDetailsList) {
        List<NetworkTransfer> nteToCreate = new ArrayList<>();
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            NetworkTransfer networkTransfer;

            if (Objects.equals(shipmentDetails.getReceivingBranch(), Long.valueOf(TenantContext.getCurrentTenant())))
                continue; // Skip processing if entry is getting created for existing branch

            var intTenantId =  Math.toIntExact(shipmentDetails.getReceivingBranch());
            networkTransfer = getNetworkTransferEntityFromShipment(shipmentDetails, intTenantId, Constants.IMP, true);
            networkTransfer.setIsHidden(true);
            networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
            nteToCreate.add(networkTransfer);
        }
        if(!nteToCreate.isEmpty())
            networkTransferDao.saveAll(nteToCreate);
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchEntityStatus(CommonGetRequest commonGetRequest) {
        String guid = commonGetRequest.getGuid();
        var tenantId = shipmentDao.findReceivingByGuid(UUID.fromString(guid));
        if(tenantId != null) {
            var status = networkTransferDao.findByEntityGuidAndTenantId(UUID.fromString(guid), tenantId);
            return ResponseHelper.buildSuccessResponse(NetworkTransferResponse.builder().status(NetworkTransferStatus.valueOf(status)).build());
        }
        return ResponseHelper.buildSuccessResponse();
    }

    public ResponseEntity<IRunnerResponse> createExternal(CommonRequestModel commonRequestModel) {
        NetworkTransferRequest networkTransferRequest = (NetworkTransferRequest) commonRequestModel.getData();
        if (networkTransferRequest == null) {
            throw new ValidationException("NetworkTransferRequest cannot be null");
        }
        NetworkTransfer networkTransfer = jsonHelper.convertCreateValue(networkTransferRequest, NetworkTransfer.class);
        networkTransfer.setSource(NetworkTransferSource.EXTERNAL);
        if(Objects.equals(networkTransfer.getStatus(), NetworkTransferStatus.TRANSFERRED)) {
            networkTransfer.setTransferredDate(LocalDateTime.now(ZoneOffset.UTC));
        }
        networkTransfer = networkTransferDao.save(networkTransfer);
        NetworkTransferExternalResponse networkTransferExternalResponse = jsonHelper.convertValue(networkTransfer, NetworkTransferExternalResponse.class);
        return ResponseHelper.buildSuccessResponse(networkTransferExternalResponse);
    }

    private void triggerRequestTransferEmail(Long entityId, String entityType, String reason) {
        try {
            String entityNumber = "";
            String assignedTo = "";
            String createdBy = "";
            if(SHIPMENT.equals(entityType)) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentByIdWithQuery(entityId);
                if (shipmentDetails.isPresent()) {
                    assignedTo = shipmentDetails.get().getAssignedTo();
                    entityNumber = shipmentDetails.get().getShipmentId();
                    createdBy = shipmentDetails.get().getCreatedBy();
                }
            }
            if(CONSOLIDATION.equals(entityType)) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findConsolidationByIdWithQuery(entityId);
                if (consolidationDetails.isPresent()) {
                    assignedTo = consolidationDetails.get().getAssignedTo();
                    entityNumber = consolidationDetails.get().getConsolidationNumber();
                    createdBy = consolidationDetails.get().getCreatedBy();
                }
            }

            List<String> emailList = new ArrayList<>();
            Map<String, String> usernameEmailsMap = new HashMap<>();
            Set<String> usernames = Stream.of(assignedTo, createdBy)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
            commonUtils.getUserDetails(usernames, usernameEmailsMap);

            if (usernameEmailsMap.containsKey(assignedTo))
                emailList.add(usernameEmailsMap.get(assignedTo));
            if(!CommonUtils.isStringNullOrEmpty(createdBy) && usernameEmailsMap.containsKey(createdBy))
                emailList.add(usernameEmailsMap.get(createdBy));
            EmailTemplatesRequest template = createRequestTransferEmailBody(entityType, reason, entityNumber);
            if(!emailList.isEmpty() && template.getBody() != null) {
                commonUtils.sendEmailNotification(template, emailList, Collections.singletonList(UserContext.getUser().getEmail()));
            }
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }
    }

    private EmailTemplatesRequest createRequestTransferEmailBody(String entityType, String reason, String entityNumber){
        UsersDto user = UserContext.getUser();
        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(List.of(TenantContext.getCurrentTenant())));
        String body = "";
        String subject = "";
        if(SHIPMENT.equals(entityType)) {
            body = EntityTransferConstants.EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_SHIPMENT_BODY;
            subject = EntityTransferConstants.EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_SHIPMENT_SUBJECT;
            subject = subject.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, entityNumber);
            body = body.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, entityNumber);
        } else if (CONSOLIDATION.equals(entityType)){
            body = EntityTransferConstants.EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CONSOLIDATION_BODY;
            subject = EntityTransferConstants.EMAIL_TEMPLATE_REQUEST_TO_TRANSFER_CONSOLIDATION_SUBJECT;
            subject = subject.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, entityNumber);
            body = body.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, entityNumber);
        }
        body = body.replace(EntityTransferConstants.USER_NAME_PLACEHOLDER, user.getDisplayName());
        body = body.replace(EntityTransferConstants.BRANCH_NAME_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(TenantContext.getCurrentTenant()).tenantName));
        body = body.replace(EntityTransferConstants.TRANSFER_REASON_PLACEHOLDER, reason);
        body = body.replace(EntityTransferConstants.REQUESTED_USER_EMAIL_PLACEHOLDER, user.getEmail());
        return EmailTemplatesRequest.builder().body(body).subject(subject).build();
    }

    private Map<Integer, TenantModel> convertToTenantModel(Map<Integer, Object> map) {
        var response = new HashMap<Integer, TenantModel>();

        for (var entry : map.entrySet())
            response.put(entry.getKey(), modelMapper.map(entry.getValue(), TenantModel.class));

        return response;
    }

    private void triggerReassignmentEmail(NetworkTransfer networkTransfer, ReassignRequest reassignRequest) {
        try {
            String reason = reassignRequest.getRemarks();
            String entityType = networkTransfer.getEntityType();
            Long entityId = networkTransfer.getEntityId();
            String createdBy = networkTransfer.getCreatedBy();
            String entityCreatedBy = "";

            String assignedTo = "";
            if(SHIPMENT.equals(entityType)) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentByIdWithQuery(entityId);
                if (shipmentDetails.isPresent()) {
                    assignedTo = shipmentDetails.get().getAssignedTo();
                    entityCreatedBy = shipmentDetails.get().getCreatedBy();
                }
            }

            if(CONSOLIDATION.equals(entityType)) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findConsolidationByIdWithQuery(entityId);
                if (consolidationDetails.isPresent()) {
                    assignedTo = consolidationDetails.get().getAssignedTo();
                    entityCreatedBy = consolidationDetails.get().getCreatedBy();
                }
            }

            Set<String> emailsList = new HashSet<>();
            addFieldsOnMailListSet(emailsList, assignedTo, createdBy, entityCreatedBy);

            Integer reAssignedBranch = getReAssignedBranch(networkTransfer, reassignRequest);

            List<String> emailList = new ArrayList<>();
            Map<String, String> usernameEmailsMap = new HashMap<>();
            commonUtils.getUserDetails(new HashSet<>(emailsList), usernameEmailsMap);
            if (NetworkTransferConstants.TRANSFER_RETRANSFER_SET.contains(networkTransfer.getStatus()) && createdBy!=null && usernameEmailsMap.containsKey(createdBy))
                emailList.add(usernameEmailsMap.get(createdBy));

            if (assignedTo!=null && usernameEmailsMap.containsKey(assignedTo))
                emailList.add(usernameEmailsMap.get(assignedTo));

            if(entityCreatedBy!=null && usernameEmailsMap.containsKey(entityCreatedBy))
                emailList.add(usernameEmailsMap.get(entityCreatedBy));

            EmailTemplatesRequest template = createRequestReassignEmailBody(networkTransfer, reason, reAssignedBranch);
            if(!emailList.isEmpty() && template.getBody() != null) {
                commonUtils.sendEmailNotification(template, emailList, Collections.singletonList(UserContext.getUser().getEmail()));
            }
        } catch (Exception ex) {
            log.error(String.format(ErrorConstants.ERROR_WHILE_EMAIL, ex.getMessage()));
        }
    }

    private Integer getReAssignedBranch(NetworkTransfer networkTransfer, ReassignRequest reassignRequest) {
        var shipmentGuidReassignBranch = reassignRequest.getShipmentGuidReassignBranch();
        Integer reAssignedBranch;

        if(reassignRequest.getShipmentGuidReassignBranch() != null && !reassignRequest.getShipmentGuidReassignBranch().isEmpty() && shipmentGuidReassignBranch.containsKey(networkTransfer.getEntityGuid().toString()) && shipmentGuidReassignBranch.get(networkTransfer.getEntityGuid().toString()) != null){
                reAssignedBranch = shipmentGuidReassignBranch.get(networkTransfer.getEntityGuid().toString());
        }else{
            reAssignedBranch = reassignRequest.getBranchId();
        }
        return reAssignedBranch;
    }

    private EmailTemplatesRequest createRequestReassignEmailBody(NetworkTransfer networkTransfer, String reason, Integer reAssignedBranch){
        UsersDto user = UserContext.getUser();
        var branchIdVsTenantModelMap = convertToTenantModel(v1ServiceUtil.getTenantDetails(List.of(TenantContext.getCurrentTenant(), networkTransfer.getTenantId(), reAssignedBranch)));
        String body = "";
        String subject = "";
        if(SHIPMENT.equals(networkTransfer.getEntityType())) {
            if(networkTransfer.getJobType().equals(DIRECTION_CTS)) {
                body = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_SHIPMENT_BODY_TRIANGULATION_BRANCH;
                subject = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_SHIPMENT_SUBJECT_TRIANGULATION_BRANCH;
            } else{
                body = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_SHIPMENT_BODY;
                subject = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_SHIPMENT_SUBJECT;
            }
            subject = subject.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, networkTransfer.getEntityNumber());
            body = body.replace(EntityTransferConstants.SHIPMENT_NUMBER_PLACEHOLDER, networkTransfer.getEntityNumber());
        } else if (CONSOLIDATION.equals(networkTransfer.getEntityType())){
            if(networkTransfer.getJobType().equals(DIRECTION_CTS)) {
                body = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_CONSOLIDATION_BODY_TRIANGULATION_BRANCH;
                subject = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_CONSOLIDATION_SUBJECT_TRIANGULATION_BRANCH;
            }else{
                body = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_CONSOLIDATION_BODY;
                subject = EntityTransferConstants.EMAIL_TEMPLATE_REASSIGN_CONSOLIDATION_SUBJECT;
            }
            subject = subject.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, networkTransfer.getEntityNumber());
            body = body.replace(EntityTransferConstants.CONSOLIDATION_NUMBER_PLACEHOLDER, networkTransfer.getEntityNumber());
        }
        body = body.replace(EntityTransferConstants.USER_NAME_PLACEHOLDER, user.getDisplayName());
        body = body.replace(NotificationConstants.ASSIGN_TO_BRANCH_PLACEHOLDER, StringUtility.convertToString(branchIdVsTenantModelMap.get(reAssignedBranch).tenantName));
        body = body.replace(EntityTransferConstants.TRANSFER_REASON_PLACEHOLDER, reason);
        body = body.replace(EntityTransferConstants.REQUESTED_USER_EMAIL_PLACEHOLDER, user.getEmail());
        body = body.replace(EntityTransferConstants.REASSIGNED_USER_EMAIL_ID, user.getEmail());
        body = body.replace(EntityTransferConstants.ORIGINAL_DESTINATION_BRANCH, StringUtility.convertToString(branchIdVsTenantModelMap.get(networkTransfer.getTenantId()).tenantName));
        return EmailTemplatesRequest.builder().body(body).subject(subject).build();
    }

    private boolean isNteAdditionalEmailFlagEnabled(){
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return shipmentSettingsDetails != null && shipmentSettingsDetails.getIsNteAdditionalEmailsEnabled() != null && shipmentSettingsDetails.getIsNteAdditionalEmailsEnabled();
    }

    private void addFieldsOnMailListSet(Set<String> emailsList, String assignedTo, String createdBy, String entityCreatedBy){
        if (!CommonUtils.isStringNullOrEmpty(assignedTo))
            emailsList.add(assignedTo);
        if (!CommonUtils.isStringNullOrEmpty(createdBy))
            emailsList.add(createdBy);
        if (!CommonUtils.isStringNullOrEmpty(entityCreatedBy))
            emailsList.add(entityCreatedBy);
    }
}
