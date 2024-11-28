package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NotificationConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationListResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationResponse;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.RequestType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotificationService;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class NotificationService implements INotificationService {

    private final ModelMapper modelMapper;

    private final INotificationDao notificationDao;

    private final JsonHelper jsonHelper;

    private final MasterDataUtils masterDataUtils;

    ExecutorService executorService;

    private final MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    public NotificationService(ModelMapper modelMapper, JsonHelper jsonHelper, INotificationDao notificationDao,
                                  MasterDataUtils masterDataUtils, ExecutorService executorService,
                                  MasterDataKeyUtils masterDataKeyUtils) {
        this.jsonHelper = jsonHelper;
        this.notificationDao = notificationDao;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.modelMapper = modelMapper;
    }

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("entityId", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(Integer.class).fieldName("entityId").build()),
            Map.entry("entityType", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(String.class).fieldName("entityType").isContainsText(true).build()),
            Map.entry("requestedBranchId", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(Integer.class).fieldName("requestedBranchId").build()),
            Map.entry("requestedUser", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(String.class).fieldName("requestedUser").isContainsText(true).build()),
            Map.entry("requestedOn", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(LocalDateTime.class).fieldName("requestedOn").build()),
            Map.entry("requestType", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(RequestType.class).fieldName("requestType").build()),
            Map.entry("reassignedToBranchId", RunnerEntityMapping.builder().tableName(Constants.NOTIFICATION_ENTITY).dataType(Integer.class).fieldName("reassignedToBranchId").build())
    );

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Notification list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<Notification>, Pageable> tuple = fetchData(request, Notification.class, tableNames);
            Page<Notification>  notificationPage = notificationDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Notification list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(notificationPage.getContent()),
                    notificationPage.getTotalPages(), notificationPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.error("Request Id is null for Notification retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("Notification Retrieve failed because Id is null.");
            }
            long id = request.getId();
            Optional<Notification> notification = notificationDao.findById(id);
            if(!notification.isPresent()) {
                log.debug(NotificationConstants.NOTIFICATION_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Notification Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            NotificationResponse response = convertEntityToDto(notification.get());
            addDependantServiceData(response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public void addDependantServiceData(NotificationResponse notificationResponse){
        try{
            Map<String, Object> response = null;
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(notificationResponse, response)), executorService);

            CompletableFuture.allOf(tenantDataFuture).join();
        } catch (Exception ex) {
            log.error("Exception during fetching tenant data in retrieve API for guid: {} with exception: {}", notificationResponse.getGuid(), ex.getMessage());
        }
    }

    public CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall(NotificationResponse notificationResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> tenantIdList = new HashSet<>(masterDataUtils.createInBulkTenantsRequest(notificationResponse,  Notification.class, fieldNameKeyMap, Notification.class.getSimpleName(), cacheMap));

            Map<String, TenantModel> v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS, tenantIdList, new TenantModel(), cacheMap);

            if(masterDataResponse == null) {
                notificationResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Notification.class.getSimpleName()), CacheConstants.TENANTS, cacheMap));
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

    private NotificationResponse convertEntityToDto(Notification notification) {
        return jsonHelper.convertValue(notification, NotificationResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Notification> lst) {
        List<NotificationListResponse> notificationListResponses = new ArrayList<>();
        lst.forEach(notification -> {
            var response = modelMapper.map(notification, NotificationListResponse.class);
            notificationListResponses.add(response);
        });

        List<IRunnerResponse> responseList = new ArrayList<>(notificationListResponses);
        try{
            if(ObjectUtils.isNotEmpty(responseList)){
                var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorService);
                CompletableFuture.allOf(tenantDataFuture).join();
            }
        } catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_NOTIFICATION_LIST, ex.getLocalizedMessage());
        }

        return responseList;
    }
}
