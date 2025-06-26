package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NotificationConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.exception.exceptions.NotificationServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class NotificationV3Service implements INotificationV3Service {


    private final INotificationDao notificationDao;

    private final JsonHelper jsonHelper;

    private final MasterDataUtils masterDataUtils;

    ExecutorService executorService;

    private final MasterDataKeyUtils masterDataKeyUtils;

    private final IShipmentServiceV3 shipmentService;

    private final IConsolidationV3Service consolidationService;

    private final V1ServiceUtil v1ServiceUtil;

    private final IConsolidationDetailsDao consolidationDetailsDao;

    private final INetworkTransferDao networkTransferDao;

    @Autowired
    public NotificationV3Service(JsonHelper jsonHelper, INotificationDao notificationDao,
                                 MasterDataUtils masterDataUtils, ExecutorService executorService,
                                 MasterDataKeyUtils masterDataKeyUtils, IShipmentServiceV3 shipmentService,
                                 IConsolidationV3Service consolidationService, V1ServiceUtil v1ServiceUtil, INetworkTransferDao networkTransferDao,
                                 IConsolidationDetailsDao consolidationDetailsDao) {
        this.jsonHelper = jsonHelper;
        this.notificationDao = notificationDao;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.shipmentService = shipmentService;
        this.consolidationService = consolidationService;
        this.v1ServiceUtil = v1ServiceUtil;
        this.networkTransferDao = networkTransferDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Notification retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<Notification> notification;
            if(id != null) {
                notification = notificationDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                notification = notificationDao.findByGuid(guid);
            }
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
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllTenantDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), NotificationResponse.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private NotificationResponse convertEntityToDto(Notification notification) {
        return jsonHelper.convertValue(notification, NotificationResponse.class);
    }

    @Override
    public void acceptNotification(Long id) {
        if(id == null) {
            log.error("Id is null for Notification accept with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Notification accept failed because Id is null.");
        }
        Optional<Notification> notification = notificationDao.findById(id);
        if(!notification.isPresent()) {
            log.debug(NotificationConstants.NOTIFICATION_RETRIEVE_BY_ID_ERROR, id, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(Objects.equals(notification.get().getNotificationRequestType(), NotificationRequestType.REASSIGN)) {
            processReassignBranchForEntityTransfer(notification.get());
        }
        notificationDao.delete(notification.get());

        log.info("Notification accept successful for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
    }


    @Override
    public NotificationConfirmationMsgResponse confirmationMessage(Long id) throws AuthenticationException, RunnerException {
        if(id == null ) {
            log.error("Id is null for Notification Confirmation message with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Notification Confirmation message failed because Id is null.");
        }
        NotificationResponse notificationResponse = getNotificationResponseById(id);
        if(notificationResponse == null) {
            log.debug(NotificationConstants.NOTIFICATION_RETRIEVE_BY_ID_ERROR, id, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Long receivingBranch = null;
        List<Long> triangulationPartners = new ArrayList<>();
        CommonGetRequest request = CommonGetRequest.builder().id(notificationResponse.getEntityId()).build();
        if(Objects.equals(notificationResponse.getEntityType(), Constants.SHIPMENT)) {
            ShipmentRetrieveLiteResponse shipmentDetailsResponse = shipmentService.retrieveById(CommonRequestModel.buildRequest(request), false, null);
            receivingBranch = shipmentDetailsResponse.getReceivingBranch();
            triangulationPartners = Optional.ofNullable(shipmentDetailsResponse.getTriangulationPartnerList())
                    .orElse(Collections.emptyList())
                    .stream()
                    .map(TriangulationPartnerResponse::getTriangulationPartner).toList();
        } else if (Objects.equals(notificationResponse.getEntityType(), Constants.CONSOLIDATION)){
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getId());
            receivingBranch = consolidationDetails.get().getReceivingBranch();
            triangulationPartners = Optional.ofNullable(consolidationDetails.get().getTriangulationPartnerList())
                    .orElse(Collections.emptyList())
                    .stream()
                    .map(TriangulationPartner::getTriangulationPartner).toList();
        }
        String oldBranchName = notificationResponse.getTenantIdsData().getOrDefault(
                NotificationConstants.RECEIVING_BRANCH_ID_FIELD,
                notificationResponse.getRequestedBranchId().toString()
        );
        String newBranchName = notificationResponse.getTenantIdsData().getOrDefault(
                NotificationConstants.REASSIGNED_TO_BRANCH_ID_FIELD,
                notificationResponse.getReassignedToBranchId().toString()
        );
        String confirmationMsg = getConfirmationMessage(Long.valueOf(notificationResponse.getReassignedFromBranchId()),
                receivingBranch, triangulationPartners,
                oldBranchName, newBranchName
        );
        return NotificationConfirmationMsgResponse.builder().message(confirmationMsg).build();
    }

    private NotificationResponse getNotificationResponseById(Long id) {
        CommonGetRequest commonRequest = CommonGetRequest.builder().id(id).build();
        ResponseEntity<IRunnerResponse> response = retrieveById(CommonRequestModel.buildRequest(commonRequest));
        RunnerResponse<NotificationResponse> runnerResponse = (RunnerResponse<NotificationResponse>) response.getBody();
        return runnerResponse != null ? runnerResponse.getData() : null;
    }

    private String getConfirmationMessage(Long requestedBranchId, Long receivingBranch, List<Long> triangulationPartners,
                                            String oldBranchName, String newBranchName) {

        if(receivingBranch != null && Objects.equals(receivingBranch, requestedBranchId)) {
            return String.format(NotificationConstants.RECEIVING_BRANCH_MSG, oldBranchName, newBranchName);
        } else if(!CommonUtils.listIsNullOrEmpty(triangulationPartners) && triangulationPartners.contains(requestedBranchId)) {
            return String.format(NotificationConstants.TRAINGULATION_BRANCH_MSG, oldBranchName, newBranchName);
        } else {
            throw new InputMismatchException("Requested Branch does not match with Receiving or Triangulation Partners.");
        }
    }

    private void processReassignBranchForEntityTransfer(Notification notification) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(notification.getEntityId()).build();
            Optional< NetworkTransfer > optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                    Math.toIntExact(notification.getReassignedToBranchId()), notification.getEntityId(), notification.getEntityType());
            if (optionalNetworkTransfer.isPresent()) {
                throw new ValidationException("Network Transfer already exist on the reassigned branch.");
            }
            if (Objects.equals(notification.getEntityType(), Constants.SHIPMENT)) {
                processShipmentReassignment(notification, request);
            } else {
                processConsolidationReassignment(notification, request);
            }
        } catch (RunnerException e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NotificationConstants.REASSIGN_BRANCH_ERROR;
            log.error(responseMsg, e);
            throw new NotificationServiceException(responseMsg);
        } catch (AuthenticationException e) {
            throw new NotificationServiceException(e.getMessage());
        }
    }

    private void processShipmentReassignment(Notification notification, CommonGetRequest request) throws RunnerException, AuthenticationException {
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = shipmentService.retrieveById(CommonRequestModel.buildRequest(request), false, null);
        List<Long> triangulationPartners = Optional.ofNullable(shipmentDetailsResponse.getTriangulationPartnerList())
                .orElse(Collections.emptyList())
                .stream()
                .map(TriangulationPartnerResponse::getTriangulationPartner).toList();
        String branchType = getReassignType(notification.getReassignedFromBranchId().longValue(), shipmentDetailsResponse.getReceivingBranch(), triangulationPartners);

        ShipmentV3Request shipmentRequest = jsonHelper.convertValue(shipmentDetailsResponse, ShipmentV3Request.class);
        Long requestedBranchId = notification.getReassignedFromBranchId() != null ? notification.getReassignedFromBranchId().longValue() : null;
        Long reassignedToBranchId = notification.getReassignedToBranchId() != null ? notification.getReassignedToBranchId().longValue() : null;
        if (Objects.equals(branchType, NotificationConstants.RECEIVING_BRANCH)) {
            shipmentRequest.setReceivingBranch(reassignedToBranchId);
            if(shipmentDetailsResponse.getConsolidationId()!=null)
                shipmentRequest.setIsReceivingBranchAdded(true);
            if (!Boolean.TRUE.equals(shipmentDetailsResponse.getIsReceivingBranchManually())) {
                PartiesRequest partiesRequest = getPartiesRequestFromTenantDefaultOrg(notification.getReassignedToBranchId());
                Optional.ofNullable(partiesRequest).ifPresent(importBroker -> shipmentRequest.getAdditionalDetails().setImportBroker(importBroker));
            }
        } else if(Objects.equals(branchType, NotificationConstants.TRAINGULATION_BRANCH)) {
            List<TriangulationPartnerRequest> triangulationPartnerRequestList = processTriangulationPartners(shipmentRequest.getTriangulationPartnerList(),
                    requestedBranchId, reassignedToBranchId);
            if (CommonUtils.listIsNullOrEmpty(triangulationPartnerRequestList)) {
                shipmentRequest.setTriangulationPartnerList(null);
            } else {
                shipmentRequest.setTriangulationPartnerList(triangulationPartnerRequestList);
            }
        }
        shipmentService.completeUpdate(CommonRequestModel.buildRequest(shipmentRequest));
    }

    private void processConsolidationReassignment(Notification notification, CommonGetRequest request) throws RunnerException {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getId());
        if(consolidationDetails.isEmpty()) {
            return;
        }
        List<Long> triangulationPartners = Optional.ofNullable(consolidationDetails.get().getTriangulationPartnerList())
                .orElse(Collections.emptyList())
                .stream()
                .map(TriangulationPartner::getTriangulationPartner).toList();
        String branchType = getReassignType(notification.getReassignedFromBranchId().longValue(), consolidationDetails.get().getReceivingBranch(), triangulationPartners);

        ConsolidationDetailsV3Request consolidationRequest = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsV3Request.class);
        Long requestedBranchId = notification.getReassignedFromBranchId() != null ? notification.getReassignedFromBranchId().longValue() : null;
        Long reassignedToBranchId = notification.getReassignedToBranchId() != null ? notification.getReassignedToBranchId().longValue() : null;
        if (Objects.equals(branchType, NotificationConstants.RECEIVING_BRANCH)) {
            consolidationRequest.setReceivingBranch(reassignedToBranchId);
            if (!Boolean.TRUE.equals(consolidationDetails.get().getIsReceivingBranchManually())) {
                PartiesRequest partiesRequest = getPartiesRequestFromTenantDefaultOrg(notification.getReassignedToBranchId());
                Optional.ofNullable(partiesRequest).ifPresent(consolidationRequest::setReceivingAgent);
            }
        } else if(Objects.equals(branchType, NotificationConstants.TRAINGULATION_BRANCH)) {
            List<TriangulationPartnerRequest> triangulationPartnerRequestList = processTriangulationPartners(consolidationRequest.getTriangulationPartnerList(),
                    requestedBranchId, reassignedToBranchId);
            if (CommonUtils.listIsNullOrEmpty(triangulationPartnerRequestList)) {
                consolidationRequest.setTriangulationPartnerList(null);
            } else {
                consolidationRequest.setTriangulationPartnerList(triangulationPartnerRequestList);
            }
        }
        consolidationService.completeUpdate(consolidationRequest);
    }

    public String getReassignType(Long requestedBranchId, Long receivingBranch, List<Long> triangulationPartners) {
        if(receivingBranch != null && Objects.equals(receivingBranch, requestedBranchId)) {
            return NotificationConstants.RECEIVING_BRANCH;
        } else if(!CommonUtils.listIsNullOrEmpty(triangulationPartners) && triangulationPartners.contains(requestedBranchId)) {
            return NotificationConstants.TRAINGULATION_BRANCH;
        } else {
            throw new InputMismatchException("Requested Branch does not match with Receiving or Triangulation Partners.");
        }
    }

    public PartiesRequest getPartiesRequestFromTenantDefaultOrg(Integer tenantId) {
        PartiesRequest partiesRequest = null;
        var v1Map = v1ServiceUtil.getTenantDetails(List.of(tenantId));
        if(!v1Map.isEmpty() && v1Map.containsKey(tenantId)) {
            TenantModel tenantModel = jsonHelper.convertValue(v1Map.get(tenantId), TenantModel.class);
            if(tenantModel.getDefaultOrgId() == null || tenantModel.getDefaultAddressId() == null)
                return partiesRequest;
            partiesRequest = v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(tenantModel.getDefaultOrgId(), tenantModel.getDefaultAddressId());
        }
        return partiesRequest;
    }

    public List<TriangulationPartnerRequest> processTriangulationPartners(List<TriangulationPartnerRequest> triangulationPartnerRequestList, Long requestedBranchId, Long reassignedToBranchId) {
        if (CommonUtils.listIsNullOrEmpty(triangulationPartnerRequestList)) {
            return new ArrayList<>();
        }

        // Remove matching triangulation partner
        if (requestedBranchId != null) {
            triangulationPartnerRequestList.removeIf(request ->
                    requestedBranchId.equals(request.getTriangulationPartner())
            );
        }

        // Add the new triangulation partner
        triangulationPartnerRequestList.add(
                TriangulationPartnerRequest.builder()
                        .triangulationPartner(reassignedToBranchId)
                        .isAccepted(false)
                        .build()
        );

        return triangulationPartnerRequestList;
    }
}
