package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentServiceImplV3 implements IShipmentServiceV3 {

    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private INotificationDao notificationDao;
    private CommonUtils commonUtils;
    private IShipmentRepository shipmentRepository;
    private IShipmentDao shipmentDao;
    private ShipmentMasterDataHelperV3 shipmentMasterDataHelper;

    @Autowired
    public ShipmentServiceImplV3(IConsoleShipmentMappingDao consoleShipmentMappingDao, INotificationDao notificationDao, CommonUtils commonUtils, IShipmentRepository shipmentRepository,
                                 IShipmentDao shipmentDao, ShipmentMasterDataHelperV3 shipmentMasterDataHelper) {
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.notificationDao = notificationDao;
        this.commonUtils = commonUtils;
        this.shipmentRepository = shipmentRepository;
        this.shipmentDao = shipmentDao;
        this.shipmentMasterDataHelper = shipmentMasterDataHelper;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {

        Integer tenantId = TenantContext.getCurrentTenant();
        Integer count = consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal(), tenantId);
        count += notificationDao.findAllPendingNotificationCount(SHIPMENT, tenantId);

        return ResponseHelper.buildSuccessResponse(NotificationCount.builder().count(count).build());
    }

    @Override
    public ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData) {
        String responseMsg;
        try {
            ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
            int totalPage = 0;
            long totalElements = 0;
            if (listCommonRequest == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
                throw new ValidationException("Include Columns field is mandatory");
            }
            Set<String> includeColumns = new HashSet<>(listCommonRequest.getIncludeColumns());
            CommonUtils.includeRequiredColumns(includeColumns);
            if(Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
                Page<Long> eligibleShipmentId = shipmentDao.getIdWithPendingActions(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED,
                        PageRequest.of(Math.max(0,listCommonRequest.getPageNo()-1), listCommonRequest.getPageSize()));

                List<Long> shipmentIds = notificationDao.findEntityIdsByEntityType(SHIPMENT);

                Set<Long> uniqueShipmentIds = new HashSet<>(eligibleShipmentId.getContent());
                uniqueShipmentIds.addAll(shipmentIds);

                List<Long> combinedShipmentIds = new ArrayList<>(uniqueShipmentIds);

                andCriteria("id", combinedShipmentIds, "IN", listCommonRequest);

                totalElements = combinedShipmentIds.size();
                int pageSize = listCommonRequest.getPageSize();
                totalPage = (int) ((totalElements + pageSize - 1) / pageSize);
            }

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentConstants.TABLES_NAMES);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentRepository.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_V3_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());

            if(!Boolean.TRUE.equals(listCommonRequest.getNotificationFlag())) {
                totalPage = shipmentDetailsPage.getTotalPages();
                totalElements = shipmentDetailsPage.getTotalElements();
            }

            List<ShipmentListResponse> shipmentListResponses = new ArrayList<>();

            for (var curr : shipmentDetailsPage.getContent()) {
                ShipmentListResponse shipmentListResponse = (ShipmentListResponse) commonUtils.setIncludedFieldsToResponse(curr, includeColumns, new ShipmentListResponse());
                shipmentListResponses.add(shipmentListResponse);
            }
            List<IRunnerResponse> filteredList = convertEntityListToDtoList(shipmentDetailsPage.getContent(), getMasterData, shipmentListResponses, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()), listCommonRequest.getNotificationFlag());

            return ResponseHelper.buildListSuccessResponse(
                    filteredList,
                    totalPage,
                    totalElements);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst, boolean getMasterData, List<ShipmentListResponse> shipmentListResponses, Set<String> includeColumns, Boolean notificationFlag) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        Map<Long, ShipmentDetails> shipmentDetailsMap = lst.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
        // Pending Notification Count
        if(Boolean.TRUE.equals(notificationFlag)) {
            List<Long> shipmentIdList = lst.stream().map(ShipmentDetails::getId).toList();
            var map = consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(shipmentIdList, ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal());
            var notificationMap = notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(shipmentIdList, SHIPMENT);
            shipmentListResponses.forEach(response -> {
                int pendingCount = map.getOrDefault(response.getId(), 0) + notificationMap.getOrDefault(response.getId(), 0);
                response.setPendingActionCount((pendingCount == 0) ? null : pendingCount);
            });
        }
        shipmentListResponses.forEach(response -> {
            var ship = shipmentDetailsMap.get(response.getId());
            if(includeColumns.contains(SHIPPER_REFERENCE))
                setShipperReferenceNumber(response, ship);
            if (includeColumns.contains(SHIPMENT_STATUS_FIELDS) && ship.getStatus() != null && ship.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[ship.getStatus()].toString());
            if(includeColumns.contains(ORDERS_COUNT) && ObjectUtils.isNotEmpty(ship.getShipmentOrders()))
                response.setOrdersCount(ship.getShipmentOrders().size());
            responseList.add(response);
        });
        shipmentMasterDataHelper.getMasterDataForList(lst, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    private void setShipperReferenceNumber(ShipmentListResponse response, ShipmentDetails ship){
        if(ship.getReferenceNumbersList() != null && !ship.getReferenceNumbersList().isEmpty()){
            Optional<String> srnReferenceNumber = ship.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equalsIgnoreCase(SRN))
                    .findFirst()
                    .map(a -> a.getReferenceNumber());

            if(srnReferenceNumber.isPresent() && response.getPickupDetails() != null){
                response.getPickupDetails().setShipperRef(srnReferenceNumber.get());
            }
        }
    }

    public ResponseEntity<IRunnerResponse> getPendingNotificationData(CommonGetRequest request){

        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(request.getId());
        if (!optionalShipmentDetails.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_FOR_GUID_MISSING_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        if (request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty()) {
            request.setIncludeColumns(List.of("transportMode", "containsHazardous", "guid", "receivingBranch", "triangulationPartnerList", "tenantId"));
        }
        Set<String> includeColumns = new HashSet<>(request.getIncludeColumns());
        CommonUtils.includeRequiredColumns(includeColumns);

        ShipmentPendingNotificationResponse shipmentResponse = (ShipmentPendingNotificationResponse) commonUtils.setIncludedFieldsToResponse(optionalShipmentDetails.get(), includeColumns, new ShipmentPendingNotificationResponse());
        shipmentMasterDataHelper.getMasterDataForEntity(shipmentResponse);
        return ResponseHelper.buildSuccessResponse(shipmentResponse);
    }

}
