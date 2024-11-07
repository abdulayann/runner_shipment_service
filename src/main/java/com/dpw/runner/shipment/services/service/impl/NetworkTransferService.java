package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NetworkTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dto.request.TransferredNetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.enums.Operators;
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
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class NetworkTransferService implements INetworkTransferService {

    private final ModelMapper modelMapper;

    private final JsonHelper jsonHelper;

    private final INetworkTransferDao networkTransferDao;

    private final IV1Service v1Service;

    @Autowired
    public NetworkTransferService(ModelMapper modelMapper, JsonHelper jsonHelper,
                                  INetworkTransferDao networkTransferDao, IV1Service v1Service) {
        this.modelMapper = modelMapper;
        this.jsonHelper = jsonHelper;
        this.networkTransferDao = networkTransferDao;
        this.v1Service = v1Service;
    }


    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(NetworkTransferStatus.class).fieldName("status").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("entityType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("sourceBranchName", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(String.class).isContainsText(true).build())
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
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(networkTransfer -> {
            NetworkTransferResponse response = modelMapper.map(networkTransfer, NetworkTransferResponse.class);
            responseList.add(response);
        });
        return responseList;
    }


    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
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
            double current = System.currentTimeMillis();
            log.info("NetworkTransfer details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch networkTransfer details from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            NetworkTransferResponse networkTransferResponse = jsonHelper.convertValue(networkTransfer.get(), NetworkTransferResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(networkTransferResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public NetworkTransfer getNetworkTransferEntityFromShipment(ShipmentDetails shipmentDetails, Long tenantId){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(Math.toIntExact(tenantId));
        networkTransfer.setEntityId(shipmentDetails.getId());
        networkTransfer.setEntityType(Constants.SHIPMENT);
        networkTransfer.setEntityNumber(shipmentDetails.getShipmentId());
        networkTransfer.setTransportMode(shipmentDetails.getTransportMode());
        networkTransfer.setSourceBranchId(shipmentDetails.getTenantId());
        return networkTransfer;
    }

    public NetworkTransfer getNetworkTransferEntityFromConsolidation(ConsolidationDetails consolidationDetails, Long tenantId){
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setTenantId(Math.toIntExact(tenantId));
        networkTransfer.setEntityId(consolidationDetails.getId());
        networkTransfer.setEntityType(Constants.CONSOLIDATION);
        networkTransfer.setEntityNumber(consolidationDetails.getConsolidationNumber());
        networkTransfer.setTransportMode(consolidationDetails.getTransportMode());
        networkTransfer.setSourceBranchId(consolidationDetails.getTenantId());
        return networkTransfer;
    }

    @Transactional
    public void processNetworkTransferEntity(String entityType, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                             Long tenantId, Long oldTenantId, Long entityId){
            NetworkTransfer networkTransfer = null;

            if (tenantId == null || (oldTenantId != null && Objects.equals(oldTenantId, tenantId)))
                return;

            if (oldTenantId != null)
                networkTransferDao.findByTenantIdAndEntityId(Math.toIntExact(oldTenantId), entityId).ifPresent(networkTransferEntity ->
                        networkTransferDao.deleteAndLog(networkTransferEntity, entityType, oldTenantId));

            Optional<String> tenantNames = getTenantName(new ArrayList<>(List.of(tenantId)));
            String tenantNamesString = tenantNames.orElse(null);
            if (Objects.equals(entityType, Constants.SHIPMENT) && ObjectUtils.isNotEmpty(shipmentDetails))
                networkTransfer = getNetworkTransferEntityFromShipment(shipmentDetails, tenantId);
            else if (Objects.equals(entityType, Constants.CONSOLIDATION) && ObjectUtils.isNotEmpty(consolidationDetails))
                networkTransfer = getNetworkTransferEntityFromConsolidation(consolidationDetails, tenantId);

            if (!Objects.isNull(networkTransfer)) {
                networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
                networkTransfer.setSourceBranchName(tenantNamesString);
                networkTransferDao.save(networkTransfer);
            }
    }


    private Optional<String> getTenantName(List<Long> tenantIds) {
        try {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of(CustomerBookingConstants.TENANT_ID));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(tenantIds)));
            request.setCriteriaRequests(criteria);
            V1DataResponse tenantName = v1Service.tenantNameByTenantId(request);
            List<V1TenantResponse> v1TenantResponse = jsonHelper.convertValueToList(tenantName.entities, V1TenantResponse.class);
            if (v1TenantResponse != null) {
                return v1TenantResponse.stream().map(V1TenantResponse::getTenantName).findFirst();
            }
        } catch (Exception e){
            log.error("Request: {} || Error while getting name with exception: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage());
        }
        return Optional.empty();
    }

    @Override
    public ResponseEntity<IRunnerResponse> transferredNetworkTransferStatus(TransferredNetworkTransferRequest request){
        if (request == null || request.getId() == null) {
            log.error("Request Id is null for NetworkTransfer update status with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        Long id = request.getId();
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findById(id);
        if (optionalNetworkTransfer.isEmpty()) {
            log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        NetworkTransfer networkTransfer = optionalNetworkTransfer.get();
        if(ObjectUtils.isEmpty(networkTransfer.getStatus()) || (ObjectUtils.isNotEmpty(networkTransfer.getStatus()) && networkTransfer.getStatus()!=NetworkTransferStatus.SCHEDULED))
            throw new ValidationException("Network Transfer alterations is not allowed with this Status.");

        networkTransfer.setStatus(NetworkTransferStatus.TRANSFERRED);
        NetworkTransfer newNetworkTransfer = networkTransferDao.save(networkTransfer);
        NetworkTransferResponse networkTransferResponse = jsonHelper.convertValue(newNetworkTransfer, NetworkTransferResponse.class);
        return ResponseHelper.buildSuccessResponse(networkTransferResponse);
    }

}
