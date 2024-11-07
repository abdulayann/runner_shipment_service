package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NetworkTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class NetworkTransferService implements INetworkTransferService {
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private AuditLogService auditLogService;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.NETWORK_TRANSFER_ENTITY).dataType(NetworkTransferStatus.class).fieldName("status").build())
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

    public void createNetworkTransferEntity(String entityType, ShipmentDetails shipmentDetails, Long tenantId, ConsolidationDetails consolidationDetails){
        NetworkTransfer networkTransfer = null;
        if(Objects.equals(entityType, Constants.SHIPMENT) && ObjectUtils.isNotEmpty(shipmentDetails)){
            networkTransfer = getNetworkTransferEntityFromShipment(shipmentDetails, tenantId);
        } else if (Objects.equals(entityType, Constants.CONSOLIDATION)  && ObjectUtils.isNotEmpty(consolidationDetails)) {
            networkTransfer = getNetworkTransferEntityFromConsolidation(consolidationDetails, tenantId);
        }
        if (!Objects.isNull(networkTransfer)){
            networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
            networkTransferDao.save(networkTransfer);
        }
    }

}
