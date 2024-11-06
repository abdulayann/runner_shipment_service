package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.EVgmConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IEVgmDao;
import com.dpw.runner.shipment.services.dto.request.EVgmRequest;
import com.dpw.runner.shipment.services.dto.response.EVgmResponse;
import com.dpw.runner.shipment.services.entity.EVgm;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEVgmService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class EVgmService implements IEVgmService{
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IEVgmDao eVgmDao;

    @Autowired
    private AuditLogService auditLogService;

    @Autowired
    private MasterDataUtils masterDataUtils;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.EVGM).dataType(String.class).fieldName("createdBy").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.EVGM).dataType(String.class).fieldName("id").build())
    );

    @Override
    @Transactional(rollbackFor = RunnerException.class)
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException {
        EVgmRequest request = (EVgmRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for EVgm Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        EVgm eVgmDetails = jsonHelper.convertValue(request, EVgm.class);
        eVgmDetails = eVgmDao.save(eVgmDetails);

        createAuditMetaData(eVgmDetails, DBOperationType.CREATE.name(), null);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(eVgmDetails, EVgmResponse.class));
    }


    private void createAuditMetaData(EVgm eVgm, String operation, EVgm prevData){
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(eVgm)
                            .prevData(prevData)
                            .parent(EVgm.class.getSimpleName())
                            .parentId(eVgm.getId())
                            .operation(operation).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || request.getId() == null) {
                log.debug("Request is empty for eVgm delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            long id = request.getId();
            Optional<EVgm> eVgm = eVgmDao.findById(id);
            if (eVgm.isEmpty()) {
                log.debug(EVgmConstants.EVGM_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            eVgmDao.delete(eVgm.get());
            log.info("Deleted eVgm details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for EVgm list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<EVgm>, Pageable> tuple = fetchData(request, EVgm.class, tableNames);
            Page<EVgm> eVgmPage = eVgmDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("EVgm list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(eVgmPage.getContent()),
                    eVgmPage.getTotalPages(),
                    eVgmPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    @NotNull
    private List<IRunnerResponse> convertEntityListToDtoList(List<EVgm> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(eVgm -> {
            EVgmResponse response = modelMapper.map(eVgm, EVgmResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return responseList;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        EVgmRequest request = (EVgmRequest) commonRequestModel.getData();
        if (request == null || request.getId() == null) {
            log.error("Request is empty for evgm update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        long id = request.getId();
        Optional<EVgm> oldEntity = eVgmDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(EVgmConstants.EVGM_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        EVgm eVgm = jsonHelper.convertValue(request, EVgm.class);
        eVgm.setCreatedAt(oldEntity.get().getCreatedAt());
        eVgm.setCreatedBy(oldEntity.get().getCreatedBy());
        
        eVgm = eVgmDao.save(eVgm);
        createAuditMetaData(eVgm, DBOperationType.UPDATE.name(), oldEntity.get());
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(eVgm, EVgm.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for EVgm retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<EVgm> eVgm;
            if(id != null) {
                eVgm = eVgmDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                eVgm = eVgmDao.findByGuid(guid);
            }

            if (eVgm.isEmpty()) {
                log.debug(EVgmConstants.EVGM_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            double current = System.currentTimeMillis();
            log.info("EVgm details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch evgm from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            EVgmResponse eVgmResponse = jsonHelper.convertValue(eVgm.get(), EVgmResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
//            createeVgmResponse(eVgm.get(), eVgmResponse);
            return ResponseHelper.buildSuccessResponse(eVgmResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
