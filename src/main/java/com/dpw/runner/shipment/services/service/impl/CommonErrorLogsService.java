package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NetworkTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dto.response.CommonErrorLogsResponse;
import com.dpw.runner.shipment.services.entity.CommonErrorLogs;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICommonErrorLogsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CommonErrorLogsService implements ICommonErrorLogsService {

    private final ModelMapper modelMapper;
    private final JsonHelper jsonHelper;
    private final ICommonErrorLogsDao commonErrorLogsDao;

    @Autowired
    public CommonErrorLogsService(ICommonErrorLogsDao commonErrorLogsDao, ModelMapper modelMapper, JsonHelper jsonHelper) {
        this.commonErrorLogsDao = commonErrorLogsDao;
        this.modelMapper = modelMapper;
        this.jsonHelper = jsonHelper;
    }

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("entityId", RunnerEntityMapping.builder().tableName(Constants.COMMON_ERROR_LOGS_ENTITY).dataType(Long.class).fieldName("entityId").build()),
            Map.entry("entityType", RunnerEntityMapping.builder().tableName(Constants.COMMON_ERROR_LOGS_ENTITY).dataType(String.class).fieldName("entityType").build()),
            Map.entry("errorType", RunnerEntityMapping.builder().tableName(Constants.COMMON_ERROR_LOGS_ENTITY).dataType(CommonErrorType.class).fieldName("errorType").build())
    );

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for CommonErrorLogs list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<CommonErrorLogs>, Pageable> tuple = fetchData(request, CommonErrorLogs.class, tableNames);
            Page<CommonErrorLogs> commonErrorLogsPage = commonErrorLogsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("CommonErrorLogs list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(commonErrorLogsPage.getContent()),
                    commonErrorLogsPage.getTotalPages(), commonErrorLogsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CommonErrorLogs> lst) {
        List<CommonErrorLogsResponse> commonErrorLogsResponses = new ArrayList<>();
        lst.forEach(commonErrorLogs -> {
            var response = modelMapper.map(commonErrorLogs, CommonErrorLogsResponse.class);
            commonErrorLogsResponses.add(response);
        });
        return new ArrayList<>(commonErrorLogsResponses);
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
            Optional<CommonErrorLogs> commonErrorLogs;
            if(id != null) {
                commonErrorLogs = commonErrorLogsDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                commonErrorLogs = commonErrorLogsDao.findByGuid(guid);
            }

            if (commonErrorLogs.isEmpty()) {
                log.debug(NetworkTransferConstants.NETWORK_TRANSFER_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            CommonErrorLogsResponse commonErrorLogsResponse = jsonHelper.convertValue(commonErrorLogs.get(), CommonErrorLogsResponse.class);
            return ResponseHelper.buildSuccessResponse(commonErrorLogsResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
