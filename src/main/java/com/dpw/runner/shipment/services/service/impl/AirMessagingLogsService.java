package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.AirMessagingLogsConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAirMessagingLogsDao;
import com.dpw.runner.shipment.services.dto.request.AirMessagingLogsRequest;
import com.dpw.runner.shipment.services.dto.response.AirMessagingLogsResponse;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class AirMessagingLogsService implements IAirMessagingLogsService {
    private final IAirMessagingLogsDao airMessagingLogsDao;
    private final JsonHelper jsonHelper;
    private final PartialFetchUtils partialFetchUtils;

    @Autowired
    public AirMessagingLogsService(IAirMessagingLogsDao airMessagingLogsDao, JsonHelper jsonHelper, PartialFetchUtils partialFetchUtils) {
        this.airMessagingLogsDao = airMessagingLogsDao;
        this.jsonHelper = jsonHelper;
        this.partialFetchUtils = partialFetchUtils;
    }

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AirMessagingLogsRequest request = null;
        request = (AirMessagingLogsRequest) commonRequestModel.getData();
        AirMessagingLogs airMessagingLogs = mapToEntityFromRequest(request);
        if(request == null) {
            log.debug("Request is empty for Air Messaging Logs create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        try {
            airMessagingLogs = airMessagingLogsDao.save(airMessagingLogs);
            log.info("Air Messaging Logs created for successfully for Id {} with Request Id {}", airMessagingLogs.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(airMessagingLogs));
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        AirMessagingLogsRequest request = (AirMessagingLogsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Air Messaging Logs update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request is empty for Air Messaging Logs update");
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Air Messaging Logs update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<AirMessagingLogs> oldEntity = airMessagingLogsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug(AirMessagingLogsConstants.AIR_MESSAGING_LOGS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        AirMessagingLogs airMessagingLogs = mapToEntityFromRequest(request);
        airMessagingLogs.setId(oldEntity.get().getId());
        if(airMessagingLogs.getGuid() != null && !oldEntity.get().getGuid().equals(airMessagingLogs.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            airMessagingLogs = airMessagingLogsDao.save(airMessagingLogs);

            log.info("Updated the Air Messaging Logs details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(airMessagingLogs));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for list Air Messaging Logs with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AirMessagingLogs>, Pageable> tuple = fetchData(request, AirMessagingLogs.class);
            Page<AirMessagingLogs> airMessagingLogsPage  = airMessagingLogsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Air Messaging Logs list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertListResponse(airMessagingLogsPage.getContent()),
                    airMessagingLogsPage.getTotalPages(),
                    airMessagingLogsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for async list Air Messaging Logs with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            log.info("Retrieving Air Messaging Logs details");
            Pair<Specification<AirMessagingLogs>, Pageable> tuple = fetchData(request, AirMessagingLogs.class);
            Page<AirMessagingLogs> airMessagingLogsPage  = airMessagingLogsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Air Messaging Logs async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertListResponse(airMessagingLogsPage.getContent()),
                    airMessagingLogsPage.getTotalPages(),
                    airMessagingLogsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request is empty");
            }
            if(request.getId() == null) {
                log.debug("Request Id is null with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<AirMessagingLogs> airMessagingLogs = airMessagingLogsDao.findById(id);
            if(!airMessagingLogs.isPresent()) {
                log.debug("Air Messaging Logs is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            airMessagingLogsDao.delete(airMessagingLogs.get());

            log.info("Deleted Air Messaging Logs for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Air Messaging Logs retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request is empty for Air Messaging Logs retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Air Messaging Logs retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AirMessagingLogs> airMessagingLogs = airMessagingLogsDao.findById(id);
            if(!airMessagingLogs.isPresent()) {
                log.debug("Air Messaging Logs is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Air Messaging Logs details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            AirMessagingLogsResponse response = convertToResponse(airMessagingLogs.get());
            if(request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else
                return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public AirMessagingLogs getRecentLogForEntityGuid(UUID guid) {
        if(Objects.isNull(guid))
            return null;

        List<AirMessagingLogs> logs = airMessagingLogsDao.findByEntityGuidByQuery(guid);
        if (logs.size() == 0) {
            return null;
        }
        List<AirMessagingLogs> sortedList = logs.stream()
                .sorted(Comparator.comparing(AirMessagingLogs :: getId).reversed()).toList();
        return sortedList.get(0);
    }

    private List<IRunnerResponse> convertListResponse(List<AirMessagingLogs> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(airMessagingLogs -> responseList.add(convertToResponse(airMessagingLogs)));
        return responseList;
    }

    private AirMessagingLogs mapToEntityFromRequest(AirMessagingLogsRequest request) {
        return jsonHelper.convertValue(request, AirMessagingLogs.class);
    }

    private AirMessagingLogsResponse convertToResponse(AirMessagingLogs airMessagingLogs) {
        return jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class);
    }
}
