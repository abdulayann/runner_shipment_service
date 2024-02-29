package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.ArrivalDepartureConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IArrivalDepartureDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ArrivalDepartureDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IArrivalDepartureDetailsService;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class ArrivalDepartureDetailsService implements IArrivalDepartureDetailsService {

    @Autowired
    IArrivalDepartureDetailsDao arrivalDepartureDetailsDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ArrivalDepartureDetailsRequest request = (ArrivalDepartureDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Arrival Departure Details Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ArrivalDepartureDetails arrivalDepartureDetails = convertRequestToArrivalDepartureDetails(request);

        try {
            arrivalDepartureDetails = arrivalDepartureDetailsDao.save(arrivalDepartureDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(arrivalDepartureDetails)
                            .prevData(null)
                            .parent(ArrivalDepartureDetails.class.getSimpleName())
                            .parentId(arrivalDepartureDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Arrival Departure Details Saved Successfully for Id {} with Request Id {}", arrivalDepartureDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(arrivalDepartureDetails));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ArrivalDepartureDetailsRequest request = (ArrivalDepartureDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Arrival Departure Details Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for Arrival Departure Details Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ArrivalDepartureDetails> oldEntity = arrivalDepartureDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(ArrivalDepartureConstants.ARRIVAL_DEPARTURE_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ArrivalDepartureDetails arrivalDepartureDetails = convertRequestToArrivalDepartureDetails(request);
        arrivalDepartureDetails.setId(oldEntity.get().getId());
        if(arrivalDepartureDetails.getGuid() != null && !oldEntity.get().getGuid().equals(arrivalDepartureDetails.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            arrivalDepartureDetails = arrivalDepartureDetailsDao.save(arrivalDepartureDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(arrivalDepartureDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ArrivalDepartureDetails.class))
                            .parent(ArrivalDepartureDetails.class.getSimpleName())
                            .parentId(arrivalDepartureDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updating the Arrival Departure Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(arrivalDepartureDetails));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Arrival Departure Details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ArrivalDepartureDetails>, Pageable> tuple = fetchData(request, ArrivalDepartureDetails.class);
            Page<ArrivalDepartureDetails> arrivalDepartureDetailsPage = arrivalDepartureDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Arrival Departure Details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(arrivalDepartureDetailsPage.getContent()),
                    arrivalDepartureDetailsPage.getTotalPages(),
                    arrivalDepartureDetailsPage.getTotalElements());
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
            if (request == null) {
                log.error("Request is empty for Arrival Departure Details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ArrivalDepartureDetails>, Pageable> tuple = fetchData(request, ArrivalDepartureDetails.class);
            Page<ArrivalDepartureDetails> arrivalDepartureDetailsPage = arrivalDepartureDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Arrival Departure Details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(arrivalDepartureDetailsPage.getContent()),
                    arrivalDepartureDetailsPage.getTotalPages(),
                    arrivalDepartureDetailsPage.getTotalElements()));
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
            if (request == null) {
                log.error("Request is empty for Arrival Departure Details Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Arrival Departure Details Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ArrivalDepartureDetails> arrivalDepartureDetails = arrivalDepartureDetailsDao.findById(id);
            if (arrivalDepartureDetails.isEmpty()) {
                log.debug(ArrivalDepartureConstants.ARRIVAL_DEPARTURE_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Deleted Arrival Departure Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            String oldEntityJsonString = jsonHelper.convertToJson(arrivalDepartureDetails.get());
            arrivalDepartureDetailsDao.delete(arrivalDepartureDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ArrivalDepartureDetails.class))
                            .parent(ArrivalDepartureDetails.class.getSimpleName())
                            .parentId(arrivalDepartureDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
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
            if (request == null) {
                log.error("Request is empty for Arrival Departure Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Arrival Departure Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ArrivalDepartureDetails> arrivalDepartureDetails = arrivalDepartureDetailsDao.findById(id);
            if (arrivalDepartureDetails.isEmpty()) {
                log.debug(ArrivalDepartureConstants.ARRIVAL_DEPARTURE_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Arrival Departure Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ArrivalDepartureDetailsResponse response = convertEntityToDto(arrivalDepartureDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ArrivalDepartureDetailsResponse convertEntityToDto(ArrivalDepartureDetails arrivalDepartureDetails) {
        return jsonHelper.convertValue(arrivalDepartureDetails, ArrivalDepartureDetailsResponse.class);
    }

    private ArrivalDepartureDetails convertRequestToArrivalDepartureDetails(ArrivalDepartureDetailsRequest request) {
        return jsonHelper.convertValue(request, ArrivalDepartureDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<ArrivalDepartureDetails> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}
