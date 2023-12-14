package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAllocationsDao;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAllocationsService;
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
public class AllocationsService implements IAllocationsService {

    @Autowired
    IAllocationsDao allocationsDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AllocationsRequest request = (AllocationsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Allocations Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Allocations allocations = convertRequestToAllocations(request);
        try {
            allocations = allocationsDao.save(allocations);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(allocations)
                            .prevData(null)
                            .parent(Allocations.class.getSimpleName())
                            .parentId(allocations.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Allocations Saved Successfully for Id {} with Request Id {}", allocations.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(allocations));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AllocationsRequest request = (AllocationsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Allocations Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for Allocations Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Allocations> oldEntity = allocationsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Allocations is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Allocations allocations = convertRequestToAllocations(request);
        allocations.setId(oldEntity.get().getId());

        if(allocations.getGuid() != null && !oldEntity.get().getGuid().equals(allocations.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            allocations = allocationsDao.save(allocations);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(allocations)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Allocations.class))
                            .parent(Allocations.class.getSimpleName())
                            .parentId(allocations.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updating the Allocations for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(allocations));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for allocations list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<Allocations>, Pageable> tuple = fetchData(request, Allocations.class);
            Page<Allocations> allocationsPage = allocationsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Allocations list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(allocationsPage.getContent()),
                    allocationsPage.getTotalPages(),
                    allocationsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for allocations async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<Allocations>, Pageable> tuple = fetchData(request, Allocations.class);
            Page<Allocations> allocationsPage = allocationsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Allocations async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(allocationsPage.getContent()),
                    allocationsPage.getTotalPages(),
                    allocationsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Allocations Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Allocations Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Allocations> allocations = allocationsDao.findById(id);
            if (allocations.isEmpty()) {
                log.debug("Allocations is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Deleted Allocations for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            String oldEntityJsonString = jsonHelper.convertToJson(allocations.get());
            allocationsDao.delete(allocations.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Allocations.class))
                            .parent(Allocations.class.getSimpleName())
                            .parentId(allocations.get().getId())
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
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Allocations retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Allocations retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Allocations> allocations = allocationsDao.findById(id);
            if (allocations.isEmpty()) {
                log.debug("Allocations is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Allocations fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            AllocationsResponse response = convertEntityToDto(allocations.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AllocationsResponse convertEntityToDto(Allocations allocations) {
        return jsonHelper.convertValue(allocations, AllocationsResponse.class);
    }

    private Allocations convertRequestToAllocations(AllocationsRequest request) {
        return jsonHelper.convertValue(request, Allocations.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<Allocations> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}
