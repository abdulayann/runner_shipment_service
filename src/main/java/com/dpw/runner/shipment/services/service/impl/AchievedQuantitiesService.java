package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAchievedQuantitiesDao;
import com.dpw.runner.shipment.services.dto.request.AchievedQuantitiesRequest;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAchievedQuantitiesService;
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
public class AchievedQuantitiesService implements IAchievedQuantitiesService {

    @Autowired
    IAchievedQuantitiesDao achievedQuantitiesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private AuditLogService auditLogService;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AchievedQuantitiesRequest request = (AchievedQuantitiesRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Achieved Quantities Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        AchievedQuantities achievedQuantities = convertRequestToAchievedQuantities(request);
        try {
            achievedQuantities = achievedQuantitiesDao.save(achievedQuantities);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(achievedQuantities)
                            .prevData(null)
                            .parent(AchievedQuantities.class.getSimpleName())
                            .parentId(achievedQuantities.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Achieved Quantities Saved Successfully for Id {} with Request Id {}", achievedQuantities.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(achievedQuantities));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AchievedQuantitiesRequest request = (AchievedQuantitiesRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Achieved Quantities Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for Achieved Quantities Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<AchievedQuantities> oldEntity = achievedQuantitiesDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Achieved Quantities is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        AchievedQuantities achievedQuantities = convertRequestToAchievedQuantities(request);
        achievedQuantities.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            achievedQuantities = achievedQuantitiesDao.save(achievedQuantities);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(achievedQuantities)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, AchievedQuantities.class))
                            .parent(AchievedQuantities.class.getSimpleName())
                            .parentId(achievedQuantities.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updating the Achieved Quantities for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(achievedQuantities));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Achieved Quantities list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AchievedQuantities>, Pageable> tuple = fetchData(request, AchievedQuantities.class);
            Page<AchievedQuantities> achievedQuantitiesPage = achievedQuantitiesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Achieved Quantities list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(achievedQuantitiesPage.getContent()),
                    achievedQuantitiesPage.getTotalPages(),
                    achievedQuantitiesPage.getTotalElements());
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
                log.error("Request is empty for Achieved Quantities async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AchievedQuantities>, Pageable> tuple = fetchData(request, AchievedQuantities.class);
            Page<AchievedQuantities> achievedQuantitiesPage = achievedQuantitiesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Achieved Quantities async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(achievedQuantitiesPage.getContent()),
                    achievedQuantitiesPage.getTotalPages(),
                    achievedQuantitiesPage.getTotalElements()));
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
                log.error("Request is empty for Achieved Quantities Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Achieved Quantities Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AchievedQuantities> achievedQuantities = achievedQuantitiesDao.findById(id);
            if (achievedQuantities.isEmpty()) {
                log.debug("Achieved Quantities is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Deleted Achieved Quantities for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            String oldEntityJsonString = jsonHelper.convertToJson(achievedQuantities.get());
            achievedQuantitiesDao.delete(achievedQuantities.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, AchievedQuantities.class))
                            .parent(AchievedQuantities.class.getSimpleName())
                            .parentId(achievedQuantities.get().getId())
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
                log.error("Request is empty for Achieved Quantities retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Achieved Quantities retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AchievedQuantities> achievedQuantities = achievedQuantitiesDao.findById(id);
            if (achievedQuantities.isEmpty()) {
                log.debug("Achieved Quantities is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Achieved Quantities fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            AchievedQuantitiesResponse response = convertEntityToDto(achievedQuantities.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AchievedQuantitiesResponse convertEntityToDto(AchievedQuantities achievedQuantities) {
        return jsonHelper.convertValue(achievedQuantities, AchievedQuantitiesResponse.class);
    }

    private AchievedQuantities convertRequestToAchievedQuantities(AchievedQuantitiesRequest request) {
        return jsonHelper.convertValue(request, AchievedQuantities.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<AchievedQuantities> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}
