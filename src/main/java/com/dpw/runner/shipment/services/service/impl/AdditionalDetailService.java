package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.AdditionalDetailConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAdditionalDetailService;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AdditionalDetailService implements IAdditionalDetailService {

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Transactional
    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AdditionalDetailRequest request = (AdditionalDetailRequest) commonRequestModel.getData();
        // TODO- implement validator
        if (request == null) {
            log.error("Request is empty for Shipment Additional Details create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        AdditionalDetails additionalDetails = convertRequestToEntity(request);
        try {
            additionalDetails = additionalDetailDao.save(additionalDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(additionalDetails)
                            .prevData(null)
                            .parent(AdditionalDetails.class.getSimpleName())
                            .parentId(additionalDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Shipment Additional Details Saved Successfully for Id {} with Request Id {}", additionalDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(additionalDetails));
    }

    @Transactional
    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AdditionalDetailRequest request = (AdditionalDetailRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        if (request == null) {
            log.error("Request is empty for shipment additional details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for shipment additional details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        long id = request.getId();
        Optional<AdditionalDetails> oldEntity = additionalDetailDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        AdditionalDetails additionalDetails = convertRequestToEntity(request);
        additionalDetails.setId(oldEntity.get().getId());

        if(additionalDetails.getGuid() != null && !oldEntity.get().getGuid().equals(additionalDetails.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            additionalDetails = additionalDetailDao.save(additionalDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(additionalDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, AdditionalDetails.class))
                            .parent(AdditionalDetails.class.getSimpleName())
                            .parentId(additionalDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the shipment additional detail for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(additionalDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for additional details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AdditionalDetails>, Pageable> tuple = fetchData(request, AdditionalDetails.class);
            Page<AdditionalDetails> additionalDetailsPage = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Additional details list retrieved successfully successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(additionalDetailsPage.getContent()),
                    additionalDetailsPage.getTotalPages(),
                    additionalDetailsPage.getTotalElements());
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
                log.error("Request is empty for additional details async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AdditionalDetails>, Pageable> tuple = fetchData(request, AdditionalDetails.class);
            Page<AdditionalDetails> additionalDetailsPage = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Additional details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(additionalDetailsPage.getContent()),
                    additionalDetailsPage.getTotalPages(),
                    additionalDetailsPage.getTotalElements()));
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
            // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for additional details delete for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for additional details delete for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AdditionalDetails> additionalDetails = additionalDetailDao.findById(id);
            if (!additionalDetails.isPresent()) {
                log.debug(AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(additionalDetails.get());
            additionalDetailDao.delete(additionalDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, AdditionalDetails.class))
                            .parent(AdditionalDetails.class.getSimpleName())
                            .parentId(additionalDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted additional detail service for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.debug("Request is empty for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AdditionalDetails> additionalDetails = additionalDetailDao.findById(id);
            if (!additionalDetails.isPresent()) {
                log.debug(AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Additional details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            AdditionalDetailResponse response = convertEntityToDto(additionalDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AdditionalDetailResponse convertEntityToDto(AdditionalDetails additionalDetails) {
        return jsonHelper.convertValue(additionalDetails, AdditionalDetailResponse.class);
    }

    private AdditionalDetails convertRequestToEntity(AdditionalDetailRequest request) {
        return jsonHelper.convertValue(request, AdditionalDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<AdditionalDetails> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(additionalDetails -> responseList.add(convertEntityToDto(additionalDetails)));
        return responseList;
    }
}
