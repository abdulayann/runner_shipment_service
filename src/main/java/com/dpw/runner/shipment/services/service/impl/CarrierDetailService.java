package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDao;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierDetailService;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CarrierDetailService implements ICarrierDetailService {

    @Autowired
    ICarrierDao carrierDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private AuditLogService auditLogService;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CarrierDetailRequest request = (CarrierDetailRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Carrier Details Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        CarrierDetails carrierDetails = convertRequestToCarrierDetail(request);
        try {
            carrierDetails = carrierDao.save(carrierDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(carrierDetails)
                            .prevData(null)
                            .parent(CarrierDetails.class.getSimpleName())
                            .parentId(carrierDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Carrier Details Saved Successfully for Id {} with Request Id {}", carrierDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(carrierDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CarrierDetailRequest request = (CarrierDetailRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Carrier Detail Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Carrier Detail Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<CarrierDetails> oldEntity = carrierDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Carrier Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        CarrierDetails carrierDetails = convertRequestToCarrierDetail(request);
        carrierDetails.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            carrierDetails = carrierDao.save(carrierDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(carrierDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, CarrierDetails.class))
                            .parent(CarrierDetails.class.getSimpleName())
                            .parentId(carrierDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updating the carrier details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(carrierDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for carrier details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<CarrierDetails>, Pageable> tuple = fetchData(request, CarrierDetails.class);
            Page<CarrierDetails> carrierDetailsPage = carrierDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Carrier detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(carrierDetailsPage.getContent()),
                    carrierDetailsPage.getTotalPages(),
                    carrierDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for carrier details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<CarrierDetails>, Pageable> tuple = fetchData(request, CarrierDetails.class);
            Page<CarrierDetails> carrierDetailsPage = carrierDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Carrier detail async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(carrierDetailsPage.getContent()),
                    carrierDetailsPage.getTotalPages(),
                    carrierDetailsPage.getTotalElements()));
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
            if(request == null) {
                log.error("Request is empty for Carrier Details Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Carrier Details Delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CarrierDetails> carrierDetails = carrierDao.findById(id);
            if (carrierDetails.isEmpty()) {
                log.debug("Carrier Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Deleted carrier details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            String oldEntityJsonString = jsonHelper.convertToJson(carrierDetails.get());
            carrierDao.delete(carrierDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, CarrierDetails.class))
                            .parent(CarrierDetails.class.getSimpleName())
                            .parentId(carrierDetails.get().getId())
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
            if(request == null) {
                log.error("Request is empty for Carrier Detail retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Carrier Detail retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CarrierDetails> carrierDetail = carrierDao.findById(id);
            if (carrierDetail.isEmpty()) {
                log.debug("Carrier Detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Carrier Detail fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            CarrierDetailResponse response = convertEntityToDto(carrierDetail.get());
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
            return ResponseHelper.buildSuccessResponse(response);
            else{
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private CarrierDetailResponse convertEntityToDto(CarrierDetails carrierDetails) {
        return jsonHelper.convertValue(carrierDetails, CarrierDetailResponse.class);
    }

    private CarrierDetails convertRequestToCarrierDetail(CarrierDetailRequest request) {
        return jsonHelper.convertValue(request, CarrierDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<CarrierDetails> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}
