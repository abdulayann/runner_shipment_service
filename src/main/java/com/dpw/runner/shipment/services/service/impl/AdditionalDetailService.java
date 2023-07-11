package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAdditionalDetailService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
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

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AdditionalDetailService implements IAdditionalDetailService {

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AdditionalDetailRequest request = (AdditionalDetailRequest) commonRequestModel.getData();
        // TODO- implement validator
        if(request == null) {
            log.error("Request is empty for Shipment Additional Details create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        AdditionalDetail additionalDetails = convertRequestToEntity(request);
        try {
            additionalDetails = additionalDetailDao.save(additionalDetails);
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
        if(request == null) {
            log.error("Request is empty for shipment additional details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.error("Request Id is null for shipment additional details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        long id = request.getId();
        Optional<AdditionalDetail> oldEntity = additionalDetailDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Additional detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        AdditionalDetail additionalDetails = convertRequestToEntity(request);
        additionalDetails.setId(oldEntity.get().getId());
        try {
            additionalDetails = additionalDetailDao.save(additionalDetails);
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
            if(request == null) {
                log.error("Request is empty for additional details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AdditionalDetail>, Pageable> tuple = fetchData(request, AdditionalDetail.class);
            Page<AdditionalDetail> additionalDetailsPage = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for additional details async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<AdditionalDetail>, Pageable> tuple = fetchData(request, AdditionalDetail.class);
            Page<AdditionalDetail> additionalDetailsPage  = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
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
            if(request == null) {
                log.error("Request is empty for additional details delete for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for additional details delete for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AdditionalDetail> additionalDetails = additionalDetailDao.findById(id);
            if (!additionalDetails.isPresent()) {
                log.debug("Shipment Additional detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            additionalDetailDao.delete(additionalDetails.get());
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
            if(request == null) {
                log.debug("Request is empty for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<AdditionalDetail> additionalDetails = additionalDetailDao.findById(id);
            if (!additionalDetails.isPresent()) {
                log.debug("Shipment Additional detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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

    private AdditionalDetailResponse convertEntityToDto(AdditionalDetail additionalDetail) {
            return modelMapper.map(additionalDetail, AdditionalDetailResponse.class);
    }

    private AdditionalDetail convertRequestToEntity(AdditionalDetailRequest request) {
        return modelMapper.map(request, AdditionalDetail.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<AdditionalDetail> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(additionalDetail -> {
            responseList.add(convertEntityToDto(additionalDetail));
        });
        return responseList;
    }
}
