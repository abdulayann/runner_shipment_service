package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersService;
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
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ReferenceNumbersService implements IReferenceNumbersService {
    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        ReferenceNumbersRequest request = null;
        request = (ReferenceNumbersRequest) commonRequestModel.getData();
        ReferenceNumbers referenceNumbers = convertRequestToEntity(request);
        referenceNumbers = referenceNumbersDao.save(referenceNumbers);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(referenceNumbers));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        ReferenceNumbersRequest request = (ReferenceNumbersRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<ReferenceNumbers> oldEntity = referenceNumbersDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Refernece Numbers is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ReferenceNumbers referenceNumbers = convertRequestToEntity(request);
        referenceNumbers.setId(oldEntity.get().getId());
        referenceNumbers = referenceNumbersDao.save(referenceNumbers);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(referenceNumbers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<ReferenceNumbers>, Pageable> tuple = fetchData(request, ReferenceNumbers.class);
            Page<ReferenceNumbers> referenceNumbersPage  = referenceNumbersDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(referenceNumbersPage.getContent()),
                    referenceNumbersPage.getTotalPages(),
                    referenceNumbersPage.getTotalElements());
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
            // construct specifications for filter request
            Pair<Specification<ReferenceNumbers>, Pageable> tuple = fetchData(request, ReferenceNumbers.class);
            Page<ReferenceNumbers> referenceNumbersPage  = referenceNumbersDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(referenceNumbersPage.getContent()),
                    referenceNumbersPage.getTotalPages(),
                    referenceNumbersPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<ReferenceNumbers> referenceNumbers = referenceNumbersDao.findById(id);
            if(!referenceNumbers.isPresent()) {
                log.debug("Reference Numbers is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            referenceNumbersDao.delete(referenceNumbers.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<ReferenceNumbers> referenceNumbers = referenceNumbersDao.findById(id);
            if(!referenceNumbers.isPresent()) {
                log.debug("Reference Numbers is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ReferenceNumbersResponse response = convertEntityToDto(referenceNumbers.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ReferenceNumbersResponse convertEntityToDto(ReferenceNumbers referenceNumbers) {
        return modelMapper.map(referenceNumbers, ReferenceNumbersResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ReferenceNumbers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(referenceNumbers -> {
            responseList.add(convertEntityToDto(referenceNumbers));
        });
        return responseList;
    }

    public ReferenceNumbers convertRequestToEntity(ReferenceNumbersRequest request) {
        return modelMapper.map(request, ReferenceNumbers.class);
    }
}
