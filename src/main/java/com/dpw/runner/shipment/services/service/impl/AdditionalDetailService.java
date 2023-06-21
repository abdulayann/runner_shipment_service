package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.service.interfaces.IAdditionalDetailService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AdditionalDetailService implements IAdditionalDetailService {

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Transactional
    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        AdditionalDetailRequest request = (AdditionalDetailRequest) commonRequestModel.getData();
        // TODO- implement validator
        AdditionalDetail additionalDetails = convertRequestToEntity(request);
        additionalDetails = additionalDetailDao.save(additionalDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(additionalDetails));
    }

    @Transactional
    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        AdditionalDetailRequest request = (AdditionalDetailRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id =request.getId();
        Optional<AdditionalDetail> oldEntity = additionalDetailDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Shipment Additional detail is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        AdditionalDetail additionalDetails = convertRequestToEntity(request);
        additionalDetails.setId(oldEntity.get().getId());
        additionalDetails = additionalDetailDao.save(additionalDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(additionalDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<AdditionalDetail>, Pageable> tuple = fetchData(request, AdditionalDetail.class.getSimpleName());
            Page<AdditionalDetail> additionalDetailsPage  = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
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
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<AdditionalDetail> additionalDetails = additionalDetailDao.findById(id);
            if(!additionalDetails.isPresent()) {
                log.debug("Shipment Additional detail is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            additionalDetailDao.delete(additionalDetails.get());
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
            long id =request.getId();
            Optional<AdditionalDetail> additionalDetails = additionalDetailDao.findById(id);
            if(!additionalDetails.isPresent()) {
                log.debug("Shipment Additional detail is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            AdditionalDetailResponse response = (AdditionalDetailResponse) convertEntityToDto(additionalDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(AdditionalDetail additionalDetails) {
        AdditionalDetailResponse additionalDetail = new AdditionalDetailResponse();
        // TODO - implement mapper
        return additionalDetail;
    }

    private AdditionalDetail convertRequestToEntity(AdditionalDetailRequest request) {
        AdditionalDetail additionalDetail = new AdditionalDetail();
        // TODO - implement mapper
        return additionalDetail;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<AdditionalDetail> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(additionalDetail -> {
            responseList.add(convertEntityToDto(additionalDetail));
        });
        return responseList;
    }
}
