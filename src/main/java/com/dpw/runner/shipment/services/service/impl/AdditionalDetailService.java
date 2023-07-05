package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailDao;
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<AdditionalDetail>, Pageable> tuple = fetchData(request, AdditionalDetail.class);
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
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<AdditionalDetail>, Pageable> tuple = fetchData(request, AdditionalDetail.class);
            Page<AdditionalDetail> additionalDetailsPage  = additionalDetailDao.findAll(tuple.getLeft(), tuple.getRight());
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
            AdditionalDetailResponse response = convertEntityToDto(additionalDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

<<<<<<< Updated upstream
    private AdditionalDetailResponse convertEntityToDto(AdditionalDetail additionalDetail) {
        return modelMapper.map(additionalDetail, AdditionalDetailResponse.class);
=======
    public ResponseEntity<?> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId)
    {
        String responseMsg;
        List<AdditionalDetail> responseAdditionalDetail = null;
        try {
            // TODO- Handle Transactions here
            List<AdditionalDetail> existingList = additionalDetailDao.findByShipmentId(shipmentId);
            HashSet<Long> existingIds = new HashSet<>( existingList.stream().map(AdditionalDetail::getId).collect(Collectors.toList()) );
            List<AdditionalDetailRequest> packingList = new ArrayList<>();
            List<AdditionalDetailRequest> requestList = (List<AdditionalDetailRequest>) commonRequestModel.getDataList();
            if(requestList != null && requestList.size() != 0)
            {
                for(AdditionalDetailRequest request: requestList)
                {
                    Long id = request.getId();
                    if(id != null) {
                        existingIds.remove(id);
                    }
                    packingList.add(request);
                }
                responseAdditionalDetail = saveAdditionalDetails(packingList);
                deleteAdditionalDetails(existingIds);
            }
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responseAdditionalDetail));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<AdditionalDetail> saveAdditionalDetails(List<AdditionalDetailRequest> packings)
    {
        return additionalDetailDao.saveAll(packings
                .stream()
                .map(this::convertRequestToEntity)
                .collect(Collectors.toList()));
    }

    private ResponseEntity<?> deleteAdditionalDetails(HashSet<Long> existingIds)
    {
        String responseMsg;
        try {
            for(Long id: existingIds)
            {
                delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(id).build()));
            }
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(AdditionalDetail additionalDetail) {
        return jsonHelper.convertValue(additionalDetail, AdditionalDetailResponse.class);
>>>>>>> Stashed changes
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
