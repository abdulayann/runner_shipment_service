package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierDao;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierDetailService;
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

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CarrierDetailService implements ICarrierDetailService {

    @Autowired
    ICarrierDao carrierDao;
    @Autowired
    ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        CarrierDetailRequest request = (CarrierDetailRequest) commonRequestModel.getData();
        CarrierDetails carrierDetails = convertRequestToCarrierDetail(request);
        carrierDetails = carrierDao.save(carrierDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(carrierDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        CarrierDetailRequest request = (CarrierDetailRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<CarrierDetails> oldEntity = carrierDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Carrier Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        CarrierDetails carrierDetails = convertRequestToCarrierDetail(request);
        carrierDetails.setId(oldEntity.get().getId());
        carrierDetails = carrierDao.save(carrierDetails);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(carrierDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<CarrierDetails>, Pageable> tuple = fetchData(request, CarrierDetails.class);
            Page<CarrierDetails> carrierDetailsPage = carrierDao.findAll(tuple.getLeft(), tuple.getRight());
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

            Pair<Specification<CarrierDetails>, Pageable> tuple = fetchData(request, CarrierDetails.class);
            Page<CarrierDetails> carrierDetailsPage = carrierDao.findAll(tuple.getLeft(), tuple.getRight());
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
            long id = request.getId();
            Optional<CarrierDetails> carrierDetails = carrierDao.findById(id);
            if (carrierDetails.isEmpty()) {
                log.debug("Carrier Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            carrierDao.delete(carrierDetails.get());
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
            long id = request.getId();
            Optional<CarrierDetails> carrierDetail = carrierDao.findById(id);
            if (carrierDetail.isEmpty()) {
                log.debug("Carrier Detail is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            CarrierDetailResponse response = convertEntityToDto(carrierDetail.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private CarrierDetailResponse convertEntityToDto(CarrierDetails carrierDetails) {
        return modelMapper.map(carrierDetails, CarrierDetailResponse.class);
    }

    private CarrierDetails convertRequestToCarrierDetail(CarrierDetailRequest request) {
        return modelMapper.map(request, CarrierDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<CarrierDetails> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}
