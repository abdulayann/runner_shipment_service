package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.service.interfaces.IServiceDetailsService;
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
public class ServiceDetailsService implements IServiceDetailsService {
    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private ModelMapper modelMapper;

    private ServiceDetailsResponse convertEntityToDto(ServiceDetails shipmentServices) {
        return modelMapper.map(shipmentServices, ServiceDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ServiceDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentServices -> {
            responseList.add(convertEntityToDto(shipmentServices));
        });
        return responseList;
    }

    public ServiceDetails convertRequestToEntity(ServiceDetailsRequest request) {
        return modelMapper.map(request, ServiceDetails.class);
    }

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        ServiceDetailsRequest request = null;
        request = (ServiceDetailsRequest) commonRequestModel.getData();
        ServiceDetails shipmentServices = convertRequestToEntity(request);
        shipmentServices = serviceDetailsDao.save(shipmentServices);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        ServiceDetailsRequest request = (ServiceDetailsRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<ServiceDetails> oldEntity = serviceDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Service Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ServiceDetails shipmentServices = convertRequestToEntity(request);
        shipmentServices.setId(oldEntity.get().getId());
        shipmentServices = serviceDetailsDao.save(shipmentServices);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
            Page<ServiceDetails> shipmentServicesPage = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentServicesPage.getContent()),
                    shipmentServicesPage.getTotalPages(),
                    shipmentServicesPage.getTotalElements());
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
            Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
            Page<ServiceDetails> shipmentServicesPage  = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentServicesPage.getContent()),
                    shipmentServicesPage.getTotalPages(),
                    shipmentServicesPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<ServiceDetails> shipmentServices = serviceDetailsDao.findById(id);
            if (!shipmentServices.isPresent()) {
                log.debug("Service Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            serviceDetailsDao.delete(shipmentServices.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<ServiceDetails> shipmentServices = serviceDetailsDao.findById(id);
            if (!shipmentServices.isPresent()) {
                log.debug("Service Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ServiceDetailsResponse response = convertEntityToDto(shipmentServices.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
