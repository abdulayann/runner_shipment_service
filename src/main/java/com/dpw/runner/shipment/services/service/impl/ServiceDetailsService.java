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

    private static ServiceDetailsResponse convertEntityToDto(ServiceDetails shipmentServices) {
        ServiceDetailsResponse response = new ServiceDetailsResponse();
        response.setId(shipmentServices.getId());
        response.setGuid(shipmentServices.getGuid());
        response.setShipmentId(shipmentServices.getShipmentId());
        response.setConsolidationId(shipmentServices.getConsolidationId());
        response.setServiceType(shipmentServices.getServiceType());
        response.setContractorId(shipmentServices.getContractorId());
        response.setContractorAddressId(shipmentServices.getContractorAddressId());
        response.setSrvLocation(shipmentServices.getSrvLocation());
        response.setBookingDate(shipmentServices.getBookingDate());
        response.setServiceCount(shipmentServices.getServiceCount());
        response.setServiceDuration(shipmentServices.getServiceDuration());
        response.setCompletionDate(shipmentServices.getCompletionDate());
        response.setRefNumber(shipmentServices.getRefNumber());
        response.setServiceNotes(shipmentServices.getServiceNotes());
        return response;
    }

    private static List<IRunnerResponse> convertEntityListToDtoList(List<ServiceDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentServices -> {
            responseList.add(convertEntityToDto(shipmentServices));
        });
        return responseList;
    }

    public static ServiceDetails convertRequestToEntity(ServiceDetailsRequest request) {
        ServiceDetails shipmentServices = new ServiceDetails();
        shipmentServices.setGuid(UUID.randomUUID());
        shipmentServices.setShipmentId(request.getShipmentId());
        shipmentServices.setConsolidationId(request.getConsolidationId());
        shipmentServices.setServiceType(request.getServiceType());
        shipmentServices.setContractorId(request.getContractorId());
        shipmentServices.setContractorAddressId(request.getContractorAddressId());
        shipmentServices.setSrvLocation(request.getSrvLocation());
        shipmentServices.setBookingDate(request.getBookingDate());
        shipmentServices.setServiceCount(request.getServiceCount());
        shipmentServices.setServiceDuration(request.getServiceDuration());
        shipmentServices.setCompletionDate(request.getCompletionDate());
        shipmentServices.setRefNumber(request.getRefNumber());
        shipmentServices.setServiceNotes(request.getServiceNotes());
        return shipmentServices;
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
