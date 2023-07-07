package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ServiceDetailsService implements IServiceDetailsService {
    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ServiceDetailsRequest request = null;
        request = (ServiceDetailsRequest) commonRequestModel.getData();
        ServiceDetails shipmentServices = convertRequestToEntity(request);
        shipmentServices = serviceDetailsDao.save(shipmentServices);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
            Page<ServiceDetails> shipmentServicesPage = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
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

    public ResponseEntity<?> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
            Page<ServiceDetails> serviceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
            HashSet<Long> existingIds = new HashSet<>(serviceDetails
                    .stream()
                    .map(ServiceDetails::getId)
                    .collect(Collectors.toList()));
            List<ServiceDetailsRequest> serviceDetailsRequests = new ArrayList<>();
            List<ServiceDetailsRequest> requestList = (List<ServiceDetailsRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (ServiceDetailsRequest request : requestList) {
                    Long id = request.getId();
                    if (id != null) {
                        existingIds.remove(id);
                    }
                    serviceDetailsRequests.add(request);
                }
                responseServiceDetails = saveServiceDetails(serviceDetailsRequests);
            }
            deleteServiceDetails(existingIds);
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responseServiceDetails));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<ServiceDetails> saveServiceDetails(List<ServiceDetailsRequest> serviceDetailsRequests) {
        List<ServiceDetails> res = new ArrayList<>();
        for(ServiceDetailsRequest req : serviceDetailsRequests){
            ServiceDetails saveEntity = convertRequestToEntity(req);
            if(req.getId() != null){
                long id = req.getId();
                Optional<ServiceDetails> oldEntity = serviceDetailsDao.findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                saveEntity = oldEntity.get();
            }
            saveEntity = serviceDetailsDao.save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private ResponseEntity<?> deleteServiceDetails(HashSet<Long> existingIds) {
        String responseMsg;
        try {
            for (Long id : existingIds) {
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
}
