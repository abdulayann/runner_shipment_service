package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
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
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ServiceDetailsRequest request = null;
        request = (ServiceDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Service create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ServiceDetails shipmentServices = convertRequestToEntity(request);
        try {
            shipmentServices = serviceDetailsDao.save(shipmentServices);
            log.info("Service details created successfully for Id {} with Request Id {}", shipmentServices.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ServiceDetailsRequest request = (ServiceDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Service update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Service update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ServiceDetails> oldEntity = serviceDetailsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Service Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ServiceDetails shipmentServices = convertRequestToEntity(request);
        shipmentServices.setId(oldEntity.get().getId());
        try {
            shipmentServices = serviceDetailsDao.save(shipmentServices);
            log.info("Updated the service details for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Service list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
            Page<ServiceDetails> shipmentServicesPage  = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Service list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
            if(request == null) {
                log.error("Request is empty for Service async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<ServiceDetails>, Pageable> tuple = fetchData(request, ServiceDetails.class);
            Page<ServiceDetails> shipmentServicesPage  = serviceDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Service async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
            if(request == null) {
                log.debug("Request is empty for Service delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for Service delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Service delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for Service delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ServiceDetails> shipmentServices;
            if(request.getId() != null) {
                long id = request.getId();
                shipmentServices = serviceDetailsDao.findById(id);
                if(!shipmentServices.isPresent()) {
                    log.debug("Service Details is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentServices = serviceDetailsDao.findByGuid(guid);
                if(!shipmentServices.isPresent()) {
                    log.debug("Service Details is null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            serviceDetailsDao.delete(shipmentServices.get());
            log.info("Deleted service details for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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
            if(request == null) {
                log.error("Request is empty for Service retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for Service retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Service retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for Service retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ServiceDetails> shipmentServices;
            ServiceDetailsResponse response;
            if(request.getId() != null) {
                long id = request.getId();
                shipmentServices = serviceDetailsDao.findById(id);
                if(!shipmentServices.isPresent()) {
                    log.debug("Service Details is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Service details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentServices = serviceDetailsDao.findByGuid(guid);
                if(!shipmentServices.isPresent()) {
                    log.debug("Service Details is null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Service details fetched successfully for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
            }
            response = convertEntityToDto(shipmentServices.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
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
