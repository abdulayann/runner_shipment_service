package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
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

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Container Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Containers container = convertRequestToEntity(request);
        try {
            container = containerDao.save(container);
            log.info("Container Details Saved Successfully for Id {} with Request Id {}", container.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Containers containers = convertRequestToEntity(request);
        containers.setId(oldEntity.get().getId());
        try {
            containers = containerDao.save(containers);
            log.info("Updated the container details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Containers List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent()),
                    containersPage.getTotalPages(),
                    containersPage.getTotalElements());
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
                log.error("Request is empty for Containers async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage  = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(containersPage.getContent()),
                                    containersPage.getTotalPages(),
                                    containersPage.getTotalElements()));
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
        if(commonRequestModel == null) {
            log.debug("Request is empty for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if(commonRequestModel.getId() == null && commonRequestModel.getGuid() == null) {
            log.debug("Request Id and GUID is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if(commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if(commonRequestModel.getGuid() == null) {
            log.debug("GUID is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Optional<Containers> container;
        if(commonRequestModel.getId() != null) {
            Long id = commonRequestModel.getId();
            container = containerDao.findById(id);
            if (container.isEmpty()) {
                log.debug("Container details are null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
            }
        } else {
            UUID guid = UUID.fromString(commonRequestModel.getGuid());
            container = containerDao.findByGuid(guid);
            if (container.isEmpty()) {
                log.debug("Container details are null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
            }
        }
        try {
            containerDao.delete(container.get());
            log.info("Deleted container for Id {} with Request Id {}", commonRequestModel.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<Containers> container;
            JobResponse response;
            if(request.getId() != null) {
                long id = request.getId();
                container = containerDao.findById(id);
                if (container.isEmpty()) {
                    log.debug("Container is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Container detail fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                container = containerDao.findByGuid(guid);
                if (container.isEmpty()) {
                    log.debug("Container is null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("Container detail fetched successfully for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
            }
            response = (JobResponse) convertEntityToDto(container.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(Containers container) {
        return modelMapper.map(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return modelMapper.map(request, Containers.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> {
            responseList.add(convertEntityToDto(containers));
        });
        return responseList;
    }

}
