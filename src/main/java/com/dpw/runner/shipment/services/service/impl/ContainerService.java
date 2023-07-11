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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

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
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();

        Containers container = convertRequestToEntity(request);
        container = containerDao.save(container);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Container is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Containers containers = convertRequestToEntity(request);
        containers.setId(oldEntity.get().getId());
        containers = containerDao.save(containers);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
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
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage  = containerDao.findAll(tuple.getLeft(), tuple.getRight());
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
        Long id = commonRequestModel.getId();
        Optional<Containers> container = containerDao.findById(id);
        if (container.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        containerDao.delete(container.get());
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<Containers> container = containerDao.findById(id);
            if (container.isEmpty()) {
                log.debug("Container is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            JobResponse response = (JobResponse) convertEntityToDto(container.get());
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
