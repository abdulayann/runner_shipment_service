package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();

        Containers container = convertRequestToEntity(request);
        container = containerDao.save(container);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        Containers updatedEntity = convertRequestToEntity(request);
        containerDao.save(updatedEntity);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(updatedEntity));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMessage;
        try {
            Long id = commonRequestModel.getId();
            List<Containers> containers = containerDao.findByShipmentId(id);
            List<IRunnerResponse> response = containers.stream()
                    .map(this::convertEntityToDto)
                    .collect(Collectors.toList());
            return ResponseHelper.buildListSuccessResponse(response);
        } catch (Exception e) {
            responseMessage = e.getMessage();
            log.debug(responseMessage);
        }
        return ResponseHelper.buildFailedResponse(responseMessage);
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        Long id = commonRequestModel.getId();
        Optional<Containers> targetJob = containerDao.findById(id);
        if (targetJob.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        containerDao.delete(targetJob.get());
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

    public ResponseEntity<RunnerResponse<ContainerResponse>> containersUpdate(List<ContainerRequest> requestList, Long shipmentId)
    {
        // TODO- Handle Transactions here
        List<Containers> existingList = new ArrayList<Containers>();
        ListCommonRequest pageable = new ListCommonRequest();
        pageable.setPageNo(0);
        pageable.setSortRequest(SortRequest.builder()
                .fieldName("deliveryMode")
                .order("DESC")
                .build());
        pageable.setFilterCriteria(Arrays.asList(
                FilterCriteria.builder()
                        .innerFilter(Arrays.asList(
                                FilterCriteria.builder()
                                        .criteria(Criteria.builder()
                                                .fieldName("ShipmentId")
                                                .operator("=")
                                                .value(shipmentId)
                                                .build()).build())).build()));
        existingList = list(CommonRequestModel.buildRequest(pageable)).getBody().;
        // TODO- fetch based on shipmentId using service
        HashSet<Long> existingIds = new HashSet<>( existingList.stream().map(Containers::getId).collect(Collectors.toList()) );
        List<ContainerRequest> newContainers = new ArrayList<>();
        List<ContainerRequest> updateContainers = new ArrayList<>();
        for(ContainerRequest request: requestList)
        {
            Long id = request.getId();
            if(id != null){
                existingIds.remove(id);
                updateContainers.add(request);
            }
            else
                newContainers.add(request);
        }
        createContainers(newContainers);
        updateContainers(updateContainers);
        for(Long id: existingIds)
        {
            delete(CommonRequestModel.buildRequest(id));
        }
        return null;
    }

    private ResponseEntity<RunnerResponse<ContainerResponse>> updateContainers(List<ContainerRequest> updateContainers)
    {
        for(ContainerRequest request: updateContainers)
        {
            String responseMessage;
            try {
                update(CommonRequestModel.buildRequest(request));
            } catch (Exception e) {
                responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
                return (ResponseEntity<RunnerResponse<ContainerResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
            }
        }
        return null;
    }

    private ResponseEntity<RunnerResponse<ContainerResponse>> createContainers(List<ContainerRequest> newContainers)
    {
        for(ContainerRequest request: newContainers)
        {
            String responseMessage;
            try {
                create(CommonRequestModel.buildRequest(request));
            } catch (Exception e) {
                responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
                log.error(responseMessage, e);
            }
        }
        return null;
    }

    private IRunnerResponse convertEntityToDto(Containers container) {
        return modelMapper.map(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return modelMapper.map(request, Containers.class);
    }

}
