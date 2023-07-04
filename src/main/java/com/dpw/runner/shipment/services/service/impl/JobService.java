package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.JobConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IJobDao;
import com.dpw.runner.shipment.services.service.interfaces.IJobService;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;


@Slf4j
@Service
public class JobService implements IJobService {
    @Autowired
    IJobDao jobDao;

    @Autowired
    ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        JobRequest request = (JobRequest) commonRequestModel.getData();

        Jobs job = convertRequestToEntity(request);
        job = jobDao.save(job);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(job));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        JobRequest request = (JobRequest) commonRequestModel.getData();
        Jobs updatedEntity = convertRequestToEntity(request);
        jobDao.save(updatedEntity);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(updatedEntity));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMessage;
        try {
            Long id = commonRequestModel.getId();
            List<Jobs> jobs = jobDao.findByShipmentId(id);
            List<IRunnerResponse> response = jobs.stream()
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
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<Jobs>, Pageable> tuple = fetchData(request, Jobs.class);
            Page<Jobs> jobsPage  = jobDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(jobsPage.getContent()),
                                    jobsPage.getTotalPages(),
                                    jobsPage.getTotalElements()));
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
        Optional<Jobs> targetJob = jobDao.findById(id);
        if (targetJob.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        jobDao.delete(targetJob.get());
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<Jobs> job = jobDao.findById(id);
            if (job.isEmpty()) {
                log.debug("Booking Carriage is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            JobResponse response = (JobResponse) convertEntityToDto(job.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(Jobs job) {
        return modelMapper.map(job, JobResponse.class);
    }

    private Jobs convertRequestToEntity(JobRequest request) {
        return modelMapper.map(request, Jobs.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Jobs> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(job -> {
            responseList.add(convertEntityToDto(job));
        });
        return responseList;
    }

}

