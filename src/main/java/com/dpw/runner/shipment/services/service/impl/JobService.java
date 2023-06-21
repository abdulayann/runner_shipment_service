package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.JobConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.helpers.MapperHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IJobDao;
import com.dpw.runner.shipment.services.service.interfaces.IJobService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;


@Slf4j
@Service
public class JobService implements IJobService {
    @Autowired
    IJobDao jobDao;

    @Autowired
    MapperHelper mapperHelper;

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
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) throws Exception {
        Long id = commonRequestModel.getId();
        Optional<Jobs> targetJob = jobDao.findById(id);
        if (targetJob.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(JobConstants.NO_DATA);
        }
        jobDao.delete(targetJob.get());
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    private IRunnerResponse convertEntityToDto(Jobs job) {
        return mapperHelper.getMapper().convertValue(job, JobResponse.class);
    }

    private Jobs convertRequestToEntity(JobRequest request) {
        return mapperHelper.getMapper().convertValue(request, Jobs.class);
    }

}

