package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IJobDao;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;


@Slf4j
@Service
public class JobService implements IJobService {
    @Autowired
    IJobDao jobDao;

    @Autowired
    ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        JobRequest request = (JobRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Job create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Jobs job = convertRequestToEntity(request);
        try {
            job = jobDao.save(job);
            log.info("Job created successfully for Id {} with Request Id {}", job.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(job));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        JobRequest request = (JobRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Job update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null Job update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Jobs> oldEntity = jobDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Jobs is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Jobs jobs = convertRequestToEntity(request);
        jobs.setId(oldEntity.get().getId());
        try {
            jobs = jobDao.save(jobs);
            log.info("Updated the job details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(jobs));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Job list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Jobs>, Pageable> tuple = fetchData(request, BookingCarriage.class);
            Page<Jobs> jobsPage  = jobDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Job list retrieved successfully with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(jobsPage.getContent()),
                    jobsPage.getTotalPages(),
                    jobsPage.getTotalElements());
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
                log.error("Request is empty for Job async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Jobs>, Pageable> tuple = fetchData(request, Jobs.class);
            Page<Jobs> jobsPage  = jobDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Job async list retrieved successfully with Request Id {}",LoggerHelper.getRequestIdFromMDC());
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
        String responseMsg;
        if(commonRequestModel == null) {
            log.debug("Request is empty for Job delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if(commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Job delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();

        Optional<Jobs> targetJob = jobDao.findById(id);
        if (targetJob.isEmpty()) {
            log.debug("No entity present for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        try {
            jobDao.delete(targetJob.get());
            log.info("Deleted job for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for Job retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Job retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Jobs> job = jobDao.findById(id);
            if (job.isEmpty()) {
                log.debug("Job is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Job details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            JobResponse response = (JobResponse) convertEntityToDto(job.get());
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
        List<Jobs> responseJobs = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
            Page<Jobs> jobs = jobDao.findAll(pair.getLeft(), pair.getRight());
            HashSet<Long> existingIds = new HashSet<>(jobs
                    .stream()
                    .map(Jobs::getId)
                    .collect(Collectors.toList()));
            List<JobRequest> containerList = new ArrayList<>();
            List<JobRequest> requestList = (List<JobRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (JobRequest request : requestList) {
                    Long id = request.getId();
                    if (id != null) {
                        existingIds.remove(id);
                    }
                    containerList.add(request);
                }
                responseJobs = saveJobs(containerList);
            }
            deleteJobs(existingIds);
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responseJobs));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<Jobs> saveJobs(List<JobRequest> jobs) {
        List<Jobs> res = new ArrayList<>();
        for(JobRequest req : jobs){
            Jobs saveEntity = convertRequestToEntity(req);
            if(req.getId() != null){
                long id = req.getId();
                Optional<Jobs> oldEntity = jobDao.findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Job is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                saveEntity = oldEntity.get();
            }
            saveEntity = jobDao.save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private ResponseEntity<?> deleteJobs(HashSet<Long> existingIds) {
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

