package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IJobDao;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IJobService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
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
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToEntityList;


@Slf4j
@Service
public class JobService implements IJobService {
    @Autowired
    IJobDao jobDao;

    @Autowired
    IEventDao eventDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        JobRequest request = (JobRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Job create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Jobs job = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {
            job = jobDao.save(job);
            Long jobId = job.getId();
            if(eventsRequestList != null){
              List<Events> events = eventDao.saveEntityFromOtherEntity(
                      convertToEntityList(eventsRequestList, Events.class), jobId, Constants.JOBS);
              job.setEventsList(events);
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(job)
                            .prevData(null)
                            .parent(Jobs.class.getSimpleName())
                            .parentId(job.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

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
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
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
        List<EventsRequest> eventsRequestList = request.getEventsList();
        jobs.setId(oldEntity.get().getId());
        if(jobs.getGuid() != null && !oldEntity.get().getGuid().equals(jobs.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if(eventsRequestList != null){
            List<Events> events = eventDao.saveEntityFromOtherEntity(
                    convertToEntityList(eventsRequestList, Events.class), jobs.getId(), Constants.JOBS);
            jobs.setEventsList(events);
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            jobs = jobDao.save(jobs);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(jobs)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Jobs.class))
                            .parent(Jobs.class.getSimpleName())
                            .parentId(jobs.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the job details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(jobs));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel){
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
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
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
            String oldEntityJsonString = jsonHelper.convertToJson(targetJob.get());
            jobDao.delete(targetJob.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Jobs.class))
                            .parent(Jobs.class.getSimpleName())
                            .parentId(targetJob.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
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
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
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
             if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                 return ResponseHelper.buildSuccessResponse(response);
             else return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response,request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(Jobs job) {
        return jsonHelper.convertValue(job, JobResponse.class);
    }

    private Jobs convertRequestToEntity(JobRequest request) {
        return jsonHelper.convertValue(request, Jobs.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Jobs> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(job -> responseList.add(convertEntityToDto(job)));
        return responseList;
    }

}

