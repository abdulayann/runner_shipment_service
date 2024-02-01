package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IJobDao;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class JobDao implements IJobDao {
    @Autowired
    private IJobRepository jobRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Jobs save(Jobs jobs) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(jobs) , Constants.JOBS, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        return jobRepository.save(jobs);
    }
    @Override
    public List<Jobs> saveAll(List<Jobs> jobsList) {
        for (var jobs: jobsList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(jobs), Constants.JOBS, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(errors.toString());
        }
        return jobRepository.saveAll(jobsList);
    }

    @Override
    public Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable) {
        return jobRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Jobs> findById(Long id) {
        return jobRepository.findById(id);
    }

    @Override
    public void delete(Jobs jobs) {
        jobRepository.delete(jobs);
    }

    @Override
    public List<Jobs> updateEntityFromShipment(List<Jobs>jobsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<Jobs> responseJobs = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Jobs> hashMap = new HashMap<>();
            var jobsIdList = jobsList.stream().map(Jobs::getId).toList();
            if(!Objects.isNull(jobsIdList) && !jobsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> routings = findAll(pair.getLeft(), pair.getRight());
                hashMap = routings.stream()
                        .collect(Collectors.toMap(Jobs::getId, Function.identity()));
            }
            Map<Long, Jobs> copyHashMap = new HashMap<>(hashMap);
            List<Jobs> jobRequestList = new ArrayList<>();
            if (jobsList != null && jobsList.size() != 0) {
                for (Jobs request : jobsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    jobRequestList.add(request);
                }
                responseJobs = saveEntityFromShipment(jobRequestList, shipmentId, copyHashMap);
            }
            deleteJobs(hashMap);
            return responseJobs;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Jobs> saveEntityFromShipment(List<Jobs> jobRequests, Long shipmentId) {
        List<Jobs> res = new ArrayList<>();
        for(Jobs req : jobRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Jobs> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Job is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Jobs> saveEntityFromShipment(List<Jobs> jobRequests, Long shipmentId, Map<Long, Jobs> oldEntityMap) {
        List<Jobs> res = new ArrayList<>();
        for(Jobs req : jobRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Job is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }

    @Override
    public List<Jobs> updateEntityFromConsole(List<Jobs>jobsList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Jobs> responseJobs = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Jobs> hashMap = new HashMap<>();
            var jobsIdList = jobsList.stream().map(Jobs::getId).toList();
            if(!Objects.isNull(jobsIdList) && !jobsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> routings = findAll(pair.getLeft(), pair.getRight());
                hashMap = routings.stream()
                        .collect(Collectors.toMap(Jobs::getId, Function.identity()));
            }
            Map<Long, Jobs> copyHashMap = new HashMap<>(hashMap);
            List<Jobs> jobRequestList = new ArrayList<>();
            if (jobsList != null && jobsList.size() != 0) {
                for (Jobs request : jobsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    jobRequestList.add(request);
                }
                responseJobs = saveEntityFromConsole(jobRequestList, consolidationId, copyHashMap);
            }
            deleteJobs(hashMap);
            return responseJobs;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Jobs> updateEntityFromConsole(List<Jobs>jobsList, Long consolidationId, List<Jobs> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, Jobs> jobsMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Jobs entity:
                    oldEntityList) {
                jobsMap.put(entity.getGuid(), entity);
            }
        }

        List<Jobs> responseJobs = new ArrayList<>();
        try {
            Jobs oldEntity;
            List<Jobs> jobRequestList = new ArrayList<>();
            if (jobsList != null && jobsList.size() != 0) {
                for (Jobs request : jobsList) {
                    oldEntity = jobsMap.get(request.getGuid());
                    if(oldEntity != null) {
                        jobsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    jobRequestList.add(request);
                }
                responseJobs = saveEntityFromConsole(jobRequestList, consolidationId);
            }
            Map<Long, Jobs> hashMap = new HashMap<>();
            jobsMap.forEach((s, jobs) ->  hashMap.put(jobs.getId(), jobs));
            deleteJobs(hashMap);
            return responseJobs;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Jobs> saveEntityFromConsole(List<Jobs> jobRequests, Long consolidationId) {
        List<Jobs> res = new ArrayList<>();
        for(Jobs req : jobRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Jobs> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Job is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Jobs> saveEntityFromConsole(List<Jobs> jobRequests, Long consolidationId, Map<Long, Jobs> oldEntityMap) {
        List<Jobs> res = new ArrayList<>();
        for(Jobs req : jobRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Job is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }

    private void deleteJobs(Map<Long, Jobs> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<Jobs> updateEntityFromShipment(List<Jobs>jobsList, Long shipmentId, List<Jobs> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, Jobs> jobsMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Jobs entity:
                    oldEntityList) {
                jobsMap.put(entity.getGuid(), entity);
            }
        }

        List<Jobs> responseJobs = new ArrayList<>();
        try {
            Jobs oldEntity;
            List<Jobs> jobRequestList = new ArrayList<>();
            if (jobsList != null && jobsList.size() != 0) {
                for (Jobs request : jobsList) {
                    oldEntity = jobsMap.get(request.getGuid());
                    if(oldEntity != null) {
                        jobsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    jobRequestList.add(request);
                }
                responseJobs = saveEntityFromShipment(jobRequestList, shipmentId);
            }
            Map<Long, Jobs> hashMap = new HashMap<>();
            jobsMap.forEach((s, jobs) ->  hashMap.put(jobs.getId(), jobs));
            deleteJobs(hashMap);
            return responseJobs;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
