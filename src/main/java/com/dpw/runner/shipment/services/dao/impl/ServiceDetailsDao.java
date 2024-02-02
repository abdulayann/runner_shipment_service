package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
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
public class ServiceDetailsDao implements IServiceDetailsDao {
    @Autowired
    private IServiceDetailsRepository serviceDetailsRepository;

    @Override
    public ServiceDetails save(ServiceDetails serviceDetails) {
        return serviceDetailsRepository.save(serviceDetails);
    }
    @Override
    public List<ServiceDetails> saveAll(List<ServiceDetails> serviceDetailsList) {
        return serviceDetailsRepository.saveAll(serviceDetailsList);
    }

    @Override
    public Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable) {
        return serviceDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ServiceDetails> findById(Long id) {
        return serviceDetailsRepository.findById(id);
    }

    @Override
    public void delete(ServiceDetails serviceDetails) {
        serviceDetailsRepository.delete(serviceDetails);
    }

    public List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, ServiceDetails> hashMap;
//            if(!Objects.isNull(serviceDetailsIdList) && !serviceDetailsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> serviceDetailsPage = findAll(pair.getLeft(), pair.getRight());
                hashMap = serviceDetailsPage.stream()
                        .collect(Collectors.toMap(ServiceDetails::getId, Function.identity()));
//            }
            Map<Long, ServiceDetails> copyHashMap = new HashMap<>();
            List<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
            if (serviceDetailsList != null && serviceDetailsList.size() != 0) {
                for (ServiceDetails request : serviceDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    serviceDetailsRequests.add(request);
                }
                responseServiceDetails = saveEntityFromShipment(serviceDetailsRequests, shipmentId, copyHashMap);
            }
            deleteServiceDetails(hashMap);
            return responseServiceDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId) {
        List<ServiceDetails> res = new ArrayList<>();
        for(ServiceDetails req : serviceDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<ServiceDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("ServiceDetails is null for Id {}", req.getId());
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
    public List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId, Map<Long, ServiceDetails> oldEntityMap) {
        List<ServiceDetails> res = new ArrayList<>();
        for(ServiceDetails req : serviceDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("ServiceDetails is null for Id {}", req.getId());
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

    private void deleteServiceDetails(Map<Long, ServiceDetails> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId, List<ServiceDetails> oldEntityList) throws Exception {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        Map<UUID, ServiceDetails> serviceDetailsMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (ServiceDetails entity:
                    oldEntityList) {
                serviceDetailsMap.put(entity.getGuid(), entity);
            }
        }

        try {
            ServiceDetails oldEntity;
            List<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
            if (serviceDetailsList != null && serviceDetailsList.size() != 0) {
                for (ServiceDetails request : serviceDetailsList) {
                    oldEntity = serviceDetailsMap.get(request.getGuid());
                    if(oldEntity != null) {
                        serviceDetailsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    serviceDetailsRequests.add(request);
                }
                responseServiceDetails = saveEntityFromShipment(serviceDetailsRequests, shipmentId);
            }
            Map<Long, ServiceDetails> hashMap = new HashMap<>();
            serviceDetailsMap.forEach((s, serviceDetails) ->  hashMap.put(serviceDetails.getId(), serviceDetails));
            deleteServiceDetails(hashMap);
            return responseServiceDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
