package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
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

@Repository
@Slf4j
public class ServiceDetailsDao implements IServiceDetailsDao {
    @Autowired
    private IServiceDetailsRepository serviceDetailsRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private ValidatorUtility validatorUtility;

    @Override
    public ServiceDetails save(ServiceDetails serviceDetails) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(serviceDetails), Constants.SERVICE_DETAILS, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return serviceDetailsRepository.save(serviceDetails);
    }
    @Override
    public List<ServiceDetails> saveAll(List<ServiceDetails> serviceDetailsList) {
        for(var serviceDetails : serviceDetailsList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(serviceDetails), Constants.SERVICE_DETAILS, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
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
    public Optional<ServiceDetails> findByGuid(UUID id) {
        return serviceDetailsRepository.findByGuid(id);
    }

    @Override
    public void delete(ServiceDetails serviceDetails) {
        serviceDetailsRepository.delete(serviceDetails);
    }

    public List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            List<ServiceDetails> serviceDetailsPage = findByShipmentId(shipmentId);
            Map<Long, ServiceDetails> hashMap = serviceDetailsPage.stream()
                        .collect(Collectors.toMap(ServiceDetails::getId, Function.identity()));
            Map<Long, ServiceDetails> copyHashMap = new HashMap<>(hashMap);
            List<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
            if (serviceDetailsList != null && !serviceDetailsList.isEmpty()) {
                for (ServiceDetails request : serviceDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    serviceDetailsRequests.add(request);
                }
                responseServiceDetails = saveEntityFromShipment(serviceDetailsRequests, shipmentId, copyHashMap);
            }
            deleteServiceDetails(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseServiceDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private List<ServiceDetails> findByShipmentId(Long shipmentId) {
        return serviceDetailsRepository.findByShipmentId(shipmentId);
    }

    public List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId) {
        List<ServiceDetails> res = new ArrayList<>();
        for(ServiceDetails req : serviceDetailsRequests){
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(req.getId() != null){
                long id = req.getId();
                Optional<ServiceDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("ServiceDetails is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ServiceDetails.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (Exception e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId, Map<Long, ServiceDetails> oldEntityMap) {
        List<ServiceDetails> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for(ServiceDetails req : serviceDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("ServiceDetails is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        for (ServiceDetails req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(oldEntityJsonStringMap.containsKey(req.getId())){
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ServiceDetails.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    private void deleteServiceDetails(Map<Long, ServiceDetails> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(serviceDetail -> {
                String json = jsonHelper.convertToJson(serviceDetail);
                delete(serviceDetail);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, ServiceDetails.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (Exception e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId, List<ServiceDetails> oldEntityList) throws RunnerException {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        Map<UUID, ServiceDetails> serviceDetailsMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (ServiceDetails entity:
                    oldEntityList) {
                serviceDetailsMap.put(entity.getGuid(), entity);
            }
        }

        try {
            ServiceDetails oldEntity;
            List<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
            if (serviceDetailsList != null && !serviceDetailsList.isEmpty()) {
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
            deleteServiceDetails(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseServiceDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public Optional<ServiceDetails> findByIdWithQuery(Long id) {
        return serviceDetailsRepository.findByIdWithQuery(id);
    }

    public Page<ServiceDetails> findAllWithoutTenantFilter(Specification<ServiceDetails> spec, Pageable pageable) {
        return serviceDetailsRepository.findAllWithoutTenantFilter(spec, pageable);
    }

    @Override
    public void deleteAdditionalServiceDetailsByShipmentId(List<Long> serviceDetailsIds, Long shipmentId) {
        serviceDetailsRepository.deleteAdditionalServiceDetailsByShipmentId(serviceDetailsIds, shipmentId);
    }

    @Override
    public void revertSoftDeleteByServiceDetailsIdsAndShipmentId(List<Long> serviceDetailsIds, Long shipmentId) {
        serviceDetailsRepository.revertSoftDeleteByServiceDetailsIdsAndShipmentId(serviceDetailsIds, shipmentId);
    }

    public Optional<ServiceDetails> findByGuidWithQuery(UUID guid){
        return serviceDetailsRepository.findByGuidWithQuery(guid);
    }

    @Override
    public List<ServiceDetails> findByIdIn(List<Long> serviceDetailsIds) {
        return serviceDetailsRepository.findByIdIn(serviceDetailsIds);
    }

    @Override
    public void deleteByIdIn(List<Long> serviceDetailsIds) {
        serviceDetailsRepository.deleteAllById(serviceDetailsIds);
    }
}
