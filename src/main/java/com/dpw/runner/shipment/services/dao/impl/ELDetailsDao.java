package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Repository
@Slf4j
public class ELDetailsDao implements IELDetailsDao {
    @Autowired
    private IELDetailsRepository elDetailsRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public ELDetails save(ELDetails elDetails) {
        return elDetailsRepository.save(elDetails);
    }
    @Override
    public List<ELDetails> saveAll(List<ELDetails> elDetailsList) {
        return elDetailsRepository.saveAll(elDetailsList);
    }

    @Override
    public Optional<ELDetails> findByGuid(UUID guid) {
        return elDetailsRepository.findByGuid(guid);
    }

    @Override
    public Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable) {
        return elDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ELDetails> findById(Long id) {
        return elDetailsRepository.findById(id);
    }

    @Override
    public void delete(ELDetails elDetails) {
        elDetailsRepository.delete(elDetails);
    }

    @Override
    public Optional<ELDetails> findByElNumber(String elNumber) {
        return elDetailsRepository.findByElNumber(elNumber);
    }

    public List<ELDetails> updateEntityFromShipment(List<ELDetails> elDetailsList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<ELDetails> responseELDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            List<ELDetails> elDetails = findByShipmentId(shipmentId);
            Map<Long, ELDetails> hashMap = elDetails.stream()
                        .collect(Collectors.toMap(ELDetails::getId, Function.identity()));
            Map<Long, ELDetails> copyHashMap = new HashMap<>(hashMap);
            List<ELDetails> elDetailsRequestList = new ArrayList<>();
            if (elDetailsList != null && elDetailsList.size() != 0) {
                for (ELDetails request : elDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    elDetailsRequestList.add(request);
                }
                responseELDetails = saveEntityFromShipment(elDetailsRequestList, shipmentId, copyHashMap);
            }
            deleteELDetails(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseELDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<ELDetails> findByShipmentId(Long shipmentId) {
        return elDetailsRepository.findByShipmentId(shipmentId);
    }

    public List<ELDetails> saveEntityFromShipment(List<ELDetails> elDetails, Long shipmentId) {
        List<ELDetails> res = new ArrayList<>();
        for (ELDetails req : elDetails) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<ELDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("EL Detail is null for Id {}", req.getId());
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ELDetails.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<ELDetails> saveEntityFromShipment(List<ELDetails> elDetails, Long shipmentId, Map<Long, ELDetails> oldEntityMap) {
        List<ELDetails> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (ELDetails req : elDetails) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("EL Detail is null for Id {}", req.getId());
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
        for (ELDetails req : res) {
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ELDetails.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    private void deleteELDetails(Map<Long, ELDetails> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(elDetail -> {
                String json = jsonHelper.convertToJson(elDetail);
                delete(elDetail);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, ELDetails.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
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

    public List<ELDetails> updateEntityFromShipment(List<ELDetails> elDetailsList, Long shipmentId, List<ELDetails> oldEntityList) throws RunnerException {
        String responseMsg;
        List<ELDetails> responseELDetails = new ArrayList<>();
        Map<UUID, ELDetails> elDetailsMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (ELDetails entity :
                    oldEntityList) {
                elDetailsMap.put(entity.getGuid(), entity);
            }
        }
        try {
            ELDetails oldEntity;
            List<ELDetails> elDetailsRequestList = new ArrayList<>();
            if (elDetailsList != null && elDetailsList.size() != 0) {
                for (ELDetails request : elDetailsList) {
                    oldEntity = elDetailsMap.get(request.getGuid());
                    if (oldEntity != null) {
                        elDetailsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    elDetailsRequestList.add(request);
                }
                responseELDetails = saveEntityFromShipment(elDetailsRequestList, shipmentId);
            }
            Map<Long, ELDetails> hashMap = new HashMap<>();
            elDetailsMap.forEach((s, elDetails) -> hashMap.put(elDetails.getId(), elDetails));
            deleteELDetails(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseELDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
