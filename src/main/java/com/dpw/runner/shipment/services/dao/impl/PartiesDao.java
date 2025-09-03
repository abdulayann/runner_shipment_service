package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
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
public class PartiesDao implements IPartiesDao {
    public static final String PARTIES_IS_NULL_FOR_ID_MSG = "Parties is null for Id {}";
    @Autowired
    private IPartiesRepository partiesRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public List<Parties> saveAll(List<Parties> parties) {
        return partiesRepository.saveAll(parties);
    }

    @Override
    public Parties save(Parties parties) {return partiesRepository.save(parties);}

    @Override
    public Page<Parties> findAll(Specification<Parties> spec, Pageable pageable) {
        return partiesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Parties> findById(Long id) {
        return partiesRepository.findById(id);
    }

    @Override
    public void delete(Parties parties) {
        partiesRepository.delete(parties);
    }

    public Parties updateEntityFromShipment(Parties parties) throws RunnerException {
        String responseMsg;
        try {
            // LATER- Handle Transactions here
            if (parties.getId() != null) {
                long id = parties.getId();
                Optional<Parties> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug(PARTIES_IS_NULL_FOR_ID_MSG, id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            parties = save(parties);
            return parties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType) throws RunnerException {
        String responseMsg;
        List<Parties> responseParties = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            List<Parties> parties = findByEntityIdAndEntityType(entityId, entityType);
            Map<Long, Parties> hashMap = parties.stream()
                        .collect(Collectors.toMap(Parties::getId, Function.identity()));
            Map<Long, Parties> copyHashMap = new HashMap<>(hashMap);
            List<Parties> partiesRequestList = new ArrayList<>();
            if (partiesList != null && !partiesList.isEmpty()) {
                for (Parties request : partiesList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    partiesRequestList.add(request);
                }
                responseParties = saveEntityFromOtherEntity(partiesRequestList, entityId, entityType, copyHashMap);
            }
            deleteParties(hashMap, entityType, entityId);
            return responseParties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Parties> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return partiesRepository.findByEntityIdAndEntityType(entityId, entityType);
    }

    public List<Parties> saveEntityFromOtherEntity(List<Parties> partiesRequests, Long entityId, String entityType) {
        List<Parties> res = new ArrayList<>();
        for(Parties req : partiesRequests){
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(req.getId() != null){
                long id = req.getId();
                Optional<Parties> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug(PARTIES_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Parties.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT_ADDRESSES) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
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
    public List<Parties> saveEntityFromOtherEntity(List<Parties> partiesRequests, Long entityId, String entityType, Map<Long, Parties> oldEntityMap) {
        List<Parties> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for(Parties req : partiesRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug(PARTIES_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Parties.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT_ADDRESSES) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    private void deleteParties(Map<Long, Parties> hashMap, String entityType, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(parties -> {
                String json = jsonHelper.convertToJson(parties);
                delete(parties);
                if(entityType != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Parties.class))
                                        .parent(Objects.equals(entityType, Constants.SHIPMENT_ADDRESSES) ? ShipmentDetails.class.getSimpleName() : entityType)
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

    public List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType, List<Parties> oldEntityList) throws RunnerException {
        String responseMsg;
        List<Parties> responseParties = new ArrayList<>();
        Map<UUID, Parties> partiesMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (Parties entity:
                    oldEntityList) {
                partiesMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Parties oldEntity;
            List<Parties> partiesRequestList = new ArrayList<>();
            if (partiesList != null && !partiesList.isEmpty()) {
                for (Parties request : partiesList) {
                    oldEntity = partiesMap.get(request.getGuid());
                    if(oldEntity != null) {
                        partiesMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    request.setEntityId(entityId);
                    request.setEntityType(entityType);
                    partiesRequestList.add(request);
                }
                responseParties = saveEntityFromOtherEntity(partiesRequestList, entityId, entityType);
            }
            Map<Long, Parties> hashMap = new HashMap<>();
            partiesMap.forEach((s, parties) ->  hashMap.put(parties.getId(), parties));
            deleteParties(hashMap, entityType, entityId);
            return responseParties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Parties> findByIds(List<Long> id) {
        return partiesRepository.findByIdIn(id);
    }

    @Override
    public void deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(List<Long> addressIds, Long entityId, String entityType) {
        partiesRepository.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(addressIds, entityId, entityType);
    }

    @Override
    public void revertSoftDeleteByPartiesIds(List<Long> addressIds) {
        partiesRepository.revertSoftDeleteByPartiesIds(addressIds);
    }

    @Override
    public void deleteAdditionalPartiesInPickupDeliveryDetailsByEntityIdAndEntityType(List<Long> partiesIds, List<Long> pickupDeliveryDetailsIds, String pickupDelivery) {
        partiesRepository.deleteAdditionalPartiesInPickupDeliveryDetailsByEntityIdAndEntityType(partiesIds, pickupDeliveryDetailsIds, pickupDelivery);
    }

}
