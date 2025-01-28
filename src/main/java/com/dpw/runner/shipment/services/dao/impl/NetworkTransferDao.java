package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferRepository;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;

import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

@Repository
@Slf4j
public class NetworkTransferDao implements INetworkTransferDao {

    private final INetworkTransferRepository networkTransferRepository;

    private final ValidatorUtility validatorUtility;

    private final JsonHelper jsonHelper;

    private final AuditLogService auditLogService;

    @Autowired
    public NetworkTransferDao(INetworkTransferRepository networkTransferRepository, ValidatorUtility validatorUtility,
                                  JsonHelper jsonHelper, AuditLogService auditLogService) {
        this.networkTransferRepository = networkTransferRepository;
        this.validatorUtility = validatorUtility;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
    }


    @Override
    public NetworkTransfer save(NetworkTransfer networkTransfer) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(networkTransfer) , Constants.NETWORK_TRANSFER_ENTITY, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (networkTransfer.getId() != null) {
            Optional<NetworkTransfer> oldEntity = findById(networkTransfer.getId());
            if (oldEntity.isEmpty()) {
                log.debug("NetworkTransfer is null for Id {}", networkTransfer.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return networkTransferRepository.save(networkTransfer);
    }


    @Override
    public Page<NetworkTransfer> findAll(Specification<NetworkTransfer> spec, Pageable pageable) {
        return networkTransferRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<NetworkTransfer> findById(Long id) {
        return networkTransferRepository.findById(id);
    }

    @Override
    public Optional<NetworkTransfer> findByGuid(UUID id) {
        return networkTransferRepository.findByGuid(id);
    }

    @Override
    public void delete(NetworkTransfer networkTransferEntity) {
        networkTransferRepository.delete(networkTransferEntity);
    }

    @Override
    public void deleteAndLog(NetworkTransfer networkTransferEntity, String entityType) {
        networkTransferRepository.delete(networkTransferEntity);
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(networkTransferEntity)
                            .parent(entityType)
                            .parentId(networkTransferEntity.getEntityId())
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .operation(DBOperationType.DELETE.name()).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                 InvocationTargetException | NoSuchMethodException | RunnerException e) {
            log.error(e.getMessage());
        }
    }

    @Override
    public void deleteByIdsAndLog(List<Long> networkTransferEntityIds) {
        networkTransferRepository.deleteAllById(networkTransferEntityIds);
    }

    @Override
    public Optional<NetworkTransfer> findByTenantAndEntity(Integer tenantId, Long entityId, String entityType) {
        return networkTransferRepository.findByTenantAndEntity(tenantId, entityId, entityType);
    }

    @Override
    public Optional<NetworkTransfer> findByTenantAndEntityAndJobType(Integer tenantId, Long entityId, String entityType, String jobType) {
        return networkTransferRepository.findByTenantAndEntityAndJobType(tenantId, entityId, entityType, jobType);
    }

    @Override
    public List<NetworkTransfer> findByEntityAndTenantList(Long entityId, String entityType, List<Integer> tenantIds) {
        return networkTransferRepository.findByEntityAndTenantList(entityId, entityType, tenantIds);
    }

    @Override
    public List<NetworkTransfer> getInterConsoleNTList(List<Long> entityIdList, String entityType) {
        return networkTransferRepository.getInterConsoleNTList(entityIdList, entityType);
    }

    public List<NetworkTransfer> saveAll(List<NetworkTransfer> networkTransferEntityList) {
        for(var networkTransferEntity : networkTransferEntityList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(networkTransferEntity), Constants.NETWORK_TRANSFER_ENTITY, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return networkTransferRepository.saveAll(networkTransferEntityList);
    }

    public void updateStatusAndCreatedEntityId(Long id, String status, Long createdEntityId) {
        networkTransferRepository.updateStatusAndCreatedEntityId(id, status, createdEntityId);
    }
}
