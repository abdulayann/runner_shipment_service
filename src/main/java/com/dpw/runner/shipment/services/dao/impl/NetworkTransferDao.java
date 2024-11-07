package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferRepository;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;

import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.sun.xml.bind.v2.TODO;
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
    @Autowired
    private INetworkTransferRepository networkTransferRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private AuditLogService auditLogService;


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
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
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
    public void deleteAndLog(NetworkTransfer networkTransferEntity, String entityType, Long entityId) {
        networkTransferRepository.delete(networkTransferEntity);
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(networkTransferEntity)
                            .parent(entityType)
                            .parentId(entityId)
                            .operation(DBOperationType.DELETE.name()).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                 InvocationTargetException | NoSuchMethodException | RunnerException e) {
            log.error(e.getMessage());
        }
    }



    public List<NetworkTransfer> saveAll(List<NetworkTransfer> networkTransferEntityList) {
        for(var networkTransferEntity : networkTransferEntityList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(networkTransferEntity), Constants.NETWORK_TRANSFER_ENTITY, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return networkTransferRepository.saveAll(networkTransferEntityList);
    }
}
