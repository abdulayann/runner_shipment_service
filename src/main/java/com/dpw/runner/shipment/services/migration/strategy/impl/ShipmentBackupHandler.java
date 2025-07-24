package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IShipmentBackupRepository;


import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static java.lang.Boolean.TRUE;

@Service
@Slf4j
@RequiredArgsConstructor
public class ShipmentBackupHandler {

    @Autowired
    @Qualifier("asyncBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private static final int DEFAULT_BATCH_SIZE = 100;
    private final ObjectMapper objectMapper;
    private final IShipmentDao shipmentDao;
    private final IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private final IShipmentBackupRepository shipmentBackupRepository;
    private final V1ServiceImpl v1Service;
    private final INetworkTransferDao networkTransferDao;
    private final PlatformTransactionManager transactionManager;


    public void backup(Integer tenantId) {

        log.info("Starting shipment backup for tenantId: {}", tenantId);
        long startTime = System.currentTimeMillis();
        Set<Long> shipmentIds = shipmentDao.findShipmentIdsByTenantId(tenantId);
        log.info("Count of shipment Ids : {}", shipmentIds.size());
        if (shipmentIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }
        TransactionTemplate batchTxTemplate = new TransactionTemplate(transactionManager);
        batchTxTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(shipmentIds), DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        wrapWithContext(() -> {
                            try {
                                batchTxTemplate.execute(status -> {
                                    try {
                                        processAndBackupShipmentsBatchData(new HashSet<>(batch));
                                        return null;
                                    } catch (Exception e) {
                                        status.setRollbackOnly();
                                        throw e;
                                    }
                                });
                            } catch (Exception e) {
                                log.error("Batch processing failed (size {}): {}", batch.size(), e.getMessage());
                                throw new BackupFailureException("Batch processing failed", e);
                            }
                        }, tenantId),
                        asyncBackupHandlerExecutor))
                .toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Shipment completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {} : {}", tenantId, e.getMessage(), e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    private void processAndBackupShipmentsBatchData(Set<Long> shipmentIds) {

        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);
        List<ShipmentBackupEntity> shipmentBackupEntities = shipmentDetails.stream()
                .map(shipment -> {
                    List<PickupDeliveryDetails> pickupDetails = pickupDeliveryDetailsDao.findByShipmentId(shipment.getId());
                    List<NetworkTransfer> networkTransfers = networkTransferDao.findByEntityNTList(shipment.getId(), Constants.SHIPMENT);
                    return mapToBackupEntity(shipment, pickupDetails, networkTransfers);
                })
                .toList();
        shipmentBackupRepository.saveAll(shipmentBackupEntities);
    }

    private ShipmentBackupEntity mapToBackupEntity(ShipmentDetails shipment, List<PickupDeliveryDetails> pickupDetails, List<NetworkTransfer> networkTransfers) {
        try {
            ShipmentBackupEntity shipmentBackupEntity = new ShipmentBackupEntity();
            shipmentBackupEntity.setTenantId(shipment.getTenantId());
            shipmentBackupEntity.setShipmentId(shipment.getId());
            shipmentBackupEntity.setShipmentGuid(shipment.getGuid());
            if (!shipment.getConsolidationList().isEmpty()) {
                shipmentBackupEntity.setIsShipmentAttached(TRUE);
            }
            Set<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
            shipment.setConsolidationList(null);
            shipmentBackupEntity.setShipmentDetail(objectMapper.writeValueAsString(shipment));
            shipment.setConsolidationList(consolidationList);
            String pickupDeliveryJson = objectMapper.writeValueAsString(pickupDetails);
            shipmentBackupEntity.setPickupDeliveryDetail(pickupDeliveryJson);
            String networkTransferJson = objectMapper.writeValueAsString(networkTransfers);
            shipmentBackupEntity.setNetworkTransferDetails(networkTransferJson);
            return shipmentBackupEntity;

        } catch (Exception e) {
            log.error("Failed to create backup entity for shipment id: {}", shipment.getId(), e);
            throw new BackupFailureException("Error creating backup for shipment id: " + shipment.getId(), e);
        }
    }

    public CompletableFuture<Void> backupAsync(Integer tenantId) {
        return CompletableFuture.runAsync(wrapWithContext(() -> backup(tenantId), tenantId),
                asyncBackupHandlerExecutor);
    }

    private Runnable wrapWithContext(Runnable task, Integer tenantId) {
        return () -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                task.run();
            } finally {
                v1Service.clearAuthContext();
            }
        };
    }

    @Transactional
    public void rollback(Integer tenantId) {
        shipmentBackupRepository.deleteByTenantId(tenantId);
    }
}
