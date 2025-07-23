package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IShipmentBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static java.lang.Boolean.TRUE;

@Service
@Slf4j
public class ShipmentBackupHandler implements BackupHandler {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private IShipmentBackupRepository shipmentBackupRepository;

    @Autowired
    private ThreadPoolTaskExecutor asyncBackupHandlerExecutor;

    @Autowired
    @Lazy
    private ShipmentBackupHandler lazyProxySelf;

    private final TransactionTemplate transactionTemplate;

    @Autowired
    public ShipmentBackupHandler(PlatformTransactionManager transactionManager) {
        this.transactionTemplate = new TransactionTemplate(transactionManager);
        this.transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }


    @Override
    public void backup(Integer tenantId) {

        log.info("Starting shipment backup for tenantId: {}", tenantId);
        long startTime = System.currentTimeMillis();
        Set<Long> shipmentIds = shipmentDao.findShipmentIdsByTenantId(tenantId);

        log.info("Shipment fetch api : {}", System.currentTimeMillis() - startTime);
        if (shipmentIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(shipmentIds), 100)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        () -> lazyProxySelf.processAndBackupShipmentsBatch(new HashSet<>(batch)),
                        asyncBackupHandlerExecutor
                ))
                .toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Shipment completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    public void processAndBackupShipmentsBatch(Set<Long> shipmentIds) {
        try {
            long startTime = System.currentTimeMillis();

            transactionTemplate.execute(status -> {
                List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);

                List<ShipmentBackupEntity> shipmentBackupEntities = shipmentDetails.stream()
                        .map(shipment -> {
                            List<PickupDeliveryDetails> pickupDetails = pickupDeliveryDetailsDao.findByShipmentId(shipment.getId());
                            List<NetworkTransfer> networkTransfers = networkTransferDao.findByEntityNTList(shipment.getId(), Constants.SHIPMENT);
                            return mapToBackupEntity(shipment, pickupDetails, networkTransfers);
                        })
                        .toList();

                shipmentBackupRepository.saveAll(shipmentBackupEntities);
                log.info("Processed {} shipments in {} ms", shipmentBackupEntities.size(), (System.currentTimeMillis() - startTime));
                return true;
            });
        } catch (Exception e) {
            throw new BackupFailureException("Failed to backup shipment: ", e);
        }
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

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        shipmentBackupRepository.deleteByTenantId(tenantId);
    }
}
