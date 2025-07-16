package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.interfaces.ShipmentBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

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
    private ShipmentBackupRepository shipmentBackupRepository;

    @Autowired
    private ThreadPoolTaskExecutor asyncExecutor;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    @Lazy
    private ShipmentBackupHandler self;


    @Override
    public void backup(Integer tenantId) {

        log.info("Starting shipment backup for tenantId: {}", tenantId);
        List<Long> shipmentIds = shipmentDao.findShipmentIdsByTenantId(tenantId);
        if (shipmentIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }

        List<List<Long>> batches = Lists.partition(shipmentIds, 100);

        batches.forEach(batch -> {
            List<CompletableFuture<Void>> futures = batch.stream()
                    .map(shipmentId -> CompletableFuture.runAsync(
                            () -> self.processAndBackupShipment(shipmentId),
                            asyncExecutor))
                    .toList();

            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Completed batch of {} shipments", batch.size());
        });
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndBackupShipment(Long shipmentId) {
        try {
            ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();

            ShipmentBackupEntity backupEntity = new ShipmentBackupEntity();
            backupEntity.setTenantId(shipmentDetails.getTenantId());
            backupEntity.setShipmentId(shipmentId);
            backupEntity.setShipmentGuid(shipmentDetails.getGuid());

            String shipmentJson = objectMapper.writeValueAsString(shipmentDetails);
            backupEntity.setShipmentDetail(shipmentJson);

            List<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findByShipmentId(shipmentId);
            String pickupDelivery = objectMapper.writeValueAsString(pickupDeliveryDetails);

            backupEntity.setPickupDeliveryDetail(pickupDelivery);
            shipmentBackupRepository.save(backupEntity);

        } catch (Exception e) {
            log.error("Failed to backup shipment id: {} with exception: ", shipmentId, e);
            throw new BackupFailureException("Failed to backup shipment id: " + shipmentId, e);
        }
    }

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        shipmentBackupRepository.deleteByTenantId(tenantId);
    }
}
