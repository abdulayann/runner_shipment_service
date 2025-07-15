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
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
@RequiredArgsConstructor
public class ShipmentBackupHandler implements BackupHandler {

    private final ObjectMapper objectMapper;
    private final IShipmentDao shipmentDao;
    private final IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private final ShipmentBackupRepository shipmentBackupRepository;
    private final ThreadPoolTaskExecutor backupExecutor;
    private final ShipmentBackupHandler self;


    @Override
    public void backup(Integer tenantId) {
        log.info("Starting shipment backup for tenantId: {}", tenantId);
        List<ShipmentDetails> shipmentDetailsListByTenantId = shipmentDao.findAllByTenantId(tenantId);
        if (shipmentDetailsListByTenantId.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }
        List<CompletableFuture<Void>> futures = shipmentDetailsListByTenantId.stream()
                .map(details -> CompletableFuture.runAsync(
                        () -> self.processAndBackupShipment(details, tenantId), backupExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed shipment backup for tenant: {}", tenantId);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndBackupShipment(ShipmentDetails details, Integer tenantId) {
        try {
            ShipmentBackupEntity backupEntity = new ShipmentBackupEntity();
            backupEntity.setTenantId(tenantId);
            backupEntity.setShipmentId(details.getId());
            backupEntity.setShipmentGuid(details.getGuid());
            String shipmentDetails = objectMapper.writeValueAsString(details);
            backupEntity.setShipmentDetail(shipmentDetails);

            List<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findByShipmentId(details.getId());
            String pickupDelivery = objectMapper.writeValueAsString(pickupDeliveryDetails);

            backupEntity.setPickupDeliveryDetail(pickupDelivery);
            shipmentBackupRepository.save(backupEntity);
        } catch (Exception e) {
            log.error("Failed to backup shipment id: {} for tenant: {}", details.getShipmentId(), tenantId, e);
            throw new BackupFailureException("Failed to backup shipment id: " + details.getShipmentId(), e);
        }
    }

    @Override
    public void rollback(Integer tenantId) {
        shipmentBackupRepository.deleteByTenantId(tenantId);
    }
}
