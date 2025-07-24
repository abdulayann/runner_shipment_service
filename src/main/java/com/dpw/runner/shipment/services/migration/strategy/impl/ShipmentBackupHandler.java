package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IShipmentBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupServiceHandler;


import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
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
public class ShipmentBackupHandler implements BackupServiceHandler {

    @Autowired
    @Lazy
    private ShipmentBackupHandler lazyProxySelf;
    private static final int DEFAULT_BATCH_SIZE = 100;
    private final ObjectMapper objectMapper;
    private final IShipmentDao shipmentDao;
    private final IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private final IShipmentBackupRepository shipmentBackupRepository;
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;

    @Override
    public void backup(Integer tenantId) {

        log.info("Starting shipment backup for tenantId: {}", tenantId);
        long startTime = System.currentTimeMillis();
        Set<Long> shipmentIds = shipmentDao.findShipmentIdsByTenantId(tenantId);
        if (shipmentIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(shipmentIds), DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.runAsync(() -> {
                            try {
                                v1Service.setAuthContext();
                                TenantContext.setCurrentTenant(tenantId);
                                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                                lazyProxySelf.processAndBackupShipmentsBatchData(new HashSet<>(batch));
                            } finally {
                                v1Service.clearAuthContext();
                            }
                        },
                        asyncBackupHandlerExecutor
                ))
                .toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Shipment completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {} : {}", tenantId, e.getMessage(), e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void processAndBackupShipmentsBatchData(Set<Long> shipmentIds) {

        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);
        List<ShipmentBackupEntity> shipmentBackupEntities = shipmentDetails.stream()
                .map(shipment -> {
                    List<PickupDeliveryDetails> pickupDetails = pickupDeliveryDetailsDao.findByShipmentId(shipment.getId());
                    return mapToBackupEntity(shipment, pickupDetails);
                })
                .toList();

        shipmentBackupRepository.saveAll(shipmentBackupEntities);
    }

    private ShipmentBackupEntity mapToBackupEntity(ShipmentDetails shipment, List<PickupDeliveryDetails> pickupDetails) {
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
