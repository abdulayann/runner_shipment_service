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
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static java.lang.Boolean.TRUE;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class ShipmentBackupHandler {

    @Autowired
    @Lazy
    ShipmentBackupHandler self;
    @Autowired
    @Qualifier("asyncShipmentBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncShipmentBackupHandlerExecutor;
    private static final int DEFAULT_BATCH_SIZE = 10;

    private final ObjectMapper objectMapper;
    private final IShipmentDao shipmentDao;
    private final IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private final V1ServiceImpl v1Service;
    private final INetworkTransferDao networkTransferDao;

    public List<ShipmentBackupEntity> backup(Integer tenantId) {
        Set<Long> shipmentIds = shipmentDao.findShipmentIdsByTenantId(tenantId);
        log.info("Count of shipment Ids : {}", shipmentIds.size());

        if (shipmentIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return Collections.emptyList();
        }

        List<CompletableFuture<List<ShipmentBackupEntity>>> futures =
                Lists.partition(new ArrayList<>(shipmentIds), DEFAULT_BATCH_SIZE)
                        .stream()
                        .map(batch -> CompletableFuture.supplyAsync(wrapWithContext(() ->
                                        self.processAndBackupShipmentsBatchData(new HashSet<>(batch)), tenantId),
                                asyncShipmentBackupHandlerExecutor
                        )).toList();

        CompletableFuture<Void> allFutures = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));

        try {
            allFutures.get(2, TimeUnit.HOURS);
            return futures.stream()
                    .map(future -> {
                        try {
                            return future.get();
                        } catch (InterruptedException | ExecutionException e) {
                            throw new CompletionException(e.getCause());
                        }
                    })
                    .flatMap(List::stream)
                    .toList();

        } catch (TimeoutException e) {
            futures.forEach(f -> f.cancel(true));
            throw new BackupFailureException("Shipment Backup processing timed out", e);
        } catch (Exception e) {
            futures.forEach(f -> f.cancel(true));
            throw new BackupFailureException("Shipment Backup failed", e instanceof CompletionException ? e.getCause() : e);
        }
    }

    @Transactional(readOnly = true)
    public List<ShipmentBackupEntity> processAndBackupShipmentsBatchData(Set<Long> shipmentIds) {
        log.info("Shipment batch process .....");
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);
        return shipmentDetails.stream()
                .map(shipment -> {
                    List<PickupDeliveryDetails> pickupDetails = pickupDeliveryDetailsDao.findByShipmentId(shipment.getId());
                    List<NetworkTransfer> networkTransfers = networkTransferDao.findByEntityNTList(shipment.getId(), Constants.SHIPMENT);
                    return mapToBackupEntity(shipment, pickupDetails, networkTransfers);
                })
                .collect(Collectors.toList());
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

    private <T> Supplier<T> wrapWithContext(Supplier<T> task, Integer tenantId) {
        return () -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                return task.get();
            } finally {
                v1Service.clearAuthContext();
            }
        };
    }
}