package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.NetworkTransferBackupEntity;
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
import java.util.concurrent.*;
import java.util.function.Supplier;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class NetworkTransferBackupHandler {


    private static final int DEFAULT_BATCH_SIZE = 200;
    private final INetworkTransferDao networkTransferDao;
    private final ObjectMapper objectMapper;
    @Autowired
    @Qualifier("asyncNetworkTransferBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncNetworkTransferBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;

    @Autowired
    @Lazy
    private NetworkTransferBackupHandler self;

    public List<NetworkTransferBackupEntity> backup(Integer tenantId) {

        log.info("Starting network transfer backup for tenantId: {}", tenantId);
        List<Long> networkTransferIds = networkTransferDao.findAllNteForMigrationStatuses(List.of(MigrationStatus.NT_CREATED.name(), MigrationStatus.NT_PROCESSED_FOR_V3.name()), tenantId);
        log.info("Count of networkTransfer Ids : {}", networkTransferIds.size());
        if (networkTransferIds.isEmpty()) {
            log.info("No network transfer records found for tenantId: {}", tenantId);
            return Collections.emptyList();
        }

        List<CompletableFuture<List<NetworkTransferBackupEntity>>> futures = Lists.partition(new ArrayList<>(networkTransferIds),
                        DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.supplyAsync(
                        wrapWithContext(() -> self.processAndBackupNetworkTransfersBatchData(new HashSet<>(batch)), tenantId),
                        asyncNetworkTransferBackupHandlerExecutor
                )).toList();

        CompletableFuture<Void> allFutures = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));

        try {
            allFutures.get(1, TimeUnit.HOURS);
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
            throw new BackupFailureException("Backup processing timed out", e);
        } catch (Exception e) {
            futures.forEach(f -> f.cancel(true));
            throw new BackupFailureException("Backup failed", e instanceof CompletionException ? e.getCause() : e);
        }
    }

    @Transactional(readOnly = true)
    public List<NetworkTransferBackupEntity> processAndBackupNetworkTransfersBatchData(Set<Long> networkTransferIds) {
        log.info("Processing network transfer batch : ");
        List<NetworkTransfer> networkTransfers = networkTransferDao.findNteByIds(new ArrayList<>(networkTransferIds));

        return networkTransfers.stream()
                .map(this::mapToBackupEntity)
                .toList();
    }

    private NetworkTransferBackupEntity mapToBackupEntity(NetworkTransfer networkTransfer) {

        try {
            NetworkTransferBackupEntity backupEntity = new NetworkTransferBackupEntity();
            backupEntity.setTenantId(networkTransfer.getTenantId());
            backupEntity.setNetworkTransferId(networkTransfer.getId());
            backupEntity.setNetworkTransferGuid(networkTransfer.getGuid());
            String networkTransferJson = objectMapper.writeValueAsString(networkTransfer);
            backupEntity.setNetworkTransferDetails(networkTransferJson);
            return backupEntity;
        } catch (Exception e) {
            log.error("Failed to create backup entity for network transfer :", e);
            throw new BackupFailureException("Error creating backup for  network transfer : ", e);
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
