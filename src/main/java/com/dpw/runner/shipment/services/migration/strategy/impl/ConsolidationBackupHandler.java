package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
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
import org.springframework.transaction.support.TransactionTemplate;

import javax.persistence.EntityManager;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class ConsolidationBackupHandler {

    private static final int DEFAULT_BATCH_SIZE = 200;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private final ObjectMapper objectMapper;
    @Autowired
    @Qualifier("asyncConsoleBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncConsoleBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;
    private final INetworkTransferDao networkTransferDao;
    @Autowired
    @Lazy
    ConsolidationBackupHandler self;

    public List<ConsolidationBackupEntity> backup(Integer tenantId) {

        log.info("Starting consolidation backup for tenantId: {}", tenantId);
        Set<Long> consolidationIds = consolidationDetailsDao.findConsolidationIdsByTenantId(tenantId);
        log.info("Count of consolidation Ids : {}", consolidationIds.size());
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return Collections.emptyList();
        }

        List<CompletableFuture<List<ConsolidationBackupEntity>>> futures =
                Lists.partition(new ArrayList<>(consolidationIds), DEFAULT_BATCH_SIZE)
                        .stream()
                        .map(batch -> CompletableFuture.supplyAsync(wrapWithContext(() ->
                                        self.processAndBackupConsolidationsBatchData(new HashSet<>(batch)), tenantId),
                                asyncConsoleBackupHandlerExecutor
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
    public List<ConsolidationBackupEntity> processAndBackupConsolidationsBatchData(Set<Long> consolidationIds) {
        log.info("Processing consolidation batch: ");
        List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(consolidationIds);


        Map<Long, List<ConsoleShipmentMapping>> consoleMappingsByConsolidationId =
                consoleShipmentMappingDao.findByConsolidationIdsByQuery(consolidationIds)
                        .stream()
                        .collect(Collectors.groupingBy(ConsoleShipmentMapping::getConsolidationId));

        List<NetworkTransfer> allTransfers = networkTransferDao.findByEntityIdsAndEntityType(consolidationIds, Constants.CONSOLIDATION);
        Map<Long, List<NetworkTransfer>> networkTransferMappingsByConsolidationId =
                allTransfers.stream().collect(Collectors.groupingBy(NetworkTransfer::getEntityId));

        return consolidationDetails.stream()
                .map((detail -> mapToBackupEntity(detail,
                        consoleMappingsByConsolidationId.getOrDefault(detail.getId(), Collections.emptyList()),
                        networkTransferMappingsByConsolidationId.getOrDefault(detail.getId(), Collections.emptyList()))))
                .toList();
    }

    private ConsolidationBackupEntity mapToBackupEntity(ConsolidationDetails consolidationDetail, List<ConsoleShipmentMapping> consoleMappings,
                                                        List<NetworkTransfer> networkTransfers) {
        try {
            ConsolidationBackupEntity consolidationBackupEntity = new ConsolidationBackupEntity();
            consolidationBackupEntity.setTenantId(consolidationDetail.getTenantId());
            consolidationBackupEntity.setConsolidationId(consolidationDetail.getId());
            consolidationBackupEntity.setConsolidationGuid(consolidationDetail.getGuid());
            Set<ShipmentDetails> shipmentList = consolidationDetail.getShipmentsList();
            consolidationDetail.setShipmentsList(null);
            consolidationBackupEntity.setConsolidationDetails(objectMapper.writeValueAsString(consolidationDetail));
            consolidationDetail.setShipmentsList(shipmentList);
            consolidationBackupEntity.setConsoleShipmentMapping(objectMapper.writeValueAsString(consoleMappings));
            consolidationBackupEntity.setNetworkTransferDetails(objectMapper.writeValueAsString(networkTransfers));
            return consolidationBackupEntity;
        } catch (Exception e) {
            log.error("Failed to create backup entity for consolidation id: {} : {}", consolidationDetail.getId(), e.getMessage());
            throw new BackupFailureException("Error creating backup for consolidation id: " + consolidationDetail.getId(), e);
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
