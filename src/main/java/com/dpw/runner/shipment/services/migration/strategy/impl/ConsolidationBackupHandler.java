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
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class ConsolidationBackupHandler {

    private static final int DEFAULT_BATCH_SIZE = 100;
    private final IConsolidationBackupRepository consolidationBackupRepository;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private final ObjectMapper objectMapper;
    @Autowired
    @Qualifier("asyncConsoleBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncConsoleBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;
    private final PlatformTransactionManager transactionManager;
    private final INetworkTransferDao networkTransferDao;


    public void backup(Integer tenantId) {
        long startTime = System.currentTimeMillis();
        log.info("Starting consolidation backup for tenantId: {}", tenantId);
        Set<Long> consolidationIds = consolidationDetailsDao.findConsolidationIdsByTenantId(tenantId);
        log.info("Count of consolidation Ids : {}", consolidationIds.size());
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }

        TransactionTemplate batchTxTemplate = new TransactionTemplate(transactionManager);
        batchTxTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(consolidationIds), DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        wrapWithContext(() -> {
                            try {
                                batchTxTemplate.execute(status -> {
                                    try {
                                        processAndBackupConsolidationsBatchData(new HashSet<>(batch));
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
                        asyncConsoleBackupHandlerExecutor)).toList();

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Consolidation completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    private void processAndBackupConsolidationsBatchData(Set<Long> consolidationIds) {

        log.info("Processing console batch");
        long startTime1 = System.currentTimeMillis();
        List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(consolidationIds);
        log.info("fetch console details: {}", System.currentTimeMillis() - startTime1);


        Map<Long, List<ConsoleShipmentMapping>> consoleMappingsByConsolidationId =
                consoleShipmentMappingDao.findByConsolidationIdsByQuery(consolidationIds)
                        .stream()
                        .collect(Collectors.groupingBy(ConsoleShipmentMapping::getConsolidationId));

        Map<Long, List<NetworkTransfer>> networkTransferMappingsByConsolidationId =
                consolidationIds.stream()
                        .flatMap(consolidationId -> networkTransferDao.findByEntityNTList(consolidationId, Constants.CONSOLIDATION).stream())
                        .collect(Collectors.groupingBy(NetworkTransfer::getEntityId));
        long startTime = System.currentTimeMillis();

        List<ConsolidationBackupEntity> backupEntities = consolidationDetails.stream()
                .map((detail -> mapToBackupEntity(detail,
                        consoleMappingsByConsolidationId.getOrDefault(detail.getId(), Collections.emptyList()),
                        networkTransferMappingsByConsolidationId.getOrDefault(detail.getId(), Collections.emptyList()))))
                .toList();
        log.info("map To Entity time : {}", System.currentTimeMillis() - startTime);

        consolidationBackupRepository.saveAll(backupEntities);
        log.info("Save all : {}", System.currentTimeMillis() - startTime);
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
            long startTime = System.currentTimeMillis();
            consolidationBackupEntity.setConsolidationDetails(objectMapper.writeValueAsString(consolidationDetail));
            log.info("ObjectMapper time : {}", System.currentTimeMillis() - startTime);
            consolidationDetail.setShipmentsList(shipmentList);
            consolidationBackupEntity.setConsoleShipmentMapping(objectMapper.writeValueAsString(consoleMappings));
            consolidationBackupEntity.setNetworkTransferDetails(objectMapper.writeValueAsString(networkTransfers));
            return consolidationBackupEntity;
        } catch (Exception e) {
            log.error("Failed to create backup entity for consolidation id: {}", consolidationDetail.getId(), e);
            throw new BackupFailureException("Error creating backup for consolidation id: " + consolidationDetail.getId(), e);
        }
    }

    @Transactional
    public void rollback(Integer tenantId) {
        consolidationBackupRepository.deleteByTenantId(tenantId);
    }


    public CompletableFuture<Void> backupAsync(Integer tenantId) {
        return CompletableFuture.runAsync(wrapWithContext(() -> backup(tenantId), tenantId),
                asyncConsoleBackupHandlerExecutor);
    }

    private Runnable wrapWithContext(Runnable task, Integer tenantId) {
        return () -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                // Execute with transaction
                new TransactionTemplate(transactionManager).execute(status -> {
                    task.run();
                    return null;
                });
            } finally {
                v1Service.clearAuthContext();
            }
        };
    }
}
