package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ConsolidationBackupHandler implements BackupHandler {


    @Autowired
    private IConsolidationBackupRepository consolidationBackupRepository;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private ObjectMapper objectMapper;

    @Lazy
    @Autowired
    private ConsolidationBackupHandler lazyProxySelf;

    @Autowired
    private ThreadPoolTaskExecutor asyncBackupHandlerExecutor;

    private final TransactionTemplate transactionTemplate;

    @Autowired
    public ConsolidationBackupHandler(PlatformTransactionManager transactionManager) {
        this.transactionTemplate = new TransactionTemplate(transactionManager);
        this.transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }


    @Override
    public void backup(Integer tenantId) {
        long startTime = System.currentTimeMillis();
        log.info("Starting consolidation backup for tenantId: {}", tenantId);
        Set<Long> consolidationIds = consolidationDetailsDao.findConsolidationIdsByTenantId(tenantId);
        log.info("Consolidation fetch apis : {} ", System.currentTimeMillis() - startTime);
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }
        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(consolidationIds), 100)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        () -> lazyProxySelf.processAndBackupConsolidationsBatch(new HashSet<>(batch)),
                        asyncBackupHandlerExecutor))
                .toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Consolidation completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }


    public void processAndBackupConsolidationsBatch(Set<Long> consolidationIds) {
        try {

            transactionTemplate.execute(status -> {
                List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(consolidationIds);
                Map<Long, List<ConsoleShipmentMapping>> consoleMappingsByConsolidationId =
                        consoleShipmentMappingDao.findByConsolidationIdsByQuery(consolidationIds)
                                .stream()
                                .collect(Collectors.groupingBy(ConsoleShipmentMapping::getConsolidationId));

                List<ConsolidationBackupEntity> backupEntities = consolidationDetails.stream()
                        .map((detail -> mapToBackupEntity(detail, consoleMappingsByConsolidationId.getOrDefault(detail.getId(),
                                Collections.emptyList()))))
                        .toList();

                consolidationBackupRepository.saveAll(backupEntities);
                return true;
            });
        } catch (Exception e) {
            log.error("Failed to backup consolidation with exception: ", e);
            throw new BackupFailureException("Failed to backup consolidation: ", e);
        }
    }

    private ConsolidationBackupEntity mapToBackupEntity(ConsolidationDetails consolidationDetail, List<ConsoleShipmentMapping> consoleMappings) {
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
            return consolidationBackupEntity;
        } catch (Exception e) {
            log.error("Failed to create backup entity for consolidation id: {}", consolidationDetail.getId(), e);
            throw new BackupFailureException("Error creating backup for consolidation id: " + consolidationDetail.getId(), e);
        }
    }

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        consolidationBackupRepository.deleteByTenantId(tenantId);
    }
}
