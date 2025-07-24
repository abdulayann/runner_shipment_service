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
public class ConsolidationBackupHandler implements BackupServiceHandler {

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Lazy
    @Autowired
    private ConsolidationBackupHandler lazyProxySelf;
    private static final int DEFAULT_BATCH_SIZE = 150;
    private final IConsolidationBackupRepository consolidationBackupRepository;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private final ObjectMapper objectMapper;
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;

    @Override
    public void backup(Integer tenantId) {
        long startTime = System.currentTimeMillis();
        log.info("Starting consolidation backup for tenantId: {}", tenantId);
        Set<Long> consolidationIds = consolidationDetailsDao.findConsolidationIdsByTenantId(tenantId);
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(consolidationIds), DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        () -> {
                            try {
                                v1Service.setAuthContext();
                                TenantContext.setCurrentTenant(tenantId);
                                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                                lazyProxySelf.processAndBackupConsolidationsBatchData(new HashSet<>(batch));
                            } finally {
                                v1Service.clearAuthContext();
                            }
                        },
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

    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void processAndBackupConsolidationsBatchData(Set<Long> consolidationIds) {
        List<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findConsolidationsByIds(consolidationIds);

        Map<Long, List<ConsoleShipmentMapping>> consoleMappingsByConsolidationId =
                consoleShipmentMappingDao.findByConsolidationIdsByQuery(consolidationIds)
                        .stream()
                        .collect(Collectors.groupingBy(ConsoleShipmentMapping::getConsolidationId));

        Map<Long, List<NetworkTransfer>> networkTransferMappingsByConsolidationId =
                consolidationIds.stream()
                        .flatMap(consolidationId -> networkTransferDao.findByEntityNTList(consolidationId, Constants.CONSOLIDATION).stream())
                        .collect(Collectors.groupingBy(NetworkTransfer::getEntityId));

        List<ConsolidationBackupEntity> backupEntities = consolidationDetails.stream()
                .map((detail -> mapToBackupEntity(detail,
                        consoleMappingsByConsolidationId.getOrDefault(detail.getId(),Collections.emptyList()),
                        networkTransferMappingsByConsolidationId.getOrDefault(detail.getId(),Collections.emptyList()))))
                .toList();

        consolidationBackupRepository.saveAll(backupEntities);
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
