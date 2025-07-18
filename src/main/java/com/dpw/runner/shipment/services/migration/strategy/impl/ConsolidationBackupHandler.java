package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;

import org.springframework.transaction.annotation.Transactional;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class ConsolidationBackupHandler implements BackupHandler {


    @Autowired
    private IConsolidationBackupRepository consolidationBackupRepository;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private ObjectMapper objectMapper;

    @Lazy
    @Autowired
    private ConsolidationBackupHandler self;

    @Autowired
    @Qualifier("asyncExecutor")
    private ThreadPoolTaskExecutor asyncExecutor;


    @Override
    public void backup(Integer tenantId) {

        log.info("Starting consolidation backup for tenantId: {}", tenantId);
        Set<Long> consolidationIds = consolidationDetailsDao.findConsolidationIdsByTenantId(tenantId);
        if (consolidationIds.isEmpty()) {
            log.info("No shipment records found for tenant: {}", tenantId);
            return;
        }
//        processAndBackupConsolidation(consolidationIds.get(0));
        List<CompletableFuture<Void>> futures = consolidationIds.stream()
                .map(consolidationId -> CompletableFuture.runAsync(
                        () -> self.processAndBackupConsolidation(consolidationId),
                        asyncExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed shipment backup for tenant: {}", tenantId);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndBackupConsolidation(Long consolidationId) {
        try {
            long dbStartTime = System.currentTimeMillis();
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolidationId);
            long dbEndTime = System.currentTimeMillis();
            log.info("Time taken to fetch consolidation details: {} ms", dbEndTime-dbStartTime);
            ConsolidationBackupEntity consolidationBackupData = new ConsolidationBackupEntity();
            consolidationBackupData.setTenantId(consolidationDetails.getTenantId());
            consolidationBackupData.setConsolidationId(consolidationDetails.getId());
            consolidationBackupData.setConsolidationGuid(consolidationDetails.getGuid());
            Set<ShipmentDetails> shipmentList = consolidationDetails.getShipmentsList();
            consolidationDetails.setShipmentsList(null);
            long objStartTime = System.currentTimeMillis();
            String response = objectMapper.writeValueAsString(consolidationDetails);
            consolidationBackupData.setConsolidationDetails(response);
            long objEndTime = System.currentTimeMillis();
            log.info("Time taken to save response: {} ms", objEndTime-objStartTime);
            consolidationDetails.setShipmentsList(shipmentList);
            consolidationBackupRepository.save(consolidationBackupData);
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
            throw new BackupFailureException("Failed to backup consolidation id: " + consolidationId, e);
        }
    }

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        consolidationBackupRepository.deleteByTenantId(tenantId);
    }
}
