package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import lombok.RequiredArgsConstructor;
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomerBookingBackupHandler implements BackupHandler {

    @Autowired
    private ICustomerBookingDao customerBookingDao;
    @Autowired
    private ICustomerBookingBackupRepository customerBookingRepository;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ThreadPoolTaskExecutor asyncBackupHandlerExecutor;

    private final TransactionTemplate transactionTemplate;

    @Autowired
    @Lazy
    private CustomerBookingBackupHandler lazyProxySelf;


    @Autowired
    public CustomerBookingBackupHandler(PlatformTransactionManager transactionManager) {
        this.transactionTemplate = new TransactionTemplate(transactionManager);
        this.transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }

    @Override
    public void backup(Integer tenantId) {
        long start = System.currentTimeMillis();
        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        Set<Long> customerBookingIds = customerBookingDao.findCustomerBookingIdsByTenantId(tenantId);
        log.info("for Booking fetch api : {}", System.currentTimeMillis() - start);
        if (customerBookingIds.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(customerBookingIds), 150).stream()
                .map(batch -> CompletableFuture.runAsync(
                                () -> lazyProxySelf.processAndBackupBookingsBatch(new HashSet<>(batch)),
                                asyncBackupHandlerExecutor)
                        .exceptionally(ex -> {
                            log.error("Async backup failed for booking batch {}: {}", batch, ex.getMessage(), ex);
                            return null;
                        })
                )
                .toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
        log.info("Completed customer booking backup for tenantId: {}", tenantId);
    }

    public void processAndBackupBookingsBatch(Set<Long> customerBookingIds) {

        try {
            transactionTemplate.execute(status -> {
                List<CustomerBooking> customerBookings =
                        customerBookingDao.findCustomerBookingByIds(customerBookingIds);

                List<CustomerBookingBackupEntity> customerBookingEntities = customerBookings.stream()
                        .map(this::mapToBackupEntity)
                        .toList();

                customerBookingRepository.saveAll(customerBookingEntities);
                return true;
            });
        } catch (Exception e) {
            log.error("Failed to backup customer booking : ", e);
            throw new BackupFailureException("Failed to backup customer booking : ", e);
        }
    }

    private CustomerBookingBackupEntity mapToBackupEntity(CustomerBooking customerBooking) {

        try {
            CustomerBookingBackupEntity backupEntity = new CustomerBookingBackupEntity();
            backupEntity.setTenantId(customerBooking.getTenantId());
            backupEntity.setBookingId(customerBooking.getId());
            backupEntity.setBookingGuid(customerBooking.getGuid());
            String customerBookingJson = objectMapper.writeValueAsString(customerBooking);
            backupEntity.setCustomerBookingDetails(customerBookingJson);
            return backupEntity;
        } catch (Exception e) {
            log.error("Failed to create backup entity for customer booking :", e);
            throw new BackupFailureException("Error creating backup for  customer booking : ", e);
        }
    }

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        customerBookingRepository.deleteByTenantId(tenantId);
    }
}
