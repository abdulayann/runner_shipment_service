package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomerBookingBackupHandler {


    private static final int DEFAULT_BATCH_SIZE = 100;
    private final ICustomerBookingDao customerBookingDao;
    private final ICustomerBookingBackupRepository customerBookingRepository;
    private final ObjectMapper objectMapper;
    @Autowired
    @Qualifier("asyncBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;
    private final PlatformTransactionManager transactionManager;

    public void backup(Integer tenantId) {
        long startTime = System.currentTimeMillis();
        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        Set<Long> customerBookingIds = customerBookingDao.findCustomerBookingIdsByTenantId(tenantId);
        log.info("Count of customerBooking Ids : {}", customerBookingIds.size());
        if (customerBookingIds.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return;
        }

        TransactionTemplate batchTxTemplate = new TransactionTemplate(transactionManager);
        batchTxTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(customerBookingIds), DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.runAsync(
                        wrapWithContext(() -> {
                            try {
                                batchTxTemplate.execute(status -> {
                                    try {
                                        processAndBackupBookingsBatchData(new HashSet<>(batch));
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
                        asyncBackupHandlerExecutor))
                .toList();

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Customer Booking completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    public void processAndBackupBookingsBatchData(Set<Long> customerBookingIds) {

        List<CustomerBooking> customerBookings =
                customerBookingDao.findCustomerBookingByIds(customerBookingIds);

        List<CustomerBookingBackupEntity> customerBookingEntities = customerBookings.stream()
                .map(this::mapToBackupEntity)
                .toList();
        customerBookingRepository.saveAll(customerBookingEntities);
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

    @Transactional
    public void rollback(Integer tenantId) {
        customerBookingRepository.deleteByTenantId(tenantId);
    }


    private Runnable wrapWithContext(Runnable task, Integer tenantId) {
        return () -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                task.run();
            } finally {
                v1Service.clearAuthContext();
            }
        };
    }

    public CompletableFuture<Void> backupAsync(Integer tenantId) {
        return CompletableFuture.runAsync(wrapWithContext(() -> backup(tenantId), tenantId),
                asyncBackupHandlerExecutor);
    }
}
