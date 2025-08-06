package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class CustomerBookingBackupHandler {


    private static final int DEFAULT_BATCH_SIZE = 200;
    private final ICustomerBookingDao customerBookingDao;
    private final ObjectMapper objectMapper;
    @Autowired
    @Qualifier("asyncBookingBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncBookingBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;

    @Autowired
    @Lazy
    private CustomerBookingBackupHandler self;

    public List<CustomerBookingBackupEntity> backup(Integer tenantId) {

        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        Set<Long> customerBookingIds = customerBookingDao.findCustomerBookingIdsByTenantId(tenantId);
        log.info("Count of customerBooking Ids : {}", customerBookingIds.size());
        if (customerBookingIds.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return Collections.emptyList();
        }

        List<CompletableFuture<List<CustomerBookingBackupEntity>>> futures = Lists.partition(new ArrayList<>(customerBookingIds),
                        DEFAULT_BATCH_SIZE)
                .stream()
                .map(batch -> CompletableFuture.supplyAsync(
                        wrapWithContext(() -> self.processAndBackupBookingsBatchData(new HashSet<>(batch)), tenantId),
                        asyncBookingBackupHandlerExecutor
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
    public List<CustomerBookingBackupEntity> processAndBackupBookingsBatchData(Set<Long> customerBookingIds) {
        log.info("Processing customer booking batch : ");
        List<CustomerBooking> customerBookings =
                customerBookingDao.findCustomerBookingByIds(customerBookingIds);

        return customerBookings.stream()
                .map(this::mapToBackupEntity)
                .toList();
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
