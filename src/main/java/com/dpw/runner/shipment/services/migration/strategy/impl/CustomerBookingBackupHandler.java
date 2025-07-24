package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomerBookingBackupHandler implements BackupServiceHandler {


    @Autowired
    @Lazy
    private CustomerBookingBackupHandler lazyProxySelf;
    private static final int DEFAULT_BATCH_SIZE = 100;
    private final ICustomerBookingDao customerBookingDao;
    private final ICustomerBookingBackupRepository customerBookingRepository;
    private final ObjectMapper objectMapper;
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;


    @Override
    public void backup(Integer tenantId) {
        long startTime = System.currentTimeMillis();
        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        Set<Long> customerBookingIds = customerBookingDao.findCustomerBookingIdsByTenantId(tenantId);
        if (customerBookingIds.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = Lists.partition(new ArrayList<>(customerBookingIds), DEFAULT_BATCH_SIZE).stream()
                .map(batch -> CompletableFuture.runAsync(
                        () -> {
                            try {
                                v1Service.setAuthContext();
                                TenantContext.setCurrentTenant(tenantId);
                                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                                lazyProxySelf.processAndBackupBookingsBatchData(new HashSet<>(batch), tenantId);
                            } finally {
                                v1Service.clearAuthContext();
                            }
                        },
                        asyncBackupHandlerExecutor)).toList();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            log.info("Customer Booking completed : {}", System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("Backup failed for tenant: " + tenantId, e);
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void processAndBackupBookingsBatchData(Set<Long> customerBookingIds, Integer tenantId) {

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

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        customerBookingRepository.deleteByTenantId(tenantId);
    }
}
