package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.interfaces.CustomerBookingRepository;
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
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class BookingBackupHandler implements BackupHandler {

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private CustomerBookingRepository customerBookingRepository;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    @Qualifier("asyncExecutor")
    private ThreadPoolTaskExecutor asyncExecutor;

    @Autowired
    @Lazy
    private BookingBackupHandler self;


    @Override
    public void backup(Integer tenantId) {

        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        List<Long> customerBookingIds = customerBookingDao.findCustomerBookingIdsByTenantId(tenantId);
        if (customerBookingIds.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = customerBookingIds.stream()
                .map(id -> CompletableFuture.runAsync(
                        () -> self.processAndBackupShipment(id), asyncExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed customer booking backup for tenantId: {}", tenantId);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndBackupShipment(Long customerBookingId) {

        try {
            CustomerBooking customerBooking;
            customerBooking = customerBookingDao.findById(customerBookingId).get();
            CustomerBookingBackupEntity customerBookingBackupEntity = new CustomerBookingBackupEntity();
            customerBookingBackupEntity.setTenantId(customerBooking.getTenantId());
            customerBookingBackupEntity.setBookingId(customerBooking.getId());
            customerBookingBackupEntity.setBookingGuid(customerBooking.getGuid());
            String customerBookingData = objectMapper.writeValueAsString(customerBooking);
            customerBookingBackupEntity.setCustomerBookingDetails(customerBookingData);
            customerBookingRepository.save(customerBookingBackupEntity);
        } catch (Exception e) {
            log.error("Failed to backup customer booking id: {} ", customerBookingId, e);
            throw new BackupFailureException("Failed to backup customer booking id: " + customerBookingId, e);
        }
    }

    @Override
    @Transactional
    public void rollback(Integer tenantId) {
        customerBookingRepository.deleteByTenantId(tenantId);
    }
}
