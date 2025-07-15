package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.interfaces.CustomerBookingRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupHandler;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
@RequiredArgsConstructor
public class BookingBackupHandler implements BackupHandler {

    private final ICustomerBookingDao customerBookingDao;
    private final CustomerBookingRepository customerBookingRepository;
    private final ObjectMapper objectMapper;
    private final BookingBackupHandler self;
    private final ThreadPoolTaskExecutor backupExecutor;

    @Override
    public void backup(Integer tenantId) {

        log.info("Starting customer booking backup for tenantId: {}", tenantId);
        List<CustomerBooking> customerBookingListByTenantId = customerBookingDao.findAllByTenantId(tenantId);
        if (customerBookingListByTenantId.isEmpty()) {
            log.info("No customer booking records found for tenantId: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = customerBookingListByTenantId.stream()
                .map(details -> CompletableFuture.runAsync(
                        () -> self.processAndBackupShipment(details, tenantId), backupExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed customer booking backup for tenantId: {}", tenantId);

    }


    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndBackupShipment(CustomerBooking customerBooking, Integer tenantId) {

        try {
            CustomerBookingBackupEntity customerBookingBackupEntity = new CustomerBookingBackupEntity();
            customerBookingBackupEntity.setTenantId(tenantId);
            customerBookingBackupEntity.setBookingId(customerBooking.getId());
            customerBookingBackupEntity.setBookingGuid(customerBooking.getGuid());
            String customerBookingDetails = objectMapper.writeValueAsString(customerBooking);
            customerBookingBackupEntity.setCustomerBookingDetails(customerBookingDetails);
            customerBookingRepository.save(customerBookingBackupEntity);
        } catch (Exception e) {
            log.error("Failed to backup customer booking id: {} for tenant: {}", customerBooking.getId(), tenantId, e);
            throw new BackupFailureException("Failed to backup customer booking id: " + customerBooking.getId(), e);
        }
    }

    @Override
    public void rollback(Integer tenantId) {
        customerBookingRepository.deleteByTenantId(tenantId);
    }
}
