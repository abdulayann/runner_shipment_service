package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataBackupService;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.hibernate.Session;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.function.Supplier;

@Service
@RequiredArgsConstructor
@Slf4j
@Generated
public class TenantDataBackupServiceImpl implements TenantDataBackupService {

    private static final int BATCH_SIZE = 150;
    private final ShipmentBackupHandler shipmentBackupHandler;
    private final ConsolidationBackupHandler consolidationBackupHandler;
    private final CustomerBookingBackupHandler customerBookingBackupHandler;
    private final PlatformTransactionManager transactionManager;
    @Qualifier("asyncBackupHandlerExecutor")
    private final ThreadPoolTaskExecutor asyncBackupHandlerExecutor;
    private final V1ServiceImpl v1Service;
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public void backupTenantData(Integer tenantId) {

        CompletableFuture<List<CustomerBookingBackupEntity>> customerBookingFuture =
                createBackupFuture(() -> customerBookingBackupHandler.backup(tenantId));
        CompletableFuture<List<ShipmentBackupEntity>> shipmentFuture =
                createBackupFuture(() -> shipmentBackupHandler.backup(tenantId));
        CompletableFuture<List<ConsolidationBackupEntity>> consolidationFuture =
                createBackupFuture(() -> consolidationBackupHandler.backup(tenantId));

        CompletableFuture<Void> allFutures = CompletableFuture.allOf(
                customerBookingFuture, shipmentFuture, consolidationFuture);

        try {
            allFutures.join();
            List<CustomerBookingBackupEntity> customerBookings = customerBookingFuture.join();
            List<ShipmentBackupEntity> shipments = shipmentFuture.join();
            List<ConsolidationBackupEntity> consolidations = consolidationFuture.join();

            saveAllToDb(customerBookings, shipments, consolidations, tenantId);

        } catch (CompletionException e) {
            log.error("Backup failed for tenant {}", tenantId, e);
            throw new BackupFailureException("One or more backup operations failed", e.getCause());
        }
    }

    private void saveAllToDb(List<CustomerBookingBackupEntity> customerBookings,
                             List<ShipmentBackupEntity> shipments,
                             List<ConsolidationBackupEntity> consolidations, Integer tenantId) {

        TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
        transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);
        transactionTemplate.setIsolationLevel(TransactionDefinition.ISOLATION_READ_COMMITTED);

        transactionTemplate.execute(status -> {
            try {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
                batchSaveAll(customerBookings, shipments, consolidations);
                return null;
            } catch (Exception e) {
                status.setRollbackOnly();
                log.error("Batch save failed, rolling back all changes", e);
                throw e;
            } finally {
                v1Service.clearAuthContext();
            }
        });
    }

    private void batchSaveAll(List<CustomerBookingBackupEntity> bookings,
                              List<ShipmentBackupEntity> shipments,
                              List<ConsolidationBackupEntity> consolidations) {

        saveBatch(bookings);
        saveBatch(shipments);
        saveBatch(consolidations);
    }

    /*
        Spring Data’s saveAll() doesn’t clear the persistence context. That leads to:
        High memory usage
        Slower flush times
        Potential OutOfMemoryError on large datasets -> Connections leaks as well
     */
    private <T> void saveBatch(List<T> entities) {

        if (entities == null || entities.isEmpty()) return;
        Session session = entityManager.unwrap(Session.class);
        session.setJdbcBatchSize(BATCH_SIZE); // Set batch size per session
        session.setHibernateFlushMode(org.hibernate.FlushMode.MANUAL); // manual flush for performance

        for (int i = 0; i < entities.size(); i++) {
            entityManager.persist(entities.get(i));

            if ((i + 1) % BATCH_SIZE == 0 || i + 1 == entities.size()) {
                entityManager.flush(); //Improves JDBC Efficiency:
                entityManager.clear(); //Reduces Memory Usage
                log.info("Flushed and cleared {} of {} entities", i + 1, entities.size());
            }
        }
    }

    private <T> CompletableFuture<T> createBackupFuture(Supplier<T> supplier) {
        return CompletableFuture.supplyAsync(supplier, asyncBackupHandlerExecutor);
    }
}
