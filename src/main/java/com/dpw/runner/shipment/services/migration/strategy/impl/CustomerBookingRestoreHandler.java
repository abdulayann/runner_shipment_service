package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.dao.impl.ContainerDao;
import com.dpw.runner.shipment.services.dao.impl.PackingDao;
import com.dpw.runner.shipment.services.dao.impl.PartiesDao;
import com.dpw.runner.shipment.services.dao.impl.ReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.impl.RoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingChargesDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingChargesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING_ADDITIONAL_PARTY;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomerBookingRestoreHandler implements RestoreHandler {

    private final ICustomerBookingBackupRepository backupRepository;
    private final ICustomerBookingDao customerBookingDao;
    // Add other repositories as needed

    @Qualifier("rollbackTaskExecutor")
    private final ThreadPoolTaskExecutor rollbackTaskExecutor;
    private final ObjectMapper objectMapper;
    private final TransactionTemplate transactionTemplate;
    private final IContainerRepository containerDao;
    private final IPackingRepository packingDao;   //method is done.
    private final IRoutingsRepository routingsDao;
    private final IReferenceNumbersRepository referenceNumbersDao;
    private final IBookingChargesRepository bookingChargesDao;
    private final IPartiesRepository partiesDao;

    @Override
    public void restore(Integer tenantId) {

        Set<Long> allBackupBookingIds = backupRepository.findCustomerBookingIdsByTenantId(tenantId);
        if (allBackupBookingIds.isEmpty()) {
            return;
        }

        Set<Long> allOriginalBookingIds = customerBookingDao.findAllCustomerBookingIdsByTenantId(tenantId);

        // Soft delete bookings not present in backup
        Set<Long> idsToDelete = Sets.difference(allOriginalBookingIds, allBackupBookingIds);
        if (!idsToDelete.isEmpty()) {
            customerBookingDao.deleteCustomerBookingIds(idsToDelete);
        }

        Lists.partition(new ArrayList<>(allBackupBookingIds), 100).forEach(batch -> {
            List<CompletableFuture<Void>> futures = batch.stream().map(bookingId ->
                    CompletableFuture.runAsync(() -> restoreBookingWithTransaction(bookingId), rollbackTaskExecutor)).toList();

            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).exceptionally(ex -> {
                log.error("Batch failed", ex);
                return null;
            }).join();
        });
    }


    private void restoreBookingWithTransaction(Long bookingId) {

        transactionTemplate.executeWithoutResult(status -> {
            try {
                CustomerBookingBackupEntity backup = backupRepository.findCustomerBookingDetailsById(bookingId);
                CustomerBooking backupData = objectMapper.readValue(backup.getCustomerBookingDetails(), CustomerBooking.class);
                restoreOneToManyMapping(backupData, bookingId);
                customerBookingDao.save(backupData);
                backupRepository.makeIsDeleteTrueToMarkRestoreSuccessful(backup.getId());
            } catch (Exception e) {
                status.setRollbackOnly();
                log.error("Failed booking {}: {}", bookingId, e.getMessage());
                throw new RestoreFailureException("Rollback failed", e);
            }
        });
    }

    private void restoreOneToManyMapping(CustomerBooking backupData, Long bookingId) {

        List<Long> packingIds = backupData.getPackingList().stream().map(Packing::getId).filter(Objects::nonNull).toList();
        validateAndRestorePackingDetails(bookingId, packingIds, backupData);

        List<Long> referenceNumberIds = backupData.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
        validateAndRestoreReferenceNumberDetails(bookingId, referenceNumberIds, backupData);

        List<Long> routingIds = backupData.getRoutingList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
        validateAndRestoreRoutingDetails(bookingId, routingIds, backupData);

        List<Long> containerIds = backupData.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
        validateAndRestoreContainersDetails(bookingId, containerIds, backupData);

        List<Long> additionalPartiesIds = backupData.getAdditionalParties().stream().map(Parties::getId).filter(Objects::nonNull).toList();
        validateAndRestoreAdditionalPartiesDetails(bookingId, additionalPartiesIds, backupData);

        List<Long> bookingChargeIds = backupData.getBookingCharges().stream().map(BookingCharges::getId).filter(Objects::nonNull).toList();
        validateAndRestoreBookingChargesDetails(bookingId, bookingChargeIds, backupData);

    }

    private void validateAndRestoreBookingChargesDetails(Long bookingId, List<Long> bookingChargeIdsList, CustomerBooking backupData) {
        List<Long> bookingChargeIds = ensureNonEmptyIds(bookingChargeIdsList);
        bookingChargesDao.deleteAdditionalPackingByCustomerBookingId(bookingChargeIds, bookingId);
        bookingChargesDao.revertSoftDeleteByPackingIdsAndBookingId(bookingChargeIds, bookingId);
        bookingChargesDao.saveAll(backupData.getBookingCharges());
    }

    private void validateAndRestoreAdditionalPartiesDetails(Long bookingId, List<Long> partiesIdsList, CustomerBooking backupData) {
        List<Long> partiesIds = ensureNonEmptyIds(partiesIdsList);
        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(partiesIds, bookingId, BOOKING_ADDITIONAL_PARTY);
        partiesDao.revertSoftDeleteByPartiesIds(partiesIds);
        partiesDao.saveAll(backupData.getAdditionalParties());
    }

    private void validateAndRestoreContainersDetails(Long bookingId, List<Long> containersIdsList, CustomerBooking backupData) {
        List<Long> containersIds = ensureNonEmptyIds(containersIdsList);
        containerDao.deleteAdditionalDataByContainersIdsBookingId(containersIds, bookingId);
        containerDao.revertSoftDeleteByContainersIdsAndBookingId(containersIds, bookingId);
        containerDao.saveAll(backupData.getContainersList());
    }

    private void validateAndRestoreRoutingDetails(Long bookingId, List<Long> routingsIdsList, CustomerBooking backupData) {
        List<Long> routingsIds = ensureNonEmptyIds(routingsIdsList);
        routingsDao.deleteAdditionalDataByRoutingsIdsBookingId(routingsIds, bookingId);
        routingsDao.revertSoftDeleteByRoutingsIdsAndBookingId(routingsIds, bookingId);
        routingsDao.saveAll(backupData.getRoutingList());
    }

    private void validateAndRestoreReferenceNumberDetails(Long bookingId, List<Long> referenceNumberIdsList, CustomerBooking backupData) {
        List<Long> referenceNumberIds = ensureNonEmptyIds(referenceNumberIdsList);
        referenceNumbersDao.deleteAdditionalDataByReferenceNumberIdsBookingId(referenceNumberIds, bookingId);
        referenceNumbersDao.revertSoftDeleteByReferenceNumberIdsAndBookingId(referenceNumberIds, bookingId);
        referenceNumbersDao.saveAll(backupData.getReferenceNumbersList());
    }

    private void validateAndRestorePackingDetails(Long bookingId, List<Long> packingIdsList, CustomerBooking backupData) {
        List<Long> packingIds = ensureNonEmptyIds(packingIdsList);
        packingDao.deleteAdditionalPackingByCustomerBookingId(packingIds, bookingId);
        packingDao.revertSoftDeleteByPackingIdsAndBookingId(packingIds, bookingId);
        packingDao.saveAll(backupData.getPackingList());
    }

    public static List<Long> ensureNonEmptyIds(List<Long> ids) {
        return (ids == null || ids.isEmpty()) ? List.of(-1L) : ids;
    }
}



