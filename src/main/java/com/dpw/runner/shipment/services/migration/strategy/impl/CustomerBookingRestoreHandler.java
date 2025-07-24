package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingChargesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING_ADDITIONAL_PARTY;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomerBookingRestoreHandler implements RestoreServiceHandler {

    @Autowired
    @Lazy
    private CustomerBookingRestoreHandler lazyProxySelf;
    private static final int DEFAULT_BATCH_SIZE = 100;
    private final ICustomerBookingBackupRepository backupRepository;
    private final ICustomerBookingRepository customerBookingDao;
    private final ThreadPoolTaskExecutor rollbackTaskExecutor;
    private final ObjectMapper objectMapper;
    private final IContainerRepository containerDao;
    private final IPackingRepository packingDao;
    private final IRoutingsRepository routingsDao;
    private final IReferenceNumbersRepository referenceNumbersDao;
    private final IBookingChargesRepository bookingChargesDao;
    private final IPartiesRepository partiesDao;
    private final V1ServiceImpl v1Service;
    private final CustomKeyGenerator keyGenerator;
    private final CacheManager cacheManager;

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

        Lists.partition(new ArrayList<>(allBackupBookingIds), DEFAULT_BATCH_SIZE).forEach(batch -> {
            List<CompletableFuture<Void>> futures = batch.stream().map(bookingId ->
                    CompletableFuture.runAsync(() -> {
                        try {
                            lazyProxySelf.restoreCustomerBookingData(bookingId, tenantId);
                        } finally {
                            v1Service.clearAuthContext();
                        }
                    }, rollbackTaskExecutor)).toList();

            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).exceptionally(ex -> {
                log.error("Customer booking failed", ex);
                return null;
            }).join();
        });
    }


    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void restoreCustomerBookingData(Long bookingId, Integer tenantId) {
        try {
            v1Service.setAuthContext();
            TenantContext.setCurrentTenant(tenantId);
            UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());

            CustomerBookingBackupEntity backup = backupRepository.findCustomerBookingDetailsById(bookingId);
            CustomerBooking backupData = objectMapper.readValue(backup.getCustomerBookingDetails(), CustomerBooking.class);
            restoreOneToManyMapping(backupData, bookingId);
            customerBookingDao.save(backupData);
            backupRepository.makeIsDeleteTrueToMarkRestoreSuccessful(backup.getId());
            updateCacheByBookingId(bookingId, backupData);

        } catch (JsonProcessingException e) {
            throw new RestoreFailureException("Failed to deserialize CustomerBooking JSON", e);
        }
    }

    private void updateCacheByBookingId(Long bookingId, CustomerBooking backupData) throws JsonProcessingException {
        Cache cache = cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING);
        if (cache == null) {
            log.warn("Cache '{}' not found for bookingId: {}", CacheConstants.CUSTOMER_BOOKING, bookingId);
            return;
        }

        String customerBookingString = objectMapper.writeValueAsString(backupData);
        String idKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, bookingId);
        String guidKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, bookingId);
        cache.put(idKey, customerBookingString);
        cache.put(guidKey, customerBookingString);
    }

    private void restoreOneToManyMapping(CustomerBooking backupData, Long bookingId) {

        List<Long> additionalPartiesIds = backupData.getAdditionalParties().stream().map(Parties::getId).filter(Objects::nonNull).toList();
        validateAndRestoreAdditionalPartiesDetails(bookingId, additionalPartiesIds, backupData);

        List<Long> packingIds = backupData.getPackingList().stream().map(Packing::getId).filter(Objects::nonNull).toList();
        validateAndRestorePackingDetails(bookingId, packingIds, backupData);

        List<Long> referenceNumberIds = backupData.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
        validateAndRestoreReferenceNumberDetails(bookingId, referenceNumberIds, backupData);

        List<Long> routingIds = backupData.getRoutingList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
        validateAndRestoreRoutingDetails(bookingId, routingIds, backupData);

        validateAndRestoreBookingChargesDetails(bookingId, backupData);

        List<Long> containerIds = backupData.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
        validateAndRestoreContainersDetails(bookingId, containerIds, backupData);

    }

    private void validateAndRestoreBookingChargesDetails(Long bookingId, CustomerBooking backupData) {

        Set<Long> existingIds = bookingChargesDao.findByBookingId(bookingId);
        bookingChargesDao.deleteAllById(existingIds);
        backupData.getBookingCharges().forEach(charge -> {
            if (!existingIds.contains(charge.getId())) {
                charge.setId(null);
            }
        });

        bookingChargesDao.saveAll(backupData.getBookingCharges());
    }

    private void validateAndRestoreAdditionalPartiesDetails(Long bookingId, List<Long> partiesIds, CustomerBooking backupData) {

        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(partiesIds, bookingId, BOOKING_ADDITIONAL_PARTY);
        partiesDao.revertSoftDeleteByPartiesIdsEntityIdAndEntityType(partiesIds, bookingId, BOOKING_ADDITIONAL_PARTY);
        partiesDao.saveAll(backupData.getAdditionalParties());
    }

    private void validateAndRestoreContainersDetails(Long bookingId, List<Long> containersIds, CustomerBooking backupData) {
        containerDao.deleteAdditionalDataByContainersIdsBookingId(containersIds, bookingId);
        containerDao.revertSoftDeleteByContainersIdsAndBookingId(containersIds, bookingId);
        containerDao.saveAll(backupData.getContainersList());
    }

    private void validateAndRestoreRoutingDetails(Long bookingId, List<Long> routingsIds, CustomerBooking backupData) {

        routingsDao.deleteAdditionalDataByRoutingsIdsBookingId(routingsIds, bookingId);
        routingsDao.revertSoftDeleteByRoutingsIdsAndBookingId(routingsIds, bookingId);
        routingsDao.saveAll(backupData.getRoutingList());
    }

    private void validateAndRestoreReferenceNumberDetails(Long bookingId, List<Long> referenceNumberIds, CustomerBooking backupData) {

        referenceNumbersDao.deleteAdditionalDataByReferenceNumberIdsBookingId(referenceNumberIds, bookingId);
        referenceNumbersDao.revertSoftDeleteByReferenceNumberIdsAndBookingId(referenceNumberIds, bookingId);
        referenceNumbersDao.saveAll(backupData.getReferenceNumbersList());
    }

    private void validateAndRestorePackingDetails(Long bookingId, List<Long> packingIds, CustomerBooking backupData) {

        packingDao.deleteAdditionalPackingByCustomerBookingId(packingIds, bookingId);
        packingDao.revertSoftDeleteByPackingIdsAndBookingId(packingIds, bookingId);
        packingDao.saveAll(backupData.getPackingList());
    }
}



