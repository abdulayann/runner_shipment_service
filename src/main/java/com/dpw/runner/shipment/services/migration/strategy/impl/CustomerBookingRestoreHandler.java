package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.migration.entity.CustomerBookingBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.ICustomerBookingBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
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
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING_ADDITIONAL_PARTY;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class CustomerBookingRestoreHandler implements RestoreServiceHandler {

    private final ICustomerBookingBackupRepository backupRepository;
    private final ICustomerBookingRepository customerBookingDao;
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

    @Autowired
    private MigrationUtil migrationUtil;

    @Override
    public void restore(Integer tenantId) {

        List<CustomerBookingBackupEntity> customerBookingBackupEntities = backupRepository.findCustomerBookingIdsByTenantId(tenantId);
        Set<Long> allBackupBookingIds = customerBookingBackupEntities.stream().map(CustomerBookingBackupEntity::getBookingId)
                .collect(Collectors.toSet());
        log.info("Count of booking ids : {}", allBackupBookingIds.size());
        if (allBackupBookingIds.isEmpty()) {
            return;
        }
        customerBookingDao.deleteAdditionalBookingsByBookingIdAndTenantId(allBackupBookingIds, tenantId);

        allBackupBookingIds =  customerBookingBackupEntities.stream().filter(ids -> !ids.getIsDeleted())
                .map(CustomerBookingBackupEntity::getBookingId).collect(Collectors.toSet());
        customerBookingDao.revertSoftDeleteByBookingIdAndTenantId(allBackupBookingIds, tenantId);

        log.info("Count of no restore booking ids data : {}", allBackupBookingIds.size());
        for (Long bookingId : allBackupBookingIds) {
            try {
                restoreCustomerBookingData(bookingId, tenantId);
            } catch (Exception e) {
                log.error("Failed to restore Booking id: {}", bookingId, e);
                migrationUtil.saveErrorResponse(bookingId, Constants.CUSTOMER_BOOKING, IntegrationType.RESTORE_DATA_SYNC, Status.FAILED, e.getLocalizedMessage());
                throw new IllegalArgumentException(e);
            }
        }
    }

    public void restoreCustomerBookingData(Long bookingId, Integer tenantId) {
        try {
            log.info("Restoration started for customer booking  : {} and tenantId : {}", bookingId, tenantId);
            v1Service.setAuthContext();
            TenantContext.setCurrentTenant(tenantId);
            UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());

            CustomerBookingBackupEntity backup = backupRepository.findCustomerBookingDetailsById(bookingId);
            if (Objects.isNull(backup)) {
                log.info("No Booking records found for booking id : {}", bookingId);
                return;
            }
            CustomerBooking backupData = objectMapper.readValue(backup.getCustomerBookingDetails(), CustomerBooking.class);
            restoreOneToManyMapping(backupData, bookingId);
            customerBookingDao.save(backupData);
            backupRepository.makeIsDeleteTrueToMarkRestoreSuccessful(backup.getId());
            updateCacheByBookingId(bookingId, backupData);
            log.info("Completed processing of customer booking id : {} and tenant id :{}", bookingId, tenantId);

        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(e);
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
        if (!existingIds.isEmpty()) {
            bookingChargesDao.deleteAllById(existingIds);
        }
        if (backupData.getBookingCharges() != null && !backupData.getBookingCharges().isEmpty()) {
            backupData.getBookingCharges().forEach(charge ->
                    charge.setId(null));
            bookingChargesDao.saveAll(backupData.getBookingCharges());
        }
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



