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
import com.dpw.runner.shipment.services.migration.HelperExecutor;
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
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING_ADDITIONAL_PARTY;
import static com.dpw.runner.shipment.services.migration.utils.MigrationUtil.collectAllProcessedIds;
import static com.dpw.runner.shipment.services.migration.utils.MigrationUtil.futureCompletion;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
@SuppressWarnings({"java:S4144", "java:S1192"})
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

    @Autowired
    private HelperExecutor trxExecutor;
    private final TransactionTemplate transactionTemplate;


    @Override
    public Map<String, Object> restore(Integer tenantId) {
        log.info("Started Booking restore for tenant: {}", tenantId);
        List<CustomerBookingBackupEntity> customerBookingBackupEntities = backupRepository.findCustomerBookingIdsByTenantId(tenantId);
        Set<Long> allBackupBookingIds = customerBookingBackupEntities.stream().map(CustomerBookingBackupEntity::getBookingId)
                .collect(Collectors.toSet());
        Map<String, Object> map = new HashMap<>();
        if (allBackupBookingIds.isEmpty()) {
            map.put("Total Booking :", 0);
            return map;
        }
        Map<Long, String>  failureMap = new HashMap<>();
        customerBookingDao.deleteAdditionalBookingsByBookingIdAndTenantId(allBackupBookingIds, tenantId);
        allBackupBookingIds = customerBookingBackupEntities.stream().filter(ids -> !ids.getIsDeleted())
                .map(CustomerBookingBackupEntity::getBookingId).collect(Collectors.toSet());
        customerBookingDao.revertSoftDeleteByBookingIdAndTenantId(allBackupBookingIds, tenantId);
        map.put("Total Booking :", allBackupBookingIds.size());
        log.info("Count of restore booking ids data : {}", allBackupBookingIds.size());
        List<Future<Long>> bookingQueue = new ArrayList<>();
        allBackupBookingIds.forEach(id -> {

            Future<Long> future = trxExecutor.runInAsyncForBooking(() -> {
                try {
                    v1Service.setAuthContext();
                    TenantContext.setCurrentTenant(tenantId);
                    UserContext.getUser().setPermissions(new HashMap<>());
                    return transactionTemplate.execute(status -> {
                        restoreCustomerBookingData(id, tenantId);
                        return null;
                    });
                } catch (Exception e) {
                    log.error("Booking migration failed [id={}]: {}", id, e.getMessage(), e);
                    migrationUtil.saveErrorResponse(id, Constants.CUSTOMER_BOOKING,
                            IntegrationType.RESTORE_DATA_SYNC, Status.FAILED, Arrays.toString(e.getStackTrace()));
                    failureMap.put(id, e.getMessage());
                    throw new IllegalArgumentException(e);
                } finally {
                    v1Service.clearAuthContext();
                }
            });
            bookingQueue.add(future);
        });

        List<Long> booking = collectAllProcessedIds(bookingQueue);
        map.put("Total Booking Restore : ", booking.size());
        if (!failureMap.isEmpty()) {
            map.put("Failed Bookings Restore details : ", failureMap);
        }
        log.info("Completed Booking restore for tenant: {}", tenantId);
        return map;
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

        List<Long> allPartiesIds = getAllPartiesIds(backupData);

        validateAndRestoreAdditionalPartiesDetails(bookingId, allPartiesIds, backupData);

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
            backupData.getBookingCharges().forEach(charge -> {
                        charge.setId(null);
                        if (Objects.nonNull(charge.getDebtor())) {
                            charge.getDebtor().setId(null);
                        }
                        if (Objects.nonNull(charge.getCreditor())) {
                            charge.getCreditor().setId(null);
                        }
                    }
            );
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

    private static List<Long> getAllPartiesIds(CustomerBooking customerBooking) {
        return Stream.of(
                nullSafeCollectionStream(customerBooking.getContainersList()).flatMap(container -> Stream.of(container.getPickupAddress(), container.getDeliveryAddress())),
                nullSafeCollectionStream(customerBooking.getBookingCharges()).flatMap(bookingCharges -> Stream.of(bookingCharges.getCreditor(), bookingCharges.getDebtor())),
                nullSafeCollectionStream(customerBooking.getAdditionalParties()))
                .flatMap(Function.identity()).filter(Objects::nonNull).map(Parties::getId).filter(Objects::nonNull).distinct().toList();
    }

    public static List<Long> ensureNonEmptyIds(List<Long> ids) {
        return (ids == null || ids.isEmpty()) ? List.of(-1L) : ids;
    }

    private static <T> Stream<T> nullSafeCollectionStream(Collection<T> collection) {
        return (collection == null) ? Stream.empty() : collection.stream();
    }
}



