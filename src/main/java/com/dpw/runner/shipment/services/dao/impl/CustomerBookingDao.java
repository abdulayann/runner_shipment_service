package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.validator.custom.validations.CustomerBookingValidations;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Slf4j
public class CustomerBookingDao implements ICustomerBookingDao {

    @Autowired
    CustomKeyGenerator keyGenerator;
    @Autowired
    private ICustomerBookingRepository customerBookingRepository;
    @Autowired
    private ValidatorUtility validatorUtility;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private CustomerBookingValidations customValidations;
    @Autowired
    private CacheManager cacheManager;
    @Autowired
    private ObjectMapper objectMapper;

    @Override
    public CustomerBooking save(CustomerBooking customerBooking) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(customerBooking), Constants.BOOKING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty()) {
            throw new ValidationException(String.join(",", errors));
        }
        CustomerBooking old = null;
        if (customerBooking.getId() != null) {
            Optional<CustomerBooking> oldEntity = findById(customerBooking.getId());
            if (!oldEntity.isPresent()) {
                log.debug("Customer Booking is null for Id {}", customerBooking.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            old = oldEntity.get();
        } else {
            Optional<CustomerBooking> oldEntity = this.findByBookingNumber(customerBooking.getBookingNumber());
            if (oldEntity.isPresent()) {
                log.error("Booking with booking number: {} already exists.", customerBooking.getBookingNumber());
                throw new ValidationException(String.format("Booking with booking number: %s already exists.", customerBooking.getBookingNumber()));
            }
        }
        customValidations.onSave(old, customerBooking); //Custom Validations
        var resp = customerBookingRepository.save(customerBooking);

        // ----- Cache update section -----
        try {
            Cache cache = cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING);
            if (cache != null && resp.getId() != null && resp.getGuid() != null) {
                String idKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, resp.getId());
                String guidKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, resp.getGuid());

                cache.evictIfPresent(idKey);
                cache.evictIfPresent(guidKey);

                log.info("Evicted stale CustomerBooking cache entries after save. [ID Key: {}, GUID Key: {}]", idKey, guidKey);
            } else {
                log.info("CustomerBooking cache eviction skipped. Cache is null or identifiers (ID/GUID) are missing.");
            }
        } catch (Exception e) {
            log.error("Exception occurred while evicting CustomerBooking cache entries. Skipping cache update. Error: {}", e.getMessage(), e);
        }

        return resp;
    }

    @Override
    public Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable) {
        return customerBookingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CustomerBooking> findById(Long id) {
        try {
            return findWithCache(id, CacheConstants.CUSTOMER_BOOKING_ID);
        } catch (Exception e) {
            return customerBookingRepository.findById(id);
        }
    }

    @Override
    public Optional<CustomerBooking> findByGuid(UUID guid) {
        try {
            return findWithCache(guid, CacheConstants.CUSTOMER_BOOKING_GUID);
        } catch (Exception e) {
            return customerBookingRepository.findByGuid(guid);
        }
    }

    private Optional<CustomerBooking> findWithCache(Object keyValue, String keyType) throws JsonProcessingException {
        String primaryKey = keyGenerator.customCacheKey(keyType, keyValue);
        Cache cache = cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING);

        log.info("Looking up CustomerBooking with keyType: {}, keyValue: {}, generatedCacheKey: {}", keyType, keyValue, primaryKey);

        if (cache != null) {
            String customerBookingString = cache.get(primaryKey, String.class);
            if (customerBookingString != null) {
                CustomerBooking customerBooking = objectMapper.readValue(customerBookingString, CustomerBooking.class);
                log.info("Cache hit for key: {}", primaryKey);
                return Optional.of(customerBooking);
            } else {
                log.info("Cache miss for key: {}", primaryKey);
            }
        } else {
            log.warn("Cache '{}' not found in cacheManager", CacheConstants.CUSTOMER_BOOKING);
        }

        // Fallback to DB
        Optional<CustomerBooking> result;
        if (CacheConstants.CUSTOMER_BOOKING_ID.equals(keyType)) {
            result = customerBookingRepository.findById((Long) keyValue);
            log.info("DB lookup by ID: {}, result present: {}", keyValue, result.isPresent());
        } else {
            result = customerBookingRepository.findByGuid((UUID) keyValue);
            log.info("DB lookup by GUID: {}, result present: {}", keyValue, result.isPresent());
        }

        // Cache both ID and GUID
        if (result.isPresent() && cache != null) {
            CustomerBooking booking = result.get();
            String customerBookingString = objectMapper.writeValueAsString(booking);
            String idKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, booking.getId());
            String guidKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, booking.getGuid());

            cache.put(idKey, customerBookingString);
            cache.put(guidKey, customerBookingString);

            log.info("Cached result for keys: [ID key: {}, GUID key: {}]", idKey, guidKey);
        } else if (cache != null) {
            log.info("Empty result. Skipping caching for keyType: {}, keyValue: {}", keyType, keyValue);
        }

        return result;
    }

    @Override
    public Optional<CustomerBooking> findByOrderManagementId(String orderId) {
        return customerBookingRepository.findByOrderManagementId(orderId);
    }

    @Override
    public void delete(CustomerBooking customerBooking) {
        customerBookingRepository.delete(customerBooking);

        Cache cache = cacheManager.getCache(CacheConstants.CUSTOMER_BOOKING);
        if (cache != null) {
            String idKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_ID, customerBooking.getId());
            String guidKey = keyGenerator.customCacheKey(CacheConstants.CUSTOMER_BOOKING_GUID, customerBooking.getGuid());

            cache.evictIfPresent(idKey);
            cache.evictIfPresent(guidKey);

            log.info("Evicted cache entries for CustomerBooking - ID: {}, GUID: {}", customerBooking.getId(), customerBooking.getGuid());
        } else {
            log.info("Cache '{}' not found while deleting CustomerBooking with ID: {}", CacheConstants.CUSTOMER_BOOKING, customerBooking.getId());
        }
    }

    public CustomerBooking updateEntityFromShipmentConsole(CustomerBooking customerBooking) throws RunnerException {
        String responseMsg;
        try {
            if (customerBooking.getId() != null) {
                long id = customerBooking.getId();
                Optional<CustomerBooking> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Customer Booking is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            customerBooking = save(customerBooking);
            return customerBooking;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public Optional<CustomerBooking> findByBookingNumber(String bookingNumber) {
        return customerBookingRepository.findByBookingNumber(bookingNumber);
    }

    @Override
    @Transactional
    public int updateIsPlatformBookingCreated(Long id, Boolean isPlatformBookingCreated){
        return customerBookingRepository.updateIsPlatformBookingCreated(id, isPlatformBookingCreated);
    }

    @Override
    @Transactional
    public int updateBillStatus(Long id, Boolean isBillCreated){
        return customerBookingRepository.updateBillingStatus(id, isBillCreated);
    }

    public Optional<CustomerBooking> findByBookingNumberQuery(String bookingNumber) {
        return customerBookingRepository.findByBookingNumberQuery(bookingNumber);
    }

    @Override
    public Optional<CustomerBooking> findByShipmentReferenceNumber(String shipmentReferenceNumber) {
        return customerBookingRepository.findByShipmentReferenceNumber(shipmentReferenceNumber);
    }

    @Override
    public List<CustomerBooking> findAllByTenantId(Integer tenantId) {
        return customerBookingRepository.findAllByTenantId(tenantId);
    }

}
