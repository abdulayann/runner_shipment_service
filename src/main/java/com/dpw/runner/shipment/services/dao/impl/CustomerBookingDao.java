package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.validator.custom.validations.CustomerBookingValidations;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
@Slf4j
public class CustomerBookingDao implements ICustomerBookingDao {
    @Autowired
    private ICustomerBookingRepository customerBookingRepository;


    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CustomerBookingValidations customValidations;

    @Override
    public CustomerBooking save(CustomerBooking customerBooking) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(customerBooking), Constants.BOOKING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
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
        return resp;
    }

    @Override
    public Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable) {
        return customerBookingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CustomerBooking> findById(Long id) {
        return customerBookingRepository.findById(id);
    }

    @Override
    public Optional<CustomerBooking> findByGuid(UUID id) {
        return customerBookingRepository.findByGuid(id);
    }

    @Override
    public Optional<CustomerBooking> findByOrderManagementId(String orderId) {
        return customerBookingRepository.findByOrderManagementId(orderId);
    }

    @Override
    public void delete(CustomerBooking customerBooking) {
        customerBookingRepository.delete(customerBooking);
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
    public int updateIsPlatformBookingCreated(Long id, Boolean isPlatformBookingCreated) {
        return customerBookingRepository.updateIsPlatformBookingCreated(id, isPlatformBookingCreated);
    }

    @Override
    @Transactional
    public int updateBillStatus(Long id, Boolean isBillCreated) {
        return customerBookingRepository.updateBillingStatus(id, isBillCreated);
    }
}
