package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingPaymentDao;
import com.dpw.runner.shipment.services.entity.BookingPayment;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingPaymentRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
@Slf4j
public class BookingPaymentDao implements IBookingPaymentDao {
    @Autowired
    private IBookingPaymentRepository bookingPaymentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;


    @Override
    public BookingPayment save(BookingPayment bookingpayment) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingpayment) , Constants.BOOKING_PAYMENT, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (bookingpayment.getId() != null) {
            Optional<BookingPayment> oldEntity = findById(bookingpayment.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Customer Booking is null for Id {}", bookingpayment.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return bookingPaymentRepository.save(bookingpayment);
    }

    @Override
    public Page<BookingPayment> findAll(Specification<BookingPayment> spec, Pageable pageable) {
        return bookingPaymentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingPayment> findById(Long id) {
        return bookingPaymentRepository.findById(id);
    }

    @Override
    public void delete(BookingPayment bookingpayment) {
        bookingPaymentRepository.delete(bookingpayment);
    }
}
