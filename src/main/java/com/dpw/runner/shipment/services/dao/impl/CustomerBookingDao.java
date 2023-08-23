package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@Slf4j
public class CustomerBookingDao implements ICustomerBookingDao {
    @Autowired
    private ICustomerBookingRepository customerBookingRepository;

    @Override
    public CustomerBooking save(CustomerBooking customerBooking) {
        return customerBookingRepository.save(customerBooking);
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
    public void delete(CustomerBooking customerBooking) {
        customerBookingRepository.delete(customerBooking);
    }

    public CustomerBooking updateEntityFromShipmentConsole(CustomerBooking customerBooking) throws Exception {
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
            throw new Exception(e);
        }
    }

    public Optional<CustomerBooking> findByBookingNumber(String bookingNumber) {
        return customerBookingRepository.findByBookingNumber(bookingNumber);
    }
}
