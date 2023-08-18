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
    private ICustomerBookingRepository CustomerBookingRepository;

    @Override
    public CustomerBooking save(CustomerBooking CustomerBooking) {
        return CustomerBookingRepository.save(CustomerBooking);
    }

    @Override
    public Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable) {
        return CustomerBookingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CustomerBooking> findById(Long id) {
        return CustomerBookingRepository.findById(id);
    }

    @Override
    public void delete(CustomerBooking CustomerBooking) {
        CustomerBookingRepository.delete(CustomerBooking);
    }

    public CustomerBooking updateEntityFromShipmentConsole(CustomerBooking CustomerBooking) throws Exception {
        String responseMsg;
        try {
            if (CustomerBooking.getId() != null) {
                long id = CustomerBooking.getId();
                Optional<CustomerBooking> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Customer Booking is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            CustomerBooking = save(CustomerBooking);
            return CustomerBooking;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
