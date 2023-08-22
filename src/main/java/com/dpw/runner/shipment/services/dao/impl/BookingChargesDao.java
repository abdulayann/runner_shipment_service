package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingChargesDao;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingChargesRepository;
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
public class BookingChargesDao implements IBookingChargesDao {
    @Autowired
    private IBookingChargesRepository bookingChargesRepository;

    @Override
    public BookingCharges save(BookingCharges bookingCharges) {
        return bookingChargesRepository.save(bookingCharges);
    }

    @Override
    public Page<BookingCharges> findAll(Specification<BookingCharges> spec, Pageable pageable) {
        return bookingChargesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingCharges> findById(Long id) {
        return bookingChargesRepository.findById(id);
    }

    @Override
    public void delete(BookingCharges bookingCharges) {
        bookingChargesRepository.delete(bookingCharges);
    }

    public BookingCharges updateEntityFromShipmentConsole(BookingCharges bookingCharges) throws Exception {
        String responseMsg;
        try {
            if (bookingCharges.getId() != null) {
                long id = bookingCharges.getId();
                Optional<BookingCharges> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Charges is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            bookingCharges = save(bookingCharges);
            return bookingCharges;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
