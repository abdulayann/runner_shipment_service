package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IBookingChargesDao {

    BookingCharges save(BookingCharges bookingCharges);

    Page<BookingCharges> findAll(Specification<BookingCharges> spec, Pageable pageable);

    Optional<BookingCharges> findById(Long id);

    void delete(BookingCharges bookingCharges);

    BookingCharges updateEntityFromShipmentConsole(BookingCharges bookingCharges) throws RunnerException;

    List<BookingCharges> updateEntityFromBooking(List<BookingCharges> bookingChargesList, Long bookingId) throws RunnerException;

}
