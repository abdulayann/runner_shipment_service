package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.BookingPayment;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IBookingPaymentDao {
    BookingPayment save(BookingPayment bookingPayment);

    Page<BookingPayment> findAll(Specification<BookingPayment> spec, Pageable pageable);

    Optional<BookingPayment> findById(Long id);

    void delete(BookingPayment bookingPayment);

    List<BookingPayment> updateEntityFromCarrierBooking(List<BookingPayment> packings, Long carrierBookingId) throws RunnerException;
}
