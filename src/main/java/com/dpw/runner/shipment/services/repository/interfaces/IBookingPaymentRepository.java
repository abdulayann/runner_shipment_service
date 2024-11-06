package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.BookingPayment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IBookingPaymentRepository extends MultiTenancyRepository<BookingPayment> {
    List<BookingPayment> findAll();

    Page<BookingPayment> findAll(Specification<BookingPayment> spec, Pageable pageable);
}
