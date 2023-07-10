package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IBookingCarriageRepository extends JpaRepository<BookingCarriage, Long> {
    Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable);
}
