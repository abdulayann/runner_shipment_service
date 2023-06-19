package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IBookingCarriageDao extends JpaRepository<BookingCarriage, Long> {
}
