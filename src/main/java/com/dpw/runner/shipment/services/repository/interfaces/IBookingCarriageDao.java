package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;

public interface IBookingCarriageDao extends JpaRepository<BookingCarriage, Long> {
    Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable);
    List<BookingCarriage> findByShipmentId(Long shipmentId);
}
