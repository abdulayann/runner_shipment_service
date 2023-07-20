package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IBookingCarriageDao {
    BookingCarriage save(BookingCarriage bookingCarriage);
    Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable);
    Optional<BookingCarriage> findById(Long id);
    void delete(BookingCarriage bookingCarriage);
    List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId) throws Exception;
    Optional<BookingCarriage> findByGuid(UUID guid);
}
