package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface IBookingCarriageDao {
    BookingCarriage save(BookingCarriage bookingCarriage);
    List<BookingCarriage> saveAll(List<BookingCarriage> bookingCarriageList);
    Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable);
    Optional<BookingCarriage> findById(Long id);
    void delete(BookingCarriage bookingCarriage);
    List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId) throws RunnerException;
    List<BookingCarriage> saveEntityFromShipment(List<BookingCarriage> bookingCarriages, Long shipmentId);
    List<BookingCarriage> saveEntityFromShipment(List<BookingCarriage> bookingCarriages, Long shipmentId, Map<Long, BookingCarriage> oldEntityMap);
    List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId, List<BookingCarriage> oldEntityList) throws RunnerException;

    void deleteAdditionalbookingCarriageByShipmentId(List<Long> bookingCarriageIds, Long shipmentId);

    void revertSoftDeleteBybookingCarriageIdsAndShipmentId(List<Long> bookingCarriageIds, Long shipmentId);
}
