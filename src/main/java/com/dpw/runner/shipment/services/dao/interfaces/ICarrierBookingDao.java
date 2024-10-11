package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierBooking;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface ICarrierBookingDao {
    CarrierBooking save(CarrierBooking carrierBooking);

    Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable);

    Optional<CarrierBooking> findById(Long id);

    void delete(CarrierBooking carrierBooking);
}
