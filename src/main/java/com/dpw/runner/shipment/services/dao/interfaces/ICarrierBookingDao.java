package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface ICarrierBookingDao {
    CarrierBooking create(CarrierBooking request);
    Optional<CarrierBooking> findById(Long id);
    Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable);
    CarrierBooking update(Long id, CarrierBooking request);
    void delete(Long id);
}


