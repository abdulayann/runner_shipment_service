package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierBooking;

import java.util.Optional;

public interface ICarrierBookingDao {
    CarrierBooking create(CarrierBooking request);
    Optional<CarrierBooking> findById(Long id);
    CarrierBooking list(CarrierBooking request);
    CarrierBooking update(Long id, CarrierBooking request);
    void delete(Long id);
}


