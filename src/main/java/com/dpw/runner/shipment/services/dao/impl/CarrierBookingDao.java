package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierBookingRepository;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;

public class CarrierBookingDao implements ICarrierBookingDao {
    @Autowired
    private ICarrierBookingRepository carrierBookingRepository;

    @Override
    public CarrierBooking create(CarrierBooking carrierBooking) {
        return carrierBookingRepository.save(carrierBooking);
    }

    @Override
    public Optional<CarrierBooking> findById(Long id) {
        return carrierBookingRepository.findById(id);
    }

    @Override
    public CarrierBooking list(CarrierBooking request) {
        return null;
    }

    @Override
    public CarrierBooking update(Long id, CarrierBooking request) {
        return null;
    }

    @Override
    public void delete(Long id) {

    }
}
