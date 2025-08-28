package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierBookingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

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
    public Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable) {
        return carrierBookingRepository.findAll(spec, pageable);
    }


    @Override
    public CarrierBooking update(Long id, CarrierBooking request) {
        return carrierBookingRepository.save(request);
    }

    @Override
    public void delete(Long id) {
        carrierBookingRepository.deleteById(id);
    }
}
